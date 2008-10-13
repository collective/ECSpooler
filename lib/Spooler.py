# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
from types import StringTypes
import os, sys, time, thread, threading
import xmlrpclib
import traceback
import logging

#from types import StringType, UnicodeType, TupleType, ListType
#from SocketServer import ThreadingMixIn
#from SimpleXMLRPCServer import SimpleXMLRPCServer, SimpleXMLRPCRequestHandler

# local imports
from lib.AbstractServer import AbstractServer
from lib.data.BackendJob import BackendJob
from lib.data.BackendResult import BackendResult

from lib.util.auth import *
from lib.util.SpoolerQueue import SpoolerQueue

from config import JOB_QUEUE_STORAGE
from config import RESULT_QUEUE_STORAGE


class Spooler(AbstractServer):
    """
    The Spooler manages incomming jobs and backends. Each job will be turned 
    over to a backend.
    """

    # resourcestring
    ERROR_AUTH_FAILED = "Authorization failed"
    DEFAULT_DOQUEUE_WAKEUP = 2 # 1 == 1000 ms
    
    def __init__(self, host, port, pwdFile):
        """
        Creates a new spooler instance at the given host and port.
        
        @param: host: host name
        @param: port: port number
        @param: pwdFile: absolute path to password file
        """
        AbstractServer.__init__(self, host, port)
        
        # reset log
        self.log = logging.getLogger('lib.Spooler')

        assert pwdFile and type(pwdFile) in (StringType, UnicodeType),\
            "%s requires a correct 'pwd_file' option" % self._className

        # a dictionary for backends
        self._backends = {}
        # q queue to managing incomming jobs
        self._queue = SpoolerQueue(JOB_QUEUE_STORAGE)

        # a queue for managing backend results
        self._results = SpoolerQueue(RESULT_QUEUE_STORAGE)
        
        # using md5 encrypted passwords in file etc/passwd for authentification
        self._auth = UserAuthMD5(pwdFile)

        # doqueue thread (we will use only one thread)
        self._doqueueThread = None
        self._doqueueThreadExit = False
        self._doqueueThreadLock = thread.allocate_lock()


    def _registerFunctions(self):
        """
        Register all methods/function which can be accessed from a client.
        """
        self._server.register_function(self.addBackend)
        self._server.register_function(self.removeBackend)
        self._server.register_function(self.stopBackend)
        self._server.register_function(self.appendJob)
        self._server.register_function(self.getResults)
        self._server.register_function(self.getResult)
        self._server.register_function(self.getStatus)
        self._server.register_function(self.getBackends)
        self._server.register_function(self.getBackendStatus)
        self._server.register_function(self.getBackendInputFields)
        self._server.register_function(self.getBackendTestFields)
                                       

    def _manageBeforeStart(self):
        """
        Starts a thread which checks the queue for new jobs.
        """
        # start a new thread which runs the doqueue-method
        self.log.info('Starting scheduler thread (%s) ...' % self._className)
        self._doqueueThread = threading.Thread(target=self._doqueue)
        self._doqueueThread.start()
        
        return True


    def _manageBeforeStop(self):
        """
        Stops the scheduler thread and all backends before shutting down
        the spooler itself.
        """
        # stop doqueue thread
        self.log.info('Stopping scheduler thread (%s) ...' % self._className)
        self._doqueueThreadExit = True

        while self._doqueueThread and self._doqueueThread.isAlive():
            self.log.debug('Scheduler thread is still alive, waiting %ds' %
                           self.DEFAULT_DOQUEUE_WAKEUP)
            time.sleep(self.DEFAULT_DOQUEUE_WAKEUP)
        
        self.log.info('Stopping backends ...')
        
        # We make a copy of _backends because it will change during 
        # iteration: backends call removeBackend in their shutdown method,
        # which will change _backends.
        backends = self._backends.copy()
        
        for backend in backends.itervalues():
             self._callBackend(backend['url'], 'shutdown')
             
        # wait a moment so that backends have enough time to unregister 
        time.sleep(0.5)


    def stopBackend(self, authdata, uid):
        """
        Stopps the backend with the given name by invoking the
        shutdown method for this backend. The backend itself then 
        invokes the removeBackend method in the spooler.
        
        @param: authdata: username and password for authentication
        @param: uid: a backend's unique ID
        @return: (code, msg) with
            code == 1, stopping backend succeeded
            code != 1, stopping backend failed; msg contains further information
        """
        if not self._auth.test(authdata, STOP_BACKEND):
            return (-110, self.ERROR_AUTH_FAILED)

        if uid in self._backends:
            chk = self._backends[uid]

            self.log.info("Stopping backend '%s' at '%s'" % (uid, chk['url']))
            self._callBackend(chk["url"], "shutdown")

            return (1, '')
        else:
            return (-120, "Backend '%s' is not registered" % uid)


    def addBackend(self, authdata, backendId, name, version, xmlrpc_url):
        """
        Adds a backend to the spooler's list of available backends.

        Each backend calls the spooler on being started, and tells 
        the server its id, name and its xmlrpc url. The spooler returns
        a specific id, which is a random number created at server
        startup. This id is sent to the backend in each request and
        thus authorizes the request. Only the spooler to which the
        backend is attached can perform requests to this backend.

        @param: authdata: username and password for authentication
        @param: backendId: a backend's unique ID
        @param: name: a backend's name
        @param: version: a backend's version
        @param: xmlr_url: a backend's URL
        @return: a tuple of (code, msg) with
            code == 1, succeeded; msg is the server-id
            code != 1, failed; msg contains further information
        """
        if not self._auth.test(authdata, ADD_BACKEND):
            return (-110, self.ERROR_AUTH_FAILED)

        if backendId in self._backends:
            return (-121, "Backend '%s (%s)' is already registered" % 
                        (name, backendId))

        self._backends[backendId] = {'id': backendId,
                                     'name': name,
                                     'version': version,
                                     'url': xmlrpc_url,
                                     'isBusy': False}

        self.log.info("Backend '%s %s (%s) (%s)' added" % (name, version,
                                                          backendId,
                                                          xmlrpc_url))

        return (1, self._srvId)


    def removeBackend(self, authdata, backendId):
        """
        Removes a backend from the list of available backends in this spooler.

        @param: authdata: username and password for authentication
        @param: backendId: a backend's ID
        @return: (code, msg) with
            code == 1, removing backend succeeded
            code != 1, removing backend failed; msg contains further information
        """
        if not self._auth.test(authdata, REMOVE_BACKEND):
            return (-110, self.ERROR_AUTH_FAILED)

        if backendId in self._backends:
            backend = self._backends[backendId]

            self.log.info("Removing backend '%s (%s)'" % (backendId, backend['url']))
            del self._backends[backendId]

            return (1, "Backend '%s' removed" % backendId)
        else:
            return (-122, "Backend '%s' not found" % backendId)


    def appendJob(self, authdata, jobdata):
        """
        Adds a new CheckJob to the queue

        @param: authdata: username and password for authentication
        @param: jobdata: relevant job data (see also class CheckJob)
        @return: (code, msg) with
            code == 1, enqueue succeeded and msg contains the job id
            code < 0, enqueue failed and msg contains further information
        """

        if not self._auth.test(authdata, APPEND_JOB):
            return (-110, self.ERROR_AUTH_FAILED)

        try:
            # create a new BackenJob instance
            job = BackendJob(data=jobdata)
            # get the backend from the job
            backenId = job['backend']
            
            # if no backend with the backendId is currently registered to the
            # spooler an appropriate message will be returned
            if not backenId in self._backends.keys():
                return (1, "No such backend: %s" % backenId)

            # append the job
            self.log.info("Enqueueing job '%s'" % job.getId())
            self._queue.enqueue(job)

            return (1, job.getId())

        except Exception, e:
            msg = 'Invalid or insufficient data: %s: %s' % (sys.exc_info()[0], e)
            self.log.error(msg)
            return (-150, msg)


    def getResults(self, authdata):
        """
        Returns a dictionary with the results of all performed jobs. Once the 
        results are polled, they are no longer stored.

        @param: authdata: username and password for authentication
        @return: a dictionary
        """

        if not self._auth.test(authdata, GET_RESULT):
            return (-110, self.ERROR_AUTH_FAILED)

        result = {}

        self.log.debug("Dequeuing results for all jobs (%d)" % self._results.getSize())

        while(not self._results.isEmpty()):
            item = self._results.dequeue()
            self.log.info("Returning result for job '%s'" % item.getId())
            result[item.getId()] = item.getData()

        return result


    def getResult(self, authdata, id):
        """
        Returns a dictionary { jobID: QueueItem.getData() } with 
        the result of the performed check job for the given id. 
        Once the result is polled, it is no longer stored.
        
        @param: authdata: username and password for authentication
        @param: id: a valid job ID
        @return: a dictionary with 'id' as key and another dictionary with keys 
                'value', 'message', etc. representing the test results
        """

        if not self._auth.test(authdata, GET_RESULT):
            return {'value':-110, 'message':self.ERROR_AUTH_FAILED}

        result = {}

        self.log.debug("Dequeuing result for job '%s'" % id)
        item = self._results.dequeue(id)

        if item: 
            self.log.info("Returning result for job '%s'" % id)
            result[item.getId()] = item.getData()
        else:
            self.log.info("No result for job '%s'" % id)

        return result


    def getStatus(self, authdata):
        """
        Returns a dict with some status information:
    
        "pid":      the process ID
        "backends": a list of the attached backends
        "queue":    the number of items in the queue
        "results":  the number of cached result data

        @param: authdata: username and password for authentication
        """
        
        self.log.debug('Returning spooler status information')

        if not self._auth.test(authdata, GET_STATUS):
            return (-110, self.ERROR_AUTH_FAILED)
        
        return (1, {
            "pid":      os.getpid(),
            "backends": self._backends.keys(),
            "queue":    self._queue.getSize(),
            "results":  self._results.getSize(),
        })


    def getBackends(self, auth):
        """
        Returns a dict with all currently available backends.
        
        @param: auth: username and password for authentication
        @return: dict with backend names as keys
        """
        
        self.log.debug('Returning all available backends')

        if not self._auth.test(auth, GET_STATUS):
            return (-110, self.ERROR_AUTH_FAILED)

        return self._backends


    def getBackendStatus(self, auth, backendId):
        """
        Returns a dict with status information of a single backend.

        @param: auth: username and password for authentication
        @param: backendId: a backend's unique ID
        """
        
        self.log.debug("Trying to return status information for backend '%s'" % backendId)

        if not self._auth.test(auth, GET_BACKEND_INFO):
            return (-110, self.ERROR_AUTH_FAILED)

        if not self._hasBackend(backendId):
            return (-112, "No such backend: %s" % backendId)

        backend = self._backends.get(backendId)
        return self._callBackend(backend['url'], 'getStatus')

    
    def getBackendInputFields(self, auth, backendId):
        """
        Returns information about additional fields required by this backend.
        
        @param: auth: username and password for authentication
        @param: backendId: a backend's unique ID

        @see: AbstractBackend.getInputFields
        """
        
        self.log.debug("Trying to return input fields for backend '%s'" % backendId)

        if not self._auth.test(auth, GET_BACKEND_INFO):
            return (-110, self.ERROR_AUTH_FAILED)

        if not self._hasBackend(backendId):
            return (-112, "No such backend: %s" % backendId)

        backend = self._backends.get(backendId)
        return self._callBackend(backend['url'], 'getInputFields')


    def getBackendTestFields(self, auth, backendId):
        """
        Returns informationen about test scenarios available by this backend.
        
        @param: auth: username and password for authentication
        @param: backendId: a backend's unique ID

        @see. AbstractBackend.getTestFields
        """
        
        self.log.debug("Trying to return test specs for backend '%s'" % backendId)

        if not self._auth.test(auth, GET_BACKEND_INFO):
            return (-110, self.ERROR_AUTH_FAILED)

        if not self._hasBackend(backendId):
            return (-112, "No such backend: %s" % backendId)

        backend = self._backends.get(backendId)
        return self._callBackend(backend['url'], 'getTestFields')


    def _hasBackend(self, backendId):
        """
        @return: True if a backend with the given backendId is registered, 
                 otherwise False
        """
        if self._backends.has_key(backendId):
            return True
        else:
            msg = "No such backend: %s" % backendId
            self.log.warn(msg)
            return False
        
        
    def _doqueue(self):
        """
        Performs the dispatching of a job to a backend.

        This method is called in a separate thread, which is opened
        and managed by the threading.Thread class. _doqueue() runs in a 
        loop until _doqueue_thread_stop is True.
        """

        while not self._doqueueThreadExit:

            try:
                #self._doqueueThreadLock.acquire()
            
                # is the queue empty?
                if not self._queue.isEmpty():
                    # get next job from the queue and the selected backend
                    job = self._queue.dequeue()
                    backend = self._backends.get(job['backend'], None)
    
                    if not backend:
                        self.log.warn("Job %s can not be executed, no such "
                                     "backend: %s" % (job.getId(), backend['id']))
                                  
                        # enqueue the job so maybe later, if the backend
                        # is available, we can process it
                        self._queue.enqueue(job)
    
                    # backend is busy
                    elif backend.get('isBusy', False):
                        # backend is bussy, try again later
                        self.log.info('Backend %s is busy (%s)' % 
                                      (backend['id'], job,))
                        # try later
                        self._queue.enqueue(job)
                    # process job
                    else:
                        backend['isBusy'] = True
                        
                        result = self._processJob(backend, job)
                        self._results.enqueue(result)
                        
                        self.log.info("Result of job '%s' added to result queue" 
                                     % (result.getId(),))
        
                        backend['isBusy'] = False
                else:
                    #self.log.debug('_doqueue: self._queue is empty');
                    pass
                    
            except Exception, e:
                msg = '%s: %s' % (sys.exc_info()[0], e)
                self.log.error(msg)
            
            # wait a defined time before resuming
            time.sleep(self.DEFAULT_DOQUEUE_WAKEUP)

        # end while loop


    def _processJob(self, backend, job):
        """
        Processing the job by dispatching it to the backend.
        
        @param: backend: a dict with backend attributes (such as id, name, url,)
        @param: job: a job instance
        """
        try:
            self.log.info("Dispatching job '%s' to backend '%s'" % 
                         (job.getId(), job['backend']))

            # invoke remote method call
            rd = self._callBackend(backend['url'], 'execute', job.getData())

            # result from backend must be of type dict
            if type(rd) == DictionaryType:
                # this should be normal case
                result = BackendResult(data=rd, id=job.getId())
            elif type(rd) in StringTypes:
                # probably some kind of error in the backend
                result = BackendResult(-152, rd, id=job.getId())
            else:
                # unexpected or unhandled result
                msg = 'Unexpected result type: %s' % repr(type(rd))
                self.log.error(msg)
                result = BackendResult(-151, msg, id=job.getId())

        except Exception, e:
            msg = '%s: %s' % (sys.exc_info()[0], e)
            self.log.error(msg)

            result = BackendResult(-153, msg, id=job.getId())
        
        #self.log.debug('jobId: %s' % job.getId())
        #self.log.debug('data: %s' % result.getData())
            
        return result


    def _callBackend(self, url, method, *kw, **args):
        """
        Executes an xmlrpc call.
        
        @param: url: backend's URL 
        @param: method: name of method that will be invoked
        @return: a tuple with code and result or an error message
        """
        #self.log.debug('xxx: %s' % repr(kw))
        self.log.debug("xmlrpclib.Server('%s')" % (url))
        
        s = xmlrpclib.Server(url)
        try:
            result = getattr(s, method)({"srv_id": self._srvId}, *kw, **args)
            #self.log.debug('_callBackend: %s' % repr(result))
            return result
        
        #except (socket.error, xmlrpclib.Fault, xmlrpclib.ProtocolError), exc:
        except Exception, e:
            msg = 'Server error: %s: %s (%s)' % (sys.exc_info()[0], e, method)

            self.log.error(''.join(traceback.format_exception(*sys.exc_info())))
            return msg

