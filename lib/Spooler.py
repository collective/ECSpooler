# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2011 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
#
import os
import sys
import time
import thread
import threading
import xmlrpclib
import traceback
import logging

from types import StringTypes
from types import DictionaryType

# local imports
import config

# define a default logger for spooler classes
LOG = logging.getLogger("spooler")


from lib.AbstractServer import AbstractServer
from lib.data.BackendJob import BackendJob
from lib.data.BackendResult import BackendResult

from lib.util import auth
from lib.util import errorcodes
from lib.util.SpoolerQueue import SpoolerQueue

class Spooler(AbstractServer):
    """
    The Spooler manages incomming jobs and backends. Each job will be turned 
    over to a backend.
    """

    # resourcestring
    DEFAULT_DOQUEUE_WAKEUP = 2 # 2 == 2000 ms
    
    def __init__(self, host, port, pwdFile):
        """
        Creates a new spooler instance at the given host and port.
        
        @param: host: host name
        @param: port: port number
        @param: pwdFile: absolute path to password file
        """
        AbstractServer.__init__(self, host, port, LOG)

        assert pwdFile and type(pwdFile) in StringTypes,\
            "%s requires a correct 'pwd_file' option" % self._className

        # a dictionary for backends
        self._backends = {}
        # q queue to managing incomming jobs
        LOG.debug('Using job store %s' % config.JOB_QUEUE_STORAGE)
        self._queue = SpoolerQueue(config.JOB_QUEUE_STORAGE)

        # a queue for managing backend results
        LOG.debug("Using result store %s" % config.RESULT_QUEUE_STORAGE)
        self._results = SpoolerQueue(config.RESULT_QUEUE_STORAGE)
        
        # a queue for managing jobs which cannot executed because backend was busy
        LOG.debug("Using retry store %s" % config.RETRY_QUEUE_STORAGE)
        self._retries = SpoolerQueue(config.RETRY_QUEUE_STORAGE)
        
        self._restoreRetryJobs()

        # using md5 encrypted passwords in file etc/passwd for authentification
        self._auth = auth.UserAuthMD5(pwdFile)

        # doqueue thread (we will use only one thread)
        self._doqueueThread = None
        self._doqueueThreadExit = False
        self._doqueueThreadLock = thread.allocate_lock()
        # TODO:
        #self._doqueueThreadCond = threading.Condition()


    def _registerFunctions(self):
        """
        Register all methods/function which can be accessed from a client.
        """
        self._server.register_function(self.addBackend)
        self._server.register_function(self.removeBackend)
        #self._server.register_function(self.stopBackend)
        self._server.register_function(self.appendJob)
        self._server.register_function(self.getResults)
        self._server.register_function(self.getResult)
        self._server.register_function(self.getStatus)
        self._server.register_function(self.getPID)
        self._server.register_function(self.getBackends)
        self._server.register_function(self.getBackendStatus)
        self._server.register_function(self.getBackendInputFields)
        self._server.register_function(self.getBackendTestFields)
                                       

    def _manageBeforeStart(self):
        """
        Starts a thread which checks the queue for new jobs.
        """
        # start a new thread which runs the doqueue-method
        LOG.info('Starting scheduler thread (%s)...' % self._className)
        self._doqueueThread = threading.Thread(target=self._doqueue)
        self._doqueueThread.start()
        
        return True


    def _manageBeforeStop(self):
        """
        Stops the scheduler thread and all backends before shutting down
        the spooler itself.
        """
        # stop doqueue thread
        LOG.info('Stopping scheduler thread (%s)...' % self._className)
        self._doqueueThreadExit = True

        while self._doqueueThread and self._doqueueThread.isAlive():
            LOG.debug('Scheduler thread is still alive, waiting %ds' %
                           self.DEFAULT_DOQUEUE_WAKEUP)
            time.sleep(self.DEFAULT_DOQUEUE_WAKEUP)
        
        LOG.info('Stopping backends...')
        
        # We make a copy of self._backends because it will change during 
        # iteration: backends call removeBackend in their shutdown method,
        # which will change self._backends.
        backends = self._backends.copy()
        
        # TODO: Don't if SMF is enabled, since the backend would be respawned
        #       automatically by SMF (self healing) - env var SMF_METHOD
        #       is an indicator, that the service is running under SMF
        # just unregister it and perhaps send a message to the backend to
        # give a hint to free resources/unbind
        for grp in backends.itervalues():
            for backend in grp:
                self._callBackend(backend['url'], 'shutdown', backend['id'])
             
        # wait a moment so that backends have enough time to unregister 
        time.sleep(1.0)


#    def stopBackend(self, authdata, uid):
#        """
#        Stopps the backend with the given name by invoking the
#        shutdown method for this backend. The backend itself then 
#        invokes the removeBackend method in the spooler.
#        
#        @param: authdata: username and password for authentication
#        @param: uid: a backend's unique ID
#        @return: (code, msg) with
#            code == 1, stopping backend succeeded
#            code != 1, stopping backend failed; msg contains further information
#        """
#        if not self._auth.test(authdata, auth.STOP_BACKEND):
#            return (-110, self.ERROR_AUTH_FAILED)
#
#        if uid in self._backends:
#            grp = self._backends[uid]
#
#            for backend in grp:
#                LOG.info("Stopping backend '%s' at '%s'" % (uid, backend['url']))
#                # TODO: don't if SMF is enabled - self healing - see above
#                self._callBackend(backend["url"], "shutdown")
#
#            return (1, '')
#        else:
#            return (-120, "Backend '%s' is not registered" % uid)


    # xmlrpc
    def addBackend(self, authdata, backendId, name, version, url):
        """
        Adds a backend to the spooler's list of available backends.

        Each backend calls the spooler on being started, and tells 
        the server its ID, name and URL. The spooler returns
        a specific ID, which is a random number created at server
        startup. This ID is sent to the backend on each request and
        thus authorizes the request. Only the spooler to which the
        backend is attached can perform requests to this backend.

        @param: authdata: username and password for authentication
        @param: backendId: a backend's unique ID
        @param: name: a backend's name
        @param: version: a backend's version
        @param: url: a backend's URL
        @return: this server's ID (for further communication)
        """
        if not self._auth.test(authdata, auth.ADD_BACKEND):
            #return (-110, self.ERROR_AUTH_FAILED)
            raise Exception(errorcodes.ERROR_AUTH_FAILED)

        if backendId not in self._backends:
            # 1st backend of this type
            self._backends[backendId] = list()

        self._backends[backendId].append({'id': backendId,
                                          'name': name,
                                          'version': version,
                                          'url': url,
                                          'isBusy': False})


        LOG.info("Backend '%s %s (%s) (%s)' registered" % 
                 (name, version, backendId, url))

        return self._srvId


    def removeBackend(self, authdata, backendId, url):
        """
        Removes a backend from the list of available backends in this spooler.

        @param: authdata: username and password for authentication
        @param: backendId: a backend's ID
        @return: (code, msg) with
            code == 1, removing backend succeeded
            code != 1, removing backend failed; msg contains further information
        """
        
        if not self._auth.test(authdata, auth.REMOVE_BACKEND):
            #return (-110, self.ERROR_AUTH_FAILED)
            raise Exception(errorcodes.ERROR_AUTH_FAILED)

        if backendId in self._backends:
            grp = self._backends[backendId]

            for backend in grp:
                #backend = self._backends[backendId]

                if url == backend['url']:
                    LOG.info("Unregistering backend '%s %s (%s) (%s)'..." % 
                                  (backend['name'], 
                                   backend['version'], 
                                   backend['id'], 
                                   backend['url']))
                    
                    #self._backends[backendId].remove(backend)
                    grp.remove(backend)
                # end if
            # end for
            
            if len(grp) == 0:
                self._backends.pop(backendId)

            LOG.info("Backend '%s (%s)' unregistered" % (backendId, url))

            return True
        else:
            raise Exception(errorcodes.NO_SUCH_BACKEND)


    def appendJob(self, authdata, jobdata):
        """
        Adds a new CheckJob to the queue

        @param: authdata: username and password for authentication
        @param: jobdata: relevant job data (see also class CheckJob)
        @return: (code, msg) with
            code == 1, enqueue succeeded and msg contains the job id
            code < 0, enqueue failed and msg contains further information
        """

        if not self._auth.test(authdata, auth.PUT):
            #return (-110, self.ERROR_AUTH_FAILED)
            raise Exception(errorcodes.ERROR_AUTH_FAILED)

        try:
            # create a new BackenJob instance
            job = BackendJob(data=jobdata)
            # get the backend from the job
            backenId = job['backend']
            
            # if no backend with the backendId is currently registered to this
            # spooler an appropriate message will be returned
            if not backenId in self._backends.keys():
                LOG.warn("No such backend: %s" % backenId)
                raise Exception(errorcodes.NO_SUCH_BACKEND)

            # append the job
            LOG.info("Enqueueing job '%s'" % job.getId())
            self._queue.enqueue(job)

            return job.getId()

        except Exception, e:
            msg = 'Invalid or insufficient data: %s: %s' % (sys.exc_info()[0], e)
            LOG.error(msg)
            raise Exception(msg)


    def getResults(self, authdata):
        """
        Returns a dictionary with the results of all performed jobs. Once the 
        results are polled, they are no longer stored.

        @param: authdata: username and password for authentication
        @return: a dictionary
        """

        if not self._auth.test(authdata, auth.POP):
            #return (-110, self.ERROR_AUTH_FAILED)
            raise Exception(errorcodes.ERROR_AUTH_FAILED)

        result = {}

        LOG.debug("Dequeuing results for all jobs (%d)" % self._results.getSize())

        while (not self._results.isEmpty()):
            item = self._results.dequeue()
            LOG.info("Returning result for job '%s'" % item.getId())
            result[item.getId()] = item.getData()

        return result


    def getResult(self, authdata, jobId):
        """
        Returns a dictionary { jobID: QueueItem.getData() } with 
        the result of the performed check job for the given ID. 
        Once the result is polled, it is no longer stored.
        
        @param: authdata: username and password for authentication
        @param: jobId: a valid job ID
        @return: a dictionary with 'id' as key and another dictionary with keys 
                'value', 'message', etc. representing the test results
        """

        if not self._auth.test(authdata, auth.POP):
            #return {'value':-110, 'message':self.ERROR_AUTH_FAILED}
            raise Exception(errorcodes.ERROR_AUTH_FAILED)

        result = {}

        LOG.debug("Dequeuing result for job '%s'" % jobId)
        item = self._results.dequeue(jobId)

        if item: 
            LOG.info("Returning result for job '%s'" % jobId)
            result[item.getId()] = item.getData()
        else:
            LOG.info("No result for job '%s'" % jobId)

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
        
        LOG.debug('Returning spooler status information')

        if not self._auth.test(authdata, auth.GET_STATUS):
            #return (-110, self.ERROR_AUTH_FAILED)
            raise Exception(errorcodes.ERROR_AUTH_FAILED)
        
        return {
            #"pid":      os.getpid(),
            "backends": ["%s:%s" % (key, len(self._backends[key])) for key in self._backends],
            "queue":    self._queue.getSize(),
            "results":  self._results.getSize(),
        }


    def getPID(self, authdata):
        """
        Returns the process ID

        @param authdata: username and password for authentication
        @return: current process ID
        """

        LOG.debug('Returning spooler PID')

        if not self._auth.test(authdata, auth.SHUTDOWN):
            #return (-110, self.ERROR_AUTH_FAILED)
            raise Exception(errorcodes.ERROR_AUTH_FAILED)
        
        return os.getpid()



    def getBackends(self, authdata):
        """
        Returns a dict with all currently available backends.
        
        @param: authdata: username and password for authentication
        @return: dict with backend names as keys
        """
        
        LOG.debug('Returning all available backends')

        if not self._auth.test(authdata, auth.GET_STATUS):
            #return (-110, self.ERROR_AUTH_FAILED)
            raise Exception(errorcodes.ERROR_AUTH_FAILED)
        
        result = {}

        for grpId in self._backends:
            # get backend group
            grp = self._backends[grpId]
            
            if len(grp) > 0:
                # if group has at least one backend instance, add it
                # to result
                result[grpId] = grp[0]

        return result


    def getBackendStatus(self, authdata, backendId):
        """
        Returns a dict with status information of a single backend.

        @param: authdata: username and password for authentication
        @param: backendId: a backend's unique ID
        """
        
        LOG.debug("Trying to return status information for backend '%s'" % backendId)

        if not self._auth.test(authdata, auth.GET_BACKEND_INFO):
            #return (-110, self.ERROR_AUTH_FAILED)
            raise Exception(errorcodes.ERROR_AUTH_FAILED)

        if not self._hasBackend(backendId):
            #return (-112, "No such backend: %s" % backendId)
            raise Exception(errorcodes.NO_SUCH_BACKEND)

        grp = self._backends.get(backendId)
        
        if len(grp) > 0:
            # if group has at least one backend instance, we will use the first entry
            backend = grp[0]
            return self._callBackend(backend['url'], 'getStatus', backend['id'])
    
        else:
            #return (-112, "No such backend: %s" % backendId)
            raise Exception(errorcodes.NO_SUCH_BACKEND)
        
    
    def getBackendInputFields(self, authdata, backendId):
        """
        Returns information about additional fields required by this backend.
        
        @param: authdata: username and password for authentication
        @param: backendId: a backend's unique ID

        @see: AbstractBackend.getInputFields
        """
        
        LOG.debug("Trying to return input fields for backend '%s'" % backendId)

        if not self._auth.test(authdata, auth.GET_BACKEND_INFO):
            #return (-110, self.ERROR_AUTH_FAILED)
            raise Exception(errorcodes.ERROR_AUTH_FAILED)

        if not self._hasBackend(backendId):
            #return (-112, "No such backend: %s" % backendId)
            raise Exception(errorcodes.NO_SUCH_BACKEND)

        grp = self._backends.get(backendId)
        
        if len(grp) > 0:
            # if group has at least one backend instance, add it
            # to result
            backend = grp[0]
            return self._callBackend(backend['url'], 'getInputFields')
    
        else:
            #return (-112, "No such backend: %s" % backendId)
            raise Exception(errorcodes.NO_SUCH_BACKEND)



    def getBackendTestFields(self, authdata, backendId):
        """
        Returns informationen about test scenarios available by this backend.
        
        @param: authdata: username and password for authentication
        @param: backendId: a backend's unique ID

        @see. AbstractBackend.getTestFields
        """
        
        LOG.debug("Trying to return test specs for backend '%s'" % backendId)

        if not self._auth.test(authdata, auth.GET_BACKEND_INFO):
            #return (-110, self.ERROR_AUTH_FAILED)
            raise Exception(errorcodes.ERROR_AUTH_FAILED)

        if not self._hasBackend(backendId):
            #return (-112, "No such backend: %s" % backendId)
            raise Exception(errorcodes.NO_SUCH_BACKEND)

        grp = self._backends.get(backendId)

        if len(grp) > 0:
            # if group has at least one backend instance, add it
            # to result
            backend = grp[0]
            return self._callBackend(backend['url'], 'getTestFields')
    
        else:
            #return (-112, "No such backend: %s" % backendId)
            raise Exception(errorcodes.NO_SUCH_BACKEND)


    def _hasBackend(self, backendId):
        """
        @return: True if a backend with the given backendId is registered, 
                 otherwise False
        """
        if self._backends.has_key(backendId):
            return True
        else:
            msg = "No such backend: %s" % backendId
            LOG.warn(msg)
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
                    
                    backendId = job['backend']
                    
                    if backendId in self._backends:
                        # backend groups
                        grp = self._backends[backendId]
                        
                        backend = None

                        for b in grp:
                            # backend busy?
                            if not b.get('isBusy', False):
                                backend = b
                                break
                        # end for
                        
                        if backend == None:
                            # backend is bussy, try again later
                            LOG.info('Backend %s is busy (%s)' % 
                                          (backendId, job.getId(),))
                            # try later
                            #self._queue.enqueue(job)
                            
                            # put this job in retry-queue
                            LOG.info("Adding job to retry queue: %s" % job.getId())
                            self._retries.enqueue(job)
                            
                    
                        # process job
                        else:
                            # dont block - continue to service other backends
                            threading.Thread(target=self._processJob, args=(backend, job)).start()
                       
                        # end if
                    else:
                        LOG.warn("Job %s can not be executed, no such "
                                     "backend: %s" % (job.getId(), backendId))
                                  
                        # enqueue the job so maybe later, if the backend
                        # is available, we can process it
                        self._queue.enqueue(job)
                    
                    # end if
    
                else:
                    #LOG.debug('_doqueue: self._queue is empty');
                    pass
                    
            except Exception, e:
                msg = '%s: %s' % (sys.exc_info()[0], e)
                LOG.error(msg)
            
            # wait a defined time before resuming
            time.sleep(self.DEFAULT_DOQUEUE_WAKEUP)

        # end while loop


    def _processJob(self, backend, job):
        """
        Processing the job by dispatching it to the backend.
        
        @param: backend: a dict with backend attributes (such as id, name, url,)
        @param: job: a job instance
        """

        backend['isBusy'] = True

        try:
            LOG.info("Dispatching job '%s' to backend '%s'" % 
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
                LOG.error(msg)
                result = BackendResult(-151, msg, id=job.getId())

        except Exception, e:
            msg = '%s: %s' % (sys.exc_info()[0], e)
            LOG.error(msg)

            result = BackendResult(-153, msg, id=job.getId())
        
        self._results.enqueue(result)
        LOG.info("Result of job '%s' added to result queue" % (result.getId(),))

        backend['isBusy'] = False

        # move jobs from retry queue back to default queue
        self._restoreRetryJobs()

        #LOG.debug('jobId: %s' % job.getId())
        #LOG.debug('data: %s' % result.getData())
            
        return result


    def _restoreRetryJobs(self):
        """
        Move jobs from retry queue back to default queue
        """
        
        size = self._retries.getSize()
        
        if size > 0:
            LOG.debug("Moving %d job(s) back from 'retry queue' to 'default queue'." 
                           % size)
            
            while not self._retries.isEmpty():
                item = self._retries.dequeue()
                self._queue.enqueue(item)


    def _callBackend(self, url, method, *kw, **args):
        """
        Executes an xmlrpc call.
        
        @param: url: backend's URL 
        @param: method: name of method that will be invoked
        @return: a tuple with code and result or an error message
        """
        #LOG.debug('xxx: %s' % repr(kw))
        #LOG.debug("xmlrpclib.Server('%s')" % (url))
        
        s = xmlrpclib.Server(url)
        try:
            result = getattr(s, method)({"srv_id": self._srvId}, *kw, **args)
            #LOG.debug('_callBackend: %s' % repr(result))
            return result
        
        #except (socket.error, xmlrpclib.Fault, xmlrpclib.ProtocolError), exc:
        except Exception, e:
            LOG.error(traceback.format_exc())
            msg = 'Server error: %s: %s (%s)' % (sys.exc_info()[0], e, method)
            raise Exception(msg)

