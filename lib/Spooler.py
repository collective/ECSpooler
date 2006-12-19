# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.
import os, sys, time, thread, threading
import xmlrpclib
import traceback
import logging

from types import StringType, UnicodeType, TupleType, ListType
#from SocketServer import ThreadingMixIn
#from SimpleXMLRPCServer import SimpleXMLRPCServer, SimpleXMLRPCRequestHandler

# local imports
#from config import 
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
        """
        AbstractServer.__init__(self, host, port)

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
        self._server.register_function(self.popResults)
        self._server.register_function(self.popResult)
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
        logging.info('Starting scheduler thread (%s)...' % self._className)
        self._doqueueThread = threading.Thread(target=self._doqueue)
        self._doqueueThread.start()
        
        return True


    def _manageBeforeStop(self):
        """
        """
        # stop doqueue thread
        logging.info('Stopping scheduler thread (%s)...' % self._className)
        self._doqueueThreadExit = True

        while self._doqueueThread and self._doqueueThread.isAlive():
            logging.debug('Scheduler thread is still alive, waiting %ds.' %
                           self.DEFAULT_DOQUEUE_WAKEUP)
            time.sleep(self.DEFAULT_DOQUEUE_WAKEUP)
        
        logging.info('Stopping backends...')
        
        # We make a copy of _backends because it will change during 
        # iteration: backends call removeBackend in their shutdown method,
        # which will change _backends.
        backends = self._backends.copy()
        
        for backend in backends.itervalues():
             self._callBackend(backend['url'], 'shutdown')


    def stopBackend(self, authdata, uid):
        """
        Stopps the backend with the given name by invoking the
        shutdown method for this backend. The backend itself then 
        invokes the removeBackend method in the spooler.
        
        @param authdata username and password for authentication
        @param id a backend's unique ID consisting of id and version information
        @return (code, msg) with
            code == 0, stopping backend succeeded
            code != 0, stopping backend failed; msg contains further information
        """
        if not self._auth.test(authdata, STOP_BACKEND):
            return (-2, self.ERROR_AUTH_FAILED)

        if uid in self._backends:
            chk = self._backends[uid]

            logging.info("Stopping backend '%s' at '%s'" % (uid, chk['url']))
            self._callBackend(chk["url"], "shutdown")

            return (0, '')
        else:
            return (-1, "Backend '%s' is not registered" % uid)


    def addBackend(self, authdata, id, name, version, xmlrpc_url):
        """
        Adds a backend to the spooler's list of available backends.

        Each backend calls the spooler on being started, and tells 
        the server its id, name and its xmlrpc url. The server returns
        a specific id, which is a random number created at server
        startup. This id is sent to the checker in each request and
        thus authorizes the request. Only the server to which the
        checker is attached can perform requests to this checker.

        @return a tuple of (code, msg) with
            code == 0, succeeded; msg is the server-id
            code != 0, failed; msg contains further information
        """
        if not self._auth.test(authdata, ADD_BACKEND):
            return (-2, self.ERROR_AUTH_FAILED)

        # construct the ID for this backend ng of id and version
        backendId = self._getBackendId(id, version)
        
        if backendId in self._backends:
            return (-1, "Backend '%s (%s)' is already registered" % 
                        (name, backendId))

        self._backends[backendId] = {'id': id, 'name': name, 'version': version,
                               'url': xmlrpc_url, 'isBusy': False}

        logging.info("Backend '%s %s (%s) (%s)' added" % (name, version, id, xmlrpc_url))

        return (0, self._srvId)


    def removeBackend(self, authdata, id, version):
        """
        Removes a backend from the list of available backends in this spooler.

        @param authdata username and password for authentication
        @param id a backend's ID
        @param version a backend's version

        @return (code, msg) with
            code == 0, removing backend succeeded
            code != 0, removing backend failed; msg contains further information
        """
        if not self._auth.test(authdata, REMOVE_BACKEND):
            return (-2, self.ERROR_AUTH_FAILED)

        uid = self._getBackendId(id, version)

        if uid in self._backends:
            backend = self._backends[uid]

            logging.info("Removing backend '%s (%s)'" % (uid, backend['url']))
            del self._backends[uid]

            return (0, '')
        else:
            return (-1, "Backend '%s' not found" % uid)


    def appendJob(self, authdata, data):
        """
        Adds a new CheckJob to the queue

        @param authdata username and password for authentication
        @param jobdata relevant job data (see also class CheckJob)
        @return (code, msg) with
            code == 0, enqueue succeeded and msg contains the job id
            code != 0, enqueue failed and msg contains further information
        """

        if not self._auth.test(authdata, APPEND_JOB):
            return (-2, self.ERROR_AUTH_FAILED)

        try:
            # create a new BackenJob instance
            job = BackendJob(data)
            # get the backend from the job
            backenId = job['backend']
            
            # if no backend with the backendId is currently registered to the
            # spooler an appropriate message will be returned
            if not backenId in self._backends.keys():
                return (1, "No such backend: %s" % backenId)

            # append the job
            logging.info("Enqueueing job '%s'" % job.getId())
            self._queue.enqueue(job)

            return (0, job.getId())

        except Exception, e:
            msg = 'Invalid or insufficient data: %s: %s' % (sys.exc_info()[0], e)
            logging.error(msg)
            return (-1, msg)


    def popResults(self, authdata):
        """
        Returns a dictionary with the results of all performed jobs. Once the 
        results are polled, they are no longer stored.

        @param authdata username and password for authentication
        """

        if not self._auth.test(authdata, POP_RESULT):
            return (-2, self.ERROR_AUTH_FAILED)

        result = {}

        for item in self._results.dequeue():
            logging.info("Dequeuing result of job '%s'" % item.getId())
            result[item.getId()] = item.getData()

        return result


    def popResult(self, authdata, id):
        """
        Returns a dictionary { job id: QueueItem.getData() } 
        with result of the performed check job for the given id. 
        Once the result is poped, it is no longer stored.
        
        {'aUniqueJobID': {
            'id': 'same as aUniqueJobID',
            'code': an integer,
            'value': 'a result string'
            },
        }

        @param authdata username and password for authentication
        @param id a valid job ID
        @return a dictionary with id as key and another dictionary with keys 
                code and value representing the check result
        """

        if not self._auth.test(authdata, POP_RESULT):
            return (-2, self.ERROR_AUTH_FAILED)


        result = {}
        item = self._results.dequeue(id)

        if item: 
            logging.info("Dequeuing result of job '%s'" % item.getId())
            result[item.getId()] = item.getData()
        
        return result


    def getStatus(self, authdata):
        """
        Returns a dict with some status information:
    
        "pid":      the process ID
        "backends": a list of the attached backends
        "queue":    the number of items in the queue
        "results":  the number of cached result data

        @param authdata username and password for authentication
        """
        
        if not self._auth.test(authdata, GET_STATUS):
            return (-2, self.ERROR_AUTH_FAILED)

        return (1, {
            "pid":      os.getpid(),
            "backends": self._backends.keys(),
            "queue":    self._queue.getSize(),
            "results":  self._results.getSize(),
        })


    def getBackends(self, auth):
        """
        Returns a dict with all checker backends
        @param auth username and password for authentication
        """
        
        if not self._auth.test(auth):
            return (-2, self.ERROR_AUTH_FAILED)

        return self._backends


    def getBackendStatus(self, auth, uid):
        """
        Returns a dict with all checker backends

        @param auth username and password for authentication
        @param uid a backend's unique ID
        """
        
        if not self._auth.test(auth):
            return (0, self.ERROR_AUTH_FAILED)

        if not self._backends.has_key(uid):
            return (0, "No such backend: %s" % uid)

        backend = self._backends.get(uid)
        return self._callBackend(backend['url'], 'getStatus')

    
    def getBackendInputFields(self, auth, uid):
        """
        @param auth username and password for authentication
        @param uid a backend's unique ID

        @see AbstractBackend.getInputFields
        """
        
        if not self._auth.test(auth):
            return (0, self.ERROR_AUTH_FAILED)

        if not self._backends.has_key(uid):
            return (0, "No such backend: %s" % uid)

        backend = self._backends.get(uid)
        return self._callBackend(backend['url'], 'getInputFields')


    def getBackendTestFields(self, auth, uid):
        """
        @param auth username and password for authentication
        @param uid a backend's unique ID

        @see AbstractBackend.getTestFields
        """
        
        if not self._auth.test(auth):
            return (0, self.ERROR_AUTH_FAILED)

        if not self._backends.has_key(uid):
            return (0, "No such backend: %s" % uid)

        backend = self._backends.get(uid)
        return self._callBackend(backend['url'], 'getTestFields')
        
        
    def _doqueue(self):
        """
        Performs the dispatching of CheckJobs to the backends.

        This method is called in a separate thread, which is opened
        and managed by the threading.Thread class. _doqueue() runs in a 
        loop until _doqueue_thread_stop is True.
        """

        while not self._doqueueThreadExit:

            try:
                #self._doqueueThreadLock.acquire()
            
                # is the queue empty?
                if not self._queue.isEmpty():
                    # get next job from the queue
                    job = self._queue.dequeue()
    
                    # get backend
                    backend = self._backends.get(job['backend'], None)
    
                    if not backend:
                        logging.warn("Job %s can not be executed, no such "
                                     "backend: %s" % (job.getId(), backend['id']))
                                  
                        # enqueue the job so maybe later, if the backend
                        # is available, we can process it
                        self._queue.enqueue(job)
    
                    # is backend busy?
                    elif backend.get('isBusy', False):
                        # backend is bussy, try again later
                        logging.debug('Backend %s is busy (%s)' % (backend['id'], job,))
                        self._queue.enqueue(job)
    
                    else:
                        try:
                            # dispatch the job to the backend
                            backend['isBusy'] = True
        
                            logging.info("Dispatching job '%s' to backend '%s'" % 
                                         (job.getId(), job['backend']))
        
                            # invoke remote method call
                            r = self._callBackend(backend['url'], 'execute', job.getData())
        
                            if type(r) not in [TupleType, ListType]:
                                r = (-5, r)
        
                            result = BackendResult({'code':r[0], 'value':r[1]}, job.getId())
        
                            logging.debug("Adding result of job '%s' to results "
                                          "queue" % job.getId())
                            self._results.enqueue(result)
                            
                        except Exception, e:
                            msg = '%s: %s' % (sys.exc_info()[0], e)
                            logging.error(msg)

                            result = BackendResult({'code':-42, 'value':str(e)})
                            self._results.enqueue(result)
        
                        backend['isBusy'] = False
                else:
                    #logging.debug('_doqueue: self._queue is empty');
                    pass
                    
            except Exception, e:
                msg = '%s: %s' % (sys.exc_info()[0], e)
                logging.error(msg)
            
            # wait a defined time before resuming
            time.sleep(self.DEFAULT_DOQUEUE_WAKEUP)

        # end while loop


    def _callBackend(self, url, method, *kw, **args):
        """
        Executes an xmlrpc call to a backend.
        
        @param url backend's URL 
        @param method name of method that will be invoked
        @return An tupel with code and result or an error message
        """
        #logging.debug('xxx: %s' % repr(kw))
        #logging.debug('xxx: %s' % repr(args))
        
        s = xmlrpclib.Server(url)
        try:
            result = getattr(s, method)({"srv_id": self._srvId}, *kw, **args)
            #logging.debug('_callBackend: %s' % repr(result))
            return result
        
        #except (socket.error, xmlrpclib.Fault, xmlrpclib.ProtocolError), exc:
        except Exception, e:
            msg = 'Server error: %s: %s (%s)' % (sys.exc_info()[0], e, method)

            logging.error(''.join(traceback.format_exception(*sys.exc_info())))
            return msg


    def _getBackendId(self, id, version):
        """
        Returns a new ID consisting of id and version. This method should be 
        even then a request is made for a backend with id and version 
        information.
        """
        #return '%s-%s' % (id, version)
        return id


# -- some testing -------------------------------------------------------------
#if __name__ == "__main__":
#    """
#    """
#    try:
#        cpid = os.fork()
#    except AttributeError, aerr:
#        print('os.fork not defined - skipping')
#        cpid = 0
#
#    if cpid == 0:
#        tS = Spooler(socket.getfqdn(), 5050, 
#                      os.path.join(os.path.dirname(__file__), 
#                                    '..', 'etc', 'passwd'))
#        tS.start()
#
#    else:
#        time.sleep(1)
#        print 'pid=', cpid