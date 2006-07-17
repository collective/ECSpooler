# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.
import os, time, random, md5, thread, threading, signal
import socket, xmlrpclib
import traceback , logging

from types import IntType, StringType, UnicodeType, TupleType, ListType
#from SocketServer import ThreadingMixIn
from SimpleXMLRPCServer import SimpleXMLRPCServer, SimpleXMLRPCRequestHandler

# local imports
from data import *
from util import *
from config import *

from AbstractServer import AbstractServer


class Spooler(AbstractServer):
    """
    Spooler is the central class in the whole setup.
    """

    # resourcestring
    ERROR_AUTH_FAILED = "Authorization failed"
    DEFAULT_DOQUEUE_WAKEUP = 1 # 1000 ms

    def __init__(self, host, port, pwdFile):
        """
        """
        AbstractServer.__init__(self, host, port)

        assert pwdFile and type(pwdFile) in (StringType, UnicodeType),\
            "%s requires a correct 'pwd_file' option." % self._className

        # define dictionary for backends and lists for jobs and results
        self._backends = {}
        self._queue = checkjobQueue.CheckJobQueue()
        self._results = checkresultCache.CheckResultCache()

        # using md5 encrypted passwords in file etc/passwd for authentification
        self._auth = auth.UserAuthMD5(pwdFile)

        # doqueue thread (we will use only one thread)
        self._doqueueThread = None
        self._doqueueThreadExit = False
        self._doqueueThreadLock = thread.allocate_lock()


    def _registerFunctions(self):
        """
        """
        self._server.register_function(self.addBackend)
        self._server.register_function(self.removeBackend)
        self._server.register_function(self.stopBackend)
        self._server.register_function(self.enqueue)
        self._server.register_function(self.pollResults)
        self._server.register_function(self.pollResult)
        self._server.register_function(self.getStatus)
        self._server.register_function(self.getBackends)
        self._server.register_function(self.getBackendStatus)
        self._server.register_function(self.getBackendInputFields)
        self._server.register_function(self.getBackendTestFields)
                                       

    def _manageBeforeStart(self):
        """
        """
        # start a new thread which runs the doqueue-method
        logging.info('Starting scheduler thread (%s)...' % self._className)
        self._doqueueThread = threading.Thread(target=self._doqueue)
        #self._doqueueThread.setDaemon(True)
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
        
        # Make a copy of _backends because it will change during 
        # iteration: backends call removeBackend in their shutdown method,
        # which will change _backends.
        backends = self._backends.copy()
        
        for chk in backends.itervalues():
             self._callChecker(chk['url'], 'shutdown')


    def stopBackend(self, authdata, uid):
        """Stopping the backend with the given name by invoking the
        shutdown method for this backend. The backend itself then 
        invokes the removeBackend method.
        
        @param authdata username and password for authentication
        @param id a backend's unique ID consisting of id and version information
        @return (code, msg) with
            code == 0, stopping backend succeeded
            code != 0, stopping backend failed; msg contains further information
        """
        if not self._auth.test(authdata, self._auth.ADD_CHECKER):
            return (-2, self.ERROR_AUTH_FAILED)

        if uid in self._backends:
            chk = self._backends[uid]

            logging.info("Stopping backend '%s' at '%s'" % (uid, chk['url']))
            self._callChecker(chk["url"], "shutdown")

            return (0, '')
        else:
            return (-1, "Backend '%s' is not registered." % uid)


    def addBackend(self, authdata, id, name, version, xmlrpc_url):
        """
        Add a backend to the spooler's list of available backends.

        Each backend calls the spooler on being started, and tells 
        the server its id, name and its xmlrpc url. The server returns
        a specific id, which is a random number created at server
        startup. This id is sent to the checker in each request and
        thus authorizes the request. Only the server to which the
        checker is attached can perform requests to this checker.

        @return (code, msg) with
            code == 0, addBackend succeeded; msg is a dentifying server-id
            code != 0, addBackend failed; msg contains further information
        """
        if not self._auth.test(authdata, self._auth.ADD_CHECKER):
            return (-2, self.ERROR_AUTH_FAILED)

        # construct a new unique id consisting of id and version
        uid = self._getUniqueId(id, version)
        
        if uid in self._backends:
            return (-1, "Backend '%s (%s)' is already registered." % 
                        (name, uid))

        self._backends[uid] = {'id': id, 'name': name, 'version': version,
                               'url': xmlrpc_url, 'current_job': 0}

        logging.info("Backend '%s (%s) (%s)' added." % (name, uid, xmlrpc_url))

        return (0, self._srvID)


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
        if not self._auth.test(authdata, self._auth.ADD_CHECKER):
            return (-2, self.ERROR_AUTH_FAILED)

        uid = self._getUniqueId(id, version)

        if uid in self._backends:
            backend = self._backends[uid]

            logging.info("Removing backend '%s (%s)'" % (uid, backend['url']))
            del self._backends[uid]

            return (0, '')
        else:
            return (-1, "Backend '%s' not found." % uid)


    def enqueue(self, authdata, jobdata):
        """
        Adds a CheckJob to the queue

        @param authdata username and password for authentication
        @param jobdata relevant job data (see also class CheckJob)
        @return (code, msg) with
            code == 0, enqueue succeeded and msg contains the job id
            code != 0, enqueue failed and msg contains further information
        """

        if not self._auth.test(authdata, self._auth.ENQUEUE):
            return (-2, self.ERROR_AUTH_FAILED)

        try:
            
            job = checkjob.CheckJob(jobdata, createID=1)
            
            uid = job['backend']

            if not uid in self._backends.keys():
                return (1, "No such backend: %s" % uid)

            # enqueue the job
            logging.info("Enqueueing job %s" % job['id'])
            self._queue.enqueue(job)

            return (0, job['id'])

        except exceptions.InvalidDataException, exc:
            return (-1, "Invalid job data: %s (%s)" % 
                        (repr(exc), repr(jobdata)))

        logging.error('Internal error.')
        return (-10, 'Internal error.')


    def pollResults(self, authdata):
        """
        Returns a dictionary { job id: CheckResult.getData() } 
        with results of all performed check jobs. Once
        results are polled, they are no longer stored.

        @param authdata username and password for authentication
        """

        if not self._auth.test(authdata, self._auth.POLL_RESULTS):
            return (-2, self.ERROR_AUTH_FAILED)

        rescache = self._results.pollResults()
        obj = {}
        for k,v in rescache.iteritems():
            obj[k] = v.getData()

        return obj


    def pollResult(self, authdata, id):
        """
        Returns a dictionary { job id: CheckResult.getData() } 
        with result of the performed check job for the given id. 
        Once the result is polled, it is no longer stored.

        @param authdata username and password for authentication
        @param id ID for a job
        """

        if not self._auth.test(authdata, self._auth.POLL_RESULTS):
            return (-2, self.ERROR_AUTH_FAILED)

        res = self._results.pollResult(id)
        obj = {}
        
        if res != None:
            obj[id] = res.getData()

        return obj


    def getStatus(self, authdata):
        """
        Returns a dict with some status information:
    
        "pid":      the process ID
        "backends": a list of the attached backends
        "queue":    the number of items in the queue
        "results":  the number of cached result data

        @param authdata username and password for authentication
        """
        
        if not self._auth.test(authdata, self._auth.GET_STATUS):
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
        return self._callChecker(backend['url'], 'getStatus')

    
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
        return self._callChecker(backend['url'], 'getInputFields')


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
        return self._callChecker(backend['url'], 'getTestFields')
        
        
    def _doqueue(self):
        """
        Performs the dispatching of CheckJobs to the backends.

        This method is called in a separate thread, which is opened
        and managed by the threading.Thread class. _doqueue() runs in a 
        loop until _doqueue_thread_stop is True.
        """

        while not self._doqueueThreadExit:
            
            #self._doqueueThreadLock.acquire()

            # is the queue empty?
            if not self._queue.isEmpty():
                # get next job from the queue
                job = self._queue.dequeue()

                # get backend
                chk = self._backends.get(job['backend'])
                if not chk:
                    logging.warn("Job %s can't be executed, no such "
                                 "backend: %s" % (job['id'], job['backend']))
                              
                    # enqueue the job so that, if the backend
                    # is evailable, we can check it
                    self._queue.enqueue(job)

                # is backend busy?
                elif chk["current_job"] != 0:
                    logging.debug('Backend is busy (%s)' % job)
                    self._queue.enqueue(job)

                else:
                    # hand over checkjob to backend
                    chk["current_job"] = job

                    logging.info("Dispatching job %s to backend '%s'" % 
                                 (job["id"], job['backend']))

                    # invoke remote method call
                    r = self._callChecker(chk['url'], "execute", job.getData())

                    if type(r) not in [TupleType, ListType]:
                        r = (-5, r)

                    result = checkresult.CheckResult(r[0], r[1])

                    #logging.debug('result: %s' % str(result.getData()))
                    self._results.addResult(result, job['id'])

                    chk["current_job"] = 0
            else:
                #logging.debug('_doqueue: self._queue is empty.');
                pass
                    
            #self._doqueueThreadLock.release()

            # wait a defined time before resuming
            time.sleep(self.DEFAULT_DOQUEUE_WAKEUP)

        # end while loop


    def _callChecker(self, url, method, *kw, **args):
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
            return getattr(s, method)({"srv_id": self._srvID}, *kw, **args)
            #logging.debug('_callChecker: %s' % retval)
        
        #except (socket.error, xmlrpclib.Fault, xmlrpclib.ProtocolError), exc:
        except Exception, e:
            msg = 'Server error: %s: %s (%s).' % (sys.exc_info()[0], e, method)

            #logging.error(msg)
            logging.error(''.join(traceback.format_exception(*sys.exc_info())))
            return msg


    def _getUniqueId(self, id, version):
        """
        Returns a new ID consisting of id and version. This method should be 
        even then a request is made for a backend with id and version 
        information.
        """
        return '%s-%s' % (id, version)



# -- some testing -------------------------------------------------------------
if __name__ == "__main__":
    """
    """
    try:
        cpid = os.fork()
    except AttributeError, aerr:
        print('os.fork not defined - skipping.')
        cpid = 0

    if cpid == 0:
        tS = Spooler(socket.getfqdn(), 5050, 
                      os.path.join(os.path.dirname(__file__), 
                                    '..', 'etc', 'passwd'))
        tS.start()

    else:
        time.sleep(1)
        print 'pid=', cpid