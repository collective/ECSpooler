# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.
import os, time, random, md5, thread, threading, signal
import socket, xmlrpclib
import logging

from types import IntType, StringType
#from SocketServer import ThreadingMixIn
from SimpleXMLRPCServer import SimpleXMLRPCServer, SimpleXMLRPCRequestHandler

# local imports
from data import *
from util import *
from config import *

from AbstractServer import AbstractServer


class Spooler(AbstractServer):
    """
    """

    # resourcestring
    ERROR_AUTH_FAILED = "Authorization failed"
    DEFAULT_DOQUEUE_WAKEUP = 1 # 1000 ms

    def __init__(self, host, port, pwdFile):
        """
        """
        AbstractServer.__init__(self, host, port)

        assert pwdFile and type(pwdFile) == StringType,\
            "%s requires a correct 'pwd_file' option." % self._classNAME

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


    def _manageBeforeStart(self):
        """
        """
        # start a new thread which runs the doqueue-method
        logging.info('Starting scheduler thread (%s)...' % self._classNAME)
        self._doqueueThread = threading.Thread(target=self.doqueue)
        #self._doqueueThread.setDaemon(True)
        self._doqueueThread.start()


    def _manageBeforeStop(self):
        """
        """
        # stop doqueue thread
        logging.info('Stopping scheduler thread (%s)...' % self._classNAME)
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


    def addBackend(self, authdata, id, name, xmlrpc_url):
        """
        Announces a Backend to this ECSpooler.

        Each checker calls the ECSpooler on being started, and tells 
        the server its id, name and its xmlrpc url. The server returns
        a specific id, which is a random number created at server
        startup. This id is sent to the checker in each request and
        thus authorizes the request. Only the server to which the
        checker is attached can perform requests to this checker.

        @return (code, msg) with
            code == 0, addBackend succeeded, and msg is a identifying server-id
            code != 0, addBackend failed, and msg contains further information
        """
        if not self._auth.test(authdata, self._auth.ADD_CHECKER):
            return (-2, self.ERROR_AUTH_FAILED)

        
        if id in self._backends:
            return (-1, "There already is a backend by that id (%s)." % id)

        self._backends[id] = {"name":name, "url":xmlrpc_url, "current_job":0}
        logging.info("Added backend '%s' (%s) at '%s'" % (id, name, xmlrpc_url))

        return (0, self._srvID)


    def stopBackend(self, authdata, name):
        """Stopping the backend with the given name by invoking the
        shutdown method for this backend. The backend itself then 
        invokes the removeBackend method.
        
        @param authdata username and password for authentication
        @param name id/name of a registered backend
        @return (code, msg) with
            code == 0, stopping backend succeeded
            code != 0, stopping backend failed; msg contains further information
        """
        if not self._auth.test(authdata, self._auth.ADD_CHECKER):
            return (-2, self.ERROR_AUTH_FAILED)

        if name in self._backends:
            chk = self._backends[name]

            logging.info("Stopping backend '%s' at '%s'" % (name, chk['url']))
            self._callChecker(chk["url"], "shutdown")

            return (0, '')
        else:
            return (-1, "A backend called '%s' is not registered." % name)


    def removeBackend(self, authdata, name):
        """
        Removes a backend from the list of available backends in this spooler.

        @param authdata username and password for authentication
        @param name id/name of a registered backend
        @return (code, msg) with
            code == 0, removing backend succeeded
            code != 0, removing backend failed; msg contains further information
        """
        if not self._auth.test(authdata, self._auth.ADD_CHECKER):
            return (-2, self.ERROR_AUTH_FAILED)

        if name in self._backends:
            chk = self._backends[name]

            logging.info("Removing backend '%s' at '%s'" % (name, chk['url']))
            del self._backends[name]

            return (0, '')
        else:
            return (-1, "Backend '%s' could not be found." % name)


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
            job = checkjob.CheckJob(jobdata, createID = 1)

            if not job["checker"] in self._backends.keys():
                return (1, "No such backend: %s" % job["checker"])

            # enqueue the job
            logging.info("Enqueueing job %s" % job["id"])
            self._queue.enqueue(job)

            return (0, job["id"])

        except exceptions.InvalidDataException, exc:
            return (-1, "Invalid job data: %s (%s)" % (repr(exc), repr(jobdata)))

        return (-10, "Implementation error")


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

        return {
            "pid":      os.getpid(),
            "backends": self._backends.keys(),
            "queue":    self._queue.getSize(),
            "results":  self._results.getSize(),
        }


    def getBackends(self, authdata):
        """
        Returns a dict with all checker backends
        @param authdata username and password for authentication
        """
        
        if not self._auth.test(authdata, self._auth.GET_STATUS):
            return (-2, self.ERROR_AUTH_FAILED)

        return self._backends


    def doqueue(self):
        """
        Performs the dispatching of CheckJobs to the backends.

        This method is called in a separate thread, which is opened
        and managed by the threading.Thread class. doqueue() runs in a 
        loop until _doqueue_thread_stop is True.
        """
        
        #doqueueCounter = 0
        
        while not self._doqueueThreadExit:
            
            #self._doqueueThreadLock.acquire()

            # is the queue empty?
            if not self._queue.isEmpty():
                # get next job from the queue
                job = self._queue.dequeue()

                # get backend
                chk = self._backends.get(job["checker"])
                if not chk:
                    logging.warn("Job %s can't be executed, no such "
                                 "backend: %s" % (job["id"], job["checker"]))
                              
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
                                 (job["id"], job["checker"]))

                    # invoke remote method call
                    res = self._callChecker(chk["url"], "execute", 
                                             job.getData())

                    self._results.addResult(res, job["id"])

                    chk["current_job"] = 0
            else:
                #logging.debug('doqueue: self._queue is empty.');
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
        @return a CheckResult object with return code and message
        """
        s = xmlrpclib.Server(url)
        try:
            retval = getattr(s, method)({"srv_id": self._srvID}, *kw, **args)
            return checkresult.CheckResult(retval[0], retval[1])
        
        #except (socket.error, xmlrpclib.Fault, xmlrpclib.ProtocolError), err:
        except Exception, err:
            if method != 'shutdown':
                msg = "Server error: %s. Method was '%s'" % (str(err), method)
                logging.error(msg)

            return checkresult.CheckResult(-5, msg)



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