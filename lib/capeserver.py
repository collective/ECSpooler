import os, srv
from data import *
from util import *

import time
import socket, xmlrpclib
import threading
import signal

import random,md5

# resourcestring
ERROR_AUTH_FAILED = "Authorization failed"


class CapeServer(srv.XMLRPCServer):
    """
    The Cape XMLRPC Server.

    future development proposals:
    - auth backend for LDAP
    - each checker has its own invocation thread
    - authorization-per-checker (i.e. checker usage may be 
      restricted by auth information)

    """

    DEFAULT_DOQUEUE_WAKEUP = 0.1 # 100 ms


    def __init__(self, options):

        assert options.get("host") and type(options["host"]) == type(""),\
            "ECSpooler requires a correct 'host' option."

        assert options.get("port") and type(options["port"]) == int,\
            "ECSpooler requires a correct 'port' option."

        assert options.get("pwd_file") and type(options["pwd_file"]) == type(''),\
            "ECSpooler requires a correct 'pwd_file' option."

        # the cape server identification
        self._srv_id = \
            md5.md5(repr(random.Random().random())).hexdigest()

        # configure xmlrpc server
        self._opt = options.copy()
        srv.XMLRPCServer.__init__(self, (self._opt["host"], self._opt["port"]), logRequests=0)
        self.register_function(self.addChecker)
        self.register_function(self.removeChecker)
        self.register_function(self.enqueue)
        self.register_function(self.pollResults)
        self.register_function(self.pollResult)
        self.register_function(self.getStatus)
        self.register_function(self.getBackends)
        #self.register_function(self.stop)

        self.register_introspection_functions()

        self._checkers = {}
        self._queue    = checkjobQueue.CheckJobQueue()
        self._results  = checkresultCache.CheckResultCache()
        self._auth     = auth.initAuthBackend({'filename':self._opt["pwd_file"]})

        # doqueue thread
        self._doqueue_thread = None

        # signal handler
        signal.signal(signal.SIGHUP, self.reconfig)
        signal.signal(signal.SIGTERM, self.stop)
        #signal.signal(signal.SIGINT,  self.stop)


    def reconfig(self, signal, stack):
        """
        Does nothing
        """
        return
       
    
    def stop(self, signal, stack):
        """
        Stops the server and all attached checkers when a SIGTERM or
        SIGINT signal is sent to the server process.
        """
        self._log("Stopping scheduler thread...")
        if self._doqueue_thread:
            self._doqueue_thread.cancel()

        self._log("Stopping checkers...")
        for chk in self._checkers.itervalues():
             self._callChecker(chk["url"], "stop")

        self._log("Stopping server...")
        self.server_close()


    def run(self):
        """
        Starts the server.
        """
        self.scheduleWakeup(3) # give it three seconds for the first time
        self.serve_forever()
        return 1


    def scheduleWakeup(self, time):
        """
        Schedules the execution of the doqueue() method by using a Timer.
        """
        if self._doqueue_thread: self._doqueue_thread.cancel()

        self._doqueue_thread = threading.Timer(time, self.doqueue)
        self._doqueue_thread.start()


    def addChecker(self, authdata, id, name, xmlrpc_url):
        """
        Announces a Checker to this ECSpooler.

        Each checker calls the ECSpooler on being started, and tells 
        the server its id, name and its xmlrpc url. The server returns
        a specific id, which is a random number created at server
        startup. This id is sent to the checker in each request and
        thus authorizes the request. Only the server to which the
        checker is attached can perform requests to this checker.

        @return (code, msg) with
            code == 0, then addChecker succeeded, and msg is a identifying server-id
            code != 0, then addChecker failed, and msg contains further information
        """
        if not self._auth.test(authdata, self._auth.ADD_CHECKER):
            return (-2, ERROR_AUTH_FAILED)

        
        if id in self._checkers:
            return (-1, "There already is a checker by that id (%s)." % id)

        self._log("Adding checker '%s' (%s) at '%s'" % (id, name, xmlrpc_url))
        self._checkers[id] = {"name":name, "url":xmlrpc_url, "current_job":0}
        return (0, self._srv_id)



    def removeChecker(self, authdata, name, xmlrpc_url):
        """
        Removes a Checker from this ECSpooler.

        @return (code, msg) with
            code == 0, removing the checker succeeded
            code != 0, remove the checker failed, and msg contains further information
        """
        if not self._auth.test(authdata, self._auth.ADD_CHECKER):
            return (-2, ERROR_AUTH_FAILED)

        if name in self._checkers:
            self._log("Removing checker '%s' at '%s'"%(name, xmlrpc_url))
            
            chk = self._checkers[name]
            self._callChecker(chk["url"], "stop")
            del self._checkers[name]

            return (0, "")
        else:
            return (-1, "There isn't a checker by that name.")


    def enqueue(self, authdata, jobdata):
        """
        Adds a CheckJob to the queue.

        @param authdata authorization information
        @param jobdata relevant job data (see CheckJob documentation for details)
        @return (code, msg) with
            code == 0, then enqueue succeeded and msg contains the job id
            code != 0, then enqueue failed and msg contains further information
        """

        if not self._auth.test(authdata, self._auth.ENQUEUE):
            return (-2, ERROR_AUTH_FAILED)

        try:
            job = checkjob.CheckJob(jobdata, createID = 1)

            if not job["checker"] in self._checkers.keys():
                return (1, "no such checker: %s" % job["checker"])

            # enqueue
            self._log("Enqueueing job %s"%job["id"])
            self._queue.enqueue(job)

            return (0, job["id"])

        except exceptions.InvalidDataException, exc:
            return (-1, "invalid job data: %s (%s)" % (repr(exc), repr(jobdata)))

        return (-10, "implementation error")


    def pollResults(self, authdata):
        """
        Returns a dictionary { job id: CheckResult.getData() } 
        with the results of the performed check jobs. Once the 
        results are polled, they are no longer stored in the server.
        """

        if not self._auth.test(authdata, self._auth.POLL_RESULTS):
            return (-2, ERROR_AUTH_FAILED)

        rescache = self._results.pollResults()
        obj = {}
        for k,v in rescache.iteritems():
            obj[k] = v.getData()

        return obj


    def pollResult(self, authdata, id):
        """
        Returns a dictionary { job id: CheckResult.getData() } 
        with the result of the performed check job for the given id. 
        Once the result is polled, it is no longer stored in the server.
        """

        if not self._auth.test(authdata, self._auth.POLL_RESULTS):
            return (-2, ERROR_AUTH_FAILED)

        res = self._results.pollResult(id)
        obj = {}
        
        if res != None:
            obj[id] = res.getData()

        return obj


    def getStatus(self, authdata):
        """
        Returns a dict with some status information:
    
        "checkers": a list of the attached checkers
        "queue":    the number of items in the queue
        "results":  the number of cached result data
        """
        
        if not self._auth.test(authdata, self._auth.GET_STATUS):
            return (-2, ERROR_AUTH_FAILED)

        return {
            "pid":      os.getpid(),
            "checkers": self._checkers.keys(),
            "queue":    self._queue.getSize(),
            "results":  self._results.getSize(),
        }


    def getBackends(self, authdata):
        """
        Returns a dict with all checker backends
        """
        
        if not self._auth.test(authdata, self._auth.GET_STATUS):
            return (-2, ERROR_AUTH_FAILED)

        return self._checkers


    def doqueue(self):
        """
        Performs the dispatching of CheckJobs to the checkers.

        This method is called in a separate thread, which is opened
        and managed by the threading.Timer class. doqueue() schedules
        itself to be executed after a defined sleep period.
        """
        
        # queue empty?
        if self._queue.isEmpty():
            self._log('doqueue: self._queue is empty.');
            self.scheduleWakeup(self.DEFAULT_DOQUEUE_WAKEUP)
            return 0

        # get next job from the queue
        job = self._queue.dequeue()

        # locate checker
        chk = self._checkers.get(job["checker"])
        if not chk:
            self._log("WARNING: checkJob %s cannot be executed, no such checker: %s"\
                      %(job["id"], job["checker"]))
            self._queue.enqueue(job)
             # wait some more time than usual, cape server seems not to be initialized yet
            self.scheduleWakeup( 10 * self.DEFAULT_DOQUEUE_WAKEUP)
            return 0

        # is checker busy?
        if chk["current_job"] != 0:
            self._log('doqueue: backend is busy (%s)' % job)
            self._queue.enqueue(job)
            self.scheduleWakeup(self.DEFAULT_DOQUEUE_WAKEUP)
            return 0

        # hand over checkjob to checker
        chk["current_job"] = job
        try:
            self._log("Dispatching job %s to checker '%s'" % (job["id"], job["checker"]))
            res = self._callChecker(chk["url"], "execute", job.getData())
            self._results.addResult(
                checkresult.CheckResult(res[0], res[1]), 
                job["id"]
            )
        except (socket.error, xmlrpclib.Fault), exc:
            import traceback
            traceback.print_exc()
            
            # XXX we might want to ping the checker?
            self._results.addResult(
                 checkresult.CheckResult(-5, "server error: %s" % repr(exc)),
                 job["id"]
            )

        chk["current_job"] = 0
        self.scheduleWakeup(self.DEFAULT_DOQUEUE_WAKEUP)
        return 1


    def _callChecker(self, url, method, *kw, **args):
        """
        Executes an xmlrpc call to a checker.
        """
        s = xmlrpclib.Server(url)
        return getattr(s, method)({"srv_id": self._srv_id}, *kw, **args)
