from data import *
import srv

import socket,xmlrpclib


class Checker (srv.XMLRPCServer):
    """
    The Checker class is the basis of all checker programs. It
    implements an XMLRPC server, which is waiting for check
    jobs requested using the execute() method.

    Checker implementations _should_ inherit this class and
    implement the following methods:

    checkSyntax(self) -> CheckResult
    ... test the student solution for syntactical correctness

    checkSemantics(self) -> CheckResult
    ... compare the student solution with the sample solution
    using the given comparator function.

    Both methods can access the CheckJob data instance via self._job
    """


    def __init__(self, options):
        """
        """

        assert options.get("host") and type(options["host"]) == type(""),\
            "Checker requires a correct 'host' option."

        assert options.get("port") and type(options["port"]) == int,\
            "Checker requires a correct 'port' option."

        assert options.get("capeserver") and type(options["capeserver"]) == type(""),\
            "Checker requires a correct 'capeserver' option."

        assert options.get("id") and type(options["id"]) == type(""),\
            "Checker requires a correct 'id' option."

        assert options.get("name") and type(options["name"]) == type(""),\
            "Checker requires a correct 'name' option."

        assert options.get("srv_auth") and type(options["srv_auth"]) == type({}),\
            "Checker requires a correct 'srv_auth' option."

        self._opt = options.copy()
        self._srv_id = None

        srv.XMLRPCServer.__init__(self, (self._opt["host"], self._opt["port"]), logRequests=0)

        self.register_function(self.stop)
        self.register_function(self.execute)
        self.register_introspection_functions()



    def _checkAuthData(self, data):
        
        #return 1

        if self._srv_id == None: 
            s = "Cannot authorize connection without valid cape server connection"
            self._log(s)
            raise exceptions.AuthorizationFailedException(s)

        if not data or type(data) != dict or data.get("srv_id") != self._srv_id:
            s = "Authorization failed for current connection."
            self._log(s)
            raise exceptions.AuthorizationFailedException(s)

        return 1


    def run(self):
        # register at capeserver
        try:
            srv = xmlrpclib.Server(self._opt["capeserver"])
            (code,msg) = srv.addChecker(
                    self._opt["srv_auth"],
                    self._opt["id"],
                    self._opt["name"],
                    "http://%s:%i"%(self._opt["host"], self._opt["port"])
                )

            if code != 0:
                self._log("Cannot add checker to CapeServer: %s (%i)"%(msg,code))
                return 0
            elif not msg:
                self._log("Internal Error - CapeServer returned invalid id. STOP.")
                return 0
            else:
                self._srv_id = msg

        except (socket.error, xmlrpclib.Fault), exc:
            self._log("Cannot connect to CapeServer at '%s': %s"\
                %(self._opt["capeserver"], exc))
            return 0

        # run ourselves
        self.serve_forever()
        return 1


    def stop(self, authdata):
        """
        Stops the checker.

        @param authdata authorization data
        """
        self._checkAuthData(authdata)

#        try:
#            srv = xmlrpclib.Server(self._opt["capeserver"])
#            (code, msg) = srv.removeChecker(self._opt["srv_auth"],
#                                            self._opt["name"],
#                                            "http://%s:%i" % (self._opt["host"], 
#                                                              self._opt["port"])
#                          )
#
#            if code != 0:
#                self._log("Cannot remove checker from CapeServer: %s (%i)" % 
#                           (msg, code))
#
#        except (socket.error, xmlrpclib.Fault), exc:
#            self._log("Cannot connect to CapeServer at '%s': %s" % 
#                       (self._opt["capeserver"], exc))

        self.server_close()
        return 1


    def execute(self, authdata, jobdata):
        """
Executes a check job.

@param authdata authorization information
@param jobdata relevant job data, see CheckJob docu for details
@return CheckResult data
"""
        self._checkAuthData(authdata)

        try:
            self._job = checkjob.CheckJob(jobdata)

            result = self.checkSyntax()
            #print 'checkSyntax:\n', result.getData()[1]
            if result.isFailure(): return result.getData()

            result = self.checkSemantics()
            #print 'checkSemantics:\n', result.getData()[1]
            
            return result.getData()

        except exceptions.InvalidDataException, exc:
            return checkresult.CheckResult(-1, "invalid job data").getData()

        return checkresult.CheckResult(-10, "implementation error").getData()


    def checkSyntax(self):
        # overwrite this method
        return checkresult.CheckResult(-20, "Method checkSyntax must be implemented in subclasses.")


    def checkSemantics(self):
        # overwrite this method
        return checkresult.CheckResult(-21, "Method checkSemantics must be implemented in subclasses.")
    

    def getComparatorTemplate(self):
        # overwrite this method
        return checkresult.CheckResult(-21, "Method getComparatorTemplate must be implemented in subclasses.")
        