import exceptions

class BackendResult:
    """
    Represents a return value from a backend.

    Following data are enclosed
    code: > 0 -> True, everthing is fine 
          = 0 -> False, everything went wrong
          < 0 -> internal error
              
    value:  the actually result value
    """

    def __init__(self, code, value):
        """
        @param code
        @param value
        """
        self._code = code
        self._value = value
        self.validate() # throws an exception if necessary


    def validate(self):
        """
        Tests the given BackendResult data. Not implemented yet!
        """
        # raise exceptions.InvalidDataException(
        #                   'invalid BackendResult attributes')
        return 1


#    def __repr__(self):
#        """
#        Returns the transport data structure which can be transmitted via XMLRPC.
#        """
#        return (self._code, self._value)
#
#
#    def __str__(self):
#        """
#        Returns the transport data structure which can be transmitted via XMLRPC.
#        """
#        return (self._code, self._value)


    def getResultCode(self):
        return self._code
    

    def getResultValue(self):
        return self._value


    def isFailure(self):
        """
        Returns wether or not the checkjob has failed
        """
        return (self._code == 0) or (self._code < 0)


#    def isSolved(self):
#        """
#        Returns wether or not the students' solution has passed all tests
#        """
#        return self._solved
