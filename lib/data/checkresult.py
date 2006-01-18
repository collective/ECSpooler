import exceptions

class CheckResult:
	"""
	Represents the result of a CheckJob.

	Consists of the following data:
	exitcode: = 0 job/check succeeded
    	      > 0 job/check failed
        	  < 0 request error
	#solved:   True or False
	message:  evaluation information
	"""

	def __init__(self, exitcode, message):

		self._code    = exitcode
		self._message = message
		self.validate() # throws an exception if necessary


	def validate(self):
		"""
		Tests CheckResult data.
		"""
		# raise exceptions.InvalidDataException("invalid CheckResult attributes")
		return 1


	def getData(self):
		"""
		Returns the transport data structure which can be transmitted via XMLRPC.
		"""
		return (self._code, self._message)


	def isFailure(self):
		"""
		Returns wether or not the checkjob has failed
		"""
		return self._code != 0


#	def isSolved(self):
#		"""
#		Returns wether or not the students' solution has passed all tests
#		"""
#		return self._solved
