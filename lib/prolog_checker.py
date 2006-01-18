import checker
from data import *
from prolog import *

import os, re, popen2, tempfile
import threading

PROCESS_WAIT_TIME=15

class PrologChecker(checker.Checker):


	def checkSyntax(self):
		assert self._job

		data = self._runProlog(self._job["student_solution"], "true")
		
		try:
			(code, output) = data
			assert type(output) == list
		except:
			return checkresult.CheckResult(20,
			       "Implementation Error - _runProlog returns %s" % repr(data))

		# consider exit code
		if code != 0:
			return checkresult.CheckResult(1, "Prolog returned exitcode %i" % code)

		# consider prolog output
		foundErr = 0
		for line in output:
			if line.startswith("ERROR"):
				foundErr = 1
				break

		if (foundErr):
			return checkresult.CheckResult(2, "Prolog says:\n\n%s" % ("\n".join(output)))
			
		return checkresult.CheckResult(0, "Prolog syntax check succeeded.")



	def checkSemantics(self):
		assert self._job

		fname_mod_student = tempfile.mktemp()
		#print "creating mod_student.....: %s"%fname_mod_student
		student_code = self._job["student_solution"]
		try:
			preds = pred.getPredicates(student_code)
		except:
			return checkresult.CheckResult(25, "Internal Error - getPredicates excepts")

		fd = open(fname_mod_student, "w")
		fd.write(":- module(student, [%s]).\n"%(", ".join(preds)))
		fd.write(student_code)
		fd.write("\n")
		fd.close()

		fname_mod_sample = tempfile.mktemp()
		#print "creating mod_sample....: %s"%fname_mod_sample
		sample_code  = self._job["sample_solution"]
		try:
			preds = pred.getPredicates(sample_code)
		except:
			return checkresult.CheckResult(26, "Internal Error - getPredicates excepts")
		fd = open(fname_mod_sample, "w")
		fd.write(":- module(sample, [%s]).\n"%(", ".join(preds)))
		fd.write(sample_code)
		fd.write("\n")
		fd.close()

		#print "creating test code...."
		test_code = """:- use_module('%s').
:- use_module('%s').

runtest_wrapper :- runtest, write('END_OF_TEST\\n').
%s
"""%(fname_mod_student, fname_mod_sample, self._job["comparator"])


		#print "TEST PROGRAM:\n%s"%test_code
		data = self._runProlog(test_code, "runtest_wrapper")
		os.remove(fname_mod_student)
		os.remove(fname_mod_sample)

		try:
			(code, output) = data
			assert type(output) == list
			print "CHECKER RESULT: \n   %s"%"   ".join(output)

		except:
			print "Internal Error during prolog execution: %s"%repr(data)
			return checkresult.CheckResult(-1,"Prolog execution failed - internal error.")

		if ("\n".join(output)).find("TEST FAILED") >= 0:
			return checkresult.CheckResult(1,"Prolog semantics check failed.")

		elif ("\n".join(output)).find("END_OF_TEST") >= 0:
			return checkresult.CheckResult(0,"Prolog semantics check succeeded.")

		elif code != 0:
			return checkresult.CheckResult(code,
			           "Prolog semantics check failed: %s"%("\n".join(output)))

		else:
			return checkresult.CheckResult(1, "Prolog semantics check failed.")



	def _runProlog(self, program, start):
		"""
Runs a program in prolog.

@param program the source code to run
@param start   the prolog start "goal"

@return (exitcode, output lines)
"""
		
		fname = tempfile.mktemp()

		fd = open(fname, "w")
		fd.write(program)
		fd.write("\n")
		fd.close()

		# Popen4 will provide both stdout and stderr on handle.fromchild
		handle = popen2.Popen4("pl -f none -s '%s' -t halt -g '%s'"%(fname,start))
		handle.tochild.close()
		# we don't expect to send on stdin; instead we just wait for the process 
		# to end, or kill it.

		def interruptProcess():
			print "Aborting prolog: SIGTERM -> %i"%handle.pid
			os.kill(handle.pid, 15)

		timer = threading.Timer(PROCESS_WAIT_TIME, interruptProcess)
		timer.start()
		exitcode = handle.wait()
		timer.cancel()

	

		if exitcode == 15:
			# process has been interrupted by timer
			os.remove(fname)
			return (15, ["Program aborted after %i seconds"%PROCESS_WAIT_TIME])
			

		elif exitcode != os.EX_OK:
			# this is unexpected result data and will lead to a negative CheckResult
			os.remove(fname)
			return "EXIT CODE: %i\nOUTPUT: \n > %s\n"\
				%(exitcode, ("\n > ".join(handle.fromchild.readlines())))

		buf = handle.fromchild.readlines()
		handle.fromchild.close()
		os.remove(fname)

		return (handle.poll(), buf)

