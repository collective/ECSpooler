# -*- coding: UTF-8 -*-
# $Id$
#
# Copyright (c) 2005 Otto-von-Guericke-University, Magdeburg
#
# This file is part of ECSpooler.

import os, re, popen2, tempfile
import threading

from data import *
from util.utils import *

import checker


# resourcestring
PROCESS_WAIT_TIME = 15
INTERPRETER = 'systrace -a -f /home/amelung/.systrace/opt_python_bin_python /opt/python/bin/python'
#INTERPRETER = '/opt/python/bin/python'

ID = 'python'
NAME = 'Python'

TEST_SIMPLE = \
"""
def test(a, b):
    return (a == b)
"""

TEST_PERM = \
"""
def test(a, b):
    # FIXME:
    return True
"""


TEMPLATE_SEMANTIC = \
"""import sys
import %s
import %s
%s
print >> sys.stdout, \"isEqual=\" + str(test(%s, %s)) + \";;expected=\" + str(%s) + \";;received=\" + str(%s)
"""


class PythonChecker(checker.Checker):
    """
    Simple checker class for checking Python programs.
    This class is inherited from Checker (see checker.py):
    """
    

    def checkSyntax(self):
        """
        Checks syntax of a Python program.
        @return A CheckResult object with error code and message text
        """
        assert self._job, "Job data is required."

        source = self._job["student_solution"]

        try:
            (exitcode, response) = self._runInterpreter(INTERPRETER, source, '.py')
            assert type(response) == list

            result = ''.join(response)
            assert type(result) == type('')

        except Exception, e:
            # FIXME: use log instead print
            print "Internal Error during syntax check: %s" % repr(e)
            return checkresult.CheckResult(20, repr(e))

        # consider exit code
        if exitcode != os.EX_OK:
            return checkresult.CheckResult(exitcode, result)
        else:
            return checkresult.CheckResult(0, 'Syntax check succeeded.')


    def checkSemantics(self):
        """
        Checks syntax and semantic of Python programs.
        @return A CheckResult object with error code and message text
        """
        assert self._job, "Job data is required."
        
        model = self._job["sample_solution"]
        answer = self._job["student_solution"]
        comparator = self._job["comparator"]
        testdata = self._job["testdata"]

        # write model solution and answer files
        model = self._createModule(prefix='Model', source=model)
        answer = self._createModule(prefix='Student', source=answer)

        # write wrapper code and execute it for all test data
        feedback = ''
        # solved == 0 means the check was successful
        solved = 0
        
        tests = testdata.split('\n')
        
        for test in tests:
            if not comparator:
                comparator = TEST_SIMPLE

            # complete the wrapper code
            wrapper = TEMPLATE_SEMANTIC % (
                        model['module'], 
                        answer['module'],
                        comparator,
                        model['module'] + '.' + test,
                        answer['module'] + '.' + test,
                        model['module'] + '.' + test,
                        answer['module'] + '.' + test,
                    )
        
       
            # execute wrapper code in haskell interpreter
            try:
                (exitcode, response) = self._runInterpreter(INTERPRETER, wrapper, '.py')
                assert type(response) == list

                result = ''.join(response)
                assert type(result) == type('')
    
            except Exception, e:
                # FIXME: use log
                print "Internal Error during semantic check: %s" % repr(e)
                
                # import traceback
                # traceback.print_exc()
                

                # delete temp files because we leave this method right now
                os.remove(model['file'])
                os.remove(answer['file'])
                return checkresult.CheckResult(-1, repr(e))
    
            # an error occured
            if exitcode != os.EX_OK:
                # delete temp files because we leave this method right now
                os.remove(model['file'])
                os.remove(answer['file'])
    
                return checkresult.CheckResult(exitcode, result)
                    
            # hase the students' solution passed this tests?
            else:
                # FIXME: use _log
                print result
                
                msgItems = result.split(';;')
                
                isEqual = (msgItems[0].split('=')[1])
                expected = msgItems[1].split('=')[1]
                received = msgItems[2].split('=')[1]
                
                if isEqual == 'False':
                    feedback += '\nYour submission failed. Test case was: "%s"' \
                                % (test,)
                    feedback += '\n\n  Expected result: %s\n  Received result: %s' \
                                % (expected, received,)
                    
                    solved = 1;
                    
                    break # means end testing right now
                        

        if solved == 0:
            feedback = '\nYour submission passed all tests.'

        # delete temp files
        os.remove(model['file'])
        os.remove(answer['file'])


        return checkresult.CheckResult(solved, feedback)


    def _createModule(self, prefix='', source=''):
        """
        Creates a new haskell module on disk. Returns the module's name.
        """
        moduleName = getUniqueModuleName(prefix)
        fileName = getTempFileName(moduleName, '.py')
        writeFile(source, fileName)
        
        return {'module':moduleName, 'file':fileName}


    def _runInterpreter(self, interpreter, code, suffix=''):
        """
        Runs a program in python.

        @param code the source code to run
        @param suffixthe default filename suffix
        @return (exitcode, output lines)
        """
        
        fname = tempfile.mktemp(suffix = suffix)

        fd = open(fname, "w")
        fd.write(code)
        fd.write("\n")
        fd.close()

        # TODO: use sand-box environment
        # copy files to jail using ssh
        #executeOsCmd(sCommandCopy % (mSFilename + '.hs'))
        #executeOsCmd(sCommandCopy % (sSFilename + '.hs'))
        #executeOsCmd(sCommandCopy % (wFilename + '.hs'))

        # Popen4 will provide both stdout and stderr on handle.fromchild
        handle = popen2.Popen4('%s %s' % (interpreter, fname))
        handle.tochild.close()
        # we don't expect to send on stdin; instead we just wait for the process 
        # to end, or kill it.

        def interruptProcess():
            print 'Aborting ' + interpreter + ' : SIGTERM -> %i' % handle.pid
            os.kill(handle.pid, 15)

        timer = threading.Timer(PROCESS_WAIT_TIME, interruptProcess)
        timer.start()
        exitcode = handle.wait()
        timer.cancel()

        if exitcode == 15:
            # process has been interrupted by timer
            os.remove(fname)
            return (15, ['Program aborted after %i seconds' % PROCESS_WAIT_TIME])
            
        buf = handle.fromchild.readlines()
        handle.fromchild.close()
        os.remove(fname)

        return (handle.poll(), buf)


# -- Test section --------------------------------------------------------------
if __name__ == "__main__":
    print 'TEST'
