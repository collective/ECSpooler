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

from AbstractHaskellBackend import AbstractHaskellBackend

TEMPLATE_SEMANTIC = \
"""module Main where
import %s
import %s
%s
main = putStr(\"isEqual=\" ++ show(test (%s) (%s)) ++ \";;expected=\" ++ show(%s) ++ \";;received=\" ++ show(%s))
"""

class HaskellBackend(AbstractHaskellBackend):
    """
    Simple checker class for checking Haskell programs using QuickCheck.
    This class is inherited from Checker (see checker.py):
    """
    backendId = 'haskell'
    backendName = 'Haskell'

    # checkSemantics is defined in AbstractHaskellBackend

    def checkSemantics(self):
        """
        Checks semantic of a Haskell programs.
        @return A CheckResult object with error code and message text
        """
        assert self._job

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
                (exitcode, response) = self._runHaskell(wrapper)
                assert type(response) == list
            
                # remove all special characters written by runhugs/haskell 
                result = re.sub(REGEX_RUNHUGS_SPECIALS, '', ''.join(response))
                assert type(result) == type('')
    
            except Exception, e:
                # FIXME: use log
                import traceback
                traceback.print_exc()

                print "Internal Error during semantic check: %s" % repr(e)
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
