# -*- coding: UTF-8 -*-
# $Id$
#
# Copyright (c) 2005 Otto-von-Guericke-University, Magdeburg
#
# This file is part of ECSpooler.

import os, re, popen2, tempfile
import threading
import logging

from data import checkjob, checkresult
from util.utils import *
from util.BackendSchema import TestEnv, InputField, Schema

from AbstractHaskellBackend import AbstractHaskellBackend
from AbstractHaskellBackend import REGEX_RUNHUGS_SPECIALS

TEST_SIMPLE = \
"""
test a b = a == b
"""

TEST_PERM = \
"""
test a b = test_ispermutation a b

remove item [] = []
remove item (head:tail)
    | head == item = tail
    | otherwise = (head:(remove item tail) )

member item [] = False
member item (head:tail)
    | head == item = True
    | otherwise = (member item tail)

test_ispermutation [] [] = True
test_ispermutation x  [] = False
test_ispermutation [] x  = False
test_ispermutation (head:tail) list2
    | (member head) list2 == True = test_ispermutation tail (remove head list2)
    | otherwise = False
"""

TEMPLATE_SEMANTIC = \
"""module Main where
import %s
import %s
%s
main = putStr(\"isEqual=\" ++ show(test (%s) (%s)) ++ \";;expected=\" ++ show(%s) ++ \";;received=\" ++ show(%s))
"""

localSchema = Schema((
        InputField(
            'modelSolution', 
            required = True, 
            label = 'Model solution',
            description = 'Enter a model solution.',
            i18n_domain = 'EC',
        ),
        
        InputField(
            'testData', 
            required = True, 
            label = 'Test data',
            description = 'Enter one or more function calls. '+ 
                        'A function call consists of the ' + 
                        'function name (given in the exercise directives) ' + 
                        'and test data as parameters of this funtion. '+
                        'Each function call must be written in a single line.',
            i18n_domain = 'EC',
        ),

        InputField(
            'comparator', 
            required = False, 
            label = 'Comparator',
            description = 'Enter your comparator function.' + 
                        'The comparator function expects two parameters ' +
                        'and returns a value like True or False. It must ' +
                        'be named "test" and.',
            i18n_domain = 'EC',
        ),
    ))

class HaskellBackend(AbstractHaskellBackend):
    """
    A simple checker class for checking Haskell programs by comparing
    student and model solution on given test data.
    """
    
    id = 'haskell'
    name = 'Haskell'
    schema = localSchema

    # checkSyntax is defined in AbstractHaskellBackend

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

                print "Internal error during semantic check: %s" % repr(e)
                return checkresult.CheckResult(-1, repr(e))
    
            # an error occured
            if exitcode != os.EX_OK:
                # delete temp files because we leave this method right now
                os.remove(model['file'])
                os.remove(answer['file'])
    
                return checkresult.CheckResult(exitcode, result)
                    
            # hase the students' solution passed this tests?
            else:
                logging.debug(result)
                
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
