# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.

import sys, os, re, popen2, tempfile
import threading
import logging

from types import StringType

# local imports
from data import *
from util.utils import *

from util.BackendSchema import TestEnvironment, RepeatField, InputField, Schema
from AbstractSimpleBackend import AbstractSimpleBackend

INTERPRETER = '/opt/hugs/bin/runhugs'

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
import Model
import Student

${helpFunctions}

-- must be named 'test'
${testFunction}

o1 = Model.${testData}
o2 = Student.${testData}

main = putStr(\"isEqual=\" ++ show(test (o1) (o2)) ++ \";;expected=\" ++ show(o1) ++ \";;received=\" ++ show(o2))
"""

TEMPLATE_SYNTAX = \
"""module Main where

${studentSolution}
   
main = putStr(\"\")
"""

TEMPLATE_MODULE= \
"""module %s where
%s
"""

REGEX_RUNHUGS_SPECIALS = 'Type checking\n?|Parsing\n?|[Parsing]*[Dependency analysis]*[Type checking]*[Compiling]*\x08 *'

# input schema
inputSchema = Schema((
        RepeatField(
            'testData', 
            #accessor = # must return a list; default is one element per line
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
            'modelSolution', 
            required = True, 
            label = 'Model solution',
            description = 'Enter a model solution.',
            i18n_domain = 'EC',
        ),
        
        InputField(
            'helpFunctions', 
            label = 'Help functions',
            description = 'Enter help functions if needed.',
            i18n_domain = 'EC',
        ),
    ))

# testSchema
tests = Schema((

        TestEnvironment(
            'simple',
            label = 'Simple',
            description = 'Exact matching results are allowed.',
            test = TEST_SIMPLE,
            syntax = TEMPLATE_SYNTAX,
            semantic = TEMPLATE_SEMANTIC,
            lineNumberOffset = 2,
            compiler = INTERPRETER,
            interpreter = INTERPRETER,
        ),

        TestEnvironment(
            'permutation',
            label = 'Permutation',
            description = 'Permutations are allowed.',
            test = TEST_PERM,
            syntax = TEMPLATE_SYNTAX,
            semantic = TEMPLATE_SEMANTIC,
            lineNumberOffset = 2,
            compiler = INTERPRETER,
            interpreter = INTERPRETER,
        ),
    ))

class HaskellBackend(AbstractSimpleBackend):
    """
    A simple checker class for checking Haskell programs by comparing
    student and model solution on given test data.
    """
    
    id = 'haskell'
    name = 'Haskell'
    version = '1.0'

    srcFileSuffix = '.hs'

    schema = inputSchema
    testSchema = tests

    def _postProcessCheckSyntax(self, test, message):
        """
        @see AbtractSimpleBackend._postProcessCheckSyntax
        """
        # find line number in result
        matches = re.findall('%s":(\d+)' % self.srcFileSuffix, message)
        
        if len(matches):
            # set line number minus x lines and return
            return re.sub('".*%s":(\d+)'  % self.srcFileSuffix, 
                          'line: %d' % (int(matches[0])-test.lineNumberOffset), 
                          message)
        else:
            return message
        

    def checkSemantics(self, job):
        """
        Checks semantic of a Haskell programs.
        @return a BackendResult object with result code and value
        """
        # 1. get model solution and student's submission
        modelSolution = job['modelSolution']
        studentSolution = job['studentSolution']

        assert modelSolution and type(modelSolution) == StringType, \
            "Semantic check requires valid 'model solution' (%s)" % \
            modelSolution

        assert studentSolution and type(studentSolution) == StringType, \
            "Semantic check requires valid 'student solution' (%s)" % \
            studentSolution

        modelSolution = TEMPLATE_MODULE % ('Model', modelSolution)
        studentSolution = TEMPLATE_MODULE % ('Student', studentSolution)

        # write model solution and answer files
        model = self._writeModule('Model', modelSolution, '.hs', job['id'])
        student = self._writeModule('Student', studentSolution, '.hs', job['id'])

        # 2. get all test data to iterate through them
        repeatFields = self.schema.filterFields(__name__='testData')
        
        repeatFields and len(repeatFields) == 1, \
            'None or more than one RepeatField found.'
        
        repeatField = repeatFields[0]
        testdata = repeatField.getAccessor()(job[repeatField.getName()])

        # 3. define return values
        feedback = ''
        solved = 1 # 1 means testing was successful

        if len(self._getTests(job)) == 0:
            msg = 'No test specification found.'
            logging.warn(msg)
            return(0, msg)

        # 4. run selected test specifications
        for test in self._getTests(job):

            if solved == 0: break

            logging.debug('Running semantic check with test: %s' % 
                          test.getName())

            # get the interpreter
            interpreter = test.interpreter
           
            # 4.1. get wrapper code
            src = test.semantic
            
            # 4.2. get values for all other input fields from schema which are 
            # available in the job data
            for field in self.schema.filterFields(type='InputField'):
                if job.has_key(field.getName()):
                    repl = job[field.getName()]
                    src = re.sub('\$\{%s\}' % field.getName(), repl, src)

            # 4.3 set test function
            src = re.sub('\$\{testFunction\}', test.test, src)

            # 4.4. run with all test data
            for t in testdata:
                # and now add repeatable data values
                wrapper = re.sub('\$\{%s\}' % repeatField.getName(), t, src)
        
                # remove all remaining placeholders
                wrapper = re.sub('\$\{.*\}', '', wrapper)
                                 
                # execute wrapper code in haskell interpreter
                try:
                    exitcode, result = \
                        self._runHaskell(interpreter, wrapper, job['id'])

                    # remove all special characters written by runhugs/haskell 
                    result = re.sub(REGEX_RUNHUGS_SPECIALS, '', result)
        
                except Exception, e:
                    msg = 'Internal error during semantic check: %s: %s' % \
                          (sys.exc_info()[0], e)
                                  
                    logging.error(msg)
                    return (0, msg)

                # an error occured
                if exitcode != os.EX_OK:
                    result = "\nYour submission failed. Test " \
                             "case was: '%s' (%s)" \
                             "\n\n Received result: %s"\
                             % (t, test.getName(), result)
                    return (0, result)
                        
                # has the students' solution passed this tests?
                else:
                    #logging.debug(result)
                    
                    msgItems = result.split(';;')
                    
                    isEqual = (msgItems[0].split('=')[1])
                    expected = msgItems[1].split('=')[1]
                    received = msgItems[2].split('=')[1]
                    
                    if isEqual.upper() != 'TRUE':
                        # TODO: i18n
                        feedback += "\nYour submission failed. Test " \
                                    "case was: '%s' (%s)" \
                                    % (t, test.getName())
                        feedback += '\n\n  Expected result: %s\n  ' \
                                    'Received result: %s' \
                                    % (expected, received,)
                        
                        solved = 0;
                        
                        break # means: end testing right now
            # end inner for loop
        #end out for loop 

        if solved == 1:
            # TODO: i18n
            feedback = '\nYour submission passed all tests.'

        return (solved, feedback)


    def _runHaskell(self, intepreter, source, jobID):
        """
        @see _runInterpreter in class AbstractSimpleBackend
        """
        # write module for wrapper
        wrapperModule = self._writeModule('wrapper', source, 
                                     self.srcFileSuffix, jobID)

        return self._runInterpreter(intepreter, 
                                    os.path.dirname(wrapperModule['file']),
                                    os.path.basename(wrapperModule['file']))
