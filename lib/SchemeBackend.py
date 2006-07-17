# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.

import sys, os, re
import logging

from types import StringType, UnicodeType

# local imports
#from data import *
#from util.utils import *

from util.BackendSchema import TestEnvironment, RepeatField, InputField, Schema
from AbstractProgrammingBackend import AbstractProgrammingBackend

INTERPRETER = 'mzscheme -f %s -e -m -v "(main)"'

TEST_SIMPLE = \
"""
(define (test a b) (equal? a b) )
"""

TEST_PERM = \
"""
(define (remove item list)
  (if (null? list) 
    '()
    (if (equal? (car list) item)
      (cdr list)
      (cons (car list) (remove item (cdr list))) 
    )
  )
)

(define (permutation list1 list2)
  (if (null? list1) (null? list2)
    (if (member (car list1) list2)
      (permutation (cdr list1) (remove (car list1) list2))
      #f
    )
  )
)


(define (test a b) (permutation a b) )"""

TEMPLATE_SEMANTIC = \
"""???
"""

TEMPLATE_SYNTAX = \
"""%s
"""

TEMPLATE_MODULE= \
"""(module %s mzscheme 
(provide %s)
%s)
"""

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

class SchemeBackend(AbstractProgrammingBackend):
    """
    A simple checker class for checking Haskell programs by comparing
    student and model solution on given test data.
    """
    
    id = 'scheme'
    name = 'Scheme'
    version = '1.0'

    srcFileSuffix = '.scm'

    schema = inputSchema
    testSchema = tests

    def _postProcessCheckSyntax(self, test, message):
        """
        @see AbtractSimpleBackend._postProcessCheckSyntax
        """
        # find line number in result
        matches = re.findall('%s":(\d+)' % self.srcFileSuffix, message)

        # set line number minus x lines and return
        return re.sub('".*%s":(\d+)'  % self.srcFileSuffix, 
                      'line: %d' % (int(matches[0])-test.lineNumberOffset), 
                      message)
        

    def _process_checkSemantics(self, job):
        """
        Checks semantic of a Haskell programs.
        @return a BackendResult object with result code and value
        """

        # get all test data to iterate through them
        repeatFields = self.schema.filterFields(__name__='testData')
        
        assert repeatFields and len(repeatFields) == 1, \
            'None or more than one RepeatField found.'
        
        repeatField = repeatFields[0]
        testdata = repeatField.getAccessor()(job[repeatField.getName()])

        # define return values
        feedback = ''
        solved = 1 # 1 means testing was successful

        # get model solution and student's submission
        modelSolution = job['modelSolution']
        studentSolution = job['studentSolution']

        assert modelSolution and type(modelSolution) in (StringType, UnicodeType), \
            "Semantic check requires valid 'model solution' (%s)" % \
            repr(modelSolution)

        assert studentSolution and type(studentSolution) in (StringType, UnicodeType), \
            "Semantic check requires valid 'student solution' (%s)" % \
            repr(studentSolution)

        modelSolution = TEMPLATE_MODULE % ('Model', modelSolution)
        studentSolution = TEMPLATE_MODULE % ('Student', studentSolution)

        # define return values
        feedback = ''
        solved = 1 # 1 means testing was successful

        # run selected test specifications
        for test in self._getTests(job):

            if solved == 0: break

            logging.debug('Running semantic check with test: %s' % 
                          test.getName())

            # get the interpreter
            interpreter = test.interpreter
           
            # 4.0 write model solution and answer files
            self._writeModule('Model', modelSolution, self.srcFileSuffix, 
                              job['id'], test.encoding)

            self._writeModule('Student', studentSolution, self.srcFileSuffix, 
                              job['id'], test.encoding)
    

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
                    wrapperModule = self._writeModule('wrapper', wrapper, 
                                                      self.srcFileSuffix, 
                                                      job['id'],
                                                      test.encoding)

                    exitcode, result = \
                        self._runInterpreter(interpreter, 
                                    os.path.dirname(wrapperModule['file']),
                                    os.path.basename(wrapperModule['file']))

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
                    logging.debug(result)
                    
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

