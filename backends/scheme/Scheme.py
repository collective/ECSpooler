# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2011 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.

import sys, os, re

from os.path import join, dirname
from types import StringTypes

# local imports
from lib.AbstractProgrammingBackend import AbstractProgrammingBackend
from lib.data.BackendResult import BackendResult
from lib.util.BackendSchema import InputField
from lib.util.BackendSchema import RepeatField
from lib.util.BackendSchema import Schema
from lib.util.BackendSchema import TestEnvironment

from backends.scheme import config
from backends.scheme import LOG

# load Scheme function to do a simple test
try:
    simpleTest = file(join(dirname(__file__), 'simpleTest.scm'), 'r').read()
except IOError, ioe:
    LOG.warn('%s: %s' % (sys.exc_info()[0], ioe))
    simpleTest = ''

# load Haskell function to do a test which allows permutation of list elems
try:
    permTest = file(join(dirname(__file__), 'permTest.scm'), 'r').read()
except IOError, ioe:
    LOG.warn('%s: %s' % (sys.exc_info()[0], ioe))
    permTest = ''
        
    
SYNTAX_TEMPLATE = \
""";; import SICP streams
(require (file "%s")) 



;; -------------------------------------------------------------------
${SOURCE}
;; -------------------------------------------------------------------
""" % config.STREAM_LIB_PATH

SEMANTIC_TEMPLATE = \
"""(module ${MODULE} mzscheme
(require (file "%s")) ;; import SICP streams

(define (main) ${testData})

;; -------------------------------------------------------------------
${SOURCE}
;; -------------------------------------------------------------------

;; helper functions
${helpFunctions}

(provide main))
""" % config.STREAM_LIB_PATH


WRAPPER_TEMPLATE = \
""";; required modules
(require (prefix model. (file "model.scm")))
(require (prefix student. (file "student.scm")))

;; test function
${testFunction}

;; print test results
(let-values (((ms ss) (values (model.main) (student.main))))
  (printf "isEqual=~s;;expected=~s;;received=~s" (test ms ss) ms ss))
"""

# input schema
inputSchema = Schema((

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

    RepeatField(
        'testData', 
        #accessor = # must return a list; default is one element per line
        required = True, 
        label = 'Test data',
        description = 'Enter one or more function calls. '+ 
                    'A function call consists of the ' + 
                    'function name (given in the exercise directives) ' + 
                    'and test data as parameters of this funtion, e.g, '+
                    "tally 'a '(a b c 1 2 3 b c a)"+
                    'Each function call must be written in a single line.',
        i18n_domain = 'EC',
    ),
))

# testSchema
tests = Schema((

    TestEnvironment(
        'simple',
        label = 'Simple',
        description = 'Exact matching results are allowed.',
        test = simpleTest,
        syntax = SYNTAX_TEMPLATE,
        semantic = WRAPPER_TEMPLATE,
        lineNumberOffset = 6,
        compiler = config.INTERPRETER,
        interpreter = config.INTERPRETER,
    ),

    TestEnvironment(
        'permutation',
        label = 'Permutation',
        description = 'Permutations are allowed.',
        test = permTest,
        syntax = SYNTAX_TEMPLATE,
        semantic = WRAPPER_TEMPLATE,
        lineNumberOffset = 6,
        compiler = config.INTERPRETER,
        interpreter = config.INTERPRETER,
    ),
))

class Scheme(AbstractProgrammingBackend):
    """
    This backend tests Scheme programs by comparing student and 
    model solution on given test data.
    """
    
    id = 'scheme'
    name = 'Scheme'

    srcFileSuffix = '.scm'

    schema = inputSchema
    testSchema = tests

    def __init__(self, params, versionFile=__file__):
        """
        This constructor is needed to reset the logging environment.
        """
        AbstractProgrammingBackend.__init__(self, params, versionFile, LOG)


    # -- syntax check ---------------------------------------------------------
    def _preProcessCheckSyntax(self, test, src, **kwargs):
        """
        @see: lib.AbtractProgrammingBackend._preProcessCheckSyntax
        """
        result = ''
        
        if test.syntax:
            result = re.sub('\$\{SOURCE\}', src, test.syntax)
            
        else:
            result = src

        return result, 'student'


    def _postProcessCheckSyntax(self, test, message):
        """
        @see: lib.AbtractProgrammingBackend._postProcessCheckSyntax
        """
        try:
            # find line number in result
            matches = re.findall('%s:(\d+)' % self.srcFileSuffix, message)
    
            #log.debug("xxx: %s" % matches)

            if matches:
                # set line number minus x lines and return
                return re.sub('.*%s:(\d+)'  % self.srcFileSuffix, 
                              'line:%d' % (int(matches[0])-test.lineNumberOffset), 
                              message)
            
        except Exception, e:
            LOG.warn('%s: %s' % (sys.exc_info()[0], e))

        return message


    # -- semantic check ---------------------------------------------------------
    def _postProcessCheckSemantic(self, test, message):
        """
        @see: lib.AbtractProgrammingBackend._postProcessCheckSyntax
        """
        return self._postProcessCheckSyntax(test, message)
   
    
    def _process_checkSemantics(self, job):
        """
        Checks semantic of a Haskell programs.
        
        @return: a BackendResult object with result value and message
        @see: lib.AbstractProgrammingBackend._process_checkSemantics
        """
        # test for available test specs
        testSpecs = self._getTests(job)

        if len(testSpecs) == 0:
            msg = 'No test specification selected.'
            LOG.warn('%s, %s' % (msg, job.getId()))
            return BackendResult(-217, msg)
        
        # test for defined repeat fields in the schema definition
        repeatFields = self.schema.filterFields(type='RepeatField')
        
        assert repeatFields, 'No RepeatField found.'
        assert len(repeatFields) == 1, 'More than one RepeatField found.'
        
        # test for available test data
        repeatField = repeatFields[0]
        testdata = repeatField.getAccessor()(job[repeatField.getName()])

        if len(testdata) == 0:
            msg = 'No test data defined.'
            LOG.warn('%s, %s' % (msg, job.getId()))
            return BackendResult(-216, msg)


        # get model solution and students' submission
        model = job['modelSolution']
        submission = job.getSubmission()

        assert model and type(model) in StringTypes, \
            "Semantic check requires valid 'model solution' (%s)" % \
            repr(model)

        assert submission and type(submission) in StringTypes, \
            "Semantic check requires valid 'student solution' (%s)" % \
            repr(submission)

        # prepare source code for model and student' solution which will be
        # stored in seperate files as modules
        modelSrc = re.sub('\$\{SOURCE}', model, SEMANTIC_TEMPLATE)
        modelSrc = re.sub('\$\{MODULE}', 'model', modelSrc)
        
        studentSrc = re.sub('\$\{SOURCE}', submission, SEMANTIC_TEMPLATE)
        studentSrc = re.sub('\$\{MODULE}', 'student', studentSrc)

        # define return values
        feedback = BackendResult.UNKNOWN
        solved = True

        # run selected test specifications
        for test in testSpecs:

            if not solved: break

            LOG.debug('Running semantic check with test: %s' % 
                          test.getName())

            # get the interpreter
            interpreter = test.interpreter
           
            # get wrapper code for semantic check
            wrapper = test.semantic
            
            # get values for all other input fields from schema which are 
            # available in the job data, e.g., helpFunctions
            for field in self.schema.filterFields(type='InputField'):
                if job.has_key(field.getName()):
                    repl = job[field.getName()]
                    wrapper = re.sub('\$\{%s\}' % field.getName(), repl, wrapper)
                    modelSrc = re.sub('\$\{%s\}' % field.getName(), repl, modelSrc)
                    studentSrc = re.sub('\$\{%s\}' % field.getName(), repl, studentSrc)

            # insert test function in wrapper code
            wrapper = re.sub('\$\{testFunction\}', test.test, wrapper)

            # remove all remaining placeholders
            wrapper = re.sub('\$\{.*\}', '', wrapper)

            # run with all test data
            for t in testdata:
                # add testdata to model and student modules' source code
                model = re.sub('\$\{%s\}' % repeatField.getName(), t, modelSrc)
                student = re.sub('\$\{%s\}' % repeatField.getName(), t, studentSrc)

                # remove all remaining placeholders
                model = re.sub('\$\{.*\}', '', model)
                student = re.sub('\$\{.*\}', '', student)

                try:
                    # 1st write model solution
                    self._writeModule('model', model, 
                                      self.srcFileSuffix, 
                                      job.getId(), 
                                      test.encoding)
                   
                    # 2nd write students' solution
                    self._writeModule('student', student, 
                                      self.srcFileSuffix, 
                                      job.getId(), 
                                      test.encoding)

                    # last write the wrapper and execute
                    wrapperModule = self._writeModule('wrapper', wrapper, 
                                                      self.srcFileSuffix, 
                                                      job.getId(),
                                                      test.encoding)

                    exitcode, result = \
                        self._runInterpreter(interpreter, 
                                    os.path.dirname(wrapperModule['file']),
                                    os.path.basename(wrapperModule['file']))

                except Exception, e:
                    msg = 'Internal error during semantic check: %s: %s' % \
                          (sys.exc_info()[0], e)
                                  
                    LOG.error(msg)
                    return BackendResult(-230, msg)

                # an error occured
                if exitcode != os.EX_OK:
                    result = "\nYour submission failed. Test case was: " \
                             "'%s' (%s) \n\n Received result: %s" \
                             % (t, test.getName(), 
                                self._postProcessCheckSemantic(test, result))

                    return BackendResult(False, result)
                        
                # has the students' solution passed this tests?
                else:
                    #log.debug('result: %s' % result)
                    
                    msgItems = result.split(';;')
                    
                    isEqual = (msgItems[0].split('=')[1])
                    expected = msgItems[1].split('=')[1]
                    received = msgItems[2].split('=')[1]
                    
                    #log.debug('isEqual: %s' % isEqual)
                    #log.debug('expected: %s' % expected)
                    #log.debug('received: %s' % received)
                    
                    if isEqual.lower() != '#t':
                        # TODO: i18n
                        feedback = "\nYour submission failed. Test " \
                                    "case was: '%s' (%s)" \
                                    % (t, test.getName())
                        feedback += '\n\n  Expected result: %s\n  ' \
                                    'Received result: %s' \
                                    % (expected, received,)
                        
                        solved = False;
                        
                        break # means: end testing right now
            # end inner for loop
        #end out for loop 

        if solved:
            # TODO: i18n
            feedback = '\nYour submission passed all tests.'

        return BackendResult(solved, feedback)
