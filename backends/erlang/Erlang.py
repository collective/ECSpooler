# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2011 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.

import sys, os, re
import logging

from types import StringType, UnicodeType
from os.path import join, dirname

# local imports
from lib.AbstractProgrammingBackend import AbstractProgrammingBackend
from lib.AbstractProgrammingBackend import EX_OK
from lib.data.BackendResult import BackendResult
from lib.util.BackendSchema import InputField
from lib.util.BackendSchema import RepeatField
from lib.util.BackendSchema import Schema
from lib.util.BackendSchema import TestEnvironment

from backends.erlang import config

# enable logging
log = logging.getLogger('backends.erlang')

# load source for simple testing
try:
    simpleTest = file(join(dirname(__file__), 'simpleTest.erl'), 'r').read()
except IOError, ioe:
    log.warn('%s: %s' % (sys.exc_info()[0], ioe))
    simpleTest = ''

# load source for testing permutations
try:
    permTest = file(join(dirname(__file__), 'permTest.erl'), 'r').read()
except IOError, ioe:
    log.warn('%s: %s' % (sys.exc_info()[0], ioe))
    permTest = ''


# wrapper code (TEMPLATE_SEMANTIC)
WRAPPER_TEMPLATE = \
"""-module(wrapper).
-export([start/0]).

${helpFunctions}

${testFunction}

o1() -> model:${testData}.
o2() -> student:${testData}.

start() -> io:fwrite("isEqual=~w;;expected=~w;;received=~w", [test(o1(), o2()), o1(), o2()]).
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
                    'and test data as parameters of this funtion. '+
                    'Each function call must be written in a single line.',
        i18n_domain = 'EC',
    ),
))

# testSchema
tests = Schema((

    TestEnvironment(
        'simple',
        label = 'Simple',
        description = 'Test without permutations',
        test = simpleTest,
        semantic = WRAPPER_TEMPLATE,
        #lineNumberOffset = 2,
        compiler = config.COMPILER,
        interpreter = config.INTERPRETER,
    ),

    TestEnvironment(
        'permutation',
        label = 'Permutation',
        description = 'Test with permutations',
        test = permTest,
        semantic = WRAPPER_TEMPLATE,
        #lineNumberOffset = 2,
        compiler = config.COMPILER,
        interpreter = config.INTERPRETER,
    ),
))


class Erlang(AbstractProgrammingBackend):
    """
    A simple checker class for checking Haskell programs by comparing
    student and model solution on given test data.
    """
    
    id = 'erlang'
    name = 'Erlang'

    srcFileSuffix = '.erl'

    schema = inputSchema
    testSchema = tests
    
    def __init__(self, params, versionFile=__file__):
        """
        This constructor is needed to reset the logging environment.
        """
        AbstractProgrammingBackend.__init__(self, params, versionFile, log)

    # -- check syntax ---------------------------------------------------------
    def _preProcessCheckSyntax(self, test, src, **kwargs):
        """
        Replace module name in students' submission.
        
        @see: AbstractProgrammingBackend._preProcessCheckSyntax
        @return: modified source code and new module name
        """

        result = re.sub('-module\((.*)\)\.', '-module(student).', 
                        src)

        return result, 'student'


    def _postProcessCheckSyntax(self, test, message):
        """
        @see: AbtractSimpleBackend._postProcessCheckSyntax
        """
        # replace path and filename
        return re.sub('.*%s:(\d+)' % self.srcFileSuffix, 
                      'line: \g<1>', 
                      message)
                      

    # -- check semantics ------------------------------------------------------
    def _preProcessCheckSemantic(self, test, src, **kwargs):
        """
        Pre process student's submissions and semantic check wrapper code. 
        Override this method if you need to reformat the wrapper code or 
        the student's submission. 
        """                                  
        return src

    
    def _process_checkSemantics(self, job):
        """
        Runs sematic test on a Erlang program.
        Remenber: Before we can run the wrapper code we have to compile it!!
        
        @return: a BackendResult object with result code and value
        """
        # test for available test specs
        testSpecs = self._getTests(job)

        if len(testSpecs) == 0:
            msg = 'No test specification selected.'
            log.warn('%s, %s' % (msg, job.getId()))
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
            log.warn('%s, %s' % (msg, job.getId()))
            return BackendResult(-216, msg)



        # get model solution and student's submission
        model = job['modelSolution']
        submission = job.getSubmission()

        assert model and type(model) in (StringType, UnicodeType), \
            "Semantic check requires valid 'model solution' (%s)" % \
            repr(model)

        assert submission and type(submission) in (StringType, UnicodeType), \
            "Semantic check requires valid 'student solution' (%s)" % \
            repr(submission)

        # define return values
        feedback = BackendResult.UNKNOWN
        solved = True

        # run selected test specifications
        for test in self._getTests(job):

            if not solved: break

            log.debug('Running semantic check with test: %s' % test.getName())

            # get the compiler
            compiler = test.compiler
            
            try:
                # FIXME: move the 're.sub' code to _preProcessCheckSyntax
    
                # replace module name in model solution
                model = re.sub('-module\((.*)\)\.', '-module(model).', model)
    
                # write model solution to a file
                model = self._writeModule('model', model, 
                                          self.srcFileSuffix, job.getId(),
                                          test.encoding)
    
                # compile model solution
                exitcode, result = self._runInterpreter(compiler, 
                                                 os.path.dirname(model['file']),
                                                 os.path.basename(model['file']))

                assert exitcode == EX_OK, 'Errors in model solution: %s' % result
                 
    
                # replace module name in student solution
                submission = re.sub('-module\((.*)\)\.', '-module(student).', 
                                    submission)

                # write student's solution to a file
                student = self._writeModule('student', submission, 
                                            self.srcFileSuffix, job.getId(),
                                            test.encoding)
                
                # run compiler with students' submission
                exitcode, result = self._runInterpreter(compiler,
                                                 os.path.dirname(student['file']),
                                                 os.path.basename(student['file']))
                
                assert exitcode == EX_OK, 'Errors in student solution: %s' % result
                
            except AssertionError, err:
                msg = 'Internal error during semantic check: %s: %s' % \
                      (sys.exc_info()[0], err)
                log.error(msg)
                return BackendResult(-230, msg)

           
            # get the interpreter
            interpreter = test.interpreter

            # 4.1. set wrapper source
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


                # compile and execute wrapper
                try:
                    # write module for wrapper
                    wrapperModule = self._writeModule('wrapper', wrapper, 
                                                 self.srcFileSuffix, job.getId(),
                                                 test.encoding)

                    # compile the wrapper
                    exitcode, result = \
                        self._runInterpreter(compiler,
                                       os.path.dirname(wrapperModule['file']),
                                       os.path.basename(wrapperModule['file']))
                    
                    assert exitcode == EX_OK, \
                        'Error in wrapper code for semantic check: %s' % result

                    # run the wrapper
                    exitcode, result = \
                        self._runInterpreter(interpreter, 
                                       os.path.dirname(wrapperModule['file']),
                                       'wrapper')

                except Exception, e:
                    msg = 'Internal error during semantic check: %s: %s' % \
                          (sys.exc_info()[0], e)
                                  
                    log.error(msg)
                    return BackendResult(-230, msg)

                # an error occured
                if exitcode != EX_OK:
                    result = "\nYour submission failed. Test " \
                             "case was: '%s' (%s)" \
                             "\n\n Received result: %s"\
                             % (t, test.getName(), result)

                    return BackendResult(False, result)
                        
                # has the students' solution passed this tests?
                else:
                    #log.debug(result)

                    match = re.search('^isEqual=.*$', result, re.M)
                    result = match.group()
                    
                    msgItems = result.split(';;')
                    
                    isEqual = (msgItems[0].split('=')[1])
                    expected = msgItems[1].split('=')[1]
                    received = msgItems[2].split('=')[1]
                    
                    if isEqual.upper() != 'TRUE':
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

