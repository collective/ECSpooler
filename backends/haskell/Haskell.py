# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2011 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.

################################################################################
#                                Changelog                                     #
################################################################################
#
# 16.04.2009, chbauman:
#       substituted all occurences of '.hs' with srcFileSuffix
# 30.04.2009, chbauman:
#       using replace instead of expensive re.sub operation in
#       _preProcessCheckSyntax.
#       replaced re.sub with replace whereever possible.

import sys, os, re

from os.path import join, dirname
from types import StringTypes

# local imports
from lib.data.BackendResult import BackendResult
from lib.util.BackendSchema import InputField
from lib.util.BackendSchema import RepeatField
from lib.util.BackendSchema import Schema
from lib.util.BackendSchema import TestEnvironment
from lib.ProgrammingBackend import ProgrammingBackend
from lib.ProgrammingBackend import EX_OK

from backends.haskell import config
from backends.haskell import LOG


# load Haskell function to do a simple test
try:
    simpleTest = file(join(dirname(__file__), 'simpleTest.hs'), 'r').read()
except IOError, ioe:
    LOG.warn('%s: %s' % (sys.exc_info()[0], ioe))
    simpleTest = ''

# load Haskell function to do a test which allows permutation of list elems
try:
    permTest = file(join(dirname(__file__), 'permTest.hs'), 'r').read()
except IOError, ioe:
    LOG.warn('%s: %s' % (sys.exc_info()[0], ioe))
    permTest = ''

# load Haskell function to do a test which allows tolerance
try:
    toleranceTest = file(join(dirname(__file__), 'toleranceTest.hs'), 'r').read()
except IOError, ioe:
    LOG.warn('%s: %s' % (sys.exc_info()[0], ioe))
    permTest = ''

WRAPPER_TEMPLATE = \
"""module Main where
import Model
import Student

${helpFunctions}

-- must be named 'haskell_backend_internal_equality_test'
${testFunction}

main = putStr(\"isEqual=\" ++ show(haskell_backend_internal_equality_test (o1) (o2)) ++ \";;expected=\" ++ show(o1) ++ \";;received=\" ++ show(o2))
    where
        o1 = Model.${testData}
        o2 = Student.${testData}
"""

SYNTAX_TEMPLATE = \
"""module Main where

${SOURCE}
   
main = putStr(\"\")
"""

GENERIC_TEMPLATE = \
"""module %s where
%s
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
        description = 'Exact matching results are allowed.',
        test = simpleTest,
        syntax = SYNTAX_TEMPLATE,
        semantic = WRAPPER_TEMPLATE,
        lineNumberOffset = 2,
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
        lineNumberOffset = 2,
        compiler = config.INTERPRETER,
        interpreter = config.INTERPRETER,
    ),

    TestEnvironment(
        'tolerance',
        label = 'Tolerance',
        description = 'Tolerance of 1/10^15 in result values is allowed.',
        test = toleranceTest,
        syntax = SYNTAX_TEMPLATE,
        semantic = WRAPPER_TEMPLATE,
        lineNumberOffset = 2,
        compiler = config.INTERPRETER,
        interpreter = config.INTERPRETER,
    ),
))

#runhugsRegEx = 'Type checking\n?|Parsing\n?|[Parsing]*[Dependency analysis]*[Type checking]*[Compiling]*\x08 *'
RUNHUGS_RE = re.compile(r'Type checking\n?|Parsing\n?|[Parsing]*[Dependency analysis]*[Type checking]*[Compiling]*\x08 *')

class Haskell(ProgrammingBackend):
    """
    A backend for checking Haskell programs by comparing
    student and model solution on given test data.
    """
    
    id = 'haskell'
    name = 'Haskell'

    srcFileSuffix = '.hs'

    schema = inputSchema
    testSchema = tests
    
    def __init__(self, params, versionFile=__file__):
        """
        This constructor is needed to reset the logging environment.
        """
        ProgrammingBackend.__init__(self, params, versionFile)


    def _preProcessCheckSyntax(self, test, src, **kwargs):
        """
        Pre process student's submissions and syntax check wrapper code. 
        Override this method if you need to reformat the wrapper code or 
        the student's submission. 
        
        @param: test: 
        @param: src:
        @return: source and module name if needed
        """
        result = ''
        
        if test.syntax:
            #result = re.sub('\$\{SOURCE\}', src, test.syntax)
            result = test.syntax.replace('${SOURCE}', src)
            
        else:
            result = src

        return result, 'Student'


    def _postProcessCheckSyntax(self, test, message):
        """
        @see: AbtractSimpleBackend._postProcessCheckSyntax
        """
        # take care of hexadecimal Unicode escape sequences sent to stdout
        # or stderr
        msg = message.decode('unicode_escape')
        
        # find line number in result
        matches = re.findall('%s":(\d+)' % self.srcFileSuffix, msg)
        
        if len(matches):
            # set line number minus x lines and return
            return re.sub('".*%s":(\d+)'  % self.srcFileSuffix, 
                          'line: %d' % (int(matches[0]) - test.lineNumberOffset), 
                          msg)
        else:
            return msg
        

    def _postProcessCheckSemantic(self, test, message):
        """
        Post process interpreter messages. Override this method if you need
        to remove or reformat messages.
        
        @param: message:
        """
        #result = re.sub('isEqual=', '', message)
        #result = re.sub('/.*/ECSpooler/backends/haskell/', '', result)
        
        result = message.replace('isEqual=', '')
        result = result.replace('/.*/ECSpooler/backends/haskell/', '')

        return result
    
    
    def _process_checkSemantics(self, job):
        """
        Test a Haskell programs using balck box tests, e.g., compare results
        of a model solution with results of a students' solution on same
        test data.
        
        @return: a BackendResult object with result code and value
        """
        # test for available test specs
        testSpecs = self._getTests(job)

        if len(testSpecs) == 0:
            msg = 'No test specification selected.'
            self.log.warn('%s, %s' % (msg, job.getId()))
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
            self.log.warn('%s, %s' % (msg, job.getId()))
            return BackendResult(-216, msg)


        # we have some test data -> lets start the evaluation
        # get model solution and student's submission
        model = job['modelSolution']
        submission = job['submission']

        assert model and type(model) in StringTypes, \
            "Semantic check requires valid 'model solution' (%s)" % model

        assert submission and type(submission) in StringTypes, \
            "Semantic check requires valid 'student solution' (%s)" % submission

        model = GENERIC_TEMPLATE % ('Model', model)
        submission = GENERIC_TEMPLATE % ('Student', submission)

        # define return values
        feedback = BackendResult.UNKNOWN
        solved = True

        # run selected test specifications
        for test in testSpecs:

            if not solved: break

            self.log.debug('Running semantic check with test: %s' % test.getName())

            # get the interpreter
            interpreter = test.interpreter
           
            # write model solution and students' solution to a file
            self._writeModule('Model', model, self.srcFileSuffix, job.getId(), test.encoding)
            self._writeModule('Student', submission, self.srcFileSuffix, job.getId(), test.encoding)

            # get wrapper code
            src = test.semantic
            
            # get values for all other input fields from schema which are 
            # available in the job data
            for field in self.schema.filterFields(type='InputField'):
                if job.has_key(field.getName()):
                    repl = job[field.getName()]
                    #src = re.sub('\$\{%s\}' % field.getName(), repl, src)
                    src = src.replace('${%s}' % field.getName(), repl)

            # set test function
            #src = re.sub('\$\{testFunction\}', test.test, src)
            src = src.replace('${testFunction}', test.test)

            # run with all test data
            for t in testdata:
                # and now add repeatable data values
                #wrapper = re.sub('\$\{%s\}' % repeatField.getName(), t, src)
                wrapper = src.replace('${%s}' % repeatField.getName(), t)
        
                # remove all remaining placeholders
                wrapper = re.sub('\$\{.*\}', '', wrapper)
                                 
                # execute wrapper code in haskell interpreter
                try:
                    # write module for wrapper
                    wrapperModule = self._writeModule('wrapper', wrapper, 
                                                      self.srcFileSuffix, 
                                                      job.getId(),
                                                      test.encoding)

                    # execute
                    exitcode, result = \
                        self._runInterpreter(interpreter, 
                                    os.path.dirname(wrapperModule['file']),
                                    os.path.basename(wrapperModule['file']))

                    # remove all special characters written by runhugs
                    #result = re.sub(HaskellConf.runhugsRegEx, '', result)
                    result = RUNHUGS_RE.sub('', result)  
        
                except Exception, e:
                    msg = 'Internal error during semantic check: %s: %s' % \
                          (sys.exc_info()[0], e)
                                  
                    self.log.error(msg)
                    return BackendResult(-230, msg)

                # an error occured
                # FIXME: os.EX_OK is available only for Macintosh and Unix!
                if exitcode != EX_OK:
                    result = "\nYour submission failed. Test " \
                             "case was: '%s' (%s)" \
                             "\n\n Received result: %s"\
                             % (t, test.getName(), 
                                self._postProcessCheckSemantic(test, result))

                    return BackendResult(False, result)
                        
                # has the students' solution passed this test?
                else:
                    #log.debug(result)
                    
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

        if solved :
            # TODO: i18n
            feedback = '\nYour submission passed all tests.'

        return BackendResult(solved, feedback)

