# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.
import sys, os, re
import logging
import random

from types import StringTypes
from os.path import join, dirname

# local imports
from lib.AbstractProgrammingBackend import AbstractProgrammingBackend
from lib.data.BackendResult import BackendResult
from lib.util.BackendSchema import InputField
from lib.util.BackendSchema import RepeatField
from lib.util.BackendSchema import Schema
from lib.util.BackendSchema import TestEnvironment

from backends.cl import config

# enable logging
log = logging.getLogger('backends.cl')

# The placeholder for the names of the packages that the model and
# student solution will be put in
NS_MODEL = 'modelPackage'
NS_STUDENT = 'studentPackage'

# load Cl function to do a simple test
try:
    simpleTest = file(join(dirname(__file__), 'simpleTest.lisp'), 'r').read()
except IOError, ioe:
    log.warn('%s: %s' % (sys.exc_info()[0], ioe))
    simpleTest = ''

# load Cl function to do a test which allows permutation of list elems
#     try:
#         permTest = file(join(dirname(__file__), 'permTest.lisp'), 'r').read()
#     except IOError, ioe:
#         logging.warn('%s: %s' % (sys.exc_info()[0], ioe))
#         permTest = ''
        
SYNTAX_TEMPLATE = \
"""
;; -------------------------------------------------------------------
${SOURCE}
;; -------------------------------------------------------------------
"""

SEMANTIC_TEMPLATE = \
"""
;; helper functions
${helpFunctions}

;; -------------------------------------------------------------------
${SOURCE}
;; -------------------------------------------------------------------

(defun main ()
  (progn
    ${testData}))
"""

WRAPPER_TEMPLATE = \
'''
;; load the model and the student packages
(load "${%s}.lisp") ; student package
(defparameter ${%s} (main))

(load "${%s}.lisp") ; model package
(defparameter ${%s} (main))

;; test function
${testFunction}

;; print test results
(format t "isEqual=~a;;expected=~a;;received=~a~%%"
        (test ${%s} ${%s}) ; model, student
        ${%s}  ; model
        ${%s}) ; student
''' % (NS_STUDENT, NS_STUDENT,
       NS_MODEL, NS_MODEL,
       NS_MODEL, NS_STUDENT,   # form (test ...)
       NS_MODEL,
       NS_STUDENT,
       )

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
        label = 'Helper functions',
        description = 'Enter helper functions if needed.',
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

#    TestEnvironment(
#        'permutation',
#        label = 'Permutation',
#        description = 'Permutations are allowed.',
#        test = permTest,
#        syntax = SYNTAX_TEMPLATE,
#        semantic = WRAPPER_TEMPLATE,
#        lineNumberOffset = 6,
#        compiler = config.INTERPRETER,
#        interpreter = config.INTERPRETER,
#    ),
))
    

class Cl(AbstractProgrammingBackend):
    """
    This backend tests Common Lisp programs by comparing student and 
    model solution on given test data.
    """
    
    id = 'cl'
    name = 'Cl'
    version = '0.1'

    srcFileSuffix = '.lisp'

    schema = inputSchema
    testSchema = tests

    
    def __init__(self, params, versionFile=__file__, logger = None):
        """
        This constructor is needed to reset the logging environment.
        """
        AbstractProgrammingBackend.__init__(self, params, versionFile)
        # reset logger
        if logger: 
            self.log = logger
        else:
            self.log = log


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
        # With SBCL, the first line on NetBSD is always
        #
        # Couldn't stat /proc/curproc/file; is /proc mounted?
        #
        # This line is removed here.
        uname_lower=sys.platform.lower()
        lines=message.split('\n')
        if uname_lower.startswith('netbsd'):
            lines=lines[1:]
        # Also, in case of an error, the last line of SBCL's output is
        # always
        #
        # unhandled condition in --disable-debugger mode, quitting
        #
        # This and the preceding empty line are also removed.
        if (len(lines)>=3) and lines[-2].startswith('unhandled condition in --disable-debugger'):
            lines=lines[:-3]
        
        return '\n'.join(lines)


    # -- semantic check -------------------------------------------------------
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
        random.seed(self)
        def mk_pkg_name():
            l=list("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
            random.shuffle(l)
            return ''.join(l)

        PKG_STUDENT=mk_pkg_name()
        PKG_MODEL=None
        while True:
            PKG_MODEL=mk_pkg_name()
            if PKG_MODEL!=PKG_STUDENT:
                break
        
        # test for available test specs
        testSpecs = self._getTests(job)

        if len(testSpecs) == 0:
            msg = 'No test specification selected.'
            logging.warn('%s, %s' % (msg, job.getId()))
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
            logging.warn('%s, %s' % (msg, job.getId()))
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
        modelSrc = re.sub(r'\$\{SOURCE\}', model, SEMANTIC_TEMPLATE)
        modelSrc = re.sub(r'\$\{MODULE\}', PKG_MODEL, modelSrc)
        
        studentSrc = re.sub(r'\$\{SOURCE\}', submission, SEMANTIC_TEMPLATE)
        studentSrc = re.sub(r'\$\{MODULE\}', PKG_STUDENT, studentSrc)

        #logging.debug('modelSrc: %s\n' % modelSrc)
        #logging.debug('studentSrc: %s\n' % studentSrc)

        # define return values
        feedback = BackendResult.UNKNOWN
        solved = True

        # run selected test specifications
        for test in testSpecs:

            if not solved: break

            logging.debug('Running semantic check with test: %s' % 
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
                    wrapper = re.sub(r'\$\{%s\}' % field.getName(), repl, wrapper)
                    modelSrc = re.sub(r'\$\{%s\}' % field.getName(), repl, modelSrc)
                    studentSrc = re.sub(r'\$\{%s\}' % field.getName(), repl, studentSrc)

            # insert test function into wrapper code
            wrapper = re.sub(r'\$\{testFunction\}', test.test, wrapper)

            # replace package names
            for (k, n) in ((NS_MODEL,	PKG_MODEL),
                           (NS_STUDENT,	PKG_STUDENT)):
                wrapper = re.sub(r'\$\{%s\}' % k, n, wrapper)

            # remove all remaining placeholders
            wrapper = re.sub(r'\$\{.*\}', '', wrapper)

            #logging.debug('wrapper: %s\n' % wrapper)

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
                    self._writeModule(PKG_MODEL, model, 
                                      self.srcFileSuffix, 
                                      job.getId(), 
                                      test.encoding)
                   
                    # 2nd write students' solution
                    self._writeModule(PKG_STUDENT, student, 
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
                                  
                    logging.error(msg)
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
                    #logging.debug('result: %s' % result)
                    
                    msgItems = result.split(';;')
                    
                    isEqual = (msgItems[0].split('=')[1])
                    expected = msgItems[1].split('=')[1]
                    received = msgItems[2].split('=')[1]
                    
                    #logging.debug('isEqual: %s' % isEqual)
                    #logging.debug('expected: %s' % expected)
                    #logging.debug('received: %s' % received)
                    
                    if isEqual != 'T':
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
