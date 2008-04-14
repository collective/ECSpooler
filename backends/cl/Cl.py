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

# local imports
from backends.cl.ClConf import ClConf

from lib.data.BackendResult import BackendResult
from lib.AbstractProgrammingBackend import AbstractProgrammingBackend
#from lib.AbstractProgrammingBackend import EX_OK

class Cl(AbstractProgrammingBackend):
    """
    This backend tests Common Lisp programs by comparing student and 
    model solution on given test data.
    """
    
    id = 'cl'
    name = 'Cl'
    version = '0.1'

    srcFileSuffix = '.lisp'

    schema = ClConf.inputSchema
    testSchema = ClConf.tests

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
    
            #logging.debug("xxx: %s" % matches)

            if matches:
                # set line number minus x lines and return
                return re.sub('.*%s:(\d+)'  % self.srcFileSuffix, 
                              'line:%d' % (int(matches[0])-test.lineNumberOffset), 
                              message)
            
        except Exception, e:
            logging.warn('%s: %s' % (sys.exc_info()[0], e))

        return message


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
            l=list("abcdefghijklmnopqrstuvwxyz")
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
        modelSrc = re.sub(r'\$\{SOURCE\}', model, ClConf.semanticCheckTemplate)
        modelSrc = re.sub(r'\$\{MODULE\}', PKG_MODEL, modelSrc)
        
        studentSrc = re.sub(r'\$\{SOURCE\}', submission, ClConf.semanticCheckTemplate)
        studentSrc = re.sub(r'\$\{MODULE\}', PKG_STUDENT, studentSrc)

        logging.debug('modelSrc: %s\n' % modelSrc)
        logging.debug('studentSrc: %s\n' % studentSrc)

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
            for (k, n) in ((ClConf.NS_MODEL,	PKG_MODEL),
                           (ClConf.NS_STUDENT,	PKG_STUDENT)):
                wrapper = re.sub(r'\$\{%s\}' % k, n, wrapper)

            # remove all remaining placeholders
            wrapper = re.sub(r'\$\{.*\}', '', wrapper)

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
                    
                    if isEqual.lower() != 'T':
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