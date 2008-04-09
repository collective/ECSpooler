# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.
import sys, os, re
import logging

from types import StringTypes

# local imports
from backends.haskell.HaskellConf import HaskellConf

from lib.data.BackendResult import BackendResult
from lib.AbstractProgrammingBackend import AbstractProgrammingBackend
from lib.AbstractProgrammingBackend import EX_OK

class Haskell(AbstractProgrammingBackend):
    """
    A backend for checking Haskell programs by comparing
    student and model solution on given test data.
    """
    
    id = 'haskell'
    name = 'Haskell'
    version = '1.0'

    srcFileSuffix = '.hs'

    schema = HaskellConf.inputSchema
    testSchema = HaskellConf.tests

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
            result = re.sub('\$\{SOURCE\}', src, test.syntax)
            
        else:
            result = src

        return result, 'Student'


    def _postProcessCheckSyntax(self, test, message):
        """
        @see: AbtractSimpleBackend._postProcessCheckSyntax
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
        

    def _postProcessCheckSemantic(self, test, message):
        """
        Post process interpreter messages. Override this method if you need
        to remove or reformat messages.
        
        @param: message:
        """
        result = re.sub('isEqual=', '', message)
        result = re.sub('/.*/ECSpooler/backends/haskell/', '', result)

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


        # we have some test data -> lets start the evaluation
        # get model solution and student's submission
        model = job['modelSolution']
        submission = job['submission']

        assert model and type(model) in StringTypes, \
            "Semantic check requires valid 'model solution' (%s)" % model

        assert submission and type(submission) in StringTypes, \
            "Semantic check requires valid 'student solution' (%s)" % submission

        model = HaskellConf.genericModulTemplate % ('Model', model)
        submission = HaskellConf.genericModulTemplate % ('Student', submission)

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
           
            # write model solution and students' solution to a file
            self._writeModule('Model', model, '.hs', job.getId(), test.encoding)
            self._writeModule('Student', submission, '.hs', job.getId(), test.encoding)

            # get wrapper code
            src = test.semantic
            
            # get values for all other input fields from schema which are 
            # available in the job data
            for field in self.schema.filterFields(type='InputField'):
                if job.has_key(field.getName()):
                    repl = job[field.getName()]
                    src = re.sub('\$\{%s\}' % field.getName(), repl, src)

            # set test function
            src = re.sub('\$\{testFunction\}', test.test, src)

            # run with all test data
            for t in testdata:
                # and now add repeatable data values
                wrapper = re.sub('\$\{%s\}' % repeatField.getName(), t, src)
        
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
                    result = HaskellConf.RUNHUGS_RE.sub('', result)  
        
                except Exception, e:
                    msg = 'Internal error during semantic check: %s: %s' % \
                          (sys.exc_info()[0], e)
                                  
                    logging.error(msg)
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
                    #logging.debug(result)
                    
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

