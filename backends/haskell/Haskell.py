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
from backends.haskell.HaskellConf import HaskellConf
from backends.haskell.HaskellIOConf import HaskellIOConf

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
        

    def _postProcessCheckSemantic(self, test, message):
        """
        Post process interpreter messages. Override this method if you need
        to remove or reformat messages.
        
        @param message
        """
        return re.sub('isEqual=', '', message)

    
    def _process_checkSemantics(self, job):
        """
        Checks semantic of a Haskell programs.
        @return a BackendResult object with result code and value
        """

        # get all test data and iterate through them
        repeatFields = self.schema.filterFields(type='RepeatField')
        
        assert repeatFields and len(repeatFields) == 1, \
            "None or more than one field of type 'RepeatField' found"
        
        repeatField = repeatFields[0]
        testdata = repeatField.getAccessor()(job[repeatField.getName()])
        
        if len(testdata) == 0:
            #msg = 'No test data found.'
            #logging.warn(msg)
            return

        if len(self._getTests(job)) == 0:
            msg = 'No test specification found.'
            logging.warn(msg)
            return(0, msg)

        # we have some test data -> lets start the evaluation
        # get model solution and student's submission
        modelSolution = job['modelSolution']
        studentSolution = job['studentSolution']

        assert modelSolution and type(modelSolution) in (StringType,
                                                         UnicodeType), \
            "Semantic check requires valid 'model solution' (%s)" % \
            modelSolution

        assert studentSolution and type(studentSolution) in (StringType,
                                                             UnicodeType), \
            "Semantic check requires valid 'student solution' (%s)" % \
            studentSolution

        modelSolution = HaskellConf.genericModulTemplate % ('Model', modelSolution)
        studentSolution = HaskellConf.genericModulTemplate % ('Student', studentSolution)

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
           
            # write model solution and student solutions to file
            model = self._writeModule('Model', modelSolution, '.hs', job.getId(), test.encoding)
            student = self._writeModule('Student', studentSolution, '.hs', job.getId(), test.encoding)

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

                    # remove all special characters written by runhugs/haskell 
                    result = re.sub(HaskellConf.runhugsRegEx, '', result)
        
                except Exception, e:
                    msg = 'Internal error during semantic check: %s: %s' % \
                          (sys.exc_info()[0], e)
                                  
                    logging.error(msg)
                    return (0, msg)

                # an error occured
                # FIXME: os.EX_OK is available only for Macintosh and Unix!
                if exitcode != EX_OK:
                    result = "\nYour submission failed. Test " \
                             "case was: '%s' (%s)" \
                             "\n\n Received result: %s"\
                             % (t, test.getName(), 
                                self._postProcessCheckSemantic(test, result))

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



class HaskellIO(Haskell):
    """
    Tests functions which return values of type Haskell I/O.
    """
    
    id = 'haskell-io'
    name = 'Haskell I/O'
    version = '1.0'

    testSchema = HaskellIOConf.tests