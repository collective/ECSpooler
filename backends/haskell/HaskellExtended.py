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
from Haskell import Haskell
from HaskellExtendedConf import HaskellExtendedConf

from lib.AbstractProgrammingBackend import EX_OK

class HaskellExtended(Haskell):
    """
    An alternative backend to the Haskell backend. 
    
    The test calls will be written to the model and student's modules. The 
    result of calling main for model and student's module than will be 
    compared in wrapper code. 
    
    We do this to bypass the problem that a function with the same
    name is definied in the model as well as in the student's module.    
    """
    
    id = 'haskell-extended'
    name = 'Haskell (extended)'
    version = '1.0'

    schema = HaskellExtendedConf.inputSchema
    testSchema = HaskellExtendedConf.testSchema

    def _process_checkSemantics(self, job):
        """
        This method checks the semantic of a Haskell program. Ovrrides method
        _process_checkSemantics in class Haskell.
        
        @return a tuple with result code and value
        """

        # get all test data to iterate through them
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

        #modelSolution = TEMPLATE_DEFAULT_MODULE % ('Model', modelSolution)
        #studentSolution = TEMPLATE_DEFAULT_MODULE % ('Student', studentSolution)

        modelSolution = re.sub('\$\{SOURCE}', modelSolution, 
                               HaskellExtendedConf.semanticCheckTemplate)
        modelSolution = re.sub('\$\{MODULE}', 'Model', 
                               modelSolution)
        
        studentSolution = re.sub('\$\{SOURCE}', studentSolution,
                                 HaskellExtendedConf.semanticCheckTemplate)
        studentSolution = re.sub('\$\{MODULE}', 'Student',
                                 studentSolution)

        # define return values
        feedback = ''
        solved = 1 # 1 means testing was successful

        # run selected test specifications
        for test in self._getTests(job):

            if solved == 0: break

            logging.debug('Running semantic check with test: %s' % 
                          test.getName())

            # set the interpreter
            interpreter = test.interpreter
           
            # set wrapper code
            wrapper = test.semantic
            
            # get the values for all other input fields which are defined in the
            # schema - unless the according data are available in the job object
            for field in self.schema.filterFields(type='InputField'):
                if job.has_key(field.getName()):
                    repl = job[field.getName()]
                    wrapper = re.sub('\$\{%s\}' % field.getName(), 
                                        repl, wrapper)

            # set test function in wrapper code
            wrapper = re.sub('\$\{testFunction\}', test.test, wrapper)

            # remove all remaining placeholders
            wrapperSrc = re.sub('\$\{.*\}', '', wrapper)

            # run with all test data
            for t in testdata:
                # set testdata in model and students' code
                modelSrc = re.sub('\$\{%s\}' % repeatField.getName(), t, 
                                  modelSolution)
                studentSrc = re.sub('\$\{%s\}' % repeatField.getName(), t, 
                                    studentSolution)
                
                try:
                    # 1st execute model solution
                    modelModule = self._writeModule('Model', modelSrc, 
                                                    self.srcFileSuffix, 
                                                    job.getId(), 
                                                    test.encoding)
                   
                    #exitcode, result = \
                    #    self._runInterpreter(interpreter, 
                    #                os.path.dirname(modelModule['file']),
                    #                os.path.basename(modelModule['file']))
                    #
                    #if exitcode == os.EX_OK:
                    #    received = result
                    
                    # 2nd execute students' solution
                    studentModule = self._writeModule('Student', 
                                                      studentSrc, 
                                                      self.srcFileSuffix, 
                                                      job.getId(), 
                                                      test.encoding)

                    #exitcode, result = \
                    #    self._runInterpreter(interpreter, 
                    #            os.path.dirname(studentModule['file']),
                    #            os.path.basename(studentModule['file']))
                    #
                    #if exitcode == os.EX_OK:
                    #    received = result

                    # last write the wrapper and execute
                    wrapperModule = self._writeModule('wrapper', 
                                                      wrapperSrc, 
                                                      self.srcFileSuffix, 
                                                      job.getId(),
                                                      test.encoding)

                    logging.debug('xxx: %s' % os.path.dirname(wrapperModule['file']))

                    exitcode, result = \
                        self._runInterpreter(interpreter, 
                            os.path.dirname(wrapperModule['file']),
                            os.path.basename(wrapperModule['file']))

    
                    # remove all special characters written by runhugs/haskell 
                    result = re.sub(HaskellExtendedConf.runhugsRegEx, '', result)
        
                except Exception, e:
                    msg = 'Internal error during semantic check: %s: %s' % \
                          (sys.exc_info()[0], e)
                                  
                    logging.error(msg)
                    return (0, msg)

                # an error occured
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
                        
                        # in case of one failed testcase we will
                        # return right now
                        break 

            # end inner for loop
        #end out for loop 

        if solved == 1:
            # TODO: i18n
            feedback = '\nYour submission passed all tests.'

        return (solved, feedback)
