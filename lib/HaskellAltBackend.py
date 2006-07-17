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
from data import *
from util.utils import *

from util.BackendSchema import TestEnvironment, RepeatField, InputField, Schema
import HaskellBackend

INTERPRETER = '/opt/hugs/bin/runhugs -P"{HUGS}:/home/amelung/haskell_libs/:"'

TEMPLATE_MODULE_MAIN_SYNTAX = \
"""module Main where

${SOURCE}
   
main = putStr(\"\")
"""

TEMPLATE_MODULE_MAIN_SEMANTIC= \
"""module ${MODULE} where

${SOURCE}
   
main = ${testData}
"""

TEMPLATE_MODULE_MAIN_WRAPPER = \
"""module Main where
import Model
import Student

${testFunction}

o1 = Model.main
o2 = Student.main

main = putStr(\"isEqual=\" ++ show(test (o1) (o2)) ++ \";;expected=\" ++ show(o1) ++ \";;received=\" ++ show(o2))
"""

# modify inputSchema from HaskellBackend
inputSchema = HaskellBackend.inputSchema.copy()
inputSchema.delField('helpFunctions')

# modify testSchema from HaskellBackend
testSchema = HaskellBackend.tests.copy()

testSchema['simple'].compiler = INTERPRETER
testSchema['simple'].interpreter = INTERPRETER
testSchema['simple'].syntax = TEMPLATE_MODULE_MAIN_SYNTAX
testSchema['simple'].semantic = TEMPLATE_MODULE_MAIN_WRAPPER

testSchema['permutation'].compiler = INTERPRETER
testSchema['permutation'].interpreter = INTERPRETER
testSchema['permutation'].syntax = TEMPLATE_MODULE_MAIN_SYNTAX,
testSchema['permutation'].semantic = TEMPLATE_MODULE_MAIN_WRAPPER

class HaskellAltBackend(HaskellBackend.HaskellBackend):
    """
    An alternative to HaskellBackend. Test data will for model and student's
    solution in a seperate module. The result than will be compared in the
    wrapper. We do this to bypass the problem that a function with the same
    name is definied in the model as well as in the student's module.    
    """
    
    id = 'haskell-alt'
    name = 'Haskell (alternate)'
    version = '1.0'

    schema = inputSchema
    testSchema = testSchema

#    def _postProcessCheckSyntax(self, test, message):
#        """
#        @see AbtractSimpleBackend._postProcessCheckSyntax
#        """
#        # find line number in result
#        matches = re.findall('%s":(\d+)' % self.srcFileSuffix, message)
#        
#        if len(matches):
#            # set line number minus x lines and return
#            return re.sub('".*%s":(\d+)'  % self.srcFileSuffix, 
#                          'line: %d' % (int(matches[0])-test.lineNumberOffset), 
#                          message)
#        else:
#            return message
        

#    def _postProcessCheckSemantic(self, test, message):
#        """
#        Post process interpreter messages. Override this method if you need
#        to remove or reformat messages.
#        
#        @param message
#        """
#        return re.sub('isEqual=', '', message)

    
    def _process_checkSemantics(self, job):
        """
        Checks semantic of a Haskell programs.
        @return a BackendResult object with result code and value
        """

        # get all test data to iterate through them
        repeatFields = self.schema.filterFields(type='RepeatField')
        
        assert repeatFields and len(repeatFields) == 1, \
            "None or more than one field of type 'RepeatField' found."
        
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
                               TEMPLATE_MODULE_MAIN_SEMANTIC)
        modelSolution = re.sub('\$\{MODULE}', 'Model', 
                               modelSolution)
        
        studentSolution = re.sub('\$\{SOURCE}', studentSolution,
                                 TEMPLATE_MODULE_MAIN_SEMANTIC)
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
                                                    job['id'], 
                                                    test.encoding)
                   
                    #exitcode, result = \
                    #    self._runInterpreter(interpreter, 
                    #                os.path.dirname(modelModule['file']),
                    #                os.path.basename(modelModule['file']))
                    #
                    #if exitcode == os.EX_OK:
                    #    expected = result
                    
                    # 2nd execute students' solution
                    studentModule = self._writeModule('Student', 
                                                      studentSrc, 
                                                      self.srcFileSuffix, 
                                                      job['id'], 
                                                      test.encoding)

                        #exitcode, result = \
                        #    self._runInterpreter(interpreter, 
                        #            os.path.dirname(studentModule['file']),
                        #            os.path.basename(studentModule['file']))
                        #
                        #if exitcode == os.EX_OK:
                        #    received = result
                            
                            #wrapperSrc = re.sub('\$\{ResultModel\}', expected, 
                            #                    wrapper)
        
                            #wrapperSrc = re.sub('\$\{ResultStudent\}', received, 
                            #                    wrapperSrc)

                            # remove all remaining placeholders
                            #wrapperSrc = re.sub('\$\{.*\}', '', wrapperSrc)

                    # last write the wrapper and execute
                    wrapperModule = self._writeModule('wrapper', 
                                                      wrapperSrc, 
                                                      self.srcFileSuffix, 
                                                      job['id'],
                                                      test.encoding)

                    logging.debug('xxx: %s' % os.path.dirname(wrapperModule['file']))

                    exitcode, result = \
                        self._runInterpreter(interpreter, 
                            os.path.dirname(wrapperModule['file']),
                            os.path.basename(wrapperModule['file']))

    
                    # remove all special characters written by runhugs/haskell 
                    result = re.sub(HaskellBackend.REGEX_RUNHUGS_SPECIALS, 
                                    '', result)
        
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
