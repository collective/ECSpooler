# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.

################################################################################
#                                Changelog                                     #
################################################################################
#
# 30.04.2009, chbauman:
#       replaced re.sub with replace whereever possible.

import sys, os, re
import logging

from types import StringTypes

from lib.data.BackendResult import BackendResult
from lib.AbstractProgrammingBackend import EX_OK

# local imports
from backends.haskell.Haskell import Haskell
from backends.haskell.Haskell import inputSchema
from backends.haskell.Haskell import tests
from backends.haskell.Haskell import SYNTAX_TEMPLATE
from backends.haskell.Haskell import RUNHUGS_RE

from backends.haskellext import config

# enable logging
log = logging.getLogger('backends.haskellext')

semanticCheckTemplate = \
"""module ${MODULE} where

${SOURCE}

main = ${testData}
"""
    
wrapperTemplate = \
"""module Main where
import Model
import Student

${testFunction}

o1 = Model.main
o2 = Student.main

main = putStr(\"isEqual=\" ++ show(haskell_backend_internal_equality_test (o1) (o2)) ++ \";;expected=\" ++ show(o1) ++ \";;received=\" ++ show(o2))
"""

# use inputSchema of backend Haskel and modify it
inputSchema = inputSchema.copy()
inputSchema.delField('helpFunctions')

# use testSchema of backend Haskell and modify it
testSchema = tests.copy()

testSchema['simple'].compiler = config.INTERPRETER
testSchema['simple'].interpreter = config.INTERPRETER
testSchema['simple'].syntax = SYNTAX_TEMPLATE
testSchema['simple'].semantic = wrapperTemplate

testSchema['permutation'].compiler = config.INTERPRETER
testSchema['permutation'].interpreter = config.INTERPRETER
testSchema['permutation'].syntax = SYNTAX_TEMPLATE
testSchema['permutation'].semantic = wrapperTemplate
    
class HaskellExt(Haskell):
    """
    An alternative backend to the Haskell backend. 
    
    The test calls will be written to the model and student's modules. The 
    result of calling main for model and student's module than will be 
    compared in wrapper code. 
    
    We do this to bypass the problem that a function with the same
    name is definied in the model as well as in the student's module.    
    """
    
    id = 'haskellext'
    name = 'Haskell (extended)'

    schema = inputSchema
    testSchema = testSchema

    def __init__(self, params, versionFile=__file__):
        """
        """
        Haskell.__init__(self, params, versionFile, log)

    def _process_checkSemantics(self, job):
        """
        This method checks the semantic of a Haskell program. Ovrrides method
        _process_checkSemantics in class Haskell.
        
        @return: a tuple with result code and value
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
        submission = job['submission']

        assert model and type(model) in StringTypes, \
            "Semantic check requires valid 'model solution' (%s)" % model

        assert submission and type(submission) in StringTypes, \
            "Semantic check requires valid 'student solution' (%s)" % submission

        #model = TEMPLATE_DEFAULT_MODULE % ('Model', model)
        #submission = TEMPLATE_DEFAULT_MODULE % ('Student', submission)

        #model = re.sub('\$\{SOURCE}', model, HaskellExtConf.semanticCheckTemplate)
        #model = re.sub('\$\{MODULE}', 'Model', model)
        # 
        #submission = re.sub('\$\{SOURCE}', submission, HaskellExtConf.semanticCheckTemplate)
        #submission = re.sub('\$\{MODULE}', 'Student', submission)
        
        model = semanticCheckTemplate.replace('${SOURCE}', model)
        model = model.replace('${MODULE}', 'Model')
        
        submission = semanticCheckTemplate.replace('${SOURCE}', submission)
        submission = submission.replace('${MODULE}', 'Student')

        # define return values
        feedback = BackendResult.UNKNOWN
        solved = True

        # run selected test specifications
        for test in self._getTests(job):

            if not solved: break

            log.debug('Running semantic check with test: %s' % test.getName())

            # set the interpreter
            interpreter = test.interpreter
           
            # set wrapper code
            wrapper = test.semantic
            
            # get the values for all other input fields which are defined in the
            # schema - unless the according data are available in the job object
            for field in self.schema.filterFields(type='InputField'):
                if job.has_key(field.getName()):
                    repl = job[field.getName()]
                    #wrapper = re.sub('\$\{%s\}' % field.getName(), 
                    #                    repl, wrapper)
                    wrapper = wrapper.replace('${%s}' % field.getName(), repl)

            # set test function in wrapper code
            #wrapper = re.sub('\$\{testFunction\}', test.test, wrapper)
            wrapper = wrapper.replace('${testFunction}', test.test)

            # remove all remaining placeholders
            wrapperSrc = re.sub('\$\{.*\}', '', wrapper)

            # run with all test data
            for t in testdata:
                # set testdata in model and students' code
                #modelSrc = re.sub('\$\{%s\}' % repeatField.getName(), t, 
                #                  model)
                #studentSrc = re.sub('\$\{%s\}' % repeatField.getName(), t, 
                #                    submission)
                modelSrc = model.replace('${%s}' % repeatField.getName(), t)
                studentSrc = submission.replace('${%s}' % repeatField.getName(), t)
                
                try:
                    # 1st write model solution
                    self._writeModule('Model', modelSrc, 
                                      self.srcFileSuffix, 
                                      job.getId(), 
                                      test.encoding)
                                       
                    # 2nd write students' solution
                    self._writeModule('Student', 
                                      studentSrc, 
                                      self.srcFileSuffix, 
                                      job.getId(), 
                                      test.encoding)

                    # at least write the wrapper and execute
                    wrapperModule = self._writeModule('wrapper', 
                                                      wrapperSrc, 
                                                      self.srcFileSuffix, 
                                                      job.getId(),
                                                      test.encoding)

                    #log.debug('xxx: %s' % os.path.dirname(wrapperModule['file']))
                    
                    exitcode, result = \
                        self._runInterpreter(interpreter, 
                            os.path.dirname(wrapperModule['file']),
                            os.path.basename(wrapperModule['file']))
    
                    # remove all special characters written by runhugs/haskell
                    #result = re.sub(HaskellExtConf.runhugsRegEx, '', result)
                    result = RUNHUGS_RE.sub('', result) 
        
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
                             % (t, test.getName(), 
                                self._postProcessCheckSemantic(test, result))

                    return BackendResult(False, result)
                        
                # has the students' solution passed this tests?
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
                        
                        # in case of one failed testcase we will
                        # return right now
                        break 

            # end inner for loop
        #end out for loop 

        if solved:
            # TODO: i18n
            feedback = '\nYour submission passed all tests.'

        return BackendResult(solved, feedback)
