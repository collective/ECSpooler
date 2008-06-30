# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.

import sys, os, re
import logging

from types import StringType, UnicodeType

# local imports
from backends.python.PythonConf import PythonConf

from lib.data.BackendResult import BackendResult
from lib.AbstractProgrammingBackend import AbstractProgrammingBackend
from lib.AbstractProgrammingBackend import EX_OK

class Python(AbstractProgrammingBackend):
    """
    Backend for checking Python programs. This class is inherited 
    from AbstractFPBackend.
    """
    
    id = 'python'
    name = 'Python'
    #version = '1.0'

    srcFileSuffix = '.py'

    schema = PythonConf.inputSchema
    testSchema = PythonConf.tests


    def _postProcessCheckSyntax(self, test, message):
        """
        @see: AbtractSimpleBackend._postProcessCheckSyntax
        """

        # replace path and filename
        return re.sub('File ".*%s", ' % self.srcFileSuffix, '', message)


    def _process_checkSemantics(self, job):
        """
        Checks semantic of a Python programs.
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


        # get model solution and student's submission
        model = job['modelSolution']
        submission = job['submission']

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

            logging.debug('Running semantic check with test: %s' % 
                          test.getName())

            # get the interpreter
            interpreter = test.interpreter
           
            # 4.0 write model solution and answer files
            self._writeModule('Model', model, '.py', job.getId(), test.encoding)
            self._writeModule('Student', submission, '.py', job.getId(), test.encoding)

            # 4.1. get wrapper code
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
                                 
                # execute wrapper code in interpreter
                try:
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
                if exitcode != EX_OK:
                    result = "\nYour submission failed. Test " \
                             "case was: '%s' (%s)" \
                             "\n\n Received result: %s"\
                             % (t, test.getName(), result)

                    return BackendResult(False, result)
                
                # has the students' solution passed this tests?
                else:
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

