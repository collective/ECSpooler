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
from backends.scheme.SchemeConf import SchemeConf

from lib.AbstractProgrammingBackend import AbstractProgrammingBackend
from lib.AbstractProgrammingBackend import EX_OK

class Scheme(AbstractProgrammingBackend):
    """
    This backends tests Scheme programs by comparing student and 
    model solution on given test data.
    """
    
    id = 'scheme'
    name = 'Scheme'
    version = '1.0'

    srcFileSuffix = '.scm'

    schema = SchemeConf.inputSchema
    testSchema = SchemeConf.tests


    def _preProcessCheckSyntax(self, test, src, **kwargs):
        """
        @see _preProcessCheckSyntax in lib/AbstractProgrammingBackend
        """
        result = ''
        
        if test.syntax:
            result = re.sub('\$\{SOURCE\}', src, test.syntax)
            
        else:
            result = src

        return result, 'student'


    def _postProcessCheckSyntax(self, test, message):
        """
        @see AbtractSimpleBackend._postProcessCheckSyntax
        """
        try:
            # find line number in result
            matches = re.findall('%s:(\d+)' % self.srcFileSuffix, message)
    
            logging.debug("xxx: %s" % matches)
    
            # set line number minus x lines and return
            return re.sub('.*%s:(\d+)'  % self.srcFileSuffix, 
                          'line: %d' % (int(matches[0])-test.lineNumberOffset), 
                          message)
            
        except Exception, e:
            logging.warn('%s: %s' % (sys.exc_info()[0], e))
            return message


    def _process_checkSemantics(self, job):
        """
        Checks semantic of a Haskell programs.
        @return a BackendResult object with result code and value
        """

        # get all test data to iterate through them
        repeatFields = self.schema.filterFields(__name__='testData')
        
        assert repeatFields and len(repeatFields) == 1, \
            'None or more than one RepeatField found.'
        
        repeatField = repeatFields[0]
        testdata = repeatField.getAccessor()(job[repeatField.getName()])

        # define return values
        feedback = ''
        solved = 1 # 1 means testing was successful

        # get model solution and student's submission
        modelSolution = job['modelSolution']
        studentSolution = job['studentSolution']

        assert modelSolution and type(modelSolution) in (StringType, UnicodeType), \
            "Semantic check requires valid 'model solution' (%s)" % \
            repr(modelSolution)

        assert studentSolution and type(studentSolution) in (StringType, UnicodeType), \
            "Semantic check requires valid 'student solution' (%s)" % \
            repr(studentSolution)

        #modelSolution = TEMPLATE_MODULE % ('Model', modelSolution)
        #studentSolution = TEMPLATE_MODULE % ('Student', studentSolution)

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
           
            # get wrapper code for semantic check
            src = test.semantic
            
            # insert model solution as module in wrapper code
            src = re.sub('\$\{modelSolution\}', modelSolution, src)
            
            # insert student's solution as module in wrapper code
            src = re.sub('\$\{studentSolution\}', studentSolution, src)

            # get values for all other input fields from schema which are 
            # available in the job data
            for field in self.schema.filterFields(type='InputField'):
                if job.has_key(field.getName()):
                    repl = job[field.getName()]
                    src = re.sub('\$\{%s\}' % field.getName(), repl, src)

            # insert test function in wrapper code
            src = re.sub('\$\{testFunction\}', test.test, src)

            # 4.4. run with all test data
            for t in testdata:
                # and now add repeatable data values
                wrapper = re.sub('\$\{%s\}' % repeatField.getName(), t, src)
        
                # remove all remaining placeholders
                wrapper = re.sub('\$\{.*\}', '', wrapper)
                                 
                # execute wrapper code in haskell interpreter
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
                    return (0, msg)

                # an error occured
                if exitcode != os.EX_OK:
                    result = "\nYour submission failed. Test case was: " \
                             "'%s' (%s) \n\n Received result: %s" \
                             % (t, test.getName(), result)
                    return (0, result)
                        
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
                    
                    if isEqual.lower() != '#t':
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
