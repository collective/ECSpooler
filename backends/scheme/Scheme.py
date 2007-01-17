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
    version = '1.1'

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
    
            #logging.debug("xxx: %s" % matches)
    
            # set line number minus x lines and return
            return re.sub('.*%s:(\d+)'  % self.srcFileSuffix, 
                          'line:%d' % (int(matches[0])-test.lineNumberOffset), 
                          message)
            
        except Exception, e:
            logging.warn('%s: %s' % (sys.exc_info()[0], e))
            return message


    def _postProcessCheckSemantic(self, test, message):
        """
        @see _postProcessCheckSyntax
        """
        return self._postProcessCheckSyntax(test, message)
   
    
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

        # prepare source code for model and student' solution which will be
        # stored in seperate files as modules
        modelSrc = re.sub('\$\{SOURCE}', modelSolution, SchemeConf.semanticCheckTemplate)
        modelSrc = re.sub('\$\{MODULE}', 'model', modelSrc)
        
        studentSrc = re.sub('\$\{SOURCE}', studentSolution, SchemeConf.semanticCheckTemplate)
        studentSrc = re.sub('\$\{MODULE}', 'student', studentSrc)

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
            wrapper = test.semantic
            
            # get values for all other input fields from schema which are 
            # available in the job data, e.g., helpFunctions
            for field in self.schema.filterFields(type='InputField'):
                if job.has_key(field.getName()):
                    repl = job[field.getName()]
                    wrapper = re.sub('\$\{%s\}' % field.getName(), repl, wrapper)
                    modelSrc = re.sub('\$\{%s\}' % field.getName(), repl, modelSrc)
                    studentSrc = re.sub('\$\{%s\}' % field.getName(), repl, studentSrc)

            # insert test function in wrapper code
            wrapper = re.sub('\$\{testFunction\}', test.test, wrapper)

            # remove all remaining placeholders
            wrapper = re.sub('\$\{.*\}', '', wrapper)

            # run with all test data
            for t in testdata:
                # add testdata to model and student modules source code
                model = re.sub('\$\{%s\}' % repeatField.getName(), t, modelSrc)
                student = re.sub('\$\{%s\}' % repeatField.getName(), t, studentSrc)

                #logging.debug('xxx: %s' % t)
                
                try:
                    # 1st write model solution
                    modelModule = self._writeModule('model', model, 
                                                    self.srcFileSuffix, 
                                                    job.getId(), 
                                                    test.encoding)
                   
                    # 2nd write students' solution
                    studentModule = self._writeModule('student', student, 
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
                    return (0, msg)

                # an error occured
                if exitcode != os.EX_OK:
                    result = "\nYour submission failed. Test case was: " \
                             "'%s' (%s) \n\n Received result: %s" \
                             % (t, test.getName(), 
                                self._postProcessCheckSemantic(test, result))

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
