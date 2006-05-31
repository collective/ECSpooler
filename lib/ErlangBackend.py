# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.

import sys, os, re, popen2, tempfile
import threading
import logging

from types import StringType

# local imports
from data import *
from util.utils import *

from util.BackendSchema import TestEnvironment, RepeatField, InputField, Schema
from AbstractSimpleBackend import AbstractSimpleBackend, EX_OK

TEST_SIMPLE = \
"""
test(A, A) -> true;
test(_, _) -> false.
"""

# test for equality of lists modulo permutation
# multiple occurrences of elements allowed
TEST_PERM = \
"""
test([],[])  -> true;
test(L1, L2) -> if length(L1) /= length(L2) -> false;
    true -> lists:member(hd(L1), L2) andalso test(tl(L1), lists:delete(hd(L1),L2))
    end.
"""

# wrapper code
TEMPLATE_SEMANTIC = \
"""-module(wrapper).
-export([start/0]).

${helpFunctions}

${testFunction}

o1() -> model:${testData}.
o2() -> student:${testData}.

start() -> io:fwrite("isEqual=~w;;expected=~w;;received=~w", [test(o1(), o2()), o1(), o2()]).
"""

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
            label = 'Help functions',
            description = 'Enter help functions if needed.',
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
                        'and test data as parameters of this funtion. '+
                        'Each function call must be written in a single line.',
            i18n_domain = 'EC',
        ),
))

# testSchema
tests = Schema((

        TestEnvironment(
            'simple',
            label = 'Simple',
            description = 'Test without permutations',
            test = TEST_SIMPLE,
            semantic = TEMPLATE_SEMANTIC,
            #lineNumberOffset = 2,
            compiler = '/opt/erlang/bin/erlc %s',
            interpreter = '/opt/erlang/bin/erl -noshell -s %s -s init stop',
        ),

        TestEnvironment(
            'permutation',
            label = 'Permutation',
            description = 'Test with permutations',
            test = TEST_PERM,
            semantic = TEMPLATE_SEMANTIC,
            #lineNumberOffset = 2,
            compiler = '/opt/erlang/bin/erlc %s',
            interpreter = '/opt/erlang/bin/erl -noshell -s %s -s init stop',
        ),
    ))

class ErlangBackend(AbstractSimpleBackend):
    """
    A simple checker class for checking Haskell programs by comparing
    student and model solution on given test data.
    """
    
    id = 'erlang'
    name = 'Erlang'

    srcFileSuffix = '.erl'

    schema = inputSchema
    testSchema = tests
    version = '1.0'

    # -- check syntax ---------------------------------------------------------
    def _preProcessCheckSyntax(self, test, src, **kwargs):
        """
        Replace module name in students' submission.
        
        @see AbstractSimpleBackend._preProcessCheckSyntax
        @return modified source code and new module name
        """

        result = re.sub('-module\((.*)\)\.', '-module(student).', 
                        src)

        return result, 'student'


    def _postProcessCheckSyntax(self, test, message):
        """
        @see AbtractSimpleBackend._postProcessCheckSyntax
        """
        # replace path and filename
        return re.sub('.*%s:(\d+)' % self.srcFileSuffix, 
                      'line: \g<1>', 
                      message)
                      

    # -- check semantics ------------------------------------------------------
    def _preProcessCheckSemantic(self, test, src, **kwargs):
        """
        Pre process student's submissions and semantic check wrapper code. 
        Override this method if you need to reformat the wrapper code or 
        the student's submission. 
        """                                  
        return src

    
    def _process_checkSemantics(self, job):
        """
        Runs sematic test on a Erlang program.
        Remenber: Before we can run the wrapper code we have to compile it!!
        
        @return a BackendResult object with result code and value
        """
        # 1. get model solution and student's submission
        modelSolution = job['modelSolution']
        studentSolution = job['studentSolution']

        assert modelSolution and type(modelSolution) == StringType, \
            "Semantic check requires valid 'model solution' (%s)" % \
            modelSolution

        assert studentSolution and type(studentSolution) == StringType, \
            "Semantic check requires valid 'student solution' (%s)" % \
            studentSolution
            
        self._preProcessCheckSemantic(test, src)

        # replace module name in model and student solution
        modelSolution = re.sub('-module\((.*)\)\.', 
                               '-module(model).', 
                               modelSolution)

        #studentSolution = re.sub('-module\((.*)\)\.', 
        #                         '-module(student).', 
        #                         studentSolution)

        # write model solution in a file
        model = self._writeModule('model', modelSolution, 
                                  self.srcFileSuffix, job['id'])

        #student = self._writeModule('student', studentSolution, 
        #                            self.srcFileSuffix, job['id'])


        # 2. get a repeat field and iterate through the corresponding data
        #repeatFields = self.schema.filterFields(__name__='testData')
        repeatFields = self.schema.filterFields(type='RepeatField')
        
        assert repeatFields and len(repeatFields) == 1, \
            'None or more than one RepeatField found.'
        
        repeatField = repeatFields[0]
        testdata = repeatField.getAccessor()(job[repeatField.getName()])

        # 3. define return values
        feedback = ''
        solved = 1

        if len(self._getTests(job)) == 0:
            msg = 'No test specification found.'
            logging.warn(msg)
            return(0, msg)

        # 4. run selected test specifications
        for test in self._getTests(job):

            if solved == 0: break

            logging.debug('Running semantic check with test: %s' % 
                          test.getName())

            # get the compiler
            compiler = test.compiler
            
            # FIXME: We expect that the student solution was already written 
            #        and compiled in process_checkSyntax. That could cause some
            #        problems. In case we have more than one test scenario
            #        the written/compiled student solution corresponds to the 
            #        last test scenario. Therefore we have to write and compile
            #        it twice.

            # compile model solution
            exitcode, result = self._runInterpreter(compiler, 
                                             os.path.dirname(model['file']),
                                             os.path.basename(model['file']))
            assert exitcode == EX_OK, 'Errors in model solution: %s' % result
             
            # we have already written and compiled the student solution 
            # in checkSynatx method

            #exitcode, result = self._runInterpreter(compiler,
            #                                 os.path.dirname(student['file']),
            #                                 os.path.basename(student['file']))
            #assert exitcode == EX_OK, 'Errors in student solution: %s' % result
            
            # get the interpreter
            interpreter = test.interpreter
           
            # 4.1. set wrapper source
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


                # compile and execute wrapper
                try:
                    # write module for wrapper
                    wrapperModule = self._writeModule('wrapper', wrapper, 
                                                 self.srcFileSuffix, job['id'])

                    # compile the wrapper
                    exitcode, result = \
                        self._runInterpreter(compiler,
                                       os.path.dirname(wrapperModule['file']),
                                       os.path.basename(wrapperModule['file']))
                    
                    assert exitcode == EX_OK, \
                        'Error in wrapper code for semantic check: %s' % result

                    # run the wrapper
                    exitcode, result = \
                        self._runInterpreter(interpreter, 
                                       os.path.dirname(wrapperModule['file']),
                                       'wrapper')

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
                             % (t, test.getName(), result)

                    return (0, result)
                        
                # has the students' solution passed this tests?
                else:
                    logging.debug(result)
                    
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

# -- some testing -------------------------------------------------------------
import socket

if __name__ == "__main__":
    """
    """
    try:
        cpid = os.fork()
    except AttributeError, aerr:
        print('os.fork not defined - skipping.')
        cpid = 0

    if cpid == 0:
        tB = ErlangBackend({
                    'host': socket.getfqdn(), 
                    'port': 5060, 
                    'capeserver': 'http://%s:5050' % socket.getfqdn(), 
                    'srv_auth': {'username':'demo', 'password':'foobar'}
                })

        tB.start()

    else:
        time.sleep(1)
        print 'pid=', cpid