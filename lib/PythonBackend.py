# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.

import sys, os, re, popen2, tempfile, threading
import logging

from types import StringType

# local imports
from data import *
from util.utils import *
from util.BackendSchema import TestEnvironment, RepeatField, InputField, Schema

from AbstractSimpleBackend import AbstractSimpleBackend


#INTERPRETER = '-a -f ../.systrace/opt_python_bin_python /opt/python/bin/python'
#INTERPRETER = '/usr/local/bin/python'
INTERPRETER = '/opt/python/bin/python'

TEMPLATE_SEMANTIC = \
"""import sys
import Model
import Student

${helpFunctions}

# must be named 'test'
${testFunction}

o1 = Model.${testData}
o2 = Student.${testData}

print >> sys.stdout, \"isEqual=\" + str(test(o1, o2)) + \";;expected=\" + str(o1) + \";;received=\" + str(o2)
"""


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
            description = 'Simple test; only exact result are allowed.',
            test = 'def test(a, b): return (a == b)',
            semantic = TEMPLATE_SEMANTIC,
            interpreter = INTERPRETER,
        ),

        TestEnvironment(
            'permutation',
            label = 'Permutation',
            description = 'Permutations are allowed.',
            test = 'def test(a, b): return (b == a)',
            semantic = TEMPLATE_SEMANTIC,
            interpreter = INTERPRETER,
        ),
    ))

class PythonBackend(AbstractSimpleBackend):
    """
    Backend for checking Python programs. This class is inherited 
    from AbstractFPBackend.
    """
    
    id = 'python'
    name = 'Python'
    version = '1.0'

    srcFileSuffix = '.py'

    schema = inputSchema
    testSchema = tests


    def _postProcessCheckSyntax(self, test, message):
        """
        @see AbtractSimpleBackend._postProcessCheckSyntax
        """

        # replace path and filename
        return re.sub('File ".*%s", ' % self.srcFileSuffix, '', message)


    def _process_checkSemantics(self, job):
        """
        Checks semantic of a Python programs.
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

        # write model solution and answer files
        model = self._writeModule('Model', modelSolution, '.py', job['id'])
        student = self._writeModule('Student', studentSolution, '.py', job['id'])

        # 2. get all test data to iterate through them
        repeatFields = self.schema.filterFields(__name__='testData')
        
        assert repeatFields and len(repeatFields) == 1, \
            'None or more than one RepeatField found.'
        
        repeatField = repeatFields[0]
        testdata = repeatField.getAccessor()(job[repeatField.getName()])

        # 3. define return values
        feedback = ''
        solved = 1 # 1 means testing was successful

        if len(self._getTests(job)) == 0:
            msg = 'No test specification found.'
            logging.warn(msg)
            return(0, msg)

        # 4. run selected test specifications
        for test in self._getTests(job):

            if solved == 0: break

            logging.debug('Running semantic check with test: %s' % 
                          test.getName())

            # get the interpreter
            interpreter = test.interpreter
           
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
#                    (exitcode, response) = \
#                        self._runPython(interpreter, wrapper, job['id'])
#
#                    assert type(response) == list, '%s (%s)' % (response, type(response))
#    
#                    result = ''.join(response)
#                    assert type(result) == StringType, '%s (%s)' % (result, type(result))
                    exitcode, result = \
                        self._runPython(interpreter, wrapper, job['id'])

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
                             % (t, test.getName(), result)

                    return (0, result)
                
                # has the students' solution passed this tests?
                else:
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


    def _runPython(self, intepreter, source, jobID):
        """
        @see _runInterpreter in class AbstractSimpleBackend
        """
        # write module for wrapper
        wrapperModule = self._writeModule('wrapper', source, 
                                     self.srcFileSuffix, jobID)

        return self._runInterpreter(intepreter,
                                    os.path.dirname(wrapperModule['file']),
                                    os.path.basename(wrapperModule['file']))


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
        tB = PythonBackend({
                    'host': socket.getfqdn(), 
                    'port': 5060, 
                    'capeserver': 'http://%s:5050' % socket.getfqdn(), 
                    'srv_auth': {'username':'demo', 'password':'foobar'}
                })

        tB.start()

    else:
        time.sleep(1)
        print 'pid=', cpid