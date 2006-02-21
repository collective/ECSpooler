# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.

import os, re, popen2, tempfile
import threading
import logging

from data import *
from util.utils import *
from util.BackendSchema import TestEnv, Field, Schema
from AbstractFPBackend import AbstractFPBackend


#INTERPRETER = '-a -f ../.systrace/opt_python_bin_python /opt/python/bin/python'
INTERPRETER = '/usr/local/bin/python'


TEMPLATE_SEMANTIC = \
"""import sys
import %s
import %s
%s
print >> sys.stdout, \"isEqual=\" + str(test(%s, %s)) + \";;expected=\" + str(%s) + \";;received=\" + str(%s)
"""

localSchema = Schema((

        TestEnv(
            'default',
            simple = 'def test(a, b): return (a == b)',
            semantic = TEMPLATE_SEMANTIC,
        ),

        Field(
            'modelSolution', 
            required = True, 
            label = 'Model solution',
            description = 'Enter a model solution.',
            i18n_domain = 'EC',
        ),
        
        Field(
            'testData', 
            required = True, 
            label = 'Test data',
            description = 'Enter one or more function calls. '+ 
                        'A function call consists of the ' + 
                        'function name (given in the exercise directives) ' + 
                        'and test data as parameters of this funtion. '+
                        'Each function call must be written in a single line.',
            i18n_domain = 'EC',
        ),

        Field(
            'comparator', 
            required = False, 
            label = 'Comparator',
            description = 'Enter your comparator function.' + 
                        'The comparator function expects two parameters ' +
                        'and returns a value like True or False. It must ' +
                        'be named "test" and.',
            i18n_domain = 'EC',
        ),
    ))

class PythonBackend(AbstractFPBackend):
    """
    Backend for checking Python programs. This class is inherited 
    from AbstractFPBackend.
    """
    
    id = 'python'
    name = 'Python'
    schema = localSchema
    

    def checkSyntax(self):
        """
        Checks syntax of a Python program.
        @return A CheckResult object with error code and message text
        """
        assert self._job, "Job data is required."

        source = self._job["student_solution"]

        try:
            (exitcode, response) = self._runPython(source)

            assert type(response) == list

            result = ''.join(response)
            assert type(result) == type('')

        except Exception, e:
            # FIXME: use log instead print
            import traceback
            traceback.print_exc()
            
            print "Internal Error during syntax check: %s" % repr(e)
            return checkresult.CheckResult(20, repr(e))

        # consider exit code
        if exitcode != os.EX_OK:
            return checkresult.CheckResult(exitcode, result)
        else:
            return checkresult.CheckResult(0, 'Syntax check succeeded.')


    def checkSemantics(self):
        """
        Checks syntax and semantic of Python programs.
        @return A CheckResult object with error code and message text
        """
        assert self._job, "Job data is required."
        
        model = self._job["sample_solution"]
        answer = self._job["student_solution"]
        comparator = self._job["comparator"]
        testdata = self._job["testdata"]

        # write model solution and answer files
        model = self._createModule(prefix='Model', source=model)
        answer = self._createModule(prefix='Student', source=answer)

        # write wrapper code and execute it for all test data
        feedback = ''
        # solved == 0 means the check was successful
        solved = 0
        
        tests = testdata.split('\n')
        
        for test in tests:
            if not comparator:
                comparator = TEST_SIMPLE

            if test.strip():    
                # complete the wrapper code
                wrapper = TEMPLATE_SEMANTIC % (
                            model['module'], 
                            answer['module'],
                            comparator,
                            model['module'] + '.' + test,
                            answer['module'] + '.' + test,
                            model['module'] + '.' + test,
                            answer['module'] + '.' + test,
                        )
           
                # execute wrapper code in haskell interpreter
                try:
                    (exitcode, response) = self._runPython(source)

                    assert type(response) == list
    
                    result = ''.join(response)
                    assert type(result) == type('')
        
                except Exception, e:
                    loggin.error('Internal Error during semantic check: %s' % str(e))

                    # delete temp files because we leave this method right now
                    os.remove(model['file'])
                    os.remove(answer['file'])
                    return checkresult.CheckResult(-1, repr(e))
        
                # an error occured
                if exitcode != os.EX_OK:
                    # delete temp files because we leave this method right now
                    os.remove(model['file'])
                    os.remove(answer['file'])
        
                    return checkresult.CheckResult(exitcode, result)
                        
                # has the students' solution passed this tests?
                else:
                    msgItems = result.split(';;')
                    
                    isEqual = (msgItems[0].split('=')[1])
                    expected = msgItems[1].split('=')[1]
                    received = msgItems[2].split('=')[1]
                    
                    if isEqual == 'False':
                        # FIXME: i18n
                        feedback += '\nYour submission failed. Test case was: "%s"' \
                                    % (test,)
                        feedback += '\n\n  Expected result: %s\n  Received result: %s' \
                                    % (expected, received,)
                        
                        solved = 1;
                        
                        break # means end testing right now
        # end for

        if solved == 0:
            # FIXME: i18n
            feedback = '\nYour submission passed all tests.'

        # delete temp files
        os.remove(model['file'])
        os.remove(answer['file'])


        return checkresult.CheckResult(solved, feedback)


    def _createModule(self, prefix='', source=''):
        """
        Creates a new python module. Returns the module's name and
        absolute path on disk.
        
        @return a dictonary with keys module and file
        """
        moduleName = getUniqueModuleName(prefix)
        fileName = getTempFileName(moduleName, '.py')
        writeFile(source, fileName)
        
        return {'module':moduleName, 'file':fileName}


    def _runPython(self, source):
        """
        """
        return self._runInterpreter(INTERPRETER, source, '.py')
