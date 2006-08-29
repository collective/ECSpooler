# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.

import sys, os, re, popen2, tempfile
import threading
import logging

from types import StringType, UnicodeType

# local imports
from data import *
from util.utils import *

from util.BackendSchema import TestEnvironment, RepeatField, InputField, Schema
from AbstractProgrammingBackend import AbstractProgrammingBackend, EX_OK

def non_null_str(s):
    return s and type(s) in (StringType, UnicodeType)

JAVA_PATH='/opt/diablo-jdk1.5.0_07/bin'
COMPILER=os.path.join(JAVA_PATH, 'javac')
INTERPRETER=os.path.join(JAVA_PATH, 'java')

# The packages that the model and student solution will be put in
NS_MODEL='ModelPackage'
NS_STUDENT='StudentPackage'
# The name of the wrapper class that performs the semantic check
CLASS_SEMANTIC_CHECK='SemanticCheck'

# The regular expression to extract the name of the *public* class
# from a piece of source.
#
# The grammar at <http://cobase-www.cs.ucla.edu/pub/javacc/java1.4.jj>
# says this:
#
# void ClassDeclaration() :
#     {}
#     {
#         ( "abstract" | "final" | "public" | "strictfp")*
#         UnmodifiedClassDeclaration()
#     }
#
# void UnmodifiedClassDeclaration() :
#     {}
#     {
#         "class" <IDENTIFIER> [ "extends" Name() ]
#                              [ "implements" NameList() ]
#         ClassBody()
#     }
CLASS_NAME_RE=re.compile(r'^\s*(?:public\s+(?:(?:abstract|final|public|strictfp)\s+)*)+\s*class\s+(?P<name>[a-zA-Z_]\w*)', re.MULTILINE)

TEST_SIMPLE = \
"""
    public static boolean test(Object a, Object b) {
        return a.equals(b);
    }
"""

# wrapper code
TEMPLATE_SEMANTIC = \
'''
public class %s {
    ${helpFunctions}

    ${testFunction}
    
    public static void main(String[] argv) {
        Object exp  = ${model_testData};
        Object rec  = ${student_testData};
        Boolean eql = new Boolean(test(exp, rec));
        
        System.out.println("isEqual=" + eql.toString()
                           + ";;expected=" + exp.toString()
                           + ";;received=" + rec.toString());
    }
}
''' % (CLASS_SEMANTIC_CHECK)

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
            compiler = COMPILER,
            interpreter = INTERPRETER,
        ),

#         TestEnvironment(
#             'permutation',
#             label = 'Permutation',
#             description = 'Test with permutations',
#             test = TEST_PERM,
#             semantic = TEMPLATE_SEMANTIC,
#             #lineNumberOffset = 2,
#             compiler = '/opt/erlang/bin/erlc %s',
#             interpreter = '/opt/erlang/bin/erl -noshell -s %s -s init stop',
#         ),
    ))

class JavaBackend(AbstractProgrammingBackend):
    """
    A simple checker class for checking Haskell programs by comparing
    student and model solution on given test data.
    """
    
    id = 'java'
    name = 'Java'

    srcFileSuffix = '.java'

    schema = inputSchema
    testSchema = tests
    version = '0.9'

    def getClassName(self, src):
        '''Extract the class name from the source code snippet [src]'''
        m = CLASS_NAME_RE.search(src)
        assert m is not None, 'Name of the public class could not be ' \
               'extracted from source\n\n%s' % repr(src)
        return m.group('name')


    # -- check syntax ---------------------------------------------------------
    def _preProcessCheckSyntax(self, test, src, **kwargs):
        """
        Replace module name in students' submission.
        
        @see AbstractProgrammingBackend._preProcessCheckSyntax
        @return modified source code and new module name
        """
        
        result = ("package %s;\n\n" % NS_STUDENT) + src
        cname = self.getClassName(src)
        return result, cname


    def _postProcessCheckSyntax(self, test, message):
        """
        @see AbtractSimpleBackend._postProcessCheckSyntax
        """
        # replace path and filename
        return re.sub('^.*%s:(\d+)' % self.srcFileSuffix, 
                      'line: \g<1>', 
                      message)
                      

    # -- check semantics ------------------------------------------------------
    def _preProcessCheckSemantic(self, test, src, **kwargs):
        """
        Pre process student's submissions and semantic check wrapper code. 
        Override this method if you need to reformat the wrapper code or 
        the student's submission. 
        """                                  
        return ("package %s;\n\n" % NS_STUDENT) + src

    
    def _process_checkSemantics(self, job):
        """
        Runs sematic test on a Java program.
        Remember: Before we can run the wrapper code we have to compile it!!
        
        @return a BackendResult object with result code and value
        """
            
        # get the repeat field and iterate through the corresponding data
        #repeatFields = self.schema.filterFields(__name__='testData')
        repeatFields = self.schema.filterFields(type='RepeatField')
        
        assert repeatFields and len(repeatFields) == 1, \
            'None or more than one RepeatField found.'
        
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

        # 1. get model solution and student's submission
        modelSolution = job['modelSolution']
        studentSolution = job['studentSolution']

        assert non_null_str(modelSolution), \
            "Semantic check requires valid 'model solution' (%s)" % \
            repr(modelSolution)

        assert non_null_str(studentSolution), \
            "Semantic check requires valid 'student solution' (%s)" % \
            repr(studentSolution)

        # define return values
        feedback = ''
        solved = True

        # run selected test specifications
        for test in self._getTests(job):

            if not solved: break

            logging.debug('Running semantic check with test: %s' % 
                          test.getName())

            # compile the solutions
            compiler = test.compiler
            compiled = {}
            for sol in [('model',   NS_MODEL,   modelSolution),
                        ('student', NS_STUDENT, studentSolution)]:
                name, ns, source = sol
                # FIXME: move the 're.sub' code to _preProcessCheckSyntax
                # replace module name in the solution
                source = ("package %s;\n\n" % ns) + source

                # write solution to a file
                dir=os.path.join(job['id'], ns)
                cname = self.getClassName(source)
                compiled[name+'Class'] = cname
                compiled[name] = self._writeModule(
                    cname,
                    source,
                    suffix=self.srcFileSuffix,
                    dir=dir,
                    encoding=test.encoding)

                # compile the solution
                exitcode, result = self._runInterpreter(
                    compiler,
                    os.path.dirname(compiled[name]['file']),
                    os.path.basename(compiled[name]['file']))
                
                assert exitcode == EX_OK, \
                       'Errors in %s solution: %s' % (name, result)

            model   = compiled['model']
            student = compiled['student']
            
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

            # 4.3.1 replace class names
            for cname in ('modelClass', 'studentClass'):
                src = re.sub('\$\{%s\}' % cname, compiled[cname], src)

            # 4.4. run with all test data
            for t in testdata:
                # and now add repeatable data values
                tStudent = None
                wrapper = src
                rfn = repeatField.getName()
                for (k, ns) in (('model',   NS_MODEL),
                                ('student', NS_STUDENT)):
                    # substitute the class name in the test data with
                    # the actual, package-qualified class name
                    realClass = "%s.%s" % (ns, compiled['%sClass' % k])
                    v = re.sub(r"\b%s\b" % compiled['modelClass'], realClass,
                               t)
                    if k=='student':
                        tStudent = v
                    # substitute the respective test data placeholder
                    # with the test data we just fixed up
                    wrapper = re.sub(r'\$\{%s_%s\}' % (k, rfn), v, wrapper)
        
                # remove all remaining placeholders
                wrapper = re.sub('\$\{[^\}]*\}', '', wrapper)

                # compile and execute wrapper
                try:
                    # write module for wrapper
                    wrapperModule = self._writeModule(
                        CLASS_SEMANTIC_CHECK,
                        wrapper, 
                        suffix=self.srcFileSuffix,
                        dir=job['id'],
                        encoding=test.encoding)

                    # compile the wrapper
                    exitcode, result = self._runInterpreter(
                        compiler,
                        os.path.dirname(wrapperModule['file']),
                        os.path.basename(wrapperModule['file']))
                    
                    assert exitcode == EX_OK, \
                        'Error in wrapper code for semantic check: %s' % result

                    # run the wrapper
                    exitcode, result = self._runInterpreter(
                        interpreter, 
                        os.path.dirname(wrapperModule['file']),
                        CLASS_SEMANTIC_CHECK)

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
                                    % (tStudent, test.getName())
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

        return ([0, 1][solved], feedback)

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
        tB = JavaBackend({
                    'host': socket.getfqdn(), 
                    'port': 5060, 
                    'capeserver': 'http://%s:5050' % socket.getfqdn(), 
                    'srv_auth': {'username':'demo', 'password':'foobar'}
                })

        tB.start()

    else:
        time.sleep(1)
        print 'pid=', cpid
