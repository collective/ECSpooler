# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.

import sys, os, re
import logging

from types import StringType, UnicodeType
from os.path import join, dirname

# local imports
from lib.AbstractProgrammingBackend import AbstractProgrammingBackend, EX_OK
from lib.data.BackendResult import BackendResult
from lib.util.BackendSchema import InputField
from lib.util.BackendSchema import RepeatField
from lib.util.BackendSchema import Schema
from lib.util.BackendSchema import TestEnvironment

from backends.prolog import config

# enable logging
log = logging.getLogger('backends.prolog')

# The packages that the model and student solution will be put in
NS_MODEL   = 'model'
NS_STUDENT = 'student'

# The name of the wrapper class that performs the semantic check
CLASS_SEMANTIC_CHECK = 'SemanticCheck'

VAR_NAME_RE = r'\b[_A-Z][A-Za-z0-9_]*\b'

# load Prolog functions to do tests
# try:
#     simpleTest = file(join(dirname(__file__), 'simpleTest.pl'), 'r').read()
# except IOError, ioe:
#     raise ioe
#     simpleTest = ''
# This code *should* throw up if 'simpleTest.pl' cannot be loaded.
SIMPLE_TEST = file(join(dirname(__file__), 'simpleTest.pl'), 'r').read()
PERM_TEST   = file(join(dirname(__file__), 'permTest.pl'),   'r').read()
PRED_TEST   = file(join(dirname(__file__), 'predTest.pl'),   'r').read()

WRAPPER_TEMPLATE = \
""":- use_module('%s').
:- use_module('%s').

join([],    _, '').
join([X],   _, O) :- swritef(O, '%%w', [X]).
join([X|R], I, O) :- join(R, I, JR), swritef(O, '%%w%%w%%w', [X, I, JR]).

format_res1_sub([], [], []).
format_res1_sub([N|Rn], [V|Rv], [O|Ro]) :- format_res1_sub(Rn, Rv, Ro),
    swritef(O, '%%w <- %%w', [N, V]).
format_res1([], 'Yes').
format_res1(V, O) :- format_res1_sub([${strTestVarNames}], V, A),
    join(A, ', ', B),
    swritef(O, '{%%w}', [B]).

format_res([],   'No').
format_res(I, O) :- maplist(format_res1, I, T), join(T, ' or ', O).

first_solution_or_nil(Pred, []) :- \+ call(Pred, _).
first_solution_or_nil(Pred, [X]) :- call(Pred, X).

${helpFunctions}

model([${testVarNames}])   :- %s:${testData}
student([${testVarNames}]) :- %s:${testData}

display_res(Model_results, Student_results, Equal) :-
    format_res(Model_results,   FMs),
    format_res(Student_results, FSs), 
        writef('isEqual=%%w;;expected=%%w;;received=%%w', [Equal, FMs, FSs]).

${testFunction}
""" % (NS_MODEL, NS_STUDENT, NS_MODEL, NS_STUDENT,)

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
        test = SIMPLE_TEST,
        semantic = WRAPPER_TEMPLATE,
        lineNumberOffset = 0,
        compiler = config.COMPILER,
        interpreter = config.INTERPRETER,
    ),

    TestEnvironment(
        'permutation',
        label = 'Permutation',
        description = 'Test with permutations',
        test = PERM_TEST,
        semantic = WRAPPER_TEMPLATE,
        lineNumberOffset = 0,
        compiler = config.COMPILER,
        interpreter = config.INTERPRETER,
    ),

    TestEnvironment(
        'predicate',
        label = 'Predicate',
        description = "Test the student's solution with the goal pred",
        test = PRED_TEST,
        semantic = WRAPPER_TEMPLATE,
        lineNumberOffset = 0,
        compiler = config.COMPILER,
        interpreter = config.INTERPRETER,
    ),
))


def non_null_str(s):
    """
    """
    return s and type(s) in (StringType, UnicodeType)

class Prolog(AbstractProgrammingBackend):
    """
    A backend class for checking Prolog programs by comparing
    student and model solution on given test data.
    """
    
    id = 'prolog'
    name = 'Prolog'

    srcFileSuffix = '.pl'

    schema = inputSchema
    testSchema = tests

    
    def __init__(self, params, versionFile=__file__):
        """
        This constructor is needed to reset the logging environment.
        """
        AbstractProgrammingBackend.__init__(self, params, versionFile, log)


    def cleanUpErrMsg(self, msg, path):
        if not path.endswith(os.path.sep):
            path += os.path.sep
        msg = msg.replace(path, '')
        m=re.compile('^Warning:\s*\(.*\):$'
                     '\s+Goal \(directive\) failed: user: .*',
                     re.MULTILINE).search(msg)
        if m:
            msg=(msg[:m.start()]+msg[m.end():]).rstrip()
        
        return msg

    # -- check syntax ---------------------------------------------------------
    def _preProcessCheckSyntax(self, test, src, **kwargs):
        """
        Replace module name in students' submission.
        
        @see: AbstractProgrammingBackend._preProcessCheckSyntax
        @return: modified source code and new module name
        """

        #result = (":- module('%s', []).\n\n" % PrologConf.NS_STUDENT) + src
        #cname = PrologConf.NS_STUDENT
        #return result, PrologConf.NS_STUDENT
        return src, NS_STUDENT


    def _postProcessCheckSyntax(self, test, message):
        """
        @see: AbtractSimpleBackend._postProcessCheckSyntax
        """
        # search for path, filename and line numbers result is
        # something like [('Tutor1.java:7', '7'), ('Tutor1.java:7',
        # '7')]

        matches = re.findall(r'^(ERROR: [^:]+:(\d+):(\d+):\s*(.*))',
                             message, re.MULTILINE)

        # replace each filename and line number
        for match in matches:
            # The column number is stored in `match[3]', but we don't
            # use it because the value reported by SWI-Prolog isn't
            # reliable.  It seems to be like this: If the error is in
            # the first line of a rule, the column number is always 0.
            # If the error is in any other line, the column number is
            # correct.
            message = re.sub(match[0], 
                             'line %d: %s' %
                             (int(match[1]) - test.lineNumberOffset, match[3]),
                             message,
                             1)
        
        return message
                      

    # -- check semantics ------------------------------------------------------
    def getVarNames(self, testData):
        """
        Return the list of unique variable names that are used in the
        test data.  For
        
           foo(1, 2, X, Y, X)

        return ['X', 'Y'].
        """
        raw = re.findall(VAR_NAME_RE, testData)
        # remove duplicates but preserve order
        uniq = []
        for name in raw:
            if name not in uniq:
                uniq.append(name)
        return uniq
    
    
    def _preProcessCheckSemantic(self, test, src, **kwargs):
        """
        Pre process student's submissions and semantic check wrapper code. 
        Override this method if you need to reformat the wrapper code or 
        the student's submission. 
        """
        return (":- module('%s', []).\n\n" % NS_STUDENT) + src

    
    def _process_checkSemantics(self, job):
        """
        Runs sematic test on a Prolog program.
        Remember: Before we can run the wrapper code we have to compile it!!
        
        @return: a BackendResult object with result code and value
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


        # 1. get model solution and student's submission
        model = job['modelSolution']
        submission = job['submission']

        assert non_null_str(model), \
            "Semantic check requires valid 'model solution' (%s)" % \
            repr(model)

        assert non_null_str(submission), \
            "Semantic check requires valid 'student solution' (%s)" % \
            repr(submission)

        # define return values
        feedback = ''
        solved = True

        # run selected test specifications
        for test in self._getTests(job):

            if not solved: break

            log.debug('Running semantic check with test: %s' % 
                          test.getName())

            # write solution to a file
            files = {} #XXX not used

            for name, ns, source in \
                    [('model',   NS_MODEL,   model),
                     ('student', NS_STUDENT, submission)]:
                # replace module name in the solution
                source = (":- module('%s', []).\n\n" % ns) + source

                # write solution to a file
                files[name] = self._writeModule(
                                    ns,
                                    source,
                                    suffix=self.srcFileSuffix,
                                    dir=job.getId(),
                                    encoding=test.encoding)

            # get the interpreter
            interpreter = test.interpreter
           
            # 4.1. set wrapper source
            src = test.semantic
            
            # 4.2. get values for all other input fields from schema
            # which are available in the job data
            for field in self.schema.filterFields(type='InputField'):
                if job.has_key(field.getName()):
                    repl = job[field.getName()]
                    src = re.sub('\$\{%s\}' % field.getName(), repl, src)

            # 4.3 set test function
            src = re.sub('\$\{testFunction\}', test.test, src)

            # 4.4. run with all test data
            rfn = repeatField.getName()
            for t in testdata:
                # and now add repeatable data values
                wrapper = src
                # substitute the test data placeholder with the actual
                # test data
                wrapper = re.sub(r'\$\{%s\}' % rfn, t, wrapper)

                # replace the variables name used in the test data
                varNames = self.getVarNames(t)
                # list of variable names as variables
                testVarNames = ', '.join(varNames)
                
                wrapper = re.sub(r'\$\{testVarNames\}', testVarNames,
                                 wrapper)
                # list of variable names as strings
                strTestVarNames = ", ".join(["'" + n + "'" for n in varNames])
                wrapper = re.sub(r'\$\{strTestVarNames\}', strTestVarNames,
                                 wrapper)
        
                # remove all remaining placeholders
                wrapper = re.sub(r'\$\{[^\}]*\}', '', wrapper)
                #log.debug('WRAPPER IS:\n%s' % wrapper)

                # write and execute wrapper
                try:
                    # write module for wrapper
                    wrapperModule = self._writeModule(
                        CLASS_SEMANTIC_CHECK,
                        wrapper,
                        suffix=self.srcFileSuffix,
                        dir=job.getId(),
                        encoding=test.encoding)
                    
                    # run the wrapper
                    exitcode, result = self._runInterpreter(
                        interpreter, 
                        os.path.dirname(wrapperModule['file']),
                        CLASS_SEMANTIC_CHECK + self.srcFileSuffix)

                except Exception, e:
                    msg = 'Internal error during semantic check: %s: %s' % \
                          (sys.exc_info()[0], e)
                                  
                    log.error(msg)
                    #return (0, msg)
                    return BackendResult(-230, msg)

                # an error occured
                if exitcode != EX_OK:
                    path = os.path.dirname(wrapperModule['file'])

                    #log.error("ORIGINAL: %s" % result)
                    
                    result = self.cleanUpErrMsg(result, path)
                    result = "\nYour submission failed. Test " \
                             "case was: '%s' (%s)" \
                             "\n\n Received result: %s"\
                             % (t, test.getName(), result)
                    
                    #return (0, result)
                    return BackendResult(False, result)
                        
                # has the students' solution passed this tests?
                else:
                    log.debug(result)
                    
                    msgItems = result.split(';;')
                    
                    isEqual = (msgItems[0].split('=')[1])
                    expected = msgItems[1].split('=')[1]
                    received = msgItems[2].split('=')[1]
                    
                    if isEqual.upper() != 'TRUE':
                        # TODO: i18n
                        feedback = "\nYour submission failed. Test " \
                                    "case was: '%s' (%s)" \
                                    % (t, test.getName())
                        feedback += '\n\n  Expected results: %s\n  ' \
                                    'Received results: %s' \
                                    % (expected, received,)
                        
                        solved = False;
                        
                        break # means: end testing right now
            # end inner for loop
        #end out for loop 

        if solved:
            # TODO: i18n
            feedback = '\nYour submission passed all tests.'

        #return ([0, 1][solved], feedback)
        return BackendResult(solved, feedback)
