# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.

import sys, os, re, time
import logging

from types import StringType, UnicodeType

# local imports
from lib.data.BackendResult import BackendResult
from lib.AbstractProgrammingBackend import AbstractProgrammingBackend, EX_OK

from JavaConf import JavaConf

def non_null_str(s):
    return s and type(s) in (StringType, UnicodeType)

class Java(AbstractProgrammingBackend):
    """
    A simple checker class for checking Haskell programs by comparing
    student and model solution on given test data.
    """
    
    id = 'java'
    name = 'Java'

    srcFileSuffix = '.java'

    schema = JavaConf.inputSchema
    testSchema = JavaConf.tests
    version = '0.9'

    def getClassName(self, src):
        """
        Extract the class name from the source code snippet [src]
        """
        
        m = JavaConf.CLASS_NAME_RE.search(src)

        assert m is not None, \
                'Name of the public class could not be ' \
                'extracted from source\n\n%s' % repr(src)

        return m.group('name')


    # -- check syntax ---------------------------------------------------------
    def _preProcessCheckSyntax(self, test, src, **kwargs):
        """
        Replace module name in students' submission.
        
        @see AbstractProgrammingBackend._preProcessCheckSyntax
        @return modified source code and new module name
        """
        
        result = ("package %s;\n\n" % JavaConf.NS_STUDENT) + src
        cname = self.getClassName(src)
        return result, cname


    def _postProcessCheckSyntax(self, test, message):
        """
        @see AbtractSimpleBackend._postProcessCheckSyntax
        """
        # search for path, filename and line numbers
        # result is something like [('Tutor1.java:7', '7'), ('Tutor1.java:7', '7')]
        matches = re.findall('(^.*%s:(\d+))' % 'java', message, re.MULTILINE)

        # replace each filename and line number
        for match in matches:
            message = re.sub(match[0], 
                             'line: %d' % (int(match[1]) - test.lineNumberOffset), 
                             message, 
                             1)

        return message
                      

    # -- check semantics ------------------------------------------------------
    def _preProcessCheckSemantic(self, test, src, **kwargs):
        """
        Pre process student's submissions and semantic check wrapper code. 
        Override this method if you need to reformat the wrapper code or 
        the student's submission. 
        """                                  
        return ("package %s;\n\n" % JavaConf.NS_STUDENT) + src

    
    def _process_checkSemantics(self, job):
        """
        Runs sematic test on a Java program.
        Remember: Before we can run the wrapper code we have to compile it!!
        
        @return a BackendResult object with result code and value
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

        assert non_null_str(model), \
            "Semantic check requires valid 'model solution' (%s)" % \
            repr(model)

        assert non_null_str(submission), \
            "Semantic check requires valid 'student solution' (%s)" % \
            repr(submission)

        # define return values
        feedback = BackendResult.UNKNOWN
        solved = True

        # run selected test specifications
        for test in testSpecs:

            if not solved: break

            logging.debug('Running semantic check with test: %s' % 
                          test.getName())

            # compile the solutions
            compiler = test.compiler
            compiled = {}
            
            try:
                for sol in [('model',   JavaConf.NS_MODEL,   model),
                            ('student', JavaConf.NS_STUDENT, submission)]:
                    name, ns, source = sol
                    # FIXME: move the 're.sub' code to _preProcessCheckSyntax
                    # replace module name in the solution
                    source = ("package %s;\n\n" % ns) + source
    
                    # write solution to a file
                    dir=os.path.join(job.getId(), ns)
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
    
            except AssertionError, err:
                msg = 'Internal error during semantic check: %s: %s' % \
                      (sys.exc_info()[0], err)
                logging.error(msg)
                return BackendResult(-230, msg)

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
                for (k, ns) in (('model',   JavaConf.NS_MODEL),
                                ('student', JavaConf.NS_STUDENT)):
                    # substitute the class name in the test data with
                    # the actual, package-qualified class name
                    realClass = "%s.%s" % (ns, compiled['%sClass' % k])
                    v = re.sub(r"\b%s\b" % compiled['modelClass'], realClass,
                               t)
                    if k == 'student':
                        # We use this for error reports.  It is like
                        # the test data that we actually use, but
                        # without the package qualifier.  After all,
                        # the package is something we add and seeing
                        # that in the error report might confuse the
                        # student.
                        tStudent = re.sub(r"\b%s\b" % compiled['modelClass'],
                                          compiled['studentClass'],
                                          t)
                    # substitute the respective test data placeholder
                    # with the test data we just fixed up
                    wrapper = re.sub(r'\$\{%s_%s\}' % (k, rfn), v, wrapper)
        
                # remove all remaining placeholders
                wrapper = re.sub('\$\{[^\}]*\}', '', wrapper)

                # compile and execute wrapper
                try:
                    # write module for wrapper
                    wrapperModule = self._writeModule(
                        JavaConf.CLASS_SEMANTIC_CHECK,
                        wrapper, 
                        suffix=self.srcFileSuffix,
                        dir=job.getId(),
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
                        JavaConf.CLASS_SEMANTIC_CHECK)

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
                    logging.debug(result)
                    
                    msgItems = result.split(';;')
                    
                    isEqual = (msgItems[0].split('=')[1])
                    expected = msgItems[1].split('=')[1]
                    received = msgItems[2].split('=')[1]
                    
                    if isEqual.upper() != 'TRUE':
                        # TODO: i18n
                        feedback = "\nYour submission failed. Test " \
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

        return BackendResult(solved, feedback)

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
        tB = Java({
                    'host': socket.getfqdn(), 
                    'port': 5060, 
                    'spooler': 'http://%s:5050' % socket.getfqdn(), 
                    'auth': {'username':'demo', 'password':'foobar'}
                })

        tB.start()

    else:
        time.sleep(1)
        print 'pid=', cpid
