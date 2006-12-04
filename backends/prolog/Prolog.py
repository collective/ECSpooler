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
from PrologConf import PrologConf
from lib.AbstractProgrammingBackend import AbstractProgrammingBackend, EX_OK

def non_null_str(s):
    return s and type(s) in (StringType, UnicodeType)

class Prolog(AbstractProgrammingBackend):
    """
    A simple checker class for checking Haskell programs by comparing
    student and model solution on given test data.
    """
    
    id = 'prolog'
    name = 'Prolog'

    srcFileSuffix = '.pl'

    schema = PrologConf.inputSchema
    testSchema = PrologConf.tests
    version = '0.9'

    # -- check syntax ---------------------------------------------------------
    def _preProcessCheckSyntax(self, test, src, **kwargs):
        """
        Replace module name in students' submission.
        
        @see AbstractProgrammingBackend._preProcessCheckSyntax
        @return modified source code and new module name
        """

        #result = (":- module('%s', []).\n\n" % PrologConf.NS_STUDENT) + src
        #cname = PrologConf.NS_STUDENT
        #return result, PrologConf.NS_STUDENT
        return src, PrologConf.NS_STUDENT


    def _postProcessCheckSyntax(self, test, message):
        """
        @see AbtractSimpleBackend._postProcessCheckSyntax
        """
        # search for path, filename and line numbers result is
        # something like [('Tutor1.java:7', '7'), ('Tutor1.java:7',
        # '7')]

        matches = re.findall(r'^(ERROR: [^:]+:(\d+):(\d+):\s*(.*))',
                             message, re.MULTILINE)

        # replace each filename and line number
        for match in matches:
            message = re.sub(match[0], 
                             'line: %d, column %s: %s' %
                             (int(match[1]) - test.lineNumberOffset,
                              match[2], match[3]),
                             message,
                             1)
        
        return message
                      

    # -- check semantics ------------------------------------------------------
    def getVarNames(self, testData):
        """
        Return the list of uique variable names that are used in the
        test data.  For
        
           foo(1, 2, X, Y, X)

        return ['X', 'Y'].
        """
        raw = re.findall(PrologConf.VAR_NAME_RE, testData)
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
        return (":- module('%s', []).\n\n" % PrologConf.NS_STUDENT) + src

    
    def _process_checkSemantics(self, job):
        """
        Runs sematic test on a Prolog program.
        Remember: Before we can run the wrapper code we have to compile it!!
        
        @return a BackendResult object with result code and value
        """

        # get the repeat field and iterate through the corresponding data
        #repeatFields = self.schema.filterFields(__name__='testData')
        repeatFields = self.schema.filterFields(type='RepeatField')
        
        assert repeatFields, \
            'No RepeatField found.'

        assert len(repeatFields) == 1, \
            'More than one RepeatField found.'
        
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

            # write solution to a file
            files = {} #XXX not used

            for name, ns, source in \
                    [('model',   PrologConf.NS_MODEL,   modelSolution),
                     ('student', PrologConf.NS_STUDENT, studentSolution)]:
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
                varNames = ', '.join(self.getVarNames(t))
                wrapper = re.sub(r'\$\{testVarNames\}', varNames, wrapper)
        
                # remove all remaining placeholders
                wrapper = re.sub(r'\$\{[^\}]*\}', '', wrapper)

                # write and execute wrapper
                try:
                    # write module for wrapper
                    wrapperModule = self._writeModule(
                        PrologConf.CLASS_SEMANTIC_CHECK,
                        wrapper,
                        suffix=self.srcFileSuffix,
                        dir=job.getId(),
                        encoding=test.encoding)
                    
                    # run the wrapper
                    exitcode, result = self._runInterpreter(
                        interpreter, 
                        os.path.dirname(wrapperModule['file']),
                        PrologConf.CLASS_SEMANTIC_CHECK + self.srcFileSuffix)

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
        tB = Prolog({
                    'host': socket.getfqdn(), 
                    'port': 5060, 
                    'capeserver': 'http://%s:5050' % socket.getfqdn(), 
                    'srv_auth': {'username':'demo', 'password':'foobar'}
                })

        tB.start()

    else:
        time.sleep(1)
        print 'pid=', cpid