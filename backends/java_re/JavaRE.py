# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.

import sys, os, re, time
import logging
import httplib
import tempfile
from urlparse import urlparse

from types import StringType, UnicodeType

# local imports
from JavaREConf import JavaREConf
from lib.AbstractProgrammingBackend import AbstractProgrammingBackend, EX_OK

def non_null_str(s):
    return s and type(s) in (StringType, UnicodeType)

string_re = re.compile(r'"(?:[^"\n\r\\]+|(?:\\.)+)*"')

def string_p(s):
    return string_re.match(s)

class JavaRE(AbstractProgrammingBackend):
    """
    A simple checker class for checking Haskell programs by comparing
    student and model solution on given test data.
    """
    
    id = 'java_re'
    name = 'JavaRE'

    srcFileSuffix = '.java'

    schema = JavaREConf.inputSchema
    testSchema = JavaREConf.tests
    version = '0.1'

    def download(self, url):
        parts		= urlparse(url)
        prot		= parts[0]
        host_and_port	= parts[1]
        path		= parts[2]
        if prot == 'http':
            conn = httplib.HTTPConnection(host_and_port)
            try:
                try:
                    conn.request("GET", path)
                except:
                    raise Exception("Cannot download file: %s "
                                    "Connecting to host failed."
                                    % url)
                r = conn.getresponse()
                assert r.status == 200, \
                       "Cannot download file: %s. Server response: %d, %s" \
                       % (url, r.status, r.reason,)
                return r.read()
            finally:
                conn.close()
        else:
            raise Exception("Cannot download file: %s "
                            "Unsupported protocol: %s"
                            % (url, prot,))

    # -- check syntax ---------------------------------------------------------
    def _preProcessCheckSyntax(self, test, src, **kwargs):
        """
        Replace module name in students' submission.
        
        @see AbstractProgrammingBackend._preProcessCheckSyntax
        @return modified source code and new module name
        """
        # FIXME: It'd be nice to be able to throw a 'SyntaxError' or
        # someething here.  We'd have to modify
        # AbstractProgrammingBackend._preProcessCheckSyntax for that.
        assert string_p(src), "Not a string: %s" % repr(src)[1:-1]

        # FIXME: Compiling the program is one thing, but to be able to
        # tell whether the pattern can be compiled we'd have to run
        # the program, too.
        return (test.syntax % src[1:-1],
                JavaREConf.CLASS_SYNTACTIC_CHECK)


    def _postProcessCheckSyntax(self, test, message):
        """
        @see AbtractSimpleBackend._postProcessCheckSyntax
        """
        # search for path, filename and line numbers
        #
        # result is something like
        # [('Tutor1.java:7', '7'), ('Tutor1.java:7', '7')]
        matches = re.findall(r'(^.*%s:(\d+))' % 'java', message, re.MULTILINE)
        
        # replace each filename and line number
        for match in matches:
            message = re.sub(match[0], 
                             'line: %d' % (int(match[1]) - test.lineNumberOffset), 
                             message, 
                             1)
        
        return message
                      

    # -- check semantics ------------------------------------------------------
    def _process_checkSemantics(self, job):
        """
        Runs sematic test on a Java program.
        Remember: Before we can run the wrapper code we have to compile it!!
        
        @return a BackendResult object with result code and value
        """

        def unescape(s):
            return s.replace("\\\\", "\\")
            
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
        modelSolution    = job['modelSolution']
        studentSolution  = job['studentSolution']

        assert non_null_str(modelSolution), \
            "Semantic check requires valid 'model solution' (%s)" % \
            repr(modelSolution)

        assert non_null_str(studentSolution), \
            "Semantic check requires valid 'student solution' (%s)" % \
            repr(studentSolution)

        assert string_p(modelSolution), \
            "'model solution' (%s) is not a string" % \
            repr(modelSolution)

        assert string_p(studentSolution), \
            "'student solution' (%s) is not a string" % \
            repr(studentSolution)

        # Create unescaped versions of the solutions that can be used
        # as command line arguments.  Remove the quote marks as well
        # (hence the [1:-1]).
        modelSolutionU   = unescape(modelSolution)[1:-1]
        studentSolutionU = unescape(studentSolution)[1:-1]

        # define return values
        feedback = ''
        solved = True

        # run selected test specifications
        for test in self._getTests(job):

            if not solved: break

            logging.debug('Running semantic check with test: %s' % 
                          test.getName())

            # 4.1. set wrapper source
            src = test.semantic
            
            # 4.2. get values for all other input fields from schema which are 
            # available in the job data
            for field in self.schema.filterFields(type='InputField'):
                if job.has_key(field.getName()):
                    repl = job[field.getName()]
                    src = re.sub(r'\$\{%s\}' % field.getName(), repl, src)

            # 4.3 set test function
            wrapper = re.sub(r'\$\{testFunction\}', test.test, src)

            # remove all remaining placeholders
            wrapper = re.sub(r'\$\{[^\}]*\}', '', wrapper)

            # compile the wrapper
            try:
                # write module for wrapper
                wrapperModule = self._writeModule(
                    JavaREConf.CLASS_SEMANTIC_CHECK,
                    wrapper, 
                    suffix=self.srcFileSuffix,
                    dir=job.getId(),
                    encoding=test.encoding)

                # compile the wrapper
                exitcode, result = self._runInterpreter(
                    test.compiler,
                    os.path.dirname(wrapperModule['file']),
                    os.path.basename(wrapperModule['file']))

                assert exitcode == EX_OK, \
                    'Error in wrapper code for semantic check: %s' % result

            except Exception, e:
                msg = 'Internal error during semantic check: %s: %s' % \
                      (sys.exc_info()[0], e)

                logging.error(msg)
                return (0, msg)

            # 4.4. run with all test data
            for t in testdata:
                # and now add repeatable data values
                wrapper = src
                #rfn = repeatField.getName()

                # download the test data
                contents = None
                try:
                    contents = self.download(t)
                except Exception, e:
                    msg = 'Internal error during semantic check: %s: %s' % \
                          (sys.exc_info()[0], e)

                    logging.error(msg)
                    return (0, msg)

                # write the test data to a file
                fn = None
                try:
                    dn = os.path.dirname(wrapperModule['file'])
                    fd,fn = tempfile.mkstemp(".txt", "input-", dn)
                    
                    # "man 2 write" says:
                    #
                    #   When using non-blocking I/O on objects such as
                    #   sockets that are subject to flow control,
                    #   write() and writev() may write fewer bytes than
                    #   requested; the return value must be noted, and
                    #   the remainder of the oper- ation should be
                    #   retried when possible.
                    #
                    # Since we're not using non-blocking I/O, I guess
                    # this means that all of [contents] should be
                    # written in one go.
                    
                    os.write(fd, contents)
                    os.close(fd)
                    
                except Exception, e:
                    msg = 'Internal error during semantic check: %s: %s' % \
                          (sys.exc_info()[0], e)

                    logging.error(msg)
                    return (0, msg)

                # execute wrapper
                try:
                    # run the wrapper
                    exitcode, result = self._runInterpreter(
                        test.interpreter,
                        os.path.dirname(wrapperModule['file']),
                        JavaREConf.CLASS_SEMANTIC_CHECK,
                        args=(modelSolutionU, studentSolutionU, fn,))

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

                    return 0, result
                        
                # has the student's solution passed this tests?
                else:
                    logging.debug(result)
                    
                    msgItems = result.split(';;', 2)
                    
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
        tB = Java({
                    'host': socket.getfqdn(), 
                    'port': 5060, 
                    'capeserver': 'http://%s:5050' % socket.getfqdn(), 
                    'srv_auth': {'username':'demo', 'password':'foobar'}
                })

        tB.start()

    else:
        time.sleep(1)
        print 'pid=', cpid
