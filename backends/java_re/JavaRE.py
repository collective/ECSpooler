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
from lib.data.BackendResult import BackendResult
from lib.AbstractProgrammingBackend import AbstractProgrammingBackend, EX_OK

from JavaREConf import JavaREConf

def non_null_str(s):
    return s and type(s) in (StringType, UnicodeType)

string_re = re.compile(r'"(?:[^"\n\r\\]+|(?:\\.)+)*"')

def string_p(s):
    return string_re.match(s)

class JavaRE(AbstractProgrammingBackend):
    """
    A backend class for checking regulare expressions using Java.
    
    Solutions have to be entered as they would appear in the Java program,
    i. e. you'd enter things like

      "!^.*foo.*$"
    
    or
    
      "Match \\[ a bracket"
    
    in the "Answer" field.  Invalid examples would be
    
      !^.*foo.*$
    
    (missing quote marks) or
    
      "Match \[ a bracket"
    
    ('\[' is not valid in a Java string.  To escape a reg-exp meta char,
    you need to prefix it with '\\', not just '\').
    
    Issues:
    
      * Syntax check has to be improved.  In particular, we'd have to run
        the syntax check wrapper, too, to find out whether Java's
        "Pattern.compile()" raises an error.
    
      * Need better test environments (find all, compare sub groups, etc.)
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


        # 1. get model solution and student's submission
        modelSolution    = job['modelSolution']
        studentSolution  = job['submission']

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
                return BackendResult(-230, msg)

            # 4.4. run with all test data
            for t in testdata:
                # and now add repeatable data values
                wrapper = src
                #rfn = repeatField.getName()

                # download the test data
                contents = None
                try:
                    contents = self.download(t)
                    #print >> sys.stdout, contents
                except Exception, e:
                    msg = 'Internal error during semantic check: %s: %s' % \
                          (sys.exc_info()[0], e)

                    logging.error(msg)
                    return BackendResult(-230, msg)

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
                    return BackendResult(-230, msg)

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
                    return BackendResult(-230, msg)

                # an error occured
                if exitcode != EX_OK:
                    
                    logging.debug('exitcode: %s' % repr(exitcode))
                    
                    result = "\nYour submission failed. Test " \
                             "case was: '%s' (%s)" \
                             "\n\n Received result: %s"\
                             % (t, test.getName(), result)

                    return BackendResult(False, result)
                        
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

        #return ([0, 1][solved], feedback)
        return BackendResult(solved, feedback)

