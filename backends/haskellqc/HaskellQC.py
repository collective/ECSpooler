# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2011 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.

################################################################################
#                                Changelog                                     #
################################################################################
#
# 20.03.2009, chbauman:
#       version 1.1
#       using self.srcFileSuffix instead of hard-coded '.hs'
#       preventing multiple result output
# 24.03.2009, chbauman:
#       refactored variable 't' to 'propertyTest'
#       using HaskellQCConf.FAILED_TEST_MESSAGE and PASSED_ALL_TESTS_MESSAGE
#       improved feedback (corporate design)

import sys, os, re
import logging

from types import StringTypes

# local imports
from lib.ProgrammingBackend import EX_OK
from lib.data.BackendJob import BackendJob
from lib.data.BackendResult import BackendResult
from lib.util.BackendSchema import TestEnvironment
from lib.util.BackendSchema import InputField
from lib.util.BackendSchema import Schema

from backends.haskellqc import config
from backends.haskell.Haskell import Haskell
from backends.haskell.Haskell import SYNTAX_TEMPLATE
from backends.haskell.Haskell import GENERIC_TEMPLATE
from backends.haskell.Haskell import RUNHUGS_RE

LOG = logging.getLogger()

# regex to get all property names
PROPERTIES_RE = re.compile(r'(?P<name>prop_[A-Z,a-z,0-9]+\w*)')

# regex to substitute 'prop_' placeholder with the property name
PROP_RE = re.compile(r'\$\{prop_\}')
 
    # regex to identify failed tests
FAILED_RE_KEY = 'params'
FAILED_RE = re.compile(r'(?<=\stests:\n)(?P<%s>.*)(\n)' % FAILED_RE_KEY, re.DOTALL)
PASSED_RE = re.compile(r'OK, (passed \d+ tests?)')

FAILED_TESTS_MESSAGE = 'Your submission failed.'
PASSED_ALL_TESTS_MESSAGE = 'Your submission passed all tests.'

# template for Haskell module containing all QuickCheck properties     
PROPS_TEMPLATE = \
"""
module Properties where
import QuickCheck -- e.g. for conditional properties
import Student

%s
"""

# template for wrapper code
WRAPPER_TEMPLATE = \
"""
module Main where
import QuickCheck
import Properties

main :: IO ()
--main = quickCheck ${prop_}
main = verboseCheck ${prop_} 
"""

# input schema
inputSchema = Schema((
    InputField(
        'properties', 
        required = True, 
        label = 'QuickCheck properties',
        description = 'Enter one or more QuickCheck properties ' +
                      '(cf. <http://www.cs.chalmers.se/~rjmh/QuickCheck/>).',
        i18n_domain = 'EC',
    ),
))

# test environments
testEnvs = Schema((
    TestEnvironment(
        'default',
        label = 'Default',
        description = 'Default using QuickCheck.',
        #test = '',
        syntax = SYNTAX_TEMPLATE,
        semantic = WRAPPER_TEMPLATE,
        lineNumberOffset = 2,
        compiler = config.INTERPRETER,
        interpreter = config.INTERPRETER,
    ),
))

class HaskellQC(Haskell):
    """
    Backend for testing Haskell programs using QuickCheck.
    """
        
    id = 'haskellqc'
    name = 'Haskell QuickCheck'
    version = '1.1'
    
    schema = inputSchema
    testSchema = testEnvs

    def __init__(self, params, versionFile=__file__):
        """
        """
        Haskell.__init__(self, params, versionFile)


    def _postProcessCheckSemantic(self, test, message):
        """
        Post process QuickCheck result messages.
        
        @param: test:
        @param: message:
        """
        result = re.sub('/.*/ECSpooler/backends/haskellqc/', '', message)

        return result
    

    def _process_checkSemantics(self, job):
        """
        @return: a BackendResult object with result code and value
        """
        
        # test for available test specs
        testSpecs = self._getTests(job)

        if len(testSpecs) == 0:
            msg = 'No test specification selected.'
            LOG.warn('%s, %s' % (msg, job.getId()))
            return BackendResult(-217, msg)
        
        # get student's submission and QuickCheck properties
        submission = job['submission']
        properties = job['properties']

        #assert properties and type(properties) in StringTypes, \
        #    "Semantic check requires valid 'properties' (%s)" % properties

        assert submission and type(submission) in StringTypes, \
            "Semantic check requires valid 'student solution' (%s)" % submission

        testdata = None
        
        if properties:
            testdata = PROPERTIES_RE.findall(properties)
            
            # If a student submitted a function declaration additionally to its
            # definition, testdata will contain multiple occurrences of the same
            # function name, which causes this backend to output the results
            # multiple times, too. By 'setifying' testdata we'll get an array
            # of unique entries.
            testdata = list(set(testdata))

        if (not testdata) or (len(testdata) == 0):
            msg = 'No QuickCheck propeties defined.'
            LOG.warn('%s, %s' % (msg, job.getId()))
            return BackendResult(-216, msg)

        submission = GENERIC_TEMPLATE % ('Student', submission)
        properties = PROPS_TEMPLATE % (properties)

        # define return values
        feedback = BackendResult.UNKNOWN
        reason = BackendResult.UNKNOWN
        solved = True

        # run selected test specifications
        for test in testSpecs:

            if not solved: break

            LOG.debug('Running semantic check with test: %s' % test.getName())

            # get the interpreter
            interpreter = test.interpreter
           
            # write submission and propeties to files
            self._writeModule('Student', submission, self.srcFileSuffix, job.getId(), test.encoding)
            self._writeModule('Properties', properties, self.srcFileSuffix, job.getId(), test.encoding)

            # run with all test data
            for propertyTest in testdata:
                # add property name
                wrapper = PROP_RE.sub(propertyTest, test.semantic)
                
                # execute wrapper code in haskell interpreter
                try:
                    # write module for wrapper
                    wrapperModule = self._writeModule('wrapper', wrapper, 
                                                      self.srcFileSuffix, 
                                                      job.getId(),
                                                      test.encoding)
                    
                    # execute
                    exitcode, result = \
                        self._runInterpreter(interpreter, 
                                    os.path.dirname(wrapperModule['file']),
                                    os.path.basename(wrapperModule['file']))

                    # remove all special characters written by runhugs 
                    result = RUNHUGS_RE.sub('', result)
        
                except Exception, e:
                    msg = 'Internal error during semantic check: %s: %s' % \
                          (sys.exc_info()[0], e)
                                  
                    LOG.error(msg)
                    return BackendResult(-230, msg)

                # an error occured
                # FIXME: os.EX_OK is available only for Macintosh and Unix!
                if exitcode != EX_OK:
                    result = "\nYour submission failed. Test " \
                             "case was: '%s' (%s)" \
                             "\n\n Received result: %s"\
                             % (propertyTest, test.getName(), 
                                self._postProcessCheckSemantic(test, result))

                    return BackendResult(False, result)
                        
                # has the students' solution passed this test?
                else:
                    #LOG.debug(result)
                    failed = FAILED_RE.search(result)
                    #passed = PASSED_RE.search(result)
                    
                    # only consider errornous submissions
                    if failed:
                        # store the reason
                        reason = failed.group(FAILED_RE_KEY)
                        solved = False                    
                        break # means: end testing right now
                    # end elif
                #end else                    
            # end inner for loop
        #end out for loop 
        
        # does student's submission have all teacher-defined properties?
        if solved:
            # Yes -> Show that all tests were passed.
            feedback = PASSED_ALL_TESTS_MESSAGE
        else:
            # No -> Show that submission failed.
            feedback = \
            '%s\n\nParameters were: %s.' % (FAILED_TESTS_MESSAGE, reason.replace('\n', ', '))

        # TODO: i18n
        feedback = feedback

        return BackendResult(solved, feedback)


# -- do some testing ----------------------------------------------------------

if __name__ == "__main__":
    """
    """
    import socket

    host = socket.getfqdn()
    port = 15060
    spooler = 'http://%s:%d' % (socket.getfqdn(), 15050)
    auth = {"username":"demo", "password":"foobar"}
    
    params = {'host': host, 'port': port, 'spooler': spooler, 'auth': auth, }

    jobdata = {
        'backend':'haskellqc', 
        'submission':'square x = x * x',
        'properties': 'prop_square x = (square x) == (x * x)\n  where types= x::Int',
        'tests': ['default'],
    }

    job = BackendJob(data=jobdata)

    result = HaskellQC(params)._process_checkSemantics(job)
    
    print result.getValue()
    print result.getMessage()
