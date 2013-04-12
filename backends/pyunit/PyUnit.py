# -*- coding: utf-8 -*-
# $Id:PyUnit.py 1199 2009-08-12 12:53:59Z amelung $
#
# Copyright (c) 2007-2011 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of backend 'pyunit'.

import sys, os, re
import logging

from types import StringType, UnicodeType

# local imports
from backends.pyunit import config

LOG = logging.getLogger()


from lib.ProgrammingBackend import ProgrammingBackend
from lib.ProgrammingBackend import EX_OK
from lib.data.BackendResult import BackendResult
from lib.util.BackendSchema import InputField
from lib.util.BackendSchema import Schema
from lib.util.BackendSchema import TestEnvironment

WRAPPER_MODULE = \
"""# -*- coding: utf-8 -*-

from unittest import TestSuite, makeSuite
from Student import * 

${imports}

${unitTests}

def test_suite():
    suite = TestSuite()
    suite.addTest(makeSuite(${testClass}))
    return suite

if __name__ == '__main__':
    TestRunner = unittest.TextTestRunner
    suite = test_suite()

    TestRunner().run(suite)

"""

# input schema
inputSchema = Schema((

    InputField(
        'imports', 
        label = 'Imports',
        description = 'Enter additional imports, like Python '\
        'libraries.',
        i18n_domain = 'EC',
    ),
    
    InputField(
        'unitTests', 
        required = True, 
        label = 'Unit tests',
        description = 'Enter one or more PyUnit tests. ',
        i18n_domain = 'EC',
    ),
))

# test environments
testEnvs = Schema((

    TestEnvironment(
        'default',
        label = 'Default',
        description = 'Run PyUnit tests',
        semantic = WRAPPER_MODULE,
        compiler = config.INTERPRETER,
        interpreter = config.INTERPRETER,
    ),
))
    
#RE_INDENT = re.compile('^', re.M)
#RE_SELF = re.compile('def test.+?\(')
#RE_FAILED = re.compile('^Ran\s\d+.*FAILED', re.M | re.DOTALL)

RE_CLASSNAME = re.compile('(?<=^class\s).+?(?=:|\()', re.M)
RE_OK = re.compile('^Ran\s\d+.*OK', re.M | re.DOTALL)


class PyUnit(ProgrammingBackend):
    """
    Backend for testing Python programs using PyUnit.
    """
    
    id = 'pyunit'
    name = 'PyUnit'

    srcFileSuffix = '.py'

    schema = inputSchema
    testSchema = testEnvs


    def __init__(self, params, versionFile=__file__):
        """
        This constructor is needed to reset the logging environment.
        """
        ProgrammingBackend.__init__(self, params, versionFile)


    def _postProcessCheckSyntax(self, test, message):
        """
        @see: AbtractSimpleBackend._postProcessCheckSyntax
        """

        # replace path and filename
        return re.sub('File ".*%s", ' % self.srcFileSuffix, '', message)


    def _process_checkSemantics(self, job):
        """
        Checks semantic of a Python program.
        @return: a BackendResult object with result code and value
        """
        # test for available test specs
        testSpecs = self._getTests(job)

        if len(testSpecs) == 0:
            msg = 'No test specification selected.'
            LOG.warn('%s, %s' % (msg, job.getId()))
            return BackendResult(-217, msg)
        
        # get additional imports
        imports = job['imports']
        # get all unit tests
        unitTests = job['unitTests']
        # get student's submission
        submission = job['submission']

        assert unitTests and type(unitTests) in (StringType, UnicodeType), \
            "Semantic check requires valid 'model solution' (%s)" % \
            repr(unitTests)

        assert submission and type(submission) in (StringType, UnicodeType), \
            "Semantic check requires valid 'student solution' (%s)" % \
            repr(submission)

        # define return values
        feedback = BackendResult.UNKNOWN
        solved = False

        # run selected test specifications
        for test in self._getTests(job):

            LOG.debug('Running semantic check with test: %s' % 
                          test.getName())

            # get the interpreter
            interpreter = test.interpreter
           
            # write module Student
            self._writeModule('Student', submission, '.py', job.getId(), test.encoding)

            # get unit test's class name
            className = re.search(RE_CLASSNAME, unitTests)
            
            assert className, "Internal error: Could not extract class name from test class."           
            
            # get wrapper code
            wrapper = test.semantic

            # add imports
            wrapper = wrapper.replace('${imports}', imports)
            # add unit tests
            wrapper = wrapper.replace('${unitTests}', unitTests)
            # replace test class's name
            wrapper = wrapper.replace('${testClass}', className.group())
            
            # execute wrapper code in interpreter
            try:
                wrapperModule = self._writeModule('wrapper', wrapper, 
                                                  self.srcFileSuffix, 
                                                  job.getId(),
                                                  test.encoding)
                
                #print os.path.dirname(wrapperModule['file'])

                exitcode, result = \
                    self._runInterpreter(interpreter,
                                os.path.dirname(wrapperModule['file']),
                                os.path.basename(wrapperModule['file']))
                
            except Exception, e:
                msg = 'Internal error during semantic check: %s: %s' % \
                      (sys.exc_info()[0], e)
                              
                LOG.error(msg)
                return BackendResult(-230, msg)

            # an error occured
            if exitcode != EX_OK:
                result = "\nYour submission failed. \n\n" \
                         "Received result: %s" % result

                return BackendResult(False, result)
            
            # has the students' solution passed this tests?
            else:
                matcher = re.search(RE_OK, result)
                
                if matcher:
                    solved = True
                else:
                    feedback = '\nYour submission failed.\n\n%s' % result
        #end for 

        if solved:
            # TODO: i18n
            feedback = '\nYour submission passed all tests.'

        return BackendResult(solved, feedback)

