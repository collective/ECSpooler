# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2012 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.

import sys, os
#import re
import logging
import tempfile

#from os.path import join, dirname
#from types import StringType, UnicodeType

# local imports
from lib.AbstractProgrammingBackend import AbstractProgrammingBackend
from lib.AbstractProgrammingBackend import EX_OK

from lib.data.BackendResult import BackendResult

from lib.util import utils
from lib.util.BackendSchema import InputField
#from lib.util.BackendSchema import RepeatField
from lib.util.BackendSchema import Schema
from lib.util.BackendSchema import TestEnvironment

from backends.sql import config

# enable logging
log = logging.getLogger('sql')

# input schema
inputSchema = Schema((

        InputField(
        'forceCheckForWrongRows',
        format = 'boolean',
        label = 'Force checking for wrong rows',
        description = 'If set, the test will not be aborted after row count of result set is wrong - ' +
                      'for students information about wrong rows only, has no influence on the result.',
        i18n_domain = 'EC',
        default = True,
    ), 

    InputField(
        'nonSelectStatements',
        format = 'boolean',
        label = 'Non-select statements are allowed',
        description = "If set, manipulating statements such as CREATE, UPDATE or ALTER are allowed.",
        i18n_domain = 'EC',
        default = True,
    ),        

    InputField(
        'noTypeCheck',
        format = 'boolean',
        label = "Do not check data types of columns",
        description = "If set, type of columns from result set will not be checked.",
        i18n_domain = 'EC',
        default = True,
    ),        

    InputField(
        'compareDatabases',
        format = 'boolean',
        label = 'Compare databases only',
        description = 'If set, databases are being compared to determine correct result; otherwise ' +
                      'use result sets for comparison.',
        i18n_domain = 'EC',
    ),        

    InputField(
        'databaseCreationQueries', 
        required = True, 
        label = 'Database creation queries',
        description = 'Enter one or more SQL statements (exclude select statements) to create the initial database.',
        i18n_domain = 'EC',
    ),
    
    InputField(
        'teacherQueries', 
        required = True, 
        label = 'Teacher queries',
        description = 'Enter SQL statements which prepare the expected results.  To compare "result sets" you need to add a select statement at the end. ',
        i18n_domain = 'EC',
    ),
))

# testSchema
tests = Schema((

    TestEnvironment(
        'simple',
        label = 'Simple',
        description = 'Simple test.',
        interpreter = config.INTERPRETER,
    ),
))
    
    
def getAdditionalProperties(job):
    """
    Reads all options out of checkboxes.
    """
    checkOnlyForDatabase = getNumForBool(job['compareDatabases'])
    nonSelectStatements = getNumForBool(job['nonSelectStatements'])
    noTypeCheck = getNumForBool(job['noTypeCheck'])
    forceCheckForWrongRows = getNumForBool(job['forceCheckForWrongRows'])
    
    return "%s%s%s%s" % (checkOnlyForDatabase, nonSelectStatements, noTypeCheck, forceCheckForWrongRows)

def getNumForBool(boolVal):
    """
    """
    if (boolVal == True):
        return 1
    else:
        return 0


class SQL(AbstractProgrammingBackend):
    """
    Backend for checking SQL assignments.
    """
    
    id = 'sql'
    name = 'SQL'

    srcFileSuffix = '.txt'

    schema = inputSchema
    testSchema = tests


    def __init__(self, params, versionFile=__file__):
        """
        This constructor is needed to reset the logging environment.
        """
        AbstractProgrammingBackend.__init__(self, params, versionFile, log)


    def _process_execute(self, job):
        """
        @return: a BackendResult object with result code and value
        """
        # test for available test specs
        testSpecs = self._getTests(job)

        if len(testSpecs) == 0:
            msg = 'No test specification selected.'
            log.warn('%s, %s' % (msg, job.getId()))
            return BackendResult(-217, msg)

        # get submission
        submission = job['submission']
        
        # get other input fields from teacher
        databaseCreationQueries = job['databaseCreationQueries']
        teacherSubmission = job['teacherQueries']
        
        additionalProperties = getAdditionalProperties(job)

        # define return values
        feedback = BackendResult.UNKNOWN
        solved = False;

        # run selected test specifications
        for test in self._getTests(job):

            log.debug('Running SQL check with test: %s' % test.getName())

            # get the sql.bat
            interpreter = test.interpreter
           
            # write submitted content to files
            execDir = os.path.join(tempfile.gettempdir(), job.getId())
            
            # script for database creation
            dbcqFile = os.path.join(execDir, 'DatabaseCreationQueries.txt')
            utils.writeFile(databaseCreationQueries, dbcqFile, test.encoding)
            
            tsFile = os.path.join(execDir, 'TeacherSubmission.txt') 
            utils.writeFile(teacherSubmission,tsFile, test.encoding)

            ssFile = os.path.join(execDir, 'StudentSubmission.txt') 
            utils.writeFile(submission, ssFile, test.encoding)
            
            apFile = os.path.join(execDir, 'AdditionalProperties.txt')
            utils.writeFile(additionalProperties, apFile, test.encoding)
            
            execFile = 'SQLAssessment.Main'

            # execute wrapper code in interpreter
            try:
                exitcode, feedback = self._runInterpreter(interpreter,  execDir, execFile)
                
            except Exception, e:
                msg = 'Internal error during SQL tests: %s: %s' % \
                      (sys.exc_info()[0], e)
                              
                log.error(msg)
                return BackendResult(-230, msg)

            # an error occured
            if exitcode != EX_OK:
                solved = False
                break
            else:
                solved = True
        #end out for loop 

        self._cleanup(job.getId())
        
        log.debug(solved)
        log.debug(feedback)

        return BackendResult(solved, feedback)

