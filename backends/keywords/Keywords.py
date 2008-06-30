# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.

from __future__ import division

import sys, re
import logging

from types import StringType, UnicodeType

# local imports
from lib.AbstractBackend import AbstractBackend
from lib.data.BackendResult import BackendResult
from backends.keywords.KeywordsConf import KeywordsConf

class Keywords(AbstractBackend):
    """
    TODO:
    """
    
    id = 'keywords'
    name =  'Keywords'
    schema = KeywordsConf.inputSchema
    testSchema = KeywordsConf.tests
    #version = '1.0'
    
    # -- internal methods used by subclasses ----------------------------------
    def _process_execute(self, job):
        """
        Executes a test job using the given test data in job.

        @param: job: a BackendJob
        @return: a BackendResult
        @see: lib.AbstractBackend._process_execute
        @see: lib.data.BackendJob, lib.data.BackendResult
        """

        try:
            logging.info('Executing tests (%s)' % job.getId())
            result = self._process_doTests(job)
        
        except Exception, e:
            msg = 'Internal error: %s: %s' % (sys.exc_info()[0], e)
            logging.error(msg)
            #logging.error(traceback.format_exc())
            result = BackendResult(-200, msg)

        #logging.debug('Returning result: %s' % repr(result))
        return result

    # -- run tests ------------------------------------------------------------
    def _process_doTests(self, job):
        """
        TODO:
        
        @return: a BackendResult object with result value and message
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


        # we have some test data -> lets start the evaluation
        submission = job['submission']

        assert submission and type(submission) in (StringType,
                                                             UnicodeType), \
            "Tests require valid 'student solution' (%s)" % \
            submission

        # define default result values
        #feedback = BackendResult.UNKNOWN
        matches = 0 # number of keywords found
        #solved = 0  # percentage of keywords found

        submission = re.sub(r'\s+', ' ', submission)

        # run selected tests (e.g., "simple", cf. schema definition)
        for test in testSpecs:
            logging.debug('Running test: %s' % test.getName())

            # run with all test data
            for t in testdata:
                try:
                    # t contains a keyword or a regular expression
                    # submission contains the text
                    if test.getName() == 'keywords':
                        t = r'\b' + re.escape(t.strip()) + r'\b'

                    if re.search(t, submission, re.IGNORECASE):
                        matches += 1

                except Exception, e:
                    msg = 'Internal error during test: %s: %s' % \
                          (sys.exc_info()[0], e)
                                  
                    logging.error(msg)
                    return BackendResult(-215, msg)
            # end inner for loop
        #end outer for loop 

        inverted = job['inverted']
        # int/int -> float requires division from future!
        solved = int(round(matches/len(testdata) * 100))
        if inverted:
            solved = 100 - solved

        feedback = '''\nYour automatically determined score: %d of 100.''' % solved

        return BackendResult(solved, feedback)
