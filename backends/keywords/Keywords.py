# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.

from __future__ import division

import sys, os, re, math
import logging

from types import StringType, UnicodeType, IntType, DictionaryType

# local imports
from lib.AbstractBackend import AbstractBackend
from lib.data.BackendJob import BackendJob
from lib.util.BackendSchema import TestEnvironment, InputField, Schema
from backends.keywords.KeywordsConf import KeywordsConf

class Keywords(AbstractBackend):
    """
    TODO
    """
    
    id = 'keywords'
    name =  'Keywords'
    schema = KeywordsConf.inputSchema
    testSchema = KeywordsConf.tests
    version = '1.0'
    
    # -- internal methods used by subclasses ----------------------------------
    def process_execute(self, jobdata):
        """Executes a check job using the given job data. This method 
        must be overridden in subclasses.

        @param jobdata all relevant job data
        @return CheckResult object or tupel consisting of result code and message
        """

        try:
            job = BackendJob(jobdata)

            assert job, 'Invalid or insufficient data: %s' % job

            # set a default result
            result = (1, '')
            
            try:
                logging.info('Executing tests (%s)' % job.getId())
                result = self._process_doTests(job)
            
            except Exception, e:
                msg = 'Internal error: %s: %s' % (sys.exc_info()[0], e)
                logging.error(msg)
                logging.error(traceback.format_exc())
                result = (-10, msg)

            #logging.debug('Returning result: %s' % repr(result))
            return result

        except Exception, e:
            msg = '%s: %s' % (sys.exc_info()[0], e)
            logging.warn(msg)
            return (-1, msg)

    # -- run tests ------------------------------------------------------------
    def _process_doTests(self, job):
        """
        TODO
        @return a BackendResult object with result code and value
        """

        # get all test data and iterate through them
        repeatFields = self.schema.filterFields(type='RepeatField')
        
        assert repeatFields and len(repeatFields) == 1, \
            "None or more than one field of type 'RepeatField' found"
        
        repeatField = repeatFields[0]
        tests = self._getTests(job)
        testdata = repeatField.getAccessor()(job[repeatField.getName()])
        
        if len(testdata) == 0:
            #msg = 'No test data found.'
            #logging.warn(msg)
            return

        if len(tests) == 0:
            msg = 'No test specification found.'
            logging.warn(msg)
            return(0, msg)

        # we have some test data -> lets start the evaluation
        studentSolution = job['studentSolution']

        assert studentSolution and type(studentSolution) in (StringType,
                                                             UnicodeType), \
            "Tests require valid 'student solution' (%s)" % \
            studentSolution

        # define return values
        feedback = ''
        matches = 0 # number of keywords found
        solved = 0  # percentage of keywords found

        studentSolution = re.sub(r'\s+', ' ', studentSolution)

        # run selected tests (e.g., "simple", cf. schema definition)
        for test in tests:
            logging.debug('Running test: %s' % test.getName())

            # run with all test data
            for t in testdata:
                try:
                    # t contains a keyword or a regular expression
                    # studentSolution contains the text
                    if test.getName() == 'keywords':
                        t = r'\b' + re.escape(t.strip()) + r'\b'

                    if re.search(t, studentSolution, re.IGNORECASE):
                        matches += 1

                except Exception, e:
                    msg = 'Internal error during test: %s: %s' % \
                          (sys.exc_info()[0], e)
                                  
                    logging.error(msg)
                    return (-30, msg)
            # end inner for loop
        #end outer for loop 

        inverted = job['inverted']
        # int/int -> float requires division from future!
        solved = int(round(matches/len(testdata) * 100))
        if inverted:
            solved = 100 - solved

        feedback = '''\nYour automatically determined score: %d of 100.''' % solved

        return (solved, feedback)
