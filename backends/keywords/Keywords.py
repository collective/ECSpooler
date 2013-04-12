# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2011 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.

from __future__ import division

import sys, re
import logging

from types import StringType, UnicodeType

# local imports
from lib.Backend import Backend
from lib.data.BackendResult import BackendResult
from lib.util.BackendSchema import InputField
from lib.util.BackendSchema import RepeatField
from lib.util.BackendSchema import Schema
from lib.util.BackendSchema import TestEnvironment

#from backends.keywords import config
LOG = logging.getLogger()

# input schema
inputSchema = Schema((
    InputField(
        'inverted',
        format = 'boolean',
        label = 'Inverted matching',
        description = 'Invert the sense of matching, i.e., ' +
                      'the keywords must not appear in the text and '
                      'matches result in a lower score.',
        i18n_domain = 'EC',
    ),
    RepeatField(
        'keywords',
        #accessor = # must return a list; default is one element per line
        required = True, 
        label = 'Keywords',
        description = 'Enter one or more keywords/regexps. '+ 
                      'One keyword/regexp per line.',
        i18n_domain = 'EC',
    ),
))

# test environsments
tests = Schema((
    TestEnvironment(
        'keywords',
        label = 'Keywords',
        description = 'Exact matching of keywords.',
    ),
    TestEnvironment(
        'regexp',
        label = 'Regular Expressions',
        description = 'Regexp matching.',
    ),
))

class Keywords(Backend):
    """
    The keywords backends was designed to give an example of how
    simple a backend can be implemented.  This backend checks wether
    or not a given number of keywords is used within a student's 
    submission.  Therefore regulare expressions will be created and
    executed.  The result is a numeric value between 0 and 100 percent; 
    depending on how many keywords were actually found.  As an 
    altenative teachers can also define regular expressions which will
    be executed with the student's submission.
    """
    
    id = 'keywords'
    name =  'Keywords'
    schema = inputSchema
    testSchema = tests
    
    def __init__(self, params, versionFile=__file__):
        """
        """
        Backend.__init__(self, params, versionFile)


    # -- internal methods used by subclasses ----------------------------------
    def _process_execute(self, job):
        """
        Executes a test job using the given test data in job.

        @param: job: a BackendJob
        @return: a BackendResult
        @see: lib.Backend._process_execute
        @see: lib.data.BackendJob, lib.data.BackendResult
        """

        try:
            LOG.info('Executing tests (%s)' % job.getId())
            result = self._process_doTests(job)
        
        except Exception, e:
            msg = 'Internal error: %s: %s' % (sys.exc_info()[0], e)
            LOG.error(msg)
            #log.error(traceback.format_exc())
            result = BackendResult(-200, msg)

        #log.debug('Returning result: %s' % repr(result))
        return result

    # -- run tests ------------------------------------------------------------
    def _process_doTests(self, job):
        """
        For each keyowrd given by the teacher a regular expression
        will be created and executed with the student's submission. 
        
        @return: a BackendResult object
        """
        # test for available test specs
        testSpecs = self._getTests(job)

        if len(testSpecs) == 0:
            msg = 'No test specification selected.'
            LOG.warn('%s, %s' % (msg, job.getId()))
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
            LOG.warn('%s, %s' % (msg, job.getId()))
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
            LOG.debug('Running test: %s' % test.getName())
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
                                  
                    LOG.error(msg)
                    return BackendResult(-215, msg)
            # end inner for loop
        #end outer for loop 

        # int/int -> float requires division from __future__!
        #solved = int(round(matches/len(testdata) * 100))
        solved = 0
        if (len(testdata)) != 0:
            solved = int(round(matches/len(testdata) * 100))

        inverted = job['inverted']
        if (inverted == True):
            solved = 100 - solved
        
        feedback = '''\nYour automatically determined score: %d of 100.''' % solved

        return BackendResult(solved, feedback)

# -- some testing -------------------------------------------------------------
if __name__ == "__main__":
    """
    """
    matches = 0
    lenTestdata = 0

    solved = 0
    if (lenTestdata) != 0:
        solved = int(round(matches/lenTestdata * 100))
    
    print "solved:", solved
