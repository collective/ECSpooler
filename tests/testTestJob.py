# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.

import unittest

from lib.data.BackendJob import BackendJob

class testTestJob(unittest.TestCase):
    """
    """
    def setUp(self):
        """
        """
        self.backend = 'haskell'
        self.submission = 'sqr x = x * x'
        self.data = {'backend':self.backend, 'submission':self.submission}
        self.id = 'x0815z'
        
        self.job = BackendJob(data=self.data, id=id)


    # -- Sanity Check ---------------------------------------------------------
    
    varBackend = 'scheme'
    varSubmission= '(define (sqr x) (* x x))' 
    
    def testGetBackend(self):
        """getBackend should give known result with known input"""
        self.assertEqual(self.backend, self.job.getBackend())
        
    def testGetSubmission(self):
        """getSubmission should give known result with known input"""
        self.assertEqual(self.submission, self.job.getSubmission())


    def testSetBackend(self):
        """getBackend should give same value as set in setBackend"""
        self.job.setBackend(self.varBackend)
        self.assertEqual(self.varBackend, self.job.getBackend())
        
    def testSetSubmission(self):
        """getSubmission should give same value as set in setSubmission"""
        self.job.setSubmission(self.varSubmission)
        self.assertEqual(self.varSubmission, self.job.getSubmission())


    # -- Bad Input ------------------------------------------------------------
    
    def testNoInput(self):
        """ BackendJob should fail with no input"""
        try:
            BackendJob()
        except AssertionError, err:
            self.assertTrue('AssertionError raised: %s' % repr(err))
        else:
            self.assertFalse('No AssertionError raised.')   

    def testNoSubmission(self):
        """ BackendJob should fail if 'submisson' is not given"""
        try:
            BackendJob(self.backend)
        except AssertionError, err:
            self.assertTrue('AssertionError raised: %s' % repr(err))
        else:
            self.assertFalse('No AssertionError raised.')

    def testInvalidData(self):
        """BackendJob should fail if 'backend' and 'submission' are not contained in 'data'"""
        try:
            data = {}
            BackendJob(data=data)
        except AssertionError, err:
            self.assertTrue('AssertionError raised: %s' % repr(err))
        else:
            self.assertFalse('No AssertionError raised.')

    def testNoSubmissionInData(self):
        """BackendJob should fail if 'submission' is not contained in 'data'"""
        try:
            data = {'backend':self.backend}
            BackendJob(data=data)
        except AssertionError, err:
            self.assertTrue('AssertionError raised: %s' % repr(err))
        else:
            self.assertFalse('No AssertionError raised.')

    def testNoBackendInData(self):
        """BackendJob should fail if 'backend' is not contained in 'data'"""
        try:
            data = {'submission':self.submission}
            BackendJob(data=data)
        except AssertionError, err:
            self.assertTrue('AssertionError raised: %s' % repr(err))
        else:
            self.assertFalse('No AssertionError raised.')


def test_suite():
    """
    """
    from unittest import TestSuite, makeSuite

    suite = TestSuite()
    suite.addTest(makeSuite(testTestJob))
    
    return suite


# -- main ---------------------------------------------------------------------
if __name__ == "__main__":
    unittest.main()
