# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
import unittest

from backends.haskell import HaskellExtended
from lib.data.BackendJob import BackendJob

from ProgrammingBackendTestCase import ProgrammingBackendTestCase

class testHaskellExtended(ProgrammingBackendTestCase):
    """
    """
    
    backendClassName = 'haskell.HaskellExtended.HaskellExtended'

    # -- individual tests -----------------------------------------------------
    def testNoTestSpec(self):
        """_process_checkSemantics should return an instance of 'BackendResult' with error code -217 if no test spec is given"""
        
        result = HaskellExtended.HaskellExtended(self.params)._process_checkSemantics(self.job)
        
        if result:
            self.assertEqual(result.getValue(), -217, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))

    
    def testNoTestData(self):
        """_process_checkSemantics should return an instance of 'BackendResult' with error code -216 if no test data are given"""
        
        self.job['tests'] = ['simple',]
        
        result = HaskellExtended.HaskellExtended(self.params)._process_checkSemantics(self.job)
        
        if result:
            self.assertEqual(result.getValue(), -216, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))


    jobdata = {'backend':'haskell-extended', 
               'submission':'square x = x * x',
               'modelSolution': 'square x = x * x',
               'tests': ['simple'],
               'testData': 'square 0\nsquare 2\nsquare 3',
               }


    def testSyntaxFail(self):
        """_manage_checkSyntax should return False for a syntactical incorrect program"""

        self.jobdata['submission'] = 'square x -> x * x'

        job = BackendJob(data=self.jobdata)

        backend = HaskellExtended.HaskellExtended(self.params)
        result = backend._manage_checkSyntax(job)
        
        if result:
            self.assertEqual(result.getValue(), False, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSyntaxSuccess(self):
        """_manage_checkSyntax should return True for a correct program"""
        
        self.jobdata['submission'] = 'square x = x * x'

        job = BackendJob(data=self.jobdata)

        backend = HaskellExtended.HaskellExtended(self.params)
        result = backend._manage_checkSyntax(job)
        
        #print 'jobId', job.getId()
        
        if result:
            self.assertEqual(result.getValue(), True, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSemanticFailure(self):
        """_manage_checkSemantic should return False for an incorrect program"""
        
        self.jobdata['submission'] = 'square x = x + x'

        job = BackendJob(data=self.jobdata)

        backend = HaskellExtended.HaskellExtended(self.params)
        result = backend._manage_checkSemantics(job)
        
        if result:
            self.assertEqual(result.getValue(), False, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSemanticSuccess(self):
        """_manage_checkSemantic should return True for a correct program"""
        
        self.jobdata['submission'] = 'square x = x * x'

        job = BackendJob(data=self.jobdata)

        backend = HaskellExtended.HaskellExtended(self.params)
        result = backend._manage_checkSemantics(job)
        
        #print 'jobId', job.getId()

        if result:
            self.assertEqual(result.getValue(), True, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))


def test_suite():
    """
    """
    from unittest import TestSuite, makeSuite

    suite = TestSuite()
    suite.addTest(makeSuite(testHaskellExtended))
    
    return suite


# -- main ---------------------------------------------------------------------
if __name__ == '__main__':
    TestRunner = unittest.TextTestRunner
    suite = test_suite()

    TestRunner().run(suite)
