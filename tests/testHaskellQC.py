# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2011 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
import unittest

from backends.haskellqc.HaskellQC import HaskellQC
from lib.data.BackendJob import BackendJob

from ProgrammingBackendTestCase import ProgrammingBackendTestCase

class testHaskellQC(ProgrammingBackendTestCase):
    """
    """
    
    backendClassName = 'haskellqc.HaskellQC.HaskellQC'

    # -- individual tests -----------------------------------------------------
    def testNoTestSpec(self):
        """_process_checkSemantics should return an instance of 'BackendResult' with error code -217 if no test spec is given"""
        
        result = HaskellQC(self.params)._process_checkSemantics(self.job)
        
        if result:
            self.assertEqual(result.getValue(), -217, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))

    
    def testNoTestData(self):
        """_process_checkSemantics should return an instance of 'BackendResult' with error code -216 if no properties are given"""
        
        self.job['tests'] = ['default',]
        self.job['properties'] = ''
        
        result = HaskellQC(self.params)._process_checkSemantics(self.job)
        
        if result:
            self.assertEqual(result.getValue(), -216, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))


    jobdata = {'backend':'haskellqc', 
               'submission': \
"""
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = (rev xs) ++ [x]
""",
               'properties': \
"""
prop_Reverse xs = rev xs == reverse xs
  where types = xs::[Int]

prop_RevRev xs = rev (rev xs) == xs
  where types = xs::[Int]
""",
               'tests': ['default'],
               }


    def testSyntaxFail(self):
        """_manage_checkSyntax should return False for a syntactical incorrect program"""

        self.jobdata['submission'] = 'rev xs -> xs'

        job = BackendJob(data=self.jobdata)

        backend = HaskellQC(self.params)
        result = backend._manage_checkSyntax(job)
        
        if result:
            self.assertEqual(result.getValue(), False, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSyntaxSuccess(self):
        """_manage_checkSyntax should return True for a correct program"""
        
        self.jobdata['submission'] = 'rev xs = xs'

        job = BackendJob(data=self.jobdata)

        backend = HaskellQC(self.params)
        result = backend._manage_checkSyntax(job)
        
        #print 'jobId', job.getId()
        
        if result:
            self.assertEqual(result.getValue(), True, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSemanticFailure(self):
        """_manage_checkSemantic should return False for an incorrect program"""
        
        self.jobdata['submission'] = 'rev xs = xs'

        job = BackendJob(data=self.jobdata)

        backend = HaskellQC(self.params)
        result = backend._manage_checkSemantics(job)
        
        #print 'jobId', job.getId()

        if result:
            self.assertEqual(result.getValue(), False, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSemanticSuccess(self):
        """_manage_checkSemantic should return True for a correct program"""
        
        self.jobdata['submission'] = \
"""
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = (rev xs) ++ [x]
"""

        job = BackendJob(data=self.jobdata)

        backend = HaskellQC(self.params)
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
    suite.addTest(makeSuite(testHaskellQC))
    
    return suite


# -- main ---------------------------------------------------------------------
if __name__ == '__main__':
    TestRunner = unittest.TextTestRunner
    suite = test_suite()

    TestRunner().run(suite)
