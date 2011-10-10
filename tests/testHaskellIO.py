# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2011 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
import unittest

from backends.haskellio.HaskellIO import HaskellIO
from lib.data.BackendJob import BackendJob

from ProgrammingBackendTestCase import ProgrammingBackendTestCase

class testHaskellIO(ProgrammingBackendTestCase):
    """
    """
    
    backendClassName = 'haskellio.HaskellIO.HaskellIO'

    # -- individual tests -----------------------------------------------------
    jobdata = {'backend':'haskell-io', 
               'submission':'s70n :: IO Int\ns70n = do return 70',
               'modelSolution': 's70n :: IO Int\ns70n = do return 70',
               'tests': ['simple'],
               'testData': 's70n',
               }


    def testSyntaxFail(self):
        """_manage_checkSyntax should return False for a syntactical incorrect program"""

        self.jobdata['submission'] = 's70n :: IO ()\ns70n = do putStr 70'

        job = BackendJob(data=self.jobdata)

        backend = HaskellIO(self.params)
        result = backend._manage_checkSyntax(job)
        
        #print 'jobId', job.getId()

        if result:
            self.assertEqual(result.getValue(), False, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSyntaxSuccess(self):
        """_manage_checkSyntax should return True for a correct program"""
        
        self.jobdata['submission'] = 's70n :: IO ()\ns70n = do putStr "70"'

        job = BackendJob(data=self.jobdata)

        backend = HaskellIO(self.params)
        result = backend._manage_checkSyntax(job)
        
        #print 'jobId', job.getId()
        
        if result:
            self.assertEqual(result.getValue(), True, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSemanticFailure(self):
        """_manage_checkSemantic should return False for an incorrect program"""
        
        self.jobdata['submission'] = 's70n :: IO Int\ns70n = do return 42'

        job = BackendJob(data=self.jobdata)
        
        backend = HaskellIO(self.params)
        result = backend._manage_checkSemantics(job)
        
        #print 'jobId', job.getId()

        if result:
            self.assertEqual(result.getValue(), False, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSemanticSuccess(self):
        """_manage_checkSemantic should return True for a correct program"""
        
        self.jobdata['submission'] = 's70n :: IO Int\ns70n = do return 70'

        job = BackendJob(data=self.jobdata)

        backend = HaskellIO(self.params)
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
    suite.addTest(makeSuite(testHaskellIO))
    
    return suite


# -- main ---------------------------------------------------------------------
if __name__ == '__main__':
    TestRunner = unittest.TextTestRunner
    suite = test_suite()

    TestRunner().run(suite)
