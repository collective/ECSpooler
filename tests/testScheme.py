# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
import os, sys
import unittest

# add parent directory to the system path
sys.path.append(os.path.join(os.path.dirname(__file__), os.pardir))

from backends.scheme.Scheme import Scheme
from lib.data.BackendJob import BackendJob
#from lib.data.BackendResult import BackendResult

from ProgrammingBackendTestCase import ProgrammingBackendTestCase

class testScheme(ProgrammingBackendTestCase):
    """
    """
    
    backendClassName = 'Scheme'

    # -- individual tests -----------------------------------------------------
    def testNoTestSpec(self):
        """_process_checkSemantics should return an instance of 'BackendResult' with error code -217 if no test spec is given"""
        
        result = Scheme(self.params)._process_checkSemantics(self.job)
        
        if result:
            self.assertEqual(result.getValue(), -217)
        else:
            self.assertFalse('No result: %s' % repr(result))

    
    def testNoTestData(self):
        """_process_checkSemantics should return an instance of 'BackendResult' with error code -216 if no test data are given"""
        
        self.job['tests'] = ['simple',]
        
        result = Scheme(self.params)._process_checkSemantics(self.job)
        
        if result:
            self.assertEqual(result.getValue(), -216)
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSyntaxFail(self):
        """_manage_checkSyntax should return False for a syntactical incorrect program"""

        jobdata = {'backend':'scheme', 
                   'submission':'(define (square x) (* x x)',
                   'tests': ['simple'],
                   }

        job = BackendJob(data=jobdata)

        backend = Scheme(self.params)
        result = backend._manage_checkSyntax(job)
        
        if result:
            #print >> sys.stderr, result.getValue(), result.getMessage()
            self.assertEqual(result.getValue(), False)
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSyntaxSuccess(self):
        """_manage_checkSyntax should return True for a correct program"""
        
        jobdata = {'backend':'scheme', 
                   'submission':'(define (square x) (* x x))',
                   'tests': ['simple'],
                   }

        job = BackendJob(data=jobdata)

        backend = Scheme(self.params)
        result = backend._manage_checkSyntax(job)
        
        if result:
            #print >> sys.stderr, result.getValue(), result.getMessage()
            self.assertEqual(result.getValue(), True)
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSemanticFailure(self):
        """_manage_checkSemantic should return False for an incorrect program"""
        
        jobdata = {'backend':'scheme', 
                   'submission':'(define (square x) (* x 2))',
                   'model': '(define (square x) (* x x))',
                   'tests': ['simple'],
                   'testData': '(square 0)\n(square 2)\n(square 3)',
                   }

        job = BackendJob(data=jobdata)

        backend = Scheme(self.params)
        result = backend._manage_checkSemantics(job)
        
        if result:
            #print >> sys.stderr, result.getValue(), result.getMessage()
            self.assertEqual(result.getValue(), False)
        else:
            self.assertFalse('No result: %s' % repr(result))

    
    def testSemanticSuccess(self):
        """_manage_checkSemantic should return True for a correct program"""
        
        jobdata = {'backend':'scheme', 
                   'submission':'(define (square x) (* x x))',
                   'model': '(define (square x) (* x x))',
                   'tests': ['simple'],
                   'testData': '(square 0)\n(square 2)\n(square 3)',
                   }

        job = BackendJob(data=jobdata)

        backend = Scheme(self.params)
        result = backend._manage_checkSemantics(job)
        
        if result:
            self.assertEqual(result.getValue(), True)
        else:
            self.assertFalse('No result: %s' % repr(result))


def test_suite():
    """
    """
    from unittest import TestSuite, makeSuite

    suite = TestSuite()
    suite.addTest(makeSuite(testScheme))
    
    return suite


# -- main ---------------------------------------------------------------------
if __name__ == '__main__':
    TestRunner = unittest.TextTestRunner
    suite = test_suite()

    TestRunner().run(suite)
    