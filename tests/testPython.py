# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
import unittest

from backends import Python
from lib.data.BackendJob import BackendJob
from ProgrammingBackendTestCase import ProgrammingBackendTestCase

class testPython(ProgrammingBackendTestCase):
    """
    """
    
    backendClassName = 'Python'

    # -- individual tests -----------------------------------------------------
    jobdata = {'backend':'python', 
               'submission':'def square(x): return x * x',
               'model': 'def square(x): return x * x',
               'tests': ['simple'],
               'testData': 'square(0)\nsquare(2)\nsquare(3)',
               }


    def testSyntaxFail(self):
        """_manage_checkSyntax should return False for a syntactical incorrect program"""

        self.jobdata['submission'] = 'def square(x) x * x'

        job = BackendJob(data=self.jobdata)

        backend = Python(self.params)
        result = backend._manage_checkSyntax(job)
        
        if result:
            #print >> sys.stderr, result.getValue(), result.getMessage()
            self.assertEqual(result.getValue(), False)
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSyntaxSuccess(self):
        """_manage_checkSyntax should return True for a correct program"""

        self.jobdata['submission'] = 'def square(x): return x * x'

        job = BackendJob(data=self.jobdata)

        backend = Python(self.params)
        result = backend._manage_checkSyntax(job)
        
        if result:
            #print >> sys.stderr, result.getValue(), result.getMessage()
            self.assertEqual(result.getValue(), True)
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSemanticFailure(self):
        """_manage_checkSemantic should return False for an incorrect program"""
        
        self.jobdata['submission'] = 'def square(x): return x + x'

        job = BackendJob(data=self.jobdata)

        backend = Python(self.params)
        result = backend._manage_checkSemantics(job)
        
        if result:
            #print >> sys.stderr, result.getValue(), result.getMessage()
            self.assertEqual(result.getValue(), False)
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSemanticSuccess(self):
        """_manage_checkSemantic should return True for a correct program"""
        
        self.jobdata['submission'] = 'def square(x): return (x * x)'

        job = BackendJob(data=self.jobdata)

        backend = Python(self.params)
        result = backend._manage_checkSemantics(job)

        if result:
            #print >> sys.stderr, result.getValue(), result.getMessage()
            self.assertEqual(result.getValue(), True)
        else:
            self.assertFalse('No result: %s' % repr(result))



def test_suite():
    """
    """
    from unittest import TestSuite, makeSuite

    suite = TestSuite()
    suite.addTest(makeSuite(testPython))
    
    return suite


# -- main ---------------------------------------------------------------------
if __name__ == '__main__':
    TestRunner = unittest.TextTestRunner
    suite = test_suite()

    TestRunner().run(suite)
