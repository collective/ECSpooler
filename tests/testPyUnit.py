# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
import unittest

from backends.pyunit import PyUnit
from lib.data.BackendJob import BackendJob
from ProgrammingBackendTestCase import ProgrammingBackendTestCase

class testPyUnit(ProgrammingBackendTestCase):
    """
    """
    
    backendClassName = 'pyunit.PyUnit.PyUnit'

    # -- individual tests -----------------------------------------------------
    jobdata = {'backend':'pyunit', 
               'submission':'def square(x): return x * x',
               'unitTests': \
"""import unittest

class testSquare(unittest.TestCase):
    def testSquare(self):
        self.assertEqual(25, square(5), 'Stupid student')
 
""",
               'tests': ['default'],
               'imports': '#',
               }


    def testSyntaxFail(self):
        """_manage_checkSyntax should return False for a syntactical incorrect program"""

        self.jobdata['submission'] = 'def square(x) x * x'

        job = BackendJob(data=self.jobdata)

        backend = PyUnit.PyUnit(self.params)
        result = backend._manage_checkSyntax(job)
        
        if result:
            self.assertEqual(result.getValue(), False, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSyntaxSuccess(self):
        """_manage_checkSyntax should return True for a correct program"""

        self.jobdata['submission'] = 'def square(x): return x * x'

        job = BackendJob(data=self.jobdata)

        backend = PyUnit.PyUnit(self.params)
        result = backend._manage_checkSyntax(job)
        
        if result:
            self.assertEqual(result.getValue(), True, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSemanticFailure(self):
        """_manage_checkSemantic should return False for an incorrect program"""
        
        self.jobdata['submission'] = 'def square(x): return -1'

        job = BackendJob(data=self.jobdata)

        backend = PyUnit.PyUnit(self.params)
        result = backend._manage_checkSemantics(job)
        
        if result:
            self.assertEqual(result.getValue(), False, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSemanticSuccess(self):
        """_manage_checkSemantic should return True for a correct program"""
        
        self.jobdata['submission'] = 'def square(x): return x * x'

        job = BackendJob(data=self.jobdata)

        backend = PyUnit.PyUnit(self.params)
        result = backend._manage_checkSemantics(job)
        
        if result:
            self.assertEqual(result.getValue(), True, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))



def test_suite():
    """
    """
    from unittest import TestSuite, makeSuite

    suite = TestSuite()
    suite.addTest(makeSuite(testPyUnit))
    
    return suite


# -- main ---------------------------------------------------------------------
if __name__ == '__main__':
    TestRunner = unittest.TextTestRunner
    suite = test_suite()

    TestRunner().run(suite)
