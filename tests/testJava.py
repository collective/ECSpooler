# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
import sys
import unittest

from backends import Java
from lib.data.BackendJob import BackendJob
from ProgrammingBackendTestCase import ProgrammingBackendTestCase

class testJava(ProgrammingBackendTestCase):
    """
    """
    
    backendClassName = 'Java'

    # -- individual tests -----------------------------------------------------
    modelSolution = \
"""
public class Test
{
    public int max3(int a, int b, int c)
    {
        return Math.max(a, Math.max(b, c));
    }
}
"""

    submission = \
"""
public class Test
{
    public int max3(int a, int b, int c)
    {
        return Math.max(a, Math.max(b, c));
    }
}
"""

    jobdata = {'backend': 'java', 
               'submission': submission,
               'modelSolution': modelSolution,
               'tests': ['simple'],
               'testData': 'new Test().max3(1,2,3)\nnew Test().max3(1,3,2)\nnew Test().max3(3,2,1)',
               }


    def testSyntaxFail(self):
        """_manage_checkSyntax should return False for a syntactical incorrect program"""

        self.jobdata['submission'] = 'public class Test { public int max3(int a, int b, int c) {} }'

        job = BackendJob(data=self.jobdata)

        backend = Java(self.params)
        result = backend._manage_checkSyntax(job)
        
        if result:
            #print >> sys.stderr, result.getValue(), result.getMessage()
            self.assertEqual(result.getValue(), False)
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSyntaxSuccess(self):
        """_manage_checkSyntax should return True for a correct program"""
        
        self.jobdata['submission'] = self.submission

        job = BackendJob(data=self.jobdata)

        backend = Java(self.params)
        result = backend._manage_checkSyntax(job)
        
        #print 'jobId', job.getId()
        
        if result:
            #print >> sys.stdout, result.getValue(), result.getMessage()
            self.assertEqual(result.getValue(), True)
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSemanticFailure(self):
        """_manage_checkSemantic should return False for an incorrect program"""
        
        self.jobdata['submission'] = \
"""
public class Test
{
    public int max3(int a, int b, int c) 
    {
        return a;
    } 
}
"""

        job = BackendJob(data=self.jobdata)

        backend = Java(self.params)
        result = backend._manage_checkSemantics(job)
        
        if result:
            #print >> sys.stdout, result.getValue(), result.getMessage()
            self.assertEqual(result.getValue(), False)
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSemanticSuccess(self):
        """_manage_checkSemantic should return True for a correct program"""
        
        self.jobdata['submission'] = self.submission

        job = BackendJob(data=self.jobdata)

        backend = Java(self.params)
        result = backend._manage_checkSemantics(job)
        
        #print 'jobId', job.getId()

        if result:
            #print >> sys.stdout, result.getValue(), result.getMessage()
            self.assertEqual(result.getValue(), True)
        else:
            self.assertFalse('No result: %s' % repr(result))



def test_suite():
    """
    """
    from unittest import TestSuite, makeSuite

    suite = TestSuite()
    suite.addTest(makeSuite(testJava))
    
    return suite


# -- main ---------------------------------------------------------------------
if __name__ == '__main__':
    TestRunner = unittest.TextTestRunner
    suite = test_suite()

    TestRunner().run(suite)
