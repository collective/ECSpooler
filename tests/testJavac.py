# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2011 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
import unittest

from backends.javac.Javac import Javac
from lib.data.BackendJob import BackendJob
from ProgrammingBackendTestCase import ProgrammingBackendTestCase

class testJava(ProgrammingBackendTestCase):
    """
    """
    
    backendClassName = 'javac.Javac.Javac'

    # -- individual tests -----------------------------------------------------
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

    jobdata = {'backend': 'javac', 
               'submission': submission,
               'tests': ['simple'],
               }


    def testSyntaxFail(self):
        """_manage_checkSyntax should return False for a syntactical incorrect program"""

        self.jobdata['submission'] = 'public class Test { public int max3(int a, int b, int c) {} }'

        job = BackendJob(data=self.jobdata)

        backend = Javac(self.params)
        result = backend._manage_checkSyntax(job)
        
        if result:
            self.assertEqual(result.getValue(), False, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSyntaxSuccess(self):
        """_manage_checkSyntax should return True for a correct program"""
        
        self.jobdata['submission'] = self.submission

        job = BackendJob(data=self.jobdata)

        backend = Javac(self.params)
        result = backend._manage_checkSyntax(job)
        
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
    suite.addTest(makeSuite(testJava))
    
    return suite


# -- main ---------------------------------------------------------------------
if __name__ == '__main__':
    TestRunner = unittest.TextTestRunner
    suite = test_suite()

    TestRunner().run(suite)
