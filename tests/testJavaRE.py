# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
import sys
import unittest

from backends.java_re import JavaRE
from lib.data.BackendJob import BackendJob
from ProgrammingBackendTestCase import ProgrammingBackendTestCase

class testJavaRE(ProgrammingBackendTestCase):
    """
    """
    
    backendClassName = 'java_re.JavaRE.JavaRE'

    # -- individual tests -----------------------------------------------------

    jobdata = {'backend': 'java_re', 
               'submission': '"\\bheise online\\b"',
               'modelSolution': '"\\bheise online\\b"',
               'tests': ['findFirstGroup0'],
               'testData': 'http://www.heise.de/newsticker/',
               }


    def testSyntaxFail(self):
        """_manage_checkSyntax should return False for a syntactical incorrect program"""

        self.jobdata['submission'] = '"!^.*Terrorist.\*$"'

        job = BackendJob(data=self.jobdata)

        backend = JavaRE.JavaRE(self.params)
        result = backend._manage_checkSyntax(job)

        #print 'jobId', job.getId()
        
        if result:
            self.assertEqual(result.getValue(), False, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSyntaxSuccess(self):
        """_manage_checkSyntax should return True for a correct program"""
        
        self.jobdata['submission'] = '"!^.*Terrorist.*$"'

        job = BackendJob(data=self.jobdata)

        backend = JavaRE.JavaRE(self.params)
        result = backend._manage_checkSyntax(job)
        
        #print 'jobId', job.getId()
        
        if result:
            self.assertEqual(result.getValue(), True, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSemanticFailure(self):
        """_manage_checkSemantic should return False for an incorrect program"""
        
        self.jobdata['submission'] = '".*Telekom.*"'

        job = BackendJob(data=self.jobdata)

        backend = JavaRE.JavaRE(self.params)
        result = backend._manage_checkSemantics(job)
        
        #print 'jobId', job.getId()

        if result:
            self.assertEqual(result.getValue(), False, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSemanticSuccess(self):
        """_manage_checkSemantic should return True for a correct program"""
        
        self.jobdata['submission'] = '"\\bheise online\\b"'

        job = BackendJob(data=self.jobdata)

        backend = JavaRE.JavaRE(self.params)
        result = backend._manage_checkSemantics(job)
        
        if result:
            self.assertEqual(result.getValue(), True, 
                             '(job: %s) %s' % (job.getId(), result.getMessage()))
        else:
            self.assertFalse('No result: %s' % repr(result))

    # -- ----------------------------------------------------------------------

    def testSpecialRegExFail(self):
        """_manage_checkSyntax should return False for this special regex"""
        
        self.jobdata['submission'] = \
            '"(?<=(\\\Q|\\\E))(.[^\\\Q|\\\E]*)(?=\\\Q|\\\E(.[^\\\Q|\\\E]*)\\\Q|\\\E)'

        self.jobdata['modelSolution'] = \
            '"(?<=\|\s{0,5})([\w\s\.&&[^0-9]])*([\w\.&&[^0-9]])+(?=\s*\|)"'
        
        self.jobdata['tests'] = ['findFirstGroup0']
        
        self.jobdata['testData'] = \
            'http://wwwai.cs.uni-magdeburg.de/Members/makunze/dokumente-fur-lehre/weinert1.txt'

        job = BackendJob(data=self.jobdata)

        backend = JavaRE.JavaRE(self.params)
        result = backend._manage_checkSyntax(job)

        if result:
            self.assertEqual(result.getValue(), False,  
                             '(job: %s) %s' % (job.getId(), result.getMessage()))
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSpecialRegExSuccess(self):
        """_manage_checkSyntax should return True for this special regex with leading and trailing white space"""
        
        self.jobdata['submission'] = \
            '   "(?<=(\\\Q|\\\E))(.[^\\\Q|\\\E]*)(?=\\\Q|\\\E(.[^\\\Q|\\\E]*)\\\Q|\\\E)"  '

        self.jobdata['modelSolution'] = \
            '"(?<=\|\s{0,5})([\w\s\.&&[^0-9]])*([\w\.&&[^0-9]])+(?=\s*\|)"'
        
        self.jobdata['tests'] = ['findFirstGroup0']
        
        self.jobdata['testData'] = \
            'http://wwwai.cs.uni-magdeburg.de/Members/makunze/dokumente-fur-lehre/weinert1.txt'

        job = BackendJob(data=self.jobdata)

        backend = JavaRE.JavaRE(self.params)
        result = backend._manage_checkSyntax(job)

        if result:
            self.assertEqual(result.getValue(), True,  
                             '(job: %s) %s' % (job.getId(), result.getMessage()))
        else:
            self.assertFalse('No result: %s' % repr(result))

def test_suite():
    """
    """
    from unittest import TestSuite, makeSuite

    suite = TestSuite()
    suite.addTest(makeSuite(testJavaRE))
    
    return suite


# -- main ---------------------------------------------------------------------
if __name__ == '__main__':
    TestRunner = unittest.TextTestRunner
    suite = test_suite()

    TestRunner().run(suite)
