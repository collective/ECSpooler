# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
import unittest

from backends.keywords.Keywords import Keywords
from lib.data.BackendJob import BackendJob

from BackendTestCase import BackendTestCase

class testKeywords(BackendTestCase):
    """
    """
    
    backendClassName = 'keywords.Keywords.Keywords'

    # -- individual tests -----------------------------------------------------
    jobdata = {'backend':'keywords', 
               'submission':'Lets say foo, bar, bla, and blubber.',
               'tests': ['keywords'],
               'keywords': 'foo\nbar',
               }


    def testExactMatchingFail(self):
        """_process_execute should return 0 if no keyword could be found in submission"""

        self.jobdata['submission'] = '- bla and blubber'

        job = BackendJob(data=self.jobdata)

        backend = Keywords(self.params)
        result = backend._process_execute(job)
        
        if result:
            self.assertEqual(result.getValue(), 0, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testExactMatchingSuccess(self):
        """_process_execute should return 100 if all keywords were found in submission"""

        self.jobdata['submission'] = '- foo and bar are first class citizens'

        job = BackendJob(data=self.jobdata)

        backend = Keywords(self.params)
        result = backend._process_execute(job)
        
        if result:
            self.assertEqual(result.getValue(), 100, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testRegExFail(self):
        """_process_execute should return 0 if a given regulare exp doesn't match"""

        self.jobdata['submission'] = '- bla and blubber'
        self.jobdata['tests'] = ['regexp']
        self.jobdata['keywords'] = '.*foo.+.*bar'

        job = BackendJob(data=self.jobdata)

        backend = Keywords(self.params)
        result = backend._process_execute(job)
        
        if result:
            self.assertEqual(result.getValue(), 0, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testRegExSuccess(self):
        """_process_execute should return 100 if a given regulare exp matches"""

        self.jobdata['submission'] = '- foo and bar are first class citizens'
        self.jobdata['tests'] = ['regexp']
        self.jobdata['keywords'] = '.*foo.+.*bar'

        job = BackendJob(data=self.jobdata)

        backend = Keywords(self.params)
        result = backend._process_execute(job)
        
        if result:
            self.assertEqual(result.getValue(), 100, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))



def test_suite():
    """
    """
    from unittest import TestSuite, makeSuite

    suite = TestSuite()
    suite.addTest(makeSuite(testKeywords))
    
    return suite


# -- main ---------------------------------------------------------------------
if __name__ == '__main__':
    TestRunner = unittest.TextTestRunner
    suite = test_suite()

    TestRunner().run(suite)
