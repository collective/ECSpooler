# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.

import unittest

from lib.data.BackendResult import BackendResult

class testTestResult(unittest.TestCase):
    """
    """
    value = True
    message = 'Your submission passed all tests.'
    data = {'value':value, 'message':message}
    id = 'x0815z'
    result = BackendResult(data=data, id=id)
        

    def setUp(self):
        """
        """
    
    
    # -- Sanity Check ---------------------------------------------------------
    
    varValue = False
    varMessage = 'Your submission failed. Test case was: ...' 
    
    def testGetValue(self):
        """getValue should give known result with known input"""
        self.assertEqual(self.value, self.result.getValue())
        
    def testGetMessage(self):
        """getMessage should give known result with known input"""
        self.assertEqual(self.message, self.result.getMessage())


    def testSetValue(self):
        """getValue should give same value as set in setValue"""
        self.result.setValue(self.varValue)
        self.assertEqual(self.varValue, self.result.getValue())
        
    def testSetMessage(self):
        """getMessage should give same value as set in setMessage"""
        self.result.setMessage(self.varMessage)
        self.assertEqual(self.varMessage, self.result.getMessage())


    # -- Bad Input ------------------------------------------------------------
    
    def testNoInput(self):
        """ BackendResult should fail with no input"""
        try:
            BackendResult()
        except AssertionError, err:
            self.assertTrue('AssertionError raised: %s' % repr(err))
        else:
            self.assertFalse('No AssertionError raised.')   

    def testNoMessage(self):
        """ BackendResult should fail if 'submisson' is not given"""
        try:
            BackendResult(self.value)
        except AssertionError, err:
            self.assertTrue('AssertionError raised: %s' % repr(err))
        else:
            self.assertFalse('No AssertionError raised.')

    def testInvalidData(self):
        """BackendResult should fail if 'value' and 'message' are not contained in 'data'"""
        try:
            data = {}
            BackendResult(data=data)
        except AssertionError, err:
            self.assertTrue('AssertionError raised: %s' % repr(err))
        else:
            self.assertFalse('No AssertionError raised.')

    def testNoMessageInData(self):
        """BackendResult should fail if 'message' is not contained in 'data'"""
        try:
            data = {'value':self.value}
            BackendResult(data=data)
        except AssertionError, err:
            self.assertTrue('AssertionError raised: %s' % repr(err))
        else:
            self.assertFalse('No AssertionError raised.')

    def testNoBackendInData(self):
        """BackendResult should fail if 'value' is not contained in 'data'"""
        try:
            data = {'message':self.message}
            BackendResult(data=data)
        except AssertionError, err:
            self.assertTrue('AssertionError raised: %s' % repr(err))
        else:
            self.assertFalse('No AssertionError raised.')


def test_suite():
    """
    """
    from unittest import TestSuite, makeSuite

    suite = TestSuite()
    suite.addTest(makeSuite(testTestResult))
    
    return suite


# -- main ---------------------------------------------------------------------
if __name__ == "__main__":
    unittest.main()
