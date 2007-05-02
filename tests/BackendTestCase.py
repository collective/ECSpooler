# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
import unittest
import socket

from backends import *

from lib.data.BackendJob import BackendJob
from lib.data.BackendResult import BackendResult

portRange = range(5061,6060)

class BackendTestCase(unittest.TestCase):
    """
    """
    host = socket.getfqdn()
    port = 5060
    spooler = 'http://%s:%d' % (socket.getfqdn(), 5050)
    auth = {"username":"demo", "password":"foobar"}
    
    def setUp(self):
        """
        Set up before test method runs
        """
        #self.port = random.randint(self.port, self.port + 100)
        self.port = portRange.pop(0)
        
        self.params = {'host':    self.host, 
                       'port':    self.port,
                       'spooler': self.spooler, 
                       'auth':    self.auth}

        self.job = BackendJob('backend', 'submission')


    def tearDown(self):
        """
        Tidy up after test method has been run
        """
        pass


    def testNoInput(self):
        """Backend instance should fail with no input"""
        try:
            params = {}
            exec '%s(params)' % self.backendClassName
        except AssertionError, err:
            self.assertTrue('AssertionError raised: %s' % repr(err))


    def testInvalidInput(self):
        """Backend instance should fail with inproperly input"""
        try:
            params = self.params
            params.pop('spooler')
            params.pop('auth')
            exec '%s(params)' % self.backendClassName
        except AssertionError, err:
            self.assertTrue('AssertionError raised: %s' % repr(err))


    def testValidInput(self):
        """Backend instance should not fail with all required attributes"""

#        try:
#            params = self.params
#            exec '%s(params)' % self.backendClassName
#        except Exception, err:
#            self.assertFalse('Exception raised: %s' % repr(err))
#        else:
#            self.assertTrue('No Exception raised.')
        params = self.params
        exec '%s(params)' % self.backendClassName


    def testProcessExcecuteResultType(self):
        """_process_exectue should always return an instance of 'BackendResult'"""
        result = None
        #exec 'backend = %s(self.params)' % self.backendClassName
        #backend = Scheme(self.params)
        #result = backend._process_execute(self.job)
        
        exec 'result = %s(self.params)._process_execute(self.job)' % self.backendClassName
        
        if result:
            self.assertEqual(result.__class__, BackendResult,
                            "Returned object is not an instance of BackendResult")
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testInvalidTestSpec(self):
        """_process_exectue should return an instance of 'BackendResult' with error code -217 if no test spec was selected"""
        result = None
        
        exec 'result = %s(self.params)._process_execute(self.job)' % self.backendClassName
        
        if result:
            self.assertEqual(result.getValue(), -217)
        else:
            self.assertFalse('No result: %s' % repr(result))

