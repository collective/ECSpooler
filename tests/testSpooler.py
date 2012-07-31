# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2011 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
from types import DictType, IntType
import os
import unittest

from lib.Spooler import Spooler

class testSpooler(unittest.TestCase):
    """
    """
    _userAuth = {"username":"test", "password":"foobar"}
    _rootAuth = {"username":"root", "password":"foobar"}
    _pwdFile = os.path.join(os.path.dirname(__file__), 'passwd')
    _host = '0.0.0.0' #socket.getfqdn()
    _port = 15050

    spooler = Spooler(_host, _port, _pwdFile)
    
    def setUp(self):
        """
        """
        
    def tearDown(self):
        """
        """
        

    def testAuthFail(self):
        """
        """
        self.assertRaises(Exception, self.spooler.getPID, self._userAuth)

    def testGetPID(self):
        """
        """
        pid = self.spooler.getPID(self._rootAuth)
        self.assertTrue(pid != None)
        self.assertTrue(type(pid) == IntType)

    
    def testGetStatus(self):
        """
        """
        status = self.spooler.getStatus(self._userAuth)
        self.assertTrue(type(status) == DictType)

    def testGetBackends(self):
        """
        """
        backends = self.spooler.getBackends(self._userAuth)
        self.assertTrue(type(backends) == DictType)

    def testGetResults(self):
        """
        """
        backends = self.spooler.getResults(self._userAuth)
        self.assertTrue(type(backends) == DictType)


def test_suite():
    """
    """
    from unittest import TestSuite, makeSuite

    suite = TestSuite()
    suite.addTest(makeSuite(testSpooler))
    
    return suite

# -- main ---------------------------------------------------------------------
if __name__ == '__main__':
    TestRunner = unittest.TextTestRunner
    suite = test_suite()

    TestRunner().run(suite)
