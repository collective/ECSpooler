# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
from types import DictType
import sys, os, socket, signal
import unittest

import xmlrpclib

from lib.Spooler import Spooler

class testSpooler(unittest.TestCase):
    """
    """
    _auth = {"username":"demo", "password":"foobar"}
    _pwdFile = os.path.join(os.path.dirname(__file__), '..', 'etc', 'passwd')
    _host = socket.getfqdn()
    _port = 5050

    spooler = Spooler(_host, _port, _pwdFile)
    
    def setUp(self):
        """
        """
        self.spooler.start()
       
        
    def tearDown(self):
        """
        """
        spooler = xmlrpclib.ServerProxy("http://%s:%s" % (self._host, self._port))
        pid = spooler.getStatus(self._auth)[1]['pid']
        
        try:
            # stops the spooler sending kill -15 process-id
            os.kill(pid, signal.SIGTERM)         
        except AttributeError:
            print >> sys.stderr, 'os.kill and/or signal.SIGTERM not defined. ' \
                                 'Trying to stop spooler elsewhere.'
    
    def testGetStatus(self):
        """FIXME: """
        status = self.spooler.getStatus(self._auth)
        
        self.assertTrue(type(status) == DictType)


#def test_suite():
#    """
#    """
#    from unittest import TestSuite, makeSuite
#
#    suite = TestSuite()
#    suite.addTest(makeSuite(testSpooler))
#    
#    return suite
