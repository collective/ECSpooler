# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2011 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
import unittest

from backends.echo.Echo import Echo
from lib.data.BackendJob import BackendJob

portRange = range(15061,16060)

class testEcho(unittest.TestCase):
    """
    """
    
    backendClassName = 'echo.Echo.Echo'

    # -- individual tests -----------------------------------------------------
    jobdata = {'backend':'echo', 
               'submission':'Hello, world!',
               'tests': ['regulare'],
              }

    host = '0.0.0.0' #socket.getfqdn()
    port = 15060
    spooler = 'http://%s:%d' % (host, 15050)
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



    def testSuccess(self):
        """_process_execute should return True and 'Hello, world!'"""

        job = BackendJob(data=self.jobdata)

        backend = Echo(self.params)
        result = backend._process_execute(job)
        
        if result:
            self.assertEqual(result.getValue(), True)
            self.assertEqual(result.getMessage(), 'Hello, world!')
        else:
            self.assertFalse('No result: %s' % repr(result))


def test_suite():
    """
    """
    from unittest import TestSuite, makeSuite

    suite = TestSuite()
    suite.addTest(makeSuite(testEcho))
    
    return suite


# -- main ---------------------------------------------------------------------
if __name__ == '__main__':
    TestRunner = unittest.TextTestRunner
    suite = test_suite()

    TestRunner().run(suite)
