# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2013
#
# This file is part of ECSpooler.

import os
import unittest
import tempfile
import errno

#from lib.util import auth
from lib.util import utils

class testAuth(unittest.TestCase):
    """
    """
    
    def setUp(self):
        """
        """
        
    def tearDown(self):
        """
        """


class testUtils(unittest.TestCase):
    """
    """
    __pid_filename = os.path.join(tempfile.gettempdir(), 'test_utils.pid')

    def setUp(self):
        """
        """
        
    def tearDown(self):
        """
        """
        try:
            os.remove(self.__pid_filename)
        except OSError, exc:
            if exc.errno == errno.ENOENT:
                pass
            else:
                raise
                
    def test_write_pid_file(self):
        """
        """
        utils.write_pid_file(self.__pid_filename)
        self.assertTrue(os.path.exists(self.__pid_filename))


    def test_read_pid_file_None(self):
        """
        """
        pid = utils.read_pid_file(self.__pid_filename)
        self.assertIsNone(pid)


    def test_read_pid_file(self):
        """
        """
        utils.write_pid_file(self.__pid_filename)
        pid = utils.read_pid_file(self.__pid_filename)
        self.assertEqual(pid, os.getpid())

    def test_remove_pid_file(self):
        """
        """
        utils.write_pid_file(self.__pid_filename)
        self.assertTrue(os.path.exists(self.__pid_filename))
        utils.remove_pid_file(self.__pid_filename)
        self.assertFalse(os.path.exists(self.__pid_filename))


def test_suite():
    """
    """
    from unittest import TestSuite, makeSuite

    suite = TestSuite()
    suite.addTest(makeSuite(testAuth))
    suite.addTest(makeSuite(testUtils))
    
    return suite


# -- main ---------------------------------------------------------------------
if __name__ == '__main__':
    TestRunner = unittest.TextTestRunner
    suite = test_suite()

    TestRunner().run(suite)
