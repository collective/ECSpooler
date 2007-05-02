# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
import unittest

from ProgrammingBackendTestCase import ProgrammingBackendTestCase

class testJavaRE(ProgrammingBackendTestCase):
    """
    """
    
    backendClassName = 'JavaRE'

    # -- individual tests -----------------------------------------------------


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
