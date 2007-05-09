# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.

# Runs all tests in the current directory
#
# Execute like:
#   python runalltests.py

import os
import sys

import unittest

TestRunner = unittest.TextTestRunner
suite = unittest.TestSuite()

tests = os.listdir(os.curdir)
tests = [n[:-3] for n in tests if n.startswith('test') and n.endswith('.py')]

for test in tests:
    m = __import__(test)
        
    if hasattr(m, 'test_suite'):
        print >> sys.stdout, m.__name__
        suite.addTest(m.test_suite())


if __name__ == '__main__':
    TestRunner().run(suite)

