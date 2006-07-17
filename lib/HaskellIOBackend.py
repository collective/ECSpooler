# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.

import sys, os, re, popen2, tempfile
import threading
import logging

from types import StringType, UnicodeType

# local imports
from data import *
from util.utils import *

from HaskellBackend import HaskellBackend, tests

TEMPLATE_SEMANTIC = \
"""module Main where
import Model
import Student

${helpFunctions}

${testFunction}

o1 = Model.${testData}
o2 = Student.${testData}

main = do v1 <- o1
          v2 <- o2
          putStr("isEqual=" ++ show(test (v1) (v2)) ++ ";;expected=" ++ show(v1) ++ ";;received=" ++ show(v2))
"""

# testSchema
testsSchema = tests.copy()

testsSchema['simple'].semantic = TEMPLATE_SEMANTIC
testsSchema['permutation'].interpreter = TEMPLATE_SEMANTIC

class HaskellIOBackend(HaskellBackend):
    """
    Tests functions which return values of type Haskell I/O.
    """
    
    id = 'haskell-io'
    name = 'Haskell I/O'
    version = '1.0'

    testSchema = testsSchema
