# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2011 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
import logging

# local imports
from backends.haskell.Haskell import Haskell
from backends.haskell.Haskell import tests

from backends.haskellio import config

LOG = logging.getLogger()


# set a modified wrapper code for the semantic check
WRAPPER_TEMPLATE = \
"""module Main where
import Model
import Student

${helpFunctions}

${testFunction}

o1 = Model.${testData}
o2 = Student.${testData}

main = do v1 <- o1
          v2 <- o2
          putStr("isEqual=" ++ show(haskell_backend_internal_equality_test (v1) (v2)) ++ ";;expected=" ++ show(v1) ++ ";;received=" ++ show(v2))
"""

# modify schema for testing           
testEnvs = tests.copy()

testEnvs['simple'].semantic = WRAPPER_TEMPLATE
testEnvs['permutation'].semantic = WRAPPER_TEMPLATE
testEnvs['simple'].compiler = config.INTERPRETER
testEnvs['permutation'].compiler = config.INTERPRETER
testEnvs['simple'].interpreter = config.INTERPRETER
testEnvs['permutation'].interpreter = config.INTERPRETER

class HaskellIO(Haskell):
    """
    Tests functions which return values of type Haskell I/O.
    """
    
    id = 'haskell-io'
    name = 'Haskell I/O'
    testSchema = testEnvs
    

    def __init__(self, params, versionFile=__file__):
        """
        """
        Haskell.__init__(self, params, versionFile)
