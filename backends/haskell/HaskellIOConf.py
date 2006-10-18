# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.

from backends.haskell.HaskellConf import HaskellConf

class HaskellIOConf(HaskellConf):
    """
    This class will be used to define all neccessary propertis for the Haskell
    backend.
    """
    
    # set a modified wrapper code for the semantic check
    wrapperTemplate = \
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

    # modify schema for testing
    tests = HaskellConf.tests.copy()

    tests['simple'].semantic = wrapperTemplate
    tests['permutation'].semantic = wrapperTemplate
