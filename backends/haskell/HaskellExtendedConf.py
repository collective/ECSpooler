# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.

from backends.haskell.HaskellConf import HaskellConf

class HaskellExtendedConf(HaskellConf):
    """
    This class will be used to define all necessary propertis for the Haskell
    backend.
    """
    
    syntaxTemplate = \
"""module Main where
    
${SOURCE}
       
main = putStr(\"\")
"""
    
    semanticCheckTemplate = \
"""module ${MODULE} where

${SOURCE}

main = ${testData}
"""
    
    wrapperTemplate = \
"""module Main where
import Model
import Student

${testFunction}

o1 = Model.main
o2 = Student.main

main = putStr(\"isEqual=\" ++ show(test (o1) (o2)) ++ \";;expected=\" ++ show(o1) ++ \";;received=\" ++ show(o2))
"""

    # use inputSchema of backend Haskel and modify it
    inputSchema = HaskellConf.inputSchema.copy()
    inputSchema.delField('helpFunctions')

    # use testSchema of backend Haskell and modify it
    testSchema = HaskellConf.tests.copy()

    testSchema['simple'].compiler = HaskellConf.interpreter
    testSchema['simple'].interpreter = HaskellConf.interpreter
    testSchema['simple'].syntax = syntaxTemplate
    testSchema['simple'].semantic = wrapperTemplate
    
    testSchema['permutation'].compiler = HaskellConf.interpreter
    testSchema['permutation'].interpreter = HaskellConf.interpreter
    testSchema['permutation'].syntax = syntaxTemplate,
    testSchema['permutation'].semantic = wrapperTemplate
