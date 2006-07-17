# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.

# local imports
from HaskellBackend import HaskellBackend, tests

INTERPRETER = '/opt/hugs/bin/runhugs -P"{HUGS}:/home/amelung/haskell_libs/:"'
 
# testSchema
testsSchema = tests.copy()

testsSchema['simple'].compiler = INTERPRETER
testsSchema['simple'].interpreter = INTERPRETER

testsSchema['permutation'].compiler = INTERPRETER
testsSchema['permutation'].interpreter = INTERPRETER


class HaskellAddedLibrariesBackend(HaskellBackend):
    """
    Derived from HaskellBackend this backend uses a limited library path.
    """
    
    id = 'haskell-added-libs'
    name = 'Haskell (extended library path)'
    version = '1.0'

    testSchema = testsSchema
