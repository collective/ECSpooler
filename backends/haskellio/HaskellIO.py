# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.
import sys, os, re
import logging

from types import StringTypes

# local imports
from backends.haskell.Haskell import Haskell
from backends.haskellio.HaskellIOConf import HaskellIOConf

class HaskellIO(Haskell):
    """
    Tests functions which return values of type Haskell I/O.
    """
    
    id = 'haskell-io'
    name = 'Haskell I/O'
    #version = '1.0'

    testSchema = HaskellIOConf.tests
