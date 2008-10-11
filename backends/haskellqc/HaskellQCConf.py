# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2008 Otto-von-Guericke-University, Magdeburg
#
# This file is part of ECSpooler.
import re

from os.path import join, dirname, abspath

from lib.util.BackendSchema import TestEnvironment, InputField, Schema
from backends.haskell.HaskellConf import HaskellConf

#REGEX_FAILED = '(?m)Falsifiable, after \d+ tests?:'
#REGEX_FAILED_TESTDATA = '\n(-?.+)'
#REGEX_PASSED_TESTNUMBER = 'passed (\d+)'
#REGEX_LINENUMBER = ':\d+'
#DEFAULT_MODEL_MODULE_NAME = '#Model#'
#DEFAULT_STUDENT_MODULE_NAME =  '#Student#'

class HaskellQCConf(HaskellConf):
    """
    This class will be used to define all necessary propertis and settigs 
    for the Haskell QuickCheck backend.
    
    It inherits from HaskellConf, so we can use variables already 
    defined there.
    """
    
    #interpreter = join(abspath(dirname(__file__)), 'runhugs+systrace')
    interpreter = join(abspath(dirname(__file__)), 'runhugs.sh')
    
    # regex to get all property names
    PROPERTIES_RE = re.compile(r'(?P<name>prop_[A-Z,a-z,0-9]+\w*)')
    
    # regex to substitute 'prop_' placeholder with the property name
    PROP_RE = re.compile(r'\$\{prop_\}')
 
    # regex to identify failed tests
    FAILED_RE = re.compile(r'Falsifiable, after \d+ tests?:\n(-?.+)')
    PASSED_RE = re.compile(r'OK, (passed \d+ tests?)')
    
    # template for Haskell module containing all QuickCheck properties     
    propsTemplate = \
"""
module Properties where
import Student

%s
"""

    # template for wrapper code
    wrapperTemplate = \
"""
module Main where
import QuickCheck
import Properties

main :: IO ()
--main = quickCheck ${prop_}
main = verboseCheck ${prop_} 
"""

    # input schema
    inputSchema = Schema((
        InputField(
            'properties', 
            required = True, 
            label = 'QuickCheck properties',
            description = 'Enter one or more QuickCheck properties ' +
                          '(cf. <http://www.cs.chalmers.se/~rjmh/QuickCheck/>).',
            i18n_domain = 'EC',
        ),
    ))

    # testSchema
    tests = Schema((
        TestEnvironment(
            'default',
            label = 'Default',
            description = 'Default using QuickCheck.',
            #test = '',
            syntax = HaskellConf.syntaxCheckTemplate,
            semantic = wrapperTemplate,
            lineNumberOffset = 2,
            compiler = interpreter,
            interpreter = interpreter,
        ),
    ))
    
    # -- additional log settings ----------------------------------------------
