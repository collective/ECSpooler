# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.
import sys, logging
from os.path import join, dirname

from lib.util.BackendSchema import InputField
from lib.util.BackendSchema import RepeatField
from lib.util.BackendSchema import Schema
from lib.util.BackendSchema import TestEnvironment

class HaskellConf:
    """
    Defines all properties used by backend Haskell.
    """

    #interpreter = join(dirname(__file__), 'runhugs+systrace')
    interpreter = join(dirname(__file__), 'runhugs.sh')

    # load Haskell function to do a simple test
    try:
        simpleTest = file(join(dirname(__file__), 'simpleTest.hs'), 'r').read()
    except IOError, ioe:
        logging.warn('%s: %s' % (sys.exc_info()[0], ioe))
        simpleTest = ''

    # load Haskell function to do a test which allows permutation of list elems
    try:
        permTest = file(join(dirname(__file__), 'permTest.hs'), 'r').read()
    except IOError, ioe:
        logging.warn('%s: %s' % (sys.exc_info()[0], ioe))
        permTest = ''

    # load Haskell function to do a test which allows permutation of list elems
    try:
        toleranceTest = file(join(dirname(__file__), 'toleranceTest.hs'), 'r').read()
    except IOError, ioe:
        logging.warn('%s: %s' % (sys.exc_info()[0], ioe))
        permTest = ''

    wrapperTemplate = \
"""module Main where
import Model
import Student

${helpFunctions}

-- must be named 'test'
${testFunction}

main = putStr(\"isEqual=\" ++ show(test (o1) (o2)) ++ \";;expected=\" ++ show(o1) ++ \";;received=\" ++ show(o2))
    where
        o1 = Model.${testData}
        o2 = Student.${testData}
"""

    syntaxCheckTemplate = \
"""module Main where

${SOURCE}
   
main = putStr(\"\")
"""

    genericModulTemplate= \
"""module %s where
%s
"""

    runhugsRegEx = 'Type checking\n?|Parsing\n?|[Parsing]*[Dependency analysis]*[Type checking]*[Compiling]*\x08 *'

    # input schema
    inputSchema = Schema((

        InputField(
            'modelSolution', 
            required = True, 
            label = 'Model solution',
            description = 'Enter a model solution.',
            i18n_domain = 'EC',
        ),
        
        InputField(
            'helpFunctions', 
            label = 'Help functions',
            description = 'Enter help functions if needed.',
            i18n_domain = 'EC',
        ),

        RepeatField(
            'testData', 
            #accessor = # must return a list; default is one element per line
            required = True, 
            label = 'Test data',
            description = 'Enter one or more function calls. '+ 
                        'A function call consists of the ' + 
                        'function name (given in the exercise directives) ' + 
                        'and test data as parameters of this funtion. '+
                        'Each function call must be written in a single line.',
            i18n_domain = 'EC',
        ),
    ))

    # testSchema
    tests = Schema((

        TestEnvironment(
            'simple',
            label = 'Simple',
            description = 'Exact matching results are allowed.',
            test = simpleTest,
            syntax = syntaxCheckTemplate,
            semantic = wrapperTemplate,
            lineNumberOffset = 2,
            compiler = interpreter,
            interpreter = interpreter,
        ),

        TestEnvironment(
            'permutation',
            label = 'Permutation',
            description = 'Permutations are allowed.',
            test = permTest,
            syntax = syntaxCheckTemplate,
            semantic = wrapperTemplate,
            lineNumberOffset = 2,
            compiler = interpreter,
            interpreter = interpreter,
        ),

        TestEnvironment(
            'tolerance',
            label = 'Tolerance',
            description = 'Tolerance of 1/10^15 in result values allowed.',
            test = toleranceTest,
            syntax = syntaxCheckTemplate,
            semantic = wrapperTemplate,
            lineNumberOffset = 2,
            compiler = interpreter,
            interpreter = interpreter,
        ),
    ))



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
    tests['tolerance'].semantic = wrapperTemplate

