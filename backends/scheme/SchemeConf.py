# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.

import sys, logging
from os.path import join, dirname

from lib.util.BackendSchema import InputField
from lib.util.BackendSchema import RepeatField
from lib.util.BackendSchema import Schema
from lib.util.BackendSchema import TestEnvironment

class SchemeConf:
    """
    Defines all properties used by backend Scheme.
    """

    interpreter = '/opt/ECSpooler/backends/scheme/mzscheme+systrace'

    # load Scheme function to do a simple test
    try:
        simpleTest = file(join(dirname(__file__), 'simpleTest.scm'), 'r').read()
    except IOError, ioe:
        logging.warn('%s: %s' % (sys.exc_info()[0], ioe))
        simpleTest = ''

    # load Haskell function to do a test which allows permutation of list elems
    try:
        permTest = file(join(dirname(__file__), 'permTest.scm'), 'r').read()
    except IOError, ioe:
        logging.warn('%s: %s' % (sys.exc_info()[0], ioe))
        permTest = ''
        
    
    syntaxCheckTemplate = \
""";; SRFI-40
(require (lib "40.ss" "srfi"))

${SOURCE}
"""

    wrapperTemplate = \
""";;  model solution
(module model mzscheme 
(require (lib "40.ss" "srfi"))
${modelSolution}
(provide (all-defined)))

;; student's solution
(module student mzscheme
(require (lib "40.ss" "srfi"))
${studentSolution} 
(provide (all-defined)))

;; required modules
(require (prefix model. model))
(require (prefix student. student))

;; helper functions
${helpFunctions}

;; test function
${testFunction}

;;  main function
(let-values (((ms ss) (values (model.${testData}) (student.${testData}))))
  (printf "isEqual=~s;;expected=~s;;received=~s" (test ms ss) ss ms))
"""

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
            lineNumberOffset = 3,
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
            lineNumberOffset = 3,
            compiler = interpreter,
            interpreter = interpreter,
        ),
    ))
