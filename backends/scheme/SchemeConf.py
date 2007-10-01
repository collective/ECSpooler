# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.

import os, sys, logging 
from os.path import join, dirname, abspath

from lib.util.BackendSchema import InputField
from lib.util.BackendSchema import RepeatField
from lib.util.BackendSchema import Schema
from lib.util.BackendSchema import TestEnvironment

class SchemeConf:
    """
    Defines all properties used by backend Scheme.
    """

    interpreter = join(abspath(dirname(__file__)), 'mzscheme+systrace')
    #interpreter = join(abspath(dirname(__file__)), 'mzscheme.sh')
    
    streamLib = 'streams.scm'

    # construct path for stream lib
    streamLibPath = join(abspath(dirname(__file__)), 'scheme_libs', streamLib)

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
""";; import SICP streams
(require (file "%s")) 



;; -------------------------------------------------------------------
${SOURCE}
;; -------------------------------------------------------------------
""" % streamLibPath

    semanticCheckTemplate = \
"""(module ${MODULE} mzscheme
(require (file "%s")) ;; import SICP streams

(define (main) (${testData}))

;; -------------------------------------------------------------------
${SOURCE}
;; -------------------------------------------------------------------

;; helper functions
${helpFunctions}

(provide main))
""" % streamLibPath

    wrapperTemplate = \
""";; required modules
(require (prefix model. (file "model.scm")))
(require (prefix student. (file "student.scm")))

;; test function
${testFunction}

;; print test results
(let-values (((ms ss) (values (model.main) (student.main))))
  (printf "isEqual=~s;;expected=~s;;received=~s" (test ms ss) ms ss))
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
                        'and test data as parameters of this funtion, e.g, '+
                        "tally 'a '(a b c 1 2 3 b c a)"+
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
            lineNumberOffset = 6,
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
            lineNumberOffset = 6,
            compiler = interpreter,
            interpreter = interpreter,
        ),
    ))

# --Test ----------------------------------------------------------------------
#if __name__ == "__main__":
#    print SchemeConf.syntaxCheckTemplate
