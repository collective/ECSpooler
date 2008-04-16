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

class ClConf:
    """
    Defines all properties used by backend Cl.
    """

    interpreter = join(abspath(dirname(__file__)), 'sbcl+systrace')
    #interpreter = join(abspath(dirname(__file__)), 'sbcl.sh')

    # The placeholder for the names of the packages that the model and
    # student solution will be put in
    NS_MODEL = 'modelPackage'
    NS_STUDENT = 'studentPackage'
    
    # load Cl function to do a simple test
    try:
        simpleTest = file(join(dirname(__file__), 'simpleTest.lisp'), 'r').read()
    except IOError, ioe:
        logging.warn('%s: %s' % (sys.exc_info()[0], ioe))
        simpleTest = ''

    # load Cl function to do a test which allows permutation of list elems
#     try:
#         permTest = file(join(dirname(__file__), 'permTest.lisp'), 'r').read()
#     except IOError, ioe:
#         logging.warn('%s: %s' % (sys.exc_info()[0], ioe))
#         permTest = ''
        
    
    syntaxCheckTemplate = \
"""
;; -------------------------------------------------------------------
${SOURCE}
;; -------------------------------------------------------------------
"""

    semanticCheckTemplate = \
"""
;; helper functions
${helpFunctions}

;; -------------------------------------------------------------------
${SOURCE}
;; -------------------------------------------------------------------

(defun main ()
  (progn
    ${testData}))
"""

    wrapperTemplate = \
'''
;; load the model and the student packages
(load "${%s}.lisp") ; student package
(defparameter ${%s} (main))

(load "${%s}.lisp") ; model package
(defparameter ${%s} (main))

;; test function
${testFunction}

;; print test results
(format t "isEqual=~a;;expected=~a;;received=~a~%%"
        (test ${%s} ${%s}) ; model, student
        ${%s}  ; model
        ${%s}) ; student
''' % (NS_STUDENT, NS_STUDENT,
       NS_MODEL, NS_MODEL,
       NS_MODEL, NS_STUDENT,   # form (test ...)
       NS_MODEL,
       NS_STUDENT,
       )

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
            label = 'Helper functions',
            description = 'Enter helper functions if needed.',
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

#         TestEnvironment(
#             'permutation',
#             label = 'Permutation',
#             description = 'Permutations are allowed.',
#             test = permTest,
#             syntax = syntaxCheckTemplate,
#             semantic = wrapperTemplate,
#             lineNumberOffset = 6,
#             compiler = interpreter,
#             interpreter = interpreter,
#         ),
    ))

# --Test ----------------------------------------------------------------------
#if __name__ == "__main__":
#    print ClConf.syntaxCheckTemplate
