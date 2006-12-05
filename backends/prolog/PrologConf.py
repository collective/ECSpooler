# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.
import os, re
from os.path import join, dirname

from lib.util.BackendSchema import InputField
from lib.util.BackendSchema import RepeatField
from lib.util.BackendSchema import Schema
from lib.util.BackendSchema import TestEnvironment


class PrologConf:
    """
    Properties used by backend Prolog.
    """

    interpreter = join(dirname(__file__), 'pl+systrace')
    compiler = interpreter

    # The packages that the model and student solution will be put in
    NS_MODEL   = 'model'
    NS_STUDENT = 'student'

    # The name of the wrapper class that performs the semantic check
    CLASS_SEMANTIC_CHECK = 'SemanticCheck'

    VAR_NAME_RE = r'\b[_A-Z][A-Za-z0-9_]*\b'
    
    # load Prolog function to do a simple test

    # try:
    #     simpleTest = file(join(dirname(__file__), 'simpleTest.pl'), 'r').read()
    # except IOError, ioe:
    #     raise ioe
    #     simpleTest = ''
    # This code *should* throw up if 'simpleTest.pl' cannot be loaded.
    simpleTest = file(join(dirname(__file__), 'simpleTest.pl'), 'r').read()

    wrapperTemplate = (
""":- use_module('%s').
:- use_module('%s').

${helpFunctions}

model([${testVarNames}])   :- %s:${testData}
student([${testVarNames}]) :- %s:${testData}

${testFunction}
"""  % (NS_MODEL, NS_STUDENT, NS_MODEL, NS_STUDENT,)) + \
"""
:- test(E), findall(X, model(X), Ms), findall(X, student(X), Ss),
     writef('isEqual=%w;;expected=%w;;received=%w', [E, Ms, Ss]).
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
            description = 'Test without permutations',
            test = simpleTest,
            semantic = wrapperTemplate,
            lineNumberOffset = 0,
            compiler = compiler,
            interpreter = interpreter,
        ),

#         TestEnvironment(
#             'permutation',
#             label = 'Permutation',
#             description = 'Test with permutations',
#             test = permTest,
#             semantic = wrapperTemplate,
#             lineNumberOffset = 0,
#             compiler = compiler,
#             interpreter = interpreter,
#         ),
    ))
