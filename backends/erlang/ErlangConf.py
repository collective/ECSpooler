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

from config import I18N_DOMAIN

class ErlangConf:
    """
    Defines all properties used by backend Erlang.
    """

    compiler = '/opt/erlang/bin/erlc'
    #interpreter = '/opt/erlang/bin/erl -noshell -s %s -s init stop'
    #                              erl -noshell -s  -s erlang halt
    interpreter = '/opt/ECSpooler/backends/erlang/erl+systrace'

    # load Haskell function to do a simple test
    try:
        simpleTest = file(join(dirname(__file__), 'simpleTest.erl'), 'r').read()
    except IOError, ioe:
        logging.warn('%s: %s' % (sys.exc_info()[0], ioe))
        simpleTest = ''

    # load Haskell function to do a test which allows permutation of list elems
    try:
        permTest = file(join(dirname(__file__), 'permTest.erl'), 'r').read()
    except IOError, ioe:
        logging.warn('%s: %s' % (sys.exc_info()[0], ioe))
        permTest = ''

    # wrapper code (TEMPLATE_SEMANTIC)
    wrapperTemplate = \
"""-module(wrapper).
-export([start/0]).

${helpFunctions}

${testFunction}

o1() -> model:${testData}.
o2() -> student:${testData}.

start() -> io:fwrite("isEqual=~w;;expected=~w;;received=~w", [test(o1(), o2()), o1(), o2()]).
"""

    # input schema
    inputSchema = Schema((

        InputField(
            'modelSolution', 
            required = True, 
            label = 'Model solution',
            description = 'Enter a model solution.',
            i18n_domain = I18N_DOMAIN,
        ),
        
        InputField(
            'helpFunctions', 
            label = 'Help functions',
            description = 'Enter help functions if needed.',
            i18n_domain = I18N_DOMAIN,
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
            i18n_domain = I18N_DOMAIN,
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
            #lineNumberOffset = 2,
            compiler = compiler,
            interpreter = interpreter,
        ),

        TestEnvironment(
            'permutation',
            label = 'Permutation',
            description = 'Test with permutations',
            test = permTest,
            semantic = wrapperTemplate,
            #lineNumberOffset = 2,
            compiler = compiler,
            interpreter = interpreter,
        ),
    ))
