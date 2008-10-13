# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.
import os, re
from os.path import join, dirname, abspath

from lib.util.BackendSchema import InputField
from lib.util.BackendSchema import RepeatField
from lib.util.BackendSchema import Schema
from lib.util.BackendSchema import TestEnvironment


class JavaREConf:
    """
    Properties used by backend Java.
    """

    trc = os.getenv('EC_TRACE')
    comp = 'javac.sh'
    if trc == 'bsd': comp = 'javac'
    compiler = join(abspath(dirname(__file__)), comp)

    intp = 'java.sh'
    if trc == 'bsd': intp = 'java+systrace'
    interpreter = join(abspath(dirname(__file__)), intp)

    # The name of the wrapper class that performs the syntactic check
    CLASS_SYNTACTIC_CHECK = 'SyntacticCheck'

    # The name of the wrapper class that performs the semantic check
    CLASS_SEMANTIC_CHECK = 'SemanticCheck'

    # load Java function to do a simple test
    try:
        findFirstGroup0 = file(join(dirname(__file__),
                               'findFirstGroup0.java'), 'r').read()
    except IOError:
        findFirstGroup0 = ''
    
    wrapperTemplate = file(join(dirname(__file__),
                                CLASS_SEMANTIC_CHECK + '.java'),
                           'r').read()

    syntaxCheckTemplate = file(join(dirname(__file__),
                                    CLASS_SYNTACTIC_CHECK + '.java'),
                               'r').read()
    
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
            'findFirstGroup0',
            label = 'Find first, group 0',
            description = 'Compare group 0 of the first match',
            test = findFirstGroup0,
            semantic = wrapperTemplate,
            syntax = syntaxCheckTemplate,
            lineNumberOffset = 0,
            compiler = compiler,
            interpreter = interpreter,
        ),
    ))
