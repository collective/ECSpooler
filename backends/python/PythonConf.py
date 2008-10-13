# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.
import sys, logging, os
from os.path import join, dirname, abspath

from lib.util.BackendSchema import InputField
from lib.util.BackendSchema import RepeatField
from lib.util.BackendSchema import Schema
from lib.util.BackendSchema import TestEnvironment

class PythonConf:
    """
    Defines all properties used by backend Python.
    """

    trc = os.getenv('EC_TRACE')
    intp = 'python.sh'
    if trc == 'bsd': intp = 'python+systrace'
    interpreter = join(abspath(dirname(__file__)), intp)

    # load Haskell function to do a simple test
    try:
        simpleTest = file(join(dirname(__file__), 'simpleTest.py'), 'r').read()
    except IOError, ioe:
        logging.warn('%s: %s' % (sys.exc_info()[0], ioe))
        simpleTest = ''

    # load Haskell function to do a test which allows permutation of list elems
    try:
        permTest = file(join(dirname(__file__), 'permTest.py'), 'r').read()
    except IOError, ioe:
        logging.warn('%s: %s' % (sys.exc_info()[0], ioe))
        permTest = ''

    wrapperTemplate = \
"""import sys
import Model
import Student

${helpFunctions}

# must be named 'test'
${testFunction}

o1 = Model.${testData}
o2 = Student.${testData}

print >> sys.stdout, \"isEqual=\" + str(test(o1, o2)) + \";;expected=\" + str(o1) + \";;received=\" + str(o2)
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
            description = 'Simple test; only exact result are allowed.',
            test = simpleTest,
            semantic = wrapperTemplate,
            interpreter = interpreter,
        ),

        # FIXME: comment in if permTest.py ist available
        TestEnvironment(
            'permutation',
            label = 'Permutation',
            description = 'Permutations are allowed.',
            test = permTest,
            semantic = wrapperTemplate,
            interpreter = interpreter,
        ),
    ))
