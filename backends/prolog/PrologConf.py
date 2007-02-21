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
    
    # load Prolog functions to do tests

    # try:
    #     simpleTest = file(join(dirname(__file__), 'simpleTest.pl'), 'r').read()
    # except IOError, ioe:
    #     raise ioe
    #     simpleTest = ''
    # This code *should* throw up if 'simpleTest.pl' cannot be loaded.
    simpleTest = file(join(dirname(__file__), 'simpleTest.pl'), 'r').read()
    permTest   = file(join(dirname(__file__), 'permTest.pl'),   'r').read()
    predTest   = file(join(dirname(__file__), 'predTest.pl'),   'r').read()

    wrapperTemplate = \
""":- use_module('%s').
:- use_module('%s').

join([],    _, '').
join([X],   _, O) :- swritef(O, '%%w', [X]).
join([X|R], I, O) :- join(R, I, JR), swritef(O, '%%w%%w%%w', [X, I, JR]).

format_res1_sub([], [], []).
format_res1_sub([N|Rn], [V|Rv], [O|Ro]) :- format_res1_sub(Rn, Rv, Ro),
	swritef(O, '%%w <- %%w', [N, V]).
format_res1([], 'Yes').
format_res1(V, O) :- format_res1_sub([${strTestVarNames}], V, A),
	join(A, ', ', B),
	swritef(O, '{%%w}', [B]).

format_res([],   'No').
format_res(I, O) :- maplist(format_res1, I, T), join(T, ' or ', O).

first_solution_or_nil(Pred, []) :- \+ call(Pred, _).
first_solution_or_nil(Pred, [X]) :- call(Pred, X).

${helpFunctions}

model([${testVarNames}])   :- %s:${testData}
student([${testVarNames}]) :- %s:${testData}

display_res(Model_results, Student_results, Equal) :-
	format_res(Model_results,   FMs),
	format_res(Student_results, FSs), 
        writef('isEqual=%%w;;expected=%%w;;received=%%w', [Equal, FMs, FSs]).

${testFunction}
""" % (NS_MODEL, NS_STUDENT, NS_MODEL, NS_STUDENT,)

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

        TestEnvironment(
            'permutation',
            label = 'Permutation',
            description = 'Test with permutations',
            test = permTest,
            semantic = wrapperTemplate,
            lineNumberOffset = 0,
            compiler = compiler,
            interpreter = interpreter,
        ),

        TestEnvironment(
            'predicate',
            label = 'Predicate',
            description = "Test the student's solution with the goal pred",
            test = predTest,
            semantic = wrapperTemplate,
            lineNumberOffset = 0,
            compiler = compiler,
            interpreter = interpreter,
        ),
    ))
