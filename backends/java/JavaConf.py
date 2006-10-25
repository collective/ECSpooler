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


class JavaConf:
    """
    Properties used by backend Java.
    """

    compiler = '/usr/pkg/bin/javac'
    interpreter = '/opt/ECSpooler/backends/java/java+systrace'

    # The packages that the model and student solution will be put in
    NS_MODEL = 'ModelPackage'
    NS_STUDENT = 'StudentPackage'
    
    # The name of the wrapper class that performs the semantic check
    CLASS_SEMANTIC_CHECK = 'SemanticCheck'

    # The regular expression to extract the name of the *public* class
    # from a piece of source.
    #
    # The grammar at <http://cobase-www.cs.ucla.edu/pub/javacc/java1.4.jj>
    # says this:
    #
    # void ClassDeclaration() :
    #     {}
    #     {
    #         ( "abstract" | "final" | "public" | "strictfp")*
    #         UnmodifiedClassDeclaration()
    #     }
    #
    # void UnmodifiedClassDeclaration() :
    #     {}
    #     {
    #         "class" <IDENTIFIER> [ "extends" Name() ]
    #                              [ "implements" NameList() ]
    #         ClassBody()
    #     }
    CLASS_NAME_RE = re.compile(r'^\s*(?:public\s+(?:(?:abstract|final|public|strictfp)\s+)*)+\s*class\s+(?P<name>[a-zA-Z_]\w*)', re.MULTILINE)

    # load Java function to do a simple test
    try:
        simpleTest = file(join(dirname(__file__), 'simpleTest.java'), 'r').read()
    except IOError:
        simpleTest = ''

    # load Java function to do a test which allows permutation of list elems
    #try:
    #    permTest = file('permTest.hs', 'r').read()
    #except IOError:
    #    permTest = ''


    wrapperTemplate = \
'''
public class %s
{
    ${helpFunctions}

    ${testFunction}
    
    public static void main(String[] argv)
    {
        Object  exp = ${model_testData};
        Object  rec = ${student_testData};
        Boolean eql = new Boolean(test(exp, rec));
        
        System.out.println("isEqual=" + eql.toString()
                           + ";;expected=" + exp.toString()
                           + ";;received=" + rec.toString());
    }
}
''' % (CLASS_SEMANTIC_CHECK)

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
            lineNumberOffset = 2,
            compiler = compiler,
            interpreter = interpreter,
        ),

#         TestEnvironment(
#             'permutation',
#             label = 'Permutation',
#             description = 'Test with permutations',
#             test = permTest,
#             semantic = wrapperTemplate,
#             lineNumberOffset = 2,
#             compiler = compiler,
#             interpreter = interpreter,
#         ),
    ))
