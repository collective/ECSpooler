# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2011 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
#
################################################################################
#                                Changelog                                     #
################################################################################
#
# 04.03.2009, chbauman:
#       NS_STUDENT starts with lowercase letter
#       renewed CLASS_NAME_RE
#       deleted CONSTRUCTOR_ATTRIBUTES_RE -> no longer needed
#       corrected TestEnvironment
# 16.03.2009, chbauman:
#       new RE 'IMPORT_NAME_NOT_JAVA_RE'
#       corrected comments
#       corrected 'CLASSPATH_SETTINGS'
#       some beauty-changes 'in wrapperTemplate', including new error message
# 24.03.2009, chbauman:
#       reverted changes in TestEnvironment
# 06.04.2009, chbauman:
#       new Regular Expression 'FAILURE_TRACE_RE'
#       wrapperTemplate now prints the trace of a Failure
# 07.04.2009, chbauman:
#       extended wrapperTemplate: possibility to exclude certain classes from
#       Failure trace printing
#       shorter, propably more general RE for FAILURE_TRACE_RE
# 28.05.2010, chbauman:
#       Formatted Java code in wrapper template.

import os, re
from os.path import join, dirname, abspath

from lib.util.BackendSchema import InputField
from lib.util.BackendSchema import Schema
from lib.util.BackendSchema import TestEnvironment


# Properties used by backend JUnit.

#compiler = join(abspath(dirname(__file__)), 'javac')
compiler = join(abspath(dirname(__file__)), 'javac.sh')
#interpreter = join(abspath(dirname(__file__)), 'java+systrace')
interpreter = join(abspath(dirname(__file__)), 'java.sh')
 
"""   
trc = os.getenv('EC_TRACE')
comp = 'javac.sh'
if trc == 'bsd': comp = 'javac'
compiler = join(abspath(dirname(__file__)), comp)

intp = 'java.sh'
if trc == 'bsd': intp = 'java+systrace'
interpreter = join(abspath(dirname(__file__)), intp)
"""

# The packages that the model and student solution will be put in
NS_STUDENT = 'studentPackage'

# The name of the wrapper class that performs the semantic check
CLASS_SEMANTIC_CHECK = 'JUnitTester'
    
# Due to a NoSuchMethodException it is possible that all tests are displayed to the student.
# This RE will shorten the output and hide the tests from students.
METHOD_NOT_FOUND_RE = re.compile('location:.*\n.*;\n')
    
# Library directory name:
JUNIT_LIBS = 'junit_libs'

# absolute path to junit dir
CURRENT_PATH = abspath(dirname(__file__))

# Library path
LIBRARY_PATH = join(CURRENT_PATH, JUNIT_LIBS)

# Library content
LIBRARIES = os.listdir(LIBRARY_PATH)

# Platformdependent classpath separator
CLASSPATH_SEPARATOR = ':'
if os.name == 'dos' or os.name == 'nt':
    CLASSPATH_SEPARATOR = ';'
    
# Known archive types
ARCHIVES = ['jar', 'zip']
    
# Define classpath settings:
# Scans JUNIT_LIBS directory and adds all archives to CLASSPATH_SETTINGS
# CLASSPATH_SETTINGS will be passed to _runInterpreter as options
paths = CURRENT_PATH + CLASSPATH_SEPARATOR + LIBRARY_PATH
for lib in LIBRARIES:
    libExtension = lib.split('.')[-1]
    if libExtension in ARCHIVES:
        paths += CLASSPATH_SEPARATOR + join(LIBRARY_PATH, lib)
CLASSPATH_SETTINGS = ['-classpath', '.' + CLASSPATH_SEPARATOR + paths]

# Messages displayed to the student
FAILED_TESTS_MESSAGE = 'Your submission failed.'
PASSED_ALL_TESTS_MESSAGE = 'Your submission passed all tests.'

# Wrapper Template used to test the submission
wrapperTemplate = \
'''import java.util.ArrayList;
import java.util.List;

//imports for Unit testing
import org.junit.Test;
import static org.junit.Assert.*;
import org.junit.*;
import org.junit.internal.ArrayComparisonFailure;
import org.junit.runner.JUnitCore;
import org.junit.runner.Result;
import org.junit.runner.notification.Failure;

//organize teacher's additional imports
${imports}

//import submission:
import %s.*;

public class %s {
    private static final int NO_FAILURES = 0;
    
    // Used to filter out traces:
    private static List<Class> excludedTraceClasses = new ArrayList<Class>();
    
    // Initialize excludedTraceClasses via static constructor.
    // Insert Throwables, whose traces are undesired to output:
    static{
    	excludedTraceClasses.add(AssertionError.class);
    	excludedTraceClasses.add(ComparisonFailure.class);
    }
    
//teacher's help functions:
${helpFunctions}
    
//teacher's unit tests:
${unitTests}
    
    public static void main(String[] args) {
        Result result = JUnitCore.runClasses(%s.class);
        
        if(result.getFailureCount() == NO_FAILURES){
            System.out.println("%s");
            System.exit(0);
        }else{
        	  Failure failure = result.getFailures().get(0);
            System.out.println(failure.toString());
            
            // Filter out AssertionErrors and ComparisonFailures,
            // print all traces of other exceptions:
            Throwable throwable = failure.getException();
            if(!excludedTraceClasses.contains(throwable.getClass())){
                System.out.println(failure.getTrace());
            }
            
            System.exit(1);
        }
    }
}
''' % (NS_STUDENT, CLASS_SEMANTIC_CHECK, CLASS_SEMANTIC_CHECK, PASSED_ALL_TESTS_MESSAGE)

# Input Schema
inputSchema = Schema((

    InputField(
        'imports', 
        label = 'Imports',
        description = 'Enter additional imports, like java '\
        'libraries.',
        i18n_domain = 'EC',
    ),
    
    InputField(
        'helpFunctions', 
        label = 'Help functions',
        description = 'Enter help functions if needed.',
        i18n_domain = 'EC',
    ),

    InputField(
        'unitTests', 
        required = True, 
        label = 'Unit tests',
        description = 'Enter one or more JUnit tests. '\
        'If you have to reference the class of the submission '\
        'use variable "${CLASS}". [e.g. ${CLASS} myClass = new ${CLASS}(); or '\
        '${CLASS}.method()]',
        i18n_domain = 'EC',
    ),
))

# Test Schema
tests = Schema((

    TestEnvironment(
        'default',
        label = 'JUnitTests',
        description = 'Run JUnit tests',
        semantic = wrapperTemplate,
        compiler = compiler,
        interpreter = interpreter,
    ),
))
