import os, re
from os.path import join, dirname, abspath

from lib.util.BackendSchema import InputField
from lib.util.BackendSchema import Schema
from lib.util.BackendSchema import TestEnvironment


class JUnitConf:
    """
    Properties used by backend JUnit.
    """

    #compiler = join(abspath(dirname(__file__)), 'javac')
    compiler = join(abspath(dirname(__file__)), 'javac.sh')
    #interpreter = join(abspath(dirname(__file__)), 'java+systrace)
    interpreter = join(abspath(dirname(__file__)), 'java.sh')
    #interpreter = join(abspath(dirname(__file__)), 'java+systrace')

    # The packages that the model and student solution will be put in
    NS_STUDENT = 'studentPackage'
    
    # The name of the wrapper class that performs the semantic check
    CLASS_SEMANTIC_CHECK = 'JUnitTester'
    
    # Regular expressions to extract certain information
    CLASS_NAME_RE = re.compile('public\s+class\s+(?P<className>[A-Z]\w*)(\<\w*\>)?\s*\{')
    CONSTRUCTOR_ATTRIBUTES_RE = re.compile('public\s+(?P<consName>[A-Z]\w*)\s*\((?P<attr>.*)\)')
    PACKAGE_NAME_RE = re.compile('package\s+(?P<packageName>[a-z]+\w*);')
    METHOD_NOT_FOUND_RE = re.compile('location:.*\n.*;\n')
    
    # Library directory name:
    JUNIT_LIBS = 'junit_libs'
    
    # absolute path to junit archive
    CURRENT_PATH = abspath(dirname(__file__))
    
    # Library content
    LIBRARIES = os.listdir(join(CURRENT_PATH,JUNIT_LIBS))
    
    # Define classpath settings:
    # Scans JUNIT_LIBS directory and adds all archives to CLASSPATH_SETTINGS
    # CLASSPATH_SETTINGS will be passed to _runInterpreter as options
    paths = CURRENT_PATH
    for lib in LIBRARIES:
        if lib.rfind('.class') == -1:
            paths += ':'+join(CURRENT_PATH,JUNIT_LIBS,lib)
    CLASSPATH_SETTINGS = ['-classpath','.:'+paths]

    # Wrapper Template used to test the submission
    wrapperTemplate = \
r'''
//imports for Unit testing
import org.junit.Test;
import static org.junit.Assert.*;
import org.junit.*;
import org.junit.internal.ArrayComparisonFailure;
import org.junit.runner.JUnitCore;
import org.junit.runner.Result;

//organize teacher's additional imports
${imports}

//import submission:
import %s.*;

public class %s {
    private static final int NO_FAILURES = 0;
    
    //teacher's help functions:
    ${helpFunctions}
    
    //teacher's unit tests:
    ${unitTests}
    
    public static void main(String[] args) {
        Result result = JUnitCore.runClasses(%s.class);
        
        if(result.getFailureCount() == NO_FAILURES){
            System.out.println("No Errors occured");
            System.exit(0);
        }else{
            String failure = result.getFailures().get(0).toString();
            System.out.println(failure);
            System.exit(1);
        }
    }
}
''' % (NS_STUDENT, CLASS_SEMANTIC_CHECK, CLASS_SEMANTIC_CHECK)

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
            'JUnitTest',
            label = 'JUnitTests',
            description = 'Run JUnit tests',
            semantic = wrapperTemplate,
            compiler = compiler,
            interpreter = interpreter,
        ),
    ))
