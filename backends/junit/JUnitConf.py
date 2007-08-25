import os, re
from os.path import join, dirname

from lib.util.BackendSchema import InputField, RepeatField
from lib.util.BackendSchema import Schema
from lib.util.BackendSchema import TestEnvironment


class JUnitConf:
    """
    Properties used by backend JUnit.
    """

    compiler = join(dirname(__file__), 'javac')
    interpreter = join(dirname(__file__), 'java')
    #interpreter = join(dirname(__file__), 'java+systrace')

    # The packages that the model and student solution will be put in
    NS_STUDENT = 'studentPackage'
    
    # The name of the wrapper class that performs the semantic check
    CLASS_SEMANTIC_CHECK = 'JUnitTester'
    CLASS_METHOD_CHECK = 'MethodExistingTester'

    
    #Regular expressions to extract certain information
    CLASS_NAME_RE = re.compile('public\s+class\s+(?P<className>[A-Z]\w*)(\<\w*\>)?\s*\{')
    CONSTRUCTOR_ATTRIBUTES_RE = re.compile('public\s+(?P<consName>[A-Z]\w*)\s*\((?P<attr>.*)\)')
    PACKAGE_NAME_RE = re.compile('package\s+(?P<packageName>[a-z]+\w*);')
    
    #library directory:
    JUNIT_LIBS = 'junit_libs'
    
    #Library content
    LIBRARIES = os.listdir(join(dirname(__file__),JUNIT_LIBS))
    
    AUTO_COMMENT = '//auto//'

    methodTemplate = \
r'''
import java.lang.reflect.*;

import %s.*;
//Einreichung importieren!

public class %s {
    public static void main(String[] args) {
        //retrieve method signatures and store them
        Method[] methods = ${CLASS}.class.getDeclaredMethods();
        String[] signatures = new String[methods.length];
        
        for(int i = 0; i < methods.length; i++){
            signatures[i] = methods[i].getName();
            Class[] classes = methods[i].getParameterTypes();
            for(int j = 0; j < classes.length; j++){
                signatures[i] += " " + classes[j].getSimpleName();
            }
        }
        
        //compare to specified methods:
        boolean found = false;
        
        for(String incomming_method: args){
            for(String signature: signatures){
                if(incomming_method.equals(signature)){
                    //found method signature!
                    found = true;
                }
            }
            if(!found){
                String[] splitter = incomming_method.split(" ");
                String name = splitter[0];
                String parameters = "";
                if(splitter.length != 1){
                    parameters = incomming_method.replace(name+" ", "");
                }
                System.out.println("Could not find method signature \""+name+"("+parameters+")\".");
                System.exit(1);
                return;
            }else{
                //search next signature
                found = false;
            }
        }
    }
}
''' % (NS_STUDENT, CLASS_METHOD_CHECK)
    
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

    # input schema
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
        
        RepeatField(
            'methods',
            required = True,
            label = 'Methods',
            description = "Enter the method names followed by parameters you are going to test. "\
            'Write each method name in a single line.',
            i18n_domain = 'EC',
        ),

        InputField(
            'unitTests', 
            required = True, 
            label = 'Unit tests',
            description = 'Enter one or more JUnit tests. '\
            'If you have to reference the class of the submission '\
            'use variable ${CLASS}. [e.g. ${CLASS} myClass = new ${CLASS}(); or '\
            '${CLASS}.method()]',
            i18n_domain = 'EC',
        ),
))

    # testSchema
    tests = Schema((

        TestEnvironment(
            'JUnitTest',
            label = 'JUnitTests',
            description = 'Run JUnit tests',
            test = methodTemplate,
            semantic = wrapperTemplate,
            compiler = compiler,
            interpreter = interpreter,
        ),
    ))
