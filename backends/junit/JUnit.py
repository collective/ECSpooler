################################################################################
#                                Changelog                                     #
################################################################################
#
# 04.03.2009, chbauman:
#       replaced '/' by join() for better platform independency
#       formatted source code
# 16.03.2009, chbauman:
#       worked on comments
#       changed version
#       new function 'getLibInImportPos'
#       outhoused RE for importsArray to JUnitConf.py
#       improved 'handleStudentsImports'
#       improved error message
#       removed useless comments
# 17.03.2009, chbauman:
#       import junit_libs.*, if no other import is declared
# 30.03.2009, chbauman:
#       insertion of imports in handleStudentsImports causes increase of line_offset
# 06.04.2009, chbauman:
#       implemented _postProcessCheckSemantics
# 07.04.2009, chbauman:
#       added some comments
#       all post processors delete string JUnitConf.NS_STUDENT from messages now.
# 30.04.2009, chbauman:
#       replaced re.sub whenever possible
# 12.07.2009, amelung:
#       renamed JUnitConf to config; mved some settings from config to this file

import sys, os, re
import logging

from os.path import join

from lib.data.BackendResult import BackendResult
from lib.AbstractProgrammingBackend import AbstractProgrammingBackend, EX_OK

#Import Config-File
from backends.junit import config

# enable logging
log = logging.getLogger('backends.junit')

## Regular expressions to extract certain information
# CLASS_NAME_RE consists of ClassModifier? class Identifier Super? Interfaces? ClassBody
# (see http://java.sun.com/docs/books/jls/first_edition/html/8.doc.html#15372 [04.03.2009@09:50])
# We are only interested in the public class
javaClassModifier = 'public' 
# Classnames start with a lowercase letter followed by some other letters
javaClassName = '[A-Z]\w*'   
# Generics are strings surrounded by '<' and '>'.
javaGeneric = '\<\w*\>'      
# An Identifier is a name followed by an optional generic argument
javaIdentifier = '%s(%s)?' % (javaClassName, javaGeneric)
# 'extends' followed by an identifier signals that this class inherits from another class
javaSuper = 'extends\s+%s' % javaIdentifier
# 'implements' followed by a comma-separated list of identifiers signals wich interfaces a class has
javaInterfaces = 'implements\s+(%s)?(\s*,\s*%s)*' % (javaIdentifier, javaIdentifier)
# '{' is sufficient for the (our) body
javaClassBody = '\{'

# Since the class of the name we want to extract definately is public, <ClassModifier> is NOT optional
javaClassDeclaration = '%s\s+class\s+(?P<className>%s)(%s)?\s*(%s)?\s*(%s)?\s*%s' % (javaClassModifier, javaClassName, javaGeneric, javaSuper, javaInterfaces, javaClassBody)
CLASS_NAME_RE = re.compile(javaClassDeclaration)

# Determines the student's chosen package
PACKAGE_NAME_RE = re.compile('package\s+(?P<packageName>[a-z]+\w*);')
    
# Finds all import declarations excluding packages java.*
IMPORT_NAME_NOT_JAVA_RE = re.compile('import\s+(?!java\.)(?P<name>.*);')

# This RE will search for the first two lines of a Failure-Object trace.
# java.lang.ArrayIndexOutOfBoundsException: 2      <- will be matched
# at studentPackage.Matrix.mult(Matrix.java:20)    <- will be matched
# at JUnitTester.multAdd(JUnitTester.java:29)      <- will NOT be matched
#FAILURE_TRACE_RE = re.compile('(\w|\.)+?:\s\d+(\s\t)*?at\s%s\.\w+?\.\w+?\(\w+?\.\w+?:(?P<number>\d+?)\)' % NS_STUDENT)
FAILURE_TRACE_RE = re.compile('.*?%s.*?(?P<number>\d+).*?$' % config.NS_STUDENT, re.M | re.S)

    
class JUnit(AbstractProgrammingBackend):
    """
    Backend class that determines whether a submission of java code 
    returns expected values which are defined in JUnit tests, or not.
    """
    
    id = 'junit'
    name = 'JUnit'
    version = '1.1'

    schema = config.inputSchema
    testSchema = config.tests
    srcFileSuffix = '.java'
    
    # While preprocessing student's submission it may occur that some lines
    # have to be added (like package declarations). In case of failures during
    # checks the feedbacks have to be scanned for line numbers and to be
    # updated (minus line_offset).
    line_offset = 0
    
    
#--------  Methods for modifying incomming source code  ------------------------
    def getClassName(self, source):
        """
        Returns the class name of a given java source.
        
        @param source: Java source code.
        @return: Class name of given source code.
        """
        matcher = CLASS_NAME_RE.search(source)
        
        assert matcher is not None, \
        'Name of the public class could not be extracted from source\n\n%s' % source
        
        return matcher.group('className')
        
        
        
    def replaceVariableCLASS(self, source, className):
        """
        Replaces all Variables ${CLASS} with the class name of a given java source.
        
        @param source: Java source code.
        @param className: Class name that ${CLASS} will be substituted with.
        @return: source with substituted ${CLASS}.
        """
        return source.replace('${CLASS}', className)
            
            
            
    def grantValidPackage(self, source):
        """
        Determines whether source already has a package declaration.
        If yes, it will be overwritten with a new declaration.
        If not, a new package declaration will be written.
        
        @param source: Java source code.
        @return: source with valid package declaration.
        """
        matcher = PACKAGE_NAME_RE.search(source)
        
        if matcher is not None:
            # source has a package declaration -> replace it!
            return re.sub('package\s+.*;',
                'package %s;' % config.NS_STUDENT,
                source)
        else:
            tmp_result = 'package %s;\n\n' % config.NS_STUDENT
            
            # we've inserted two lines of source code:
            self.line_offset += 2
            
            return tmp_result + source
            
    def getLibInImportPos(self, libName, importDecl):
        """
        Searches in importDecl for libName and returns the right-most position.
        Since we are working on Java import declarations, we have to search for libName preceeded by space or a dot and followed by a dot or nothing.
        
        @param libName: Name of library that shall be searched
        @param importDecl: A Java import declaration libName will be searched in.
        @return: right-most position of libName in importDecl or -1 if not found.
        """
        pos = -1
        libInImports = re.search('(?<=(\s|\.))' + libName + '(\.|$)', importDecl)
        
        # if libInImports is None, libName is not in importDecl:
        if libInImports is not None:
            match = libInImports.group()
            # find right-most position:
            pos = importDecl.rfind(match)
        
        return pos
            
    def handleStudentsImports(self, source):
        """
        Student's imports could be located in a package that will not be found on server-side.
        This method scans a given java source and searches for imports that are located in a package which is not "java".
        If import packages are found they will be renamed to the config.JUNIT_LIBS package.
        
        @param source: Java source code.
        @return: source with valid import declarations.
        """
        folder = config.JUNIT_LIBS
        importsArray = IMPORT_NAME_NOT_JAVA_RE.findall(source)
        libraries = config.LIBRARIES

        for libs in libraries:
            libName = libs.split('.')[0]
            libExtension = libs.split('.')[-1]
            for imports in importsArray:
                # find libName in imports.
                pos = self.getLibInImportPos(libName, imports)

                # if libs is in importsArray
                if pos != -1:
                    package = imports[pos:]
                    # if libExtension is an archive, do NOT paste config.JUNIT_LIBS in front of import declaration
                    if libExtension in config.ARCHIVES:
                        source = source.replace(imports, package, 1)
                    else:
                        source = source.replace(imports, folder + '.' + package, 1)
                    break
                    
        # if no imports are present, import junit_libs for safety.
        # classes with no import declarations tend to expect all classes to be in the same folder, why junit_libs should be included:
        if len(importsArray) == 0:
            # since a valid package is already written, we can access it:
            packageDeclaration = 'package %s;' % config.NS_STUDENT
            replacement = packageDeclaration + '\n\n' + 'import %s.*;' % config.JUNIT_LIBS
            source = source.replace(packageDeclaration, replacement, 1)
            
            # by adding the new import declaration, line_offset increases by 2:
            self.line_offset += 2
            
        return source
            
            
            
#--------  Syntax methods that have to be overwritten  -------------------------
    def _preProcessCheckSyntax(self, test, src, **kwargs):
        # at the very beginning of our syntax check we set line_offset
        # to 0. Setting it to 0 in _postProcessCheckSyntax would lead to an
        # accumulation of offsets if a sumbission is syntactically incorrect.
        self.line_offset = 0
        
        validPackages = self.grantValidPackage(src)
        #logging.debug(validPackages)
        preProcessedSource = self.handleStudentsImports(validPackages)
        #logging.debug(preProcessedSource)
        className = self.getClassName(src)
        
        return preProcessedSource, className
        
        
    def _postProcessCheckSyntax(self, test, message):
        """
        This method subtracts line_offset from the line numbers the compiler
        returned in its message.
        After that, every occurence of config.NS_STUDENT+'.' will be erased.
        
        @see: AbstractProgrammingBackend._postProcessCheckSyntax
        """
        matches = re.findall('\w+\.\w+:(?P<numbers>\d+):', message)
        
        for match in matches:
            new_line_number = int(match) - self.line_offset
            message = message.replace(match, str(new_line_number), 1)
            
        message = message.replace(config.NS_STUDENT + '.', '')
            
        return message
        
        
        
        
        
    def _process_checkSyntax(self, jobId, testSpec, submission):
        """
        Tests the syntax of a programm.
        
        @param jobId: ID for this test job
        @param test: name of the selected test environment (cf. self.testSchema)  
        @return: a BackendResult or None if test succeeded
        """
        # get the compiler or if not available the interpreter
        compiler = testSpec.compiler or testSpec.interpreter
        
        if compiler:
            try:
                # test term (e.g., student's source code)
                try:
                    src, mName = self._preProcessCheckSyntax(testSpec, submission)
                except AssertionError, ae:
                    return BackendResult(False, str(ae))

                logging.info('Running syntax check with test: %s' % 
                    testSpec.getName())
                    
                # guarantee that the submission will be put in folder NS_STUDENT
                folder = join(jobId, config.NS_STUDENT)
                
                module = self._writeModule(
                    mName,
                    src, 
                    self.srcFileSuffix,
                    folder,
                    testSpec.encoding)
                
                exitcode, result = \
                    self._runInterpreter(
                        compiler,
                        os.path.dirname(module['file']),
                        os.path.basename(module['file']),
                        config.CLASSPATH_SETTINGS)
                    
                logging.debug('exitcode: %s' % repr(exitcode))
                logging.debug('result: %s' % repr(result))
                
            except Exception, e:
                msg = 'Internal error during syntax check: %s: %s' % \
                    (sys.exc_info()[0], e)
                              
                logging.error(msg)
                return BackendResult(-220, msg)
            
            logging.debug('exitcode: %s' % repr(-exitcode))
    
            # consider exit code
            if exitcode != EX_OK:
                result = self._postProcessCheckSyntax(testSpec, result)
                #return BackendResult(-exitcode, result or repr(-exitcode))
                return BackendResult(False, result or repr(-exitcode))

        else:
            msg = 'No compiler/interpreter defined (test spec: %s).' \
                % testSpec.getName()

            logging.error(msg)
            return BackendResult(-221, msg)

        # everything seems to be ok
        return None
        
        
        
#--------  Semantic methods that have to be overwritten  -----------------------
    def _process_checkSemantics(self, job):
        """
        Checks the semantics of a program.
        
        @param jobId: ID for this job
        @return: a BackendResult.
        """
        inputFields = self.schema.filterFields(type = 'InputField')
        
        # variable declaration
        exitcode = -42
        
        # Test if an InputField exists
        assert inputFields, 'No InputFields found!'
        
        # get submission
        submission = job['submission']
        
        assert submission is not None, \
            'Semantic check requires a valid submission:\n\n%s' % repr(submission)
            
        tests = self._getTests(job)
        if len(tests) == 0:
            message = 'No test specification selected.'
            logging.warn('%s, %s' % (message, job.getId()))
            return BackendResult(-217, message)
        test = tests[0]
        
        try:
            submissionClassName = self.getClassName(submission)
        except AssertionError, ae:
            message = str(ae)
            logging.warn('%s, %s' % (message, job.getId()))
            return BackendResult(-230, message)
            

        # get compiler
        compiler = test.compiler
        
        # get interpreter
        interpreter = test.interpreter

        # get template
        wrapper_code = test.semantic
            
        #-----------  compile and run Wrapper Template  ------------------------        
        # replace all variables in wrapper template
        for field in self.schema.filterFields(type = 'InputField'):
            field_text = job[field.getName()]
            
            # empty fields should cause that no text is written
            if field_text is None:
                field_text = ""
                
            #wrapper_code = re.sub('\$\{%s\}' % field.getName(),
            #    field_text,
            #    wrapper_code)
            wrapper_code = wrapper_code.replace('${%s}' % field.getName(), field_text)
                
        wrapper_code = self.replaceVariableCLASS(wrapper_code, submissionClassName)
        
        try:
            wrapperModule = self._writeModule(
                config.CLASS_SEMANTIC_CHECK,
                wrapper_code,
                suffix = self.srcFileSuffix,
                dir = job.getId(),
                encoding = test.encoding)
            
            # compile using javac
            exitcode, result = self._runInterpreter(
                compiler,
                os.path.dirname(wrapperModule['file']),
                os.path.basename(wrapperModule['file']),
                config.CLASSPATH_SETTINGS)
                
            assert exitcode == EX_OK, \
                'Error in wrapper code during semantic check:\n\n%s' % result
            
            # run using java
            exitcode, result = self._runInterpreter(
                interpreter,
                os.path.dirname(wrapperModule['file']),
                config.CLASS_SEMANTIC_CHECK,
                config.CLASSPATH_SETTINGS)
                
        except Exception, e:
            message = 'Internal error during semantic check: %s: %s' % \
                (sys.exc_info()[0], e)
                
            logging.error(message)
            msg = re.sub(config.METHOD_NOT_FOUND_RE, "", message)
            
            return BackendResult(-230, msg)
        
        if exitcode != EX_OK:
            # postprocess the result:
            result = self._postProcessCheckSemantic(test, result)
            result = config.FAILED_TESTS_MESSAGE + '\n\n' + result
            
            return BackendResult(False, result)
        
        
        else:
            #return BackendResult(True, '\nYour submission passed all tests.')
            #return BackendResult(True, result)
            return BackendResult(True, config.PASSED_ALL_TESTS_MESSAGE)
            
            
    def _postProcessCheckSemantic(self, test, message):
        """
        This method is used to post process interpreter messages.
        Two modifications will be performed:
        First, the message will be scanned for a Failure trace. If one exists, 
        the trace is shortened and line_offset subtracted from the returned line
        numbers.
        Second, every occurence of config.NS_STUDENT+'.' will be erased.
        
        @see: AbstractProgrammingBackend._postProcessCheckSemantic
        """
        # scan for Failure trace:
        matcher = FAILURE_TRACE_RE.search(message)
        
        # if the matcher is not None, there exists an Failure trace:
        if matcher is not None:
            match = matcher.group()
            
            # don't forget to subtract line_offset from match's line number
            number = matcher.group('number')            
            message = match.replace(number, str(int(number) - self.line_offset))
            
            # we do not display the whole trace. Show that there was more:
            message = message + '\n\t...'
            
        # erase all occurences of config.NS_STUDENT
        message = message.replace(config.NS_STUDENT + '.', '')
        
        return message
