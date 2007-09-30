import sys, os, threading, signal, re
import logging

from os.path import dirname

from lib.data.BackendResult import BackendResult
from lib.AbstractProgrammingBackend import AbstractProgrammingBackend, EX_OK

#Import Config-File
from JUnitConf import JUnitConf

    
    
class JUnit(AbstractProgrammingBackend):
    """
    Backend class that determines whether a submission of java code 
    returns expected values  which are defined in JUnit tests.
    """
    
    id = 'junit'
    name = 'JUnit'
    version = '1.0'
    schema = JUnitConf.inputSchema
    testSchema = JUnitConf.tests
    srcFileSuffix = '.java'
    
    
    
#--------  Methods for modifying incomming source code  ------------------------
    def grantEmptyConstructor(self,source):
        """
        Guarantees that source has an empty constructor.
        
        @param source: Java source code.
        """
        matcher = JUnitConf.CONSTRUCTOR_ATTRIBUTES_RE.search(source)
        
        if matcher is not None:
            attr = matcher.group('attr')
            if attr is not None:
                #add empty constructor
                pos = source.rfind('}')
                className = self.getClassName(source)
                source = source[:pos] + '\n\tpublic %s(){}\n}' % className
                
        return source
        
        
        
    def removeStaticDeclarations(self,source):
        """
        Removes all "static" declarations by surrounding it with comments.
        
        @param source: Java source code.
        @return: source without static methods.
        """
        return re.sub('[^a-z,A-Z,0-9]\s?static\s+',' /*static*/ ',source);
        
        
        
    def commentMainMethod(self,source):
        """
        Places a java comment around the main method.
        
        @param source: Java source code.
        @return: source with commented main method.
        """
        mainMethod_RE = re.compile('(?P<mainMethod>public\s+static\s+void\s+main\s*\(\w*\s*\[\]\s*\w*\))')
        lines=source.split('\n')
        comment = JUnitConf.AUTO_COMMENT
        count = 0
        main = False
        find = True
        return_value = ""
    
        for line in lines:
            if find:
                search = mainMethod_RE.search(line)
                if search is not None or main:
                    #found main method
                    line = comment+line
                    main=True
            
                    obracket = re.findall('{',line)
                    cbracket = re.findall('}',line)
            
                    count = count + len(obracket)
                    count = count - len(cbracket)
    
                    if count == 0:
                        main = False
                        find = False
        
            return_value = return_value + "\n" + line
        
        return return_value
        
        
        
        
    def getClassName(self,source):
        """
        Returns the class name of a given java source.
        
        @param source: Java source code.
        @return: Class name of given source code.
        """
        matcher = JUnitConf.CLASS_NAME_RE.search(source)
        
        assert matcher is not None,\
        'Name of the public class could not be extracted from source\n\n%s' % repr(source)
        
        return matcher.group('className')
        
        
        
    def replaceVariableCLASS(self,source,className):
        """
        Replaces all Variables ${CLASS} with the class name of a given java source.
        
        @param source: Java source code.
        @param className: Class name that ${CLASS} will be substituted with.
        @return: source with substituted ${CLASS}.
        """
        return source.replace('${CLASS}',className)
        #return re.sub('\$\{CLASS\}',className,source)
            
            
            
    def grantValidPackage(self,source):
        """
        Determines whether source already has a package declaration.
        If yes, it will be overwritten with a new declaration.
        If not, a new package declaration will be written.
        
        @param source: Java source code.
        @return: source with valid package declaration.
        """
        matcher = JUnitConf.PACKAGE_NAME_RE.search(source)
        
        if matcher is not None:
            #source has a package declaration -> replace it!
            return re.sub('package\s+.*;',
                'package %s;' % JUnitConf.NS_STUDENT,
                source)
        else:
            tmp_result = 'package %s;\n\n' % JUnitConf.NS_STUDENT
            return tmp_result + source
            
            
            
    def handleStudentsImports(self,source):
        """
        Student's imports could be located in a package that will not be found on server-side.
        This method scans a given java source and searches for imports that are located in a package which is not "java".
        If import packages are found they will be renamed into the JUnitConf.JUNIT_LIBS package.
        
        @param source: Java source code.
        @return: source with valid import declarations.
        """
        importsArray = re.findall('import\s+(?!java\.)(?P<name>.*);', source)
        libraries = JUnitConf.LIBRARIES
        
        for imports in importsArray:
            for libs in libraries:
                lib_name = libs.split('.')[0]
                if lib_name in imports:
                    start = imports.find(lib_name)
                    source = source.replace(imports, imports[start:], 1)
                    #if there are more files with the same filename:
                    break
                    
        return source
            
            
            
#--------  Syntax methods that have to be overwritten  -------------------------
    def _preProcessCheckSyntax(self,test,src,**kwargs):
        noMainMethod = self.commentMainMethod(src)
        #logging.debug(noMainMethod)
        noStaticDeclaration = self.removeStaticDeclarations(noMainMethod)
        #logging.debug(noStaticDeclaration)
        validPackages = self.grantValidPackage(noStaticDeclaration)
        #logging.debug(validPackages)
        emptyConstructor = self.grantEmptyConstructor(validPackages)
        #logging.debug(emptyConstructor)
        preProcessedSource = self.handleStudentsImports(emptyConstructor)
        #logging.debug(preProcessedSource)
        className = self.getClassName(src)
        
        return preProcessedSource,className
        
        
        
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
                    
                #guarantee that the submission will be put in folder NS_STUDENT
                folder = jobId+"/"+JUnitConf.NS_STUDENT
                
                module = self._writeModule(
                    mName,
                    src, 
                    self.srcFileSuffix,
                    folder,
                    testSpec.encoding)
                
                #logging.debug(repr(module))
                
                exitcode, result = \
                    self._runInterpreter(
                        compiler,
                        os.path.dirname(module['file']),
                        os.path.basename(module['file']))
                    
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
    def _process_checkSemantics(self,job):
        """
        Checks the semantics of a program.
        
        @param jobId: ID for this job
        @return: a BackendResult.
        """
        inputFields = self.schema.filterFields(type='InputField')
        
        #variable declaration
        exitcode=-999
        
        #Test if an InputField exists
        assert inputFields, 'No InputFields found!'
        
        #get submission
        submission = job['submission']
        
        assert submission is not None,\
            'Semantic check requires a valid submission:\n\n%s' % repr(submission)
            
        submissionClassName = self.getClassName(submission)
            
        tests = self._getTests(job)
        if len(tests) == 0:
            message = 'No test specification selected.'
            logging.warn('%s, %s' % (message, job.getId()))
            return BackendResult(-217,message)
        test = tests[0]
        
        #get compiler
        compiler = test.compiler
        
        #get interpreter
        interpreter = test.interpreter

        #get templates
        wrapper_code = test.semantic
            
        #-----------  compile and run Wrapper Template  ------------------------        
        #replace all variables in wrapper template
        for field in self.schema.filterFields(type='InputField'):
            field_text = job[field.getName()]
            
            #empty fields should cause that no text is written
            if field_text is None:
                field_text = ""
                
            wrapper_code = re.sub('\$\{%s\}' % field.getName(),
                field_text,
                wrapper_code)
                
        wrapper_code = self.replaceVariableCLASS(wrapper_code,submissionClassName)
        
        try:
            wrapperModule = self._writeModule(
                JUnitConf.CLASS_SEMANTIC_CHECK,
                wrapper_code,
                suffix=self.srcFileSuffix,
                dir=job.getId(),
                encoding=test.encoding)
                
            print "compiler"
            exitcode, result = self._runInterpreter(
                compiler,
                os.path.dirname(wrapperModule['file']),
                os.path.basename(wrapperModule['file']))
                
            print "assert"
            assert exitcode == EX_OK,\
                'Error in wrapper code during semantic check:\n\n%s' % result
                
            print "interpreter"
            exitcode, result = self._runInterpreter(
                interpreter,
                os.path.dirname(wrapperModule['file']),
                JUnitConf.CLASS_SEMANTIC_CHECK)
                
        except Exception, e:
            message = 'Internal error during semantic check: %s: %s' % \
                (sys.exc_info()[0], e)
                
            logging.error(message)
            
            print "---------------<<<-"
            print message
            print "-------------->>>---"
            print ""
            
            msg = re.sub(JUnitConf.METHOD_NOT_FOUND_RE,"",message)
            
            print "---------<<<------"
            print msg
            print "------------->>>-----"
            print ""
            
            return BackendResult(-230,msg)
        
        if exitcode != EX_OK:
            result = "\nYour submission failed. Test " \
            "case was: '%s' (%s)" \
            "\n\n Received result: %s"\
            % (result.split(" ")[0], test.getName(), result)

            return BackendResult(False, result)
        
        
        else:
            return BackendResult(True,'\nYour submission passed all tests.')
            
        
        
        
        

        

        
        
        
        
        