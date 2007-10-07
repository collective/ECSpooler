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
    
    # While preprocessing student's submission it may occur that some lines
    # have to be added (like package declarations). In case of failures during
    # checks the feedbacks have to be scanned for line numbers and to be
    # updated (minus line_offset).
    line_offset = 0
    
    
#--------  Methods for modifying incomming source code  ------------------------
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
            self.line_offset = 2
            return tmp_result + source
            
            
            
    def handleStudentsImports(self,source):
        """
        Student's imports could be located in a package that will not be found on server-side.
        This method scans a given java source and searches for imports that are located in a package which is not "java".
        If import packages are found they will be renamed into the JUnitConf.JUNIT_LIBS package.
        
        @param source: Java source code.
        @return: source with valid import declarations.
        """
        folder = JUnitConf.JUNIT_LIBS
        importsArray = re.findall('import\s+(?!java\.)(?P<name>.*);', source)
        libraries = JUnitConf.LIBRARIES
            
        for imports in importsArray:
            for libs in libraries:
                lib = libs.split('.')[0]
                start = imports.find(lib)
                if start > 0:
                    #found library
                    source = source.replace('import '+imports,'import '+folder+'.'+imports[start:],1)
                    
        return source
            
            
            
#--------  Syntax methods that have to be overwritten  -------------------------
    def _preProcessCheckSyntax(self,test,src,**kwargs):
        validPackages = self.grantValidPackage(src)
        #logging.debug(validPackages)
        preProcessedSource = self.handleStudentsImports(validPackages)
        #logging.debug(preProcessedSource)
        className = self.getClassName(src)
        return preProcessedSource,className
        
        
    def _postProcessCheckSyntax(self,test,message):
        matches = re.findall('\w+\.\w+:(?P<numbers>\d+):',message)
        
        for match in matches:
            new_line_number = int(match) - self.line_offset
            message = message.replace(match,str(new_line_number),1)
            
        self.line_offset = 0
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
                        os.path.basename(module['file']),
                        JUnitConf.CLASSPATH_SETTINGS)
                    
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
                
            exitcode, result = self._runInterpreter(
                compiler,
                os.path.dirname(wrapperModule['file']),
                os.path.basename(wrapperModule['file']),
                JUnitConf.CLASSPATH_SETTINGS)
                
            assert exitcode == EX_OK,\
                'Error in wrapper code during semantic check:\n\n%s' % result
                

            exitcode, result = self._runInterpreter(
                interpreter,
                os.path.dirname(wrapperModule['file']),
                JUnitConf.CLASS_SEMANTIC_CHECK,
                JUnitConf.CLASSPATH_SETTINGS)
                
        except Exception, e:
            message = 'Internal error during semantic check: %s: %s' % \
                (sys.exc_info()[0], e)
                
            logging.error(message)
            
            msg = re.sub(JUnitConf.METHOD_NOT_FOUND_RE,"",message)
            
            return BackendResult(-230,msg)
        
        if exitcode != EX_OK:
            result = "\nYour submission failed. Test " \
            "case was: '%s' (%s)" \
            "\n\n Received result: %s"\
            % (result.split(" ")[0], test.getName(), result)

            return BackendResult(False, result)
        
        
        else:
            return BackendResult(True,'\nYour submission passed all tests.')
            
            
            
            

            
        
        
        
        

        

        
        
        
        
        