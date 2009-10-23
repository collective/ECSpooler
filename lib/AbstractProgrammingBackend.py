# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2009 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.

from types import BooleanType
from types import UnicodeType

#import sys, os, re, popen2, tempfile, threading, signal, traceback
import sys, os, subprocess, tempfile, threading, signal, traceback

import shutil

from lib.AbstractBackend import AbstractBackend
from lib.util import utils
from lib.data.BackendResult import BackendResult

import config

try:
    EX_OK = os.EX_OK
except AttributeError:
    EX_OK = 0
    
try:
    SIGTERM = signal.SIGTERM
except AttributeError:
    SIGTERM = 15

    
class AbstractProgrammingBackend(AbstractBackend):
    """
    AbstractProgrammingBackend is the basic class of all backends for
    programming exercises.
    
    Backend implementations *should* be derived from this class and must
    implement the following methods:

    process_checkSyntax(self) -> (code, message)
    ... test the student solution for syntactical correctness.

    process_checkSemantics(self) ->  (code, message)
    ... test the student solution for semantical correctness using input data.
    """
    
    # set in subclasses
    srcFileSuffix = ''
    
    # resourcestring
    # set default time (in seconds) before killing a test job's execution
    PROCESS_WAIT_TIME = 10
   
    def _process_execute(self, job):
        """
        Executes a check job.

        @param: job: a BackendJob with all relevant test data
        @return: a BackendResult
        @see: BackendJob, BackendResult
        @see: AbstractBackend.execute, AbstractBackend._process_execute
        """

        try:
            # invoke syntax check
            self.log.info('Invoking syntax check (%s)' % job.getId())
            result = self._manage_checkSyntax(job) # returns a BackendResult
        
            # FIXME: If the returned value is always of typ
            #        BackendResult we could use isFailure!
            #if not result.hasFailed: 
            
            self.log.debug('Result from syntax check: %s' % result.getData())
            
            if result:
                rValue = result.getValue()
                
                if (type(rValue) == BooleanType and rValue == True):
                    # invoke semantic check
                    self.log.info('Invoking semantic check (%s)' % job.getId())
                    result = self._manage_checkSemantics(job)
            
                    self.log.debug('Result from semantic check: %s' % result.getData())
                # end if
            # end if

        except Exception, e:
            msg = 'Internal error: %s: %s' % (sys.exc_info()[0], e)
            self.log.debug(traceback.format_exc())
            self.log.error(msg)
            result = BackendResult(-200, msg)
                

        # delete all files and folders used in this test
        self._cleanup(job.getId())

        return result


    # -- check syntax ---------------------------------------------------------
    def _manage_checkSyntax(self, job):
        """
        Manages the syntax check for a given job depending on the 
        selected test scenarios.  The syntax check itself will be 
        done in process_checkSyntax.
        
        @param: job: a BackendJob
        @return: a BackendResult
        """
        # test for available test specs
        testSpecs = self._getTests(job)

        # test for available test specs
        if len(testSpecs) == 0:
            msg = 'No test specification selected.'
            self.log.warn('%s, %s' % (msg, job.getId()))
            return BackendResult(-217, msg)
                        
        # get the names of all test enrironments selected in this job
        for testSpec in testSpecs:
            # process syntax check for each test environment
            result = self._process_checkSyntax(job.getId(), 
                                               testSpec, 
                                               job.getSubmission())
            
            # if there is an result probably an error occoured
            if result != None: return result

        return BackendResult(True, 'Syntax check succeeded.')


    def _process_checkSyntax(self, jobId, testSpec, submission):
        """
        Tests the syntax of a programm.  Override this method if you need to 
        do some special things during syntax check.
        
        @param: jobId: ID for this test job
        @param: test: name of the selected test environment (cf. self.testSchema)  
        @return: a BackendResult or None if test succeeded
        """
        # get the compiler or if not available the interpreter
        compiler = testSpec.compiler or testSpec.interpreter
        
        #self.log.debug("testSpec.compiler: %s" % repr(testSpec.compiler))
        #self.log.debug("testSpec.interpreter: %s" % repr(testSpec.interpreter))
        #self.log.debug("compiler: %s" % repr(compiler))
        
        if compiler:
            try:
                # test term (e.g., student's source code)
                try:
                    src, mName = self._preProcessCheckSyntax(testSpec, submission)
                except AssertionError, ae:
                    return BackendResult(False, str(ae))

                self.log.info('Running syntax check with test: %s' % 
                             testSpec.getName())
                              
                module = self._writeModule(mName, src, 
                                           self.srcFileSuffix, 
                                           jobId,
                                           testSpec.encoding)
                
                #self.log.debug(repr(module))
                
                exitcode, result = \
                    self._runInterpreter(compiler, 
                                   os.path.dirname(module['file']),
                                   os.path.basename(module['file']))
                    
                #self.log.debug('exitcode: %s' % repr(exitcode))
                #self.log.debug('result: %s' % repr(result))
                
            except Exception, e:
                msg = 'Internal error during syntax check: %s: %s' % \
                        (sys.exc_info()[0], e)
                self.log.debug(traceback.format_exc())
                self.log.error(msg);
                return BackendResult(-220, msg)
            
            self.log.debug('exitcode: %s' % repr(-exitcode))
    
            # consider exit code
            if exitcode != EX_OK:
                result = self._postProcessCheckSyntax(testSpec, result)
                #return BackendResult(-exitcode, result or repr(-exitcode))
                return BackendResult(False, result or repr(-exitcode))

        else:
            msg = 'No compiler/interpreter defined (test spec: %s).' \
                    % testSpec.getName()

            self.log.error(msg)
            return BackendResult(-221, msg)

        # everything seems to be ok
        return None

   
    def _preProcessCheckSyntax(self, test, src, **kwargs):
        """
        Pre process student's submissions and syntax check wrapper code. 
        Override this method if you need to reformat the wrapper code or 
        the student's submission. 
        
        @param: test
        @param: src
        @return: source and module name if needed
        """
        
        result = ''
        
        if test.syntax:
            #result = re.sub('\$\{SOURCE\}', src, test.syntax)
            result = test.syntax.replace('${SOURCE}', src)
            
        else:
            result = src

        return result, None


    def _postProcessCheckSyntax(self, test, message):
        """
        Post process interpreter messages. Override this method if you need
        to remove or reformat messages.
        
        @param: message
        """
        return message


    # -- check semantics ------------------------------------------------------
    def _manage_checkSemantics(self, job):
        """
        """
        return self._process_checkSemantics(job)


    def _process_checkSemantics(self, job):
        """
        """
        # overwrite this method
        raise NotImplementedError("Method _process_checkSemantics must be "
                                  "implemented by a subclass")


    def _preProcessCheckSemantic(self, test, src, **kwargs):
        """
        Pre process student's submissions and semantic check wrapper code. 
        Override this method if you need to reformat the wrapper code or 
        the student's submission. 

        @param: test
        @param: src
        @return: source and module name if needed
        """                                  
        return src, None

    
    def _postProcessCheckSemantic(self, test, message):
        """
        Post process interpreter messages. Override this method if you need
        to remove or reformat messages.
        
        @param: message
        """
        return message


    # -- helper methods -------------------------------------------------------
    def _writeModule(self, name, source, suffix='', dir='', encoding='utf-8'):
        """
        Creates a new module. Returns the module's name and
        absolute path in a tuple object.
        
        @return: a dictonary with keys module and file
        """
        
        if not name:
            name = utils.getUniqueModuleName()
            
        #self.log.debug('%s' % tempfile.gettempdir())
        #self.log.debug('%s' % dir)
        #self.log.debug('%s' % name)
        #self.log.debug('%s' % suffix)
        
        # get file name
        fName = os.path.join(tempfile.gettempdir(), dir, name + suffix)
        
        # write file
        utils.writeFile(source, fName, encoding)
        #os.chmod(os.path.dirname(fName), 01777)
        
        # get modul name
        #mName = os.path.basename(fName).replace(suffix, '')
        mName = name
        
        return {'module':mName, 'file':fName}


    #def _execute(self, command, options, dir, fName)
    def _runInterpreter(self, command, dir, fName, options=(), args=()):
        """
        Change the current working directory to dir and runs the given 
        interpreter with the given file. Availability: Unix.

        @param: command command which will be executed
        @param: options options for command including flags as list
        @param: dir path to which we will change the current working directory 
        @param: fName name of the file which will be called with the interpreter
        @param: args command line arguments that fName will be called with
        @return: exitcode and result message, normally something like os.EX_OK
        """
        # change dir to current job dir
        os.chdir(dir)
        
        args_encoded = []
        options_encoded = []

        #self.log.debug('args: %s' % repr(args))
        #self.log.debug('options: %s' % repr(options))
        
        for arg in args:
            if type(arg) == UnicodeType:
                args_encoded.append(arg.encode('utf-8'))
            else:
                args_encoded.append(arg)
        
        for option in options:
            if type(option) == UnicodeType:
                options_encoded.append(option.encode('utf-8'))
            else:
                options_encoded.append(option)

        #self.log.debug('args_encoded: %s' % repr(args_encoded))
        #self.log.debug('options_encoded: %s' % repr(options_encoded))

        # create a list of alle command line elements
        commandLine = [command]
        commandLine.extend(options_encoded)
        commandLine.append(fName)
        commandLine.extend(args_encoded)
        
        #self.log.debug('commandLine: %s' % commandLine)

        """
        # Popen4 will provide both stdout and stderr on handle.fromchild
        handle = popen2.Popen4(commandLine)
        self.log.info('Started %s %s in %s with PID %d' % (command,
                                                          fName,
                                                          dir,
                                                          handle.pid))
        """
        # Popen in will provide both stdout and stderr on handle.fromchild
        handle = subprocess.Popen(commandLine, shell = False, close_fds = True,
                                  stdin = subprocess.PIPE,
                                  stdout = subprocess.PIPE,
                                  stderr = subprocess.STDOUT)

        self.log.info('Started %s %s in %s with PID %d' % (command,
                                                          fName,
                                                          dir,
                                                          handle.pid))

        handle.fromchild = handle.stdout
        handle.tochild = handle.stdin
        handle.childerr = handle.stderr

        # we don't expect to send on stdin; instead we just wait for the 
        # process to end, or kill it.
        handle.tochild.close()

        def interruptProcess():
            self.log.debug('Killing %s %s: %d -> %i' %
                          (command, fName, SIGTERM, handle.pid))

            try:
                os.kill(handle.pid, SIGTERM)
            except AttributeError:
                # we will do nothing in this case
                pass
        # end interruptProcess

        timer = threading.Timer(self.PROCESS_WAIT_TIME, interruptProcess)
        timer.start()
        exitcode = handle.wait()
        timer.cancel()


        if exitcode == SIGTERM:
            # process has been interrupted by timer
            #os.remove(fName)
            return exitcode, 'Function call cancelled after %i seconds.  ' \
                   'Check for infinite loops.' \
                   % (self.PROCESS_WAIT_TIME,)
            
        buf = handle.fromchild.readlines()
        handle.fromchild.close()
                
        exitcode = handle.poll()
        response = buf
        
        #self.log.debug('exitcode: %s' % repr(exitcode))
        #self.log.debug('response: %s' % repr(response))
        
        result = ''.join(response)


        # removing files will be done in _cleanup
        return exitcode, result


    def _cleanup(self, dir):
        """
        Delete the entire directory tree for path.
        
        @dir a directory in temporary path
        """

        if not config.CLEANUP:
            return
        
        try:
            # change dir
            os.chdir(tempfile.gettempdir())

            path = os.path.join(tempfile.gettempdir(), dir)

            self.log.debug('Deleting directory: %s' % path)

            # delete the entire directory tree
            shutil.rmtree(path)

        except Exception, e:
            self.log.debug(traceback.format_exc())
            self.log.warn('Internal error during clean up: %s: %s' % \
                          (sys.exc_info()[0], e))
