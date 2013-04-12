# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2011 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.

from types import BooleanType
from types import UnicodeType

#import sys, os, re, popen2, tempfile, threading, signal, traceback
import sys, os, subprocess, tempfile, threading, signal, traceback

import shutil
import logging

from lib.Backend import Backend
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

LOG = logging.getLogger()
    
class ProgrammingBackend(Backend):
    """
    ProgrammingBackend is the basic class of all backends for
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
   
    def __init__(self, params, versionFile=__file__):
        """
        This constructor is needed to reset the logging environment.
        """
        Backend.__init__(self, params, versionFile)
    

    def _process_execute(self, job):
        """
        Executes a check job.

        @param: job: a BackendJob with all relevant test data
        @return: a BackendResult
        @see: BackendJob, BackendResult
        @see: Backend.execute, Backend._process_execute
        """

        try:
            # invoke syntax check
            LOG.info('Invoking syntax check (%s)' % job.getId())
            result = self._manage_checkSyntax(job) # returns a BackendResult
        
            # FIXME: If the returned value is always of typ
            #        BackendResult we could use isFailure!
            #if not result.hasFailed: 
            
            LOG.debug('Result from syntax check: %s' % result.getData())
            
            if result:
                rValue = result.getValue()
                
                if (type(rValue) == BooleanType and rValue == True):
                    # invoke semantic check
                    LOG.info('Invoking semantic check (%s)' % job.getId())
                    result = self._manage_checkSemantics(job)
            
                    LOG.debug('Result from semantic check: %s' % result.getData())
                # end if
            # end if

        except Exception, e:
            msg = 'Internal error: %s: %s' % (sys.exc_info()[0], e)
            LOG.debug(traceback.format_exc())
            LOG.error(msg)
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
            LOG.warn('%s, %s' % (msg, job.getId()))
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
        
        #LOG.debug("testSpec.compiler: %s" % repr(testSpec.compiler))
        #LOG.debug("testSpec.interpreter: %s" % repr(testSpec.interpreter))
        #LOG.debug("compiler: %s" % repr(compiler))
        
        if compiler:
            try:
                # test term (e.g., student's source code)
                try:
                    src, mName = self._preProcessCheckSyntax(testSpec, submission)
                except AssertionError, ae:
                    return BackendResult(False, str(ae))

                LOG.info('Running syntax check with test: %s' % 
                             testSpec.getName())
                              
                module = self._writeModule(mName, src, 
                                           self.srcFileSuffix, 
                                           jobId,
                                           testSpec.encoding)
                
                #LOG.debug(repr(module))
                
                exitcode, result = \
                    self._runInterpreter(compiler, 
                                   os.path.dirname(module['file']),
                                   os.path.basename(module['file']))
                    
                #LOG.debug('exitcode: %s' % repr(exitcode))
                #LOG.debug('result: %s' % repr(result))
                
            except Exception, e:
                msg = 'Internal error during syntax check: %s: %s' % \
                        (sys.exc_info()[0], e)
                LOG.debug(traceback.format_exc())
                LOG.error(msg);
                return BackendResult(-220, msg)
            
            LOG.debug('exitcode: %s' % repr(-exitcode))
    
            # consider exit code
            if exitcode != EX_OK:
                result = self._postProcessCheckSyntax(testSpec, result)
                #return BackendResult(-exitcode, result or repr(-exitcode))
                return BackendResult(False, result or repr(-exitcode))

        else:
            msg = 'No compiler/interpreter defined (test spec: %s).' \
                    % testSpec.getName()

            LOG.error(msg)
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
            
        #LOG.debug('%s' % tempfile.gettempdir())
        #LOG.debug('%s' % dir)
        #LOG.debug('%s' % name)
        #LOG.debug('%s' % suffix)
        
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

        #LOG.debug('args: %s' % repr(args))
        #LOG.debug('options: %s' % repr(options))
        
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

        #LOG.debug('args_encoded: %s' % repr(args_encoded))
        #LOG.debug('options_encoded: %s' % repr(options_encoded))

        # create a list of alle command line elements
        commandLine = [command]
        commandLine.extend(options_encoded)
        commandLine.append(fName)
        commandLine.extend(args_encoded)
        
        LOG.debug('commandLine: %s' % commandLine)

        if (sys.platform=="win32") or (sys.platform=="win64"):
            close_fds = False
        else:
            close_fds = True

        # we use subprocess
        handle = subprocess.Popen(commandLine, shell = False, close_fds = close_fds,
                                  stdin = subprocess.PIPE,
                                  stdout = subprocess.PIPE,
                                  stderr = subprocess.STDOUT)

        LOG.info('Started %s %s in %s with PID %d' % (command,
                                                          fName,
                                                          dir,
                                                          handle.pid))

        # This method will be called in a timer thread to ensure that
        # handle will be killed after PROCESS_WAIT_TIME
        def interruptProcess():
            LOG.debug('Killing %s %s: %d -> %i' %
                          (command, fName, SIGTERM, handle.pid))

            try:
                os.kill(handle.pid, SIGTERM)
            except AttributeError:
                # we will do nothing in this case
                pass
        # end interruptProcess

        timer = threading.Timer(self.PROCESS_WAIT_TIME, interruptProcess)
        timer.start()

        # read stdout while communicating with process        
        stdout, stderr = handle.communicate()
        exitcode = abs(handle.returncode)

        # cancel time; process has been finished normaly before PROCESS_WAIT_TIME 
        timer.cancel()

        # check exitcode
        if exitcode == SIGTERM:
            # process has been interrupted by timer
            #os.remove(fName)
            return exitcode, 'Function call cancelled after %i seconds.  ' \
                   'Check for infinite loops.' \
                   % (self.PROCESS_WAIT_TIME,)
            
        #LOG.debug('exitcode: %s' % repr(exitcode))
        #LOG.debug('stdout: %s' % repr(stdout))
        
        # removing files will be done in _cleanup
        
        # concat result message
        if (stderr == None):
            msg = "%s" % (stdout)
        else:
            msg = "%s\n%s" % (stdout, stderr)
        
        return exitcode, msg


    def _cleanup(self, directory):
        """
        Delete the entire directory tree for path.
        
        @directory a directory in temporary path
        """

        if not config.CLEANUP:
            return
        
        try:
            # change directory
            os.chdir(tempfile.gettempdir())

            path = os.path.join(tempfile.gettempdir(), directory)

            LOG.debug('Deleting directory: %s' % path)

            # delete the entire directory tree
            shutil.rmtree(path)

        except Exception, e:
            LOG.debug(traceback.format_exc())
            LOG.warn('Internal error during clean up: %s: %s' % \
                          (sys.exc_info()[0], e))
