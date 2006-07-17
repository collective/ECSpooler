# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.

import sys, os, re, popen2, tempfile, threading, signal
import shutil
import logging

from types import StringType, UnicodeType

from AbstractBackend import AbstractBackend
from util import utils
from data import checkjob
#from data.checkresult import CheckResult
from data.exceptions import InvalidDataException

try:
    EX_OK = os.EX_OK
except AttributeError:
    EX_OK = 0
    
try:
    SIGKILL = signal.SIGKILL
except AttributeError:
    SIGKILL = 15
    
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
    
    srcFileSuffix = ''
    
    # resourcestring
    PROCESS_WAIT_TIME = 10

   
    def process_execute(self, jobdata):
        """
        Executes a check job.

        @param jobdata relevant job data, see class CheckJob for more details
        @return CheckResult-object/tupel consisting of result code and message
        """

        try:
            job = checkjob.CheckJob(jobdata)
            assert job, 'Invalid or insufficient data: %s' % job

            result = (1, '')

            try:
                # invoke syntax check
                logging.debug('Invoking syntax check (%s).' % job.getId())
                result = self.manage_checkSyntax(job)  #self.checkSyntax(job)
            
                # FIXME: If the returned value is always of typ
                #        CheckResult we could use isFailure!
                #if not result.isFailure(): 
                
                if result and result[0] > 0: 
                    # invoke semantic check
                    logging.debug('Invoking semantic check (%s).' % job.getId())
                    retval = self.manage_checkSemantics(job)
                    
                    # no return value == no test data and therefore no test resutls!
                    if retval:
                        result = retval
                
            except Exception, e:
                msg = 'Internal error: %s: %s' % (sys.exc_info()[0], e)
                logging.error(msg)
                result = (-10, msg)
                
            # delete all files and folders used in this test
            self._cleanup(job['id'])
            return result

        except (InvalidDataException, AssertionError), exc:
            logging.warn('%s: %s' % (exc, job))
            return (-1, '%s: %s' % (exc, job))


    # -- check syntax ---------------------------------------------------------
    def manage_checkSyntax(self, job):
        """
        Manages the syntax check for a given job depending on the 
        selected test scenarios.  The syntax check itself will be 
        done in process_checkSyntax.
        """
        
        studentSolution = job['studentSolution']
        
        assert studentSolution and type(studentSolution) in (StringType, 
                                                             UnicodeType), \
            "Syntax check requires a valid 'student solution' (%s)" % \
            repr(studentSolution)
            
        # src = TEMPLATE_SYNTAX % (self._job["studentSolution"],)
            
        # using test specifications
        for test in self._getTests(job):
            result = self._process_checkSyntax(job['id'], test, studentSolution)
            
            if result and result[0] <= 0:
                return result
        # end for

        return (1, 'Syntax check succeeded.')


    def _process_checkSyntax(self, jobId, test, studentSolution):
        """
        Tests the syntax of a programm. Override this method if you 
        need to do some special things during syntax check.
        
        """
        # get the compiler or if not available the interpreter
        compiler = test.compiler or test.interpreter
        
        if compiler:
            
            # student's source code
            src, mName = self._preProcessCheckSyntax(test, studentSolution)

            try:
                logging.debug('Running syntax check with test: %s' % 
                              test.getName())
                              
                module = self._writeModule(mName, src, 
                                           self.srcFileSuffix, 
                                           jobId,
                                           test.encoding)
                
                exitcode, result = \
                    self._runInterpreter(compiler, 
                                   os.path.dirname(module['file']),
                                   os.path.basename(module['file']))
                
            except Exception, e:
                msg = 'Internal error during syntax check: %s: %s' % \
                      (sys.exc_info()[0], e)
                              
                logging.error(msg)
                return (0, msg)
    
            # consider exit code
            if exitcode != EX_OK:
                result = self._postProcessCheckSyntax(test, result)
                return (-exitcode, result)

        else:
            msg = 'No compiler/interpreter given for test: %s' % test.getName()

            logging.error(msg)
            return (0, msg)

        # everything seems to be ok
        return (1, '')

   
    def _preProcessCheckSyntax(self, test, src, **kwargs):
        """
        Pre process student's submissions and syntax check wrapper code. 
        Override this method if you need to reformat the wrapper code or 
        the student's submission. 
        
        @param test
        @param src
        @return source and module name if needed
        """
        result = ''
        
        if test.syntax:
            result = re.sub('\$\{SOURCE\}', src, test.syntax)
            
        else:
            result = src

        return result, None


    def _postProcessCheckSyntax(self, test, message):
        """
        Post process interpreter messages. Override this method if you need
        to remove or reformat messages.
        
        @param message
        """
        return message


    # -- check semantics ------------------------------------------------------
    def manage_checkSemantics(self, job):
        """
        """
        studentSolution = job['studentSolution']
        
        assert studentSolution and type(studentSolution) in (StringType, 
                                                             UnicodeType), \
            "Semantic check requires a valid 'student solution' (%s)" % \
            repr(studentSolution)

        return self._process_checkSemantics(job)


    def _process_checkSemantics(self, job):
        # overwrite this method
        raise NotImplementedError("Method _process_checkSemantics must be "
                                  "implemented by a subclass")


    def _preProcessCheckSemantic(self, test, src, **kwargs):
        """
        Pre process student's submissions and semantic check wrapper code. 
        Override this method if you need to reformat the wrapper code or 
        the student's submission. 

        @param test
        @param src
        @return source and module name if needed
        """                                  
        return src, None

    
    def _postProcessCheckSemantic(self, test, message):
        """
        Post process interpreter messages. Override this method if you need
        to remove or reformat messages.
        
        @param message
        """
        return message


    # -- helper methods -------------------------------------------------------
    def _writeModule(self, name, source, suffix='', dir='', encoding='utf-8'):
        """
        Creates a new module. Returns the module's name and
        absolute path in a tuple object.
        
        @return a dictonary with keys module and file
        """
        
        if not name:
            name = utils.getUniqueModuleName()
        
        # get file name
        fName = os.path.join(tempfile.gettempdir(), dir, name + suffix)
        
        # write file
        utils.writeFile(source, fName, encoding)
        
        # get modul name
        #mName = os.path.basename(fName).replace(suffix, '')
        mName = name
        
        return {'module':mName, 'file':fName}


    def _runInterpreter(self, interpreter, dir, fName):
        """
        Change the current working directory to dir and runs the given 
        interpreter with the given file. Availability: Unix.

        @param interpreter command line for a interpreter including flags and
               in  most cases a %s conversion placeholder for the file name
        @param dir path to which we will change the current working directory 
        @param fName name of the file which will be called with the interpreter
        @return exitcode and result message, normally something like os.EX_OK
                and the output from this interpreter run
        """
        
        #logging.debug('interpreter: ' + interpreter)
            
        # TODO: Maybe we should always use a sand-box environment!
        #       On possible solution is to copy the files to a jail
        #       using ssh
        #executeOsCmd(sCommandCopy % (mSFilename + '.hs'))
        #executeOsCmd(sCommandCopy % (sSFilename + '.hs'))
        #executeOsCmd(sCommandCopy % (wFilename + '.hs'))
            
        # Popen4 will provide both stdout and stderr on handle.fromchild
        if interpreter.find('%s') == -1:
            interpreter = interpreter + ' %s'
        
        # change dir to current job dir
        os.chdir(dir)
        
        #logging.debug('xxx: %s' % (interpreter % fName))    
        
        handle = popen2.Popen4(interpreter % fName)
        handle.tochild.close()
        # we don't expect to send on stdin; instead we just wait for the 
        # process to end, or kill it.

        def interruptProcess():
            logging.debug('Aborting %s: %d -> %i' %
                          (interpreter % fName, SIGKILL, handle.pid))

            #os.kill(handle.pid, 15)
            os.kill(handle.pid, SIGKILL)

        # end interruptProcess

        timer = threading.Timer(self.PROCESS_WAIT_TIME, interruptProcess)
        timer.start()
        exitcode = handle.wait()
        timer.cancel()


        if exitcode == SIGKILL:
            # process has been interrupted by timer
            #os.remove(fName)
            return (exitcode, ['Function call cancelled after %i seconds.' % 
                            (self.PROCESS_WAIT_TIME,)])
            
        buf = handle.fromchild.readlines()
        handle.fromchild.close()
        
        exitcode = handle.poll()
        response = buf
        
        result = ''.join(response)

        # removing files will be done in _cleanup
        return exitcode, result


    def _cleanup(self, dir):
        """
        Delete the entire directory tree for path.
        
        @dir a directory in temporary path
        """
        try:
            # change dir
            os.chdir(tempfile.gettempdir())

            path = os.path.join(tempfile.gettempdir(), dir)
            # delete the entire directory tree
            shutil.rmtree(path)

        except Exception, e:
            logging.error('Internal error during _cleanup: %s: %s' % \
                          (sys.exc_info()[0], e))
        

