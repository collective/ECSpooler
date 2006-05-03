# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.

import sys, os, re, popen2, tempfile, threading, signal
import socket, xmlrpclib, shutil
import logging

from types import StringType

from AbstractBackend import AbstractBackend
from util import utils
from data import checkjob
#from data.checkresult import CheckResult
from data.exceptions import InvalidDataException

try:
    EX_OK = os.EX_OK
except AttributeError:
    EX_OK = 0

class AbstractSimpleBackend(AbstractBackend):
    """
    AbstractFPBackend is the basic class of all backends testing
    programming exercises for functional programming languages,
    such as Haskell, Erlang or Scheme.
    
    Backend implementations *should* be derived from this class and
    implement the following methods:

    checkSyntax(self) -> BackendResult
    ... test the student solution for syntactical correctness

    checkSemantics(self) -> BackendResult
    ... compare the student solution with the sample solution
    using the given comparator function.
    """
    
    srcFileSuffix = ''
    
    # resourcestring
    PROCESS_WAIT_TIME = 10
   
    def execute(self, authdata, jobdata):
        """
        Executes a check job.

        @param authdata authorization information
        @param jobdata relevant job data, see CheckJob docu for details
        @return CheckResult data (a tupel consisting of result code and message)
        """
        self._authenticate(authdata)

        try:
            job = checkjob.CheckJob(jobdata)
            assert job, 'Invalid or insufficient data: %s' % job

            result = (1, '')

            try:
                # invoke syntax check
                logging.debug('Invoking syntax check (%s).' % job.getId())
                result = self.checkSyntax(job)
            
                # FIXME: Maybe the rturned value should be of typ
                #        CheckResult so we can use method isFailure
                #if not result.isFailure(): 
                
                if result[0] > 0: 
                    # invoke semantic check
                    logging.debug('Invoking semantic check (%s).' % job.getId())
                    result = self.checkSemantics(job)
                
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

#    def checkSyntax(self):
#        # overwrite this method
#        raise NotImplementedError("Method checkSyntax must be "
#                                  "implemented by subclass")


    def manage_checkSyntax(self, job):
        """
        Manages the syntax check for a given job depending on the 
        selected test scenarios.  The syntax check itself will be 
        done in process_checkSyntax.
        """
        studentSolution = job['studentSolution']
        
        assert studentSolution and type(studentSolution) == StringType, \
            "Syntax check requires a valid 'student solution' (%s)" % \
            studentSolution
            
        # src = TEMPLATE_SYNTAX % (self._job["studentSolution"],)
            
        # using test specifications
        for test in self._getTests(job):
            process_checkSyntax(test, studentSolution)
        # end for

        return (1, 'Syntax check succeeded.')

    def process_checkSyntax(self, test, studentSolution):
        """
        Tests the syntax of a programm. Override this method if you 
        need to do some special things during syntax check. 
        """
        # get the compiler/interpreter
        compiler = test.compiler or test.interpreter
        
        if compiler:
            
            # reformat wrapper or student's source code
            src, mName = self._preProcessCheckSyntax(test, studentSolution)

            try:
                logging.debug('Running syntax check with test: %s' % 
                              test.getName())
                              
                module = self._writeModule(mName, src, 
                                           self.srcFileSuffix, 
                                           job['id'])
                
                exitcode, result = \
                    self._runInterpreter(compiler, 
                                   os.path.dirname(module['file']),
                                   os.path.basename(module['file']))
                
            except Exception, e:
                #import traceback
                #traceback.print_exc()

                msg = 'Internal error during syntax check: %s: %s' % \
                      (sys.exc_info()[0], e)
                #msg = 'Internal error during syntax check:'
                              
                logging.error(msg)
                return (0, msg)
    
            # consider exit code
            if exitcode != EX_OK:
                result = self._postProcess(test, result)
                return (-exitcode, result)

        else:
            logging.warn('No compiler/interpreter given for test: %s' % 
                         test.getName())


    def checkSemantics(self):
        # overwrite this method
        raise NotImplementedError("Method checkSemantics must be "
                                  "implemented by subclass")


    def _preProcessCheckSyntax(self, test, studentSolution):
        """
        Pre process student's submissions and syntax check wrapper code. 
        Override this method if you need to reformat the wrapper code or 
        the student's submission. 
        
        @return source and module name if needed
        """
        source = ''
        
        if test.syntax:
            source = re.sub('\$\{studentSolution\}', studentSolution, 
                         test.syntax)
        else:
            source = studentSolution

        return source, None
    

    def _preProcessCheckSemantic(self, test, studentSolution):
        """
        Pre process student's submissions and semantic check wrapper code. 
        Override this method if you need to reformat the wrapper code or 
        the student's submission. 
        """                                  
        return studentSolution


    def _postProcess(self, test, message):
        """
        Post process interpreter messages. Override this method if you need
        to remove or reformat messages.
        
        @param message
        """
        return message


    def _writeModule(self, name, source, suffix='', dir=''):
        """
        Creates a new python module. Returns the module's name and
        absolute path.
        
        @return a dictonary with keys module and file
        """
        
        if not name:
            name = utils.getUniqueModuleName()
        
        # get file name
        fName = os.path.join(tempfile.gettempdir(), dir, name + suffix)
        
        # write file
        utils.writeFile(source, fName)
        # get modul name
        #mName =  os.path.basename(fName).replace(suffix, '')
        mName =  name
        
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
            
        handle = popen2.Popen4(interpreter % fName)
        handle.tochild.close()
        # we don't expect to send on stdin; instead we just wait for the 
        # process to end, or kill it.

        def interruptProcess():
            logging.debug('Aborting %s: %d -> %i' %
                          (interpreter % fName, signal.SIGKILL, handle.pid))

            #os.kill(handle.pid, 15)
            os.kill(handle.pid, signal.SIGKILL)

        # end interruptProcess

        timer = threading.Timer(self.PROCESS_WAIT_TIME, interruptProcess)
        timer.start()
        exitcode = handle.wait()
        timer.cancel()


        if exitcode == signal.SIGKILL:
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
            # delete an entire directory tree
            # FIXME: 
            #shutil.rmtree(path)

        except Exception, e:
            logging.error('Internal error during _cleanup: %s: %s' % \
                          (sys.exc_info()[0], e))
        

