# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.

import os, re, popen2, tempfile, threading
import socket, xmlrpclib

from AbstractBackend import AbstractBackend
from data import checkjob, checkresult
from data.exceptions import InvalidDataException

class AbstractFPBackend(AbstractBackend):
    """
    AbstractFPBackend is the basic class of all backends testing
    programming exercises for functional programming languages,
    such as Haskell, Erlang or Scheme.
    
    Backend implementations *should* be derived from this class and
    implement the following methods:

    checkSyntax(self) -> CheckResult
    ... test the student solution for syntactical correctness

    checkSemantics(self) -> CheckResult
    ... compare the student solution with the sample solution
    using the given comparator function.

    Both methods can access the CheckJob data instance via self._job
    """
    
    # resourcestring
    PROCESS_WAIT_TIME = 15
   
    def execute(self, authdata, jobdata):
        """
        Executes a check job.

        @param authdata authorization information
        @param jobdata relevant job data, see CheckJob docu for details
        @return CheckResult data
        """
        self._authenticate(authdata)

        try:
            self._job = checkjob.CheckJob(jobdata)

            result = self.checkSyntax()
            #print 'checkSyntax:\n', result.getData()[1]
            if result.isFailure(): return result.getData()

            result = self.checkSemantics()
            #print 'checkSemantics:\n', result.getData()[1]
            
            return result.getData()

        except InvalidDataException, exc:
            return checkresult.CheckResult(-1, "invalid job data").getData()

        return checkresult.CheckResult(-10, "implementation error").getData()


    def checkSyntax(self):
        # overwrite this method
        raise NotImplementedError("Method checkSyntax must be "
                                  "implemented by subclass")


    def checkSemantics(self):
        # overwrite this method
        raise NotImplementedError("Method checkSemantics must be "
                                  "implemented by subclass")
    

    def getComparatorTemplate(self):
        # overwrite this method
        raise NotImplementedError("Method getComparatorTemplate must be "
                                  "implemented by subclass")



    def _runInterpreter(self, interpreter, code, suffix=''):
        """
        Runs a given code in a interpreter.

        @param interpreter 
        @param code source code to run
        @param suffix default filename suffix
        @return (exitcode, output lines)
        """
        
        fname = tempfile.mktemp(suffix = suffix)

        fd = open(fname, "w")
        fd.write(code)
        fd.write("\n")
        fd.close()

        # TODO: use sand-box environment !!!
        # copy files to jail using ssh
        #executeOsCmd(sCommandCopy % (mSFilename + '.hs'))
        #executeOsCmd(sCommandCopy % (sSFilename + '.hs'))
        #executeOsCmd(sCommandCopy % (wFilename + '.hs'))

        # Popen4 will provide both stdout and stderr on handle.fromchild
        handle = popen2.Popen4('%s %s' % (interpreter, fname))
        handle.tochild.close()
        # we don't expect to send on stdin; instead we just wait for the process 
        # to end, or kill it.

        def interruptProcess():
            print 'Aborting ' + interpreter + ' : SIGTERM -> %i' % handle.pid
            os.kill(handle.pid, 15)

        timer = threading.Timer(self.PROCESS_WAIT_TIME, interruptProcess)
        timer.start()
        exitcode = handle.wait()
        timer.cancel()

        if exitcode == 15:
            # process has been interrupted by timer
            os.remove(fname)
            return (15, ['Program aborted after %i seconds' % 
                    self.PROCESS_WAIT_TIME])
            
        buf = handle.fromchild.readlines()
        handle.fromchild.close()
        os.remove(fname)

        return (handle.poll(), buf)
