# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-University, Magdeburg
#
# This file is part of ECSpooler.
import sys, os, re

from types import StringTypes

# local imports
from backends.haskell.Haskell import Haskell
from backends.haskellqc.HaskellQCConf import HaskellQCConf

from lib.data.BackendJob import BackendJob
from lib.data.BackendResult import BackendResult
from lib.AbstractProgrammingBackend import AbstractProgrammingBackend
from lib.AbstractProgrammingBackend import EX_OK

class HaskellQC(Haskell):
    """
    Backend for testing Haskell programs using QuickCheck.
    """
        
    id = 'haskellqc'
    name = 'Haskell QuickCheck'
    version = '1.0'
    
    schema = HaskellQCConf.inputSchema
    testSchema = HaskellQCConf.tests


    def _postProcessCheckSemantic(self, test, message):
        """
        Post process QuickCheck result messages.
        
        @param: test:
        @param: message:
        """
        result = message
        
        #result = re.sub('isEqual=', '', message)
        result = re.sub('/.*/ECSpooler/backends/haskellqc/', '', message)

        return result
    

    def _process_checkSemantics(self, job):
        """
        @return: a BackendResult object with result code and value
        """
        # test for available test specs
        testSpecs = self._getTests(job)

        if len(testSpecs) == 0:
            msg = 'No test specification selected.'
            self.log.warn('%s, %s' % (msg, job.getId()))
            return BackendResult(-217, msg)
        
        # get student's submission and QuickCheck properties
        submission = job['submission']
        properties = job['properties']

        #assert properties and type(properties) in StringTypes, \
        #    "Semantic check requires valid 'properties' (%s)" % properties

        assert submission and type(submission) in StringTypes, \
            "Semantic check requires valid 'student solution' (%s)" % submission

        testdata = None
        
        if properties:
            testdata = HaskellQCConf.PROPERTIES_RE.findall(properties)

        if (not testdata) or (len(testdata) == 0):
            msg = 'No QuickCheck propeties defined.'
            self.log.warn('%s, %s' % (msg, job.getId()))
            return BackendResult(-216, msg)

        submission = HaskellQCConf.genericModulTemplate % ('Student', submission)
        properties = HaskellQCConf.propsTemplate % (properties)

        # define return values
        feedback = BackendResult.UNKNOWN
        solved = True

        # run selected test specifications
        for test in testSpecs:

            if not solved: break

            self.log.debug('Running semantic check with test: %s' % 
                          test.getName())

            # get the interpreter
            interpreter = test.interpreter
           
            # write submission and propeties to files
            self._writeModule('Student', submission, '.hs', job.getId(), test.encoding)
            self._writeModule('Properties', properties, '.hs', job.getId(), test.encoding)

            # run with all test data
            for t in testdata:
                # add property name
                wrapper = HaskellQCConf.PROP_RE.sub(t, test.semantic)
                
                # execute wrapper code in haskell interpreter
                try:
                    # write module for wrapper
                    wrapperModule = self._writeModule('wrapper', wrapper, 
                                                      self.srcFileSuffix, 
                                                      job.getId(),
                                                      test.encoding)
                    
                    # execute
                    exitcode, result = \
                        self._runInterpreter(interpreter, 
                                    os.path.dirname(wrapperModule['file']),
                                    os.path.basename(wrapperModule['file']))

                    # remove all special characters written by runhugs 
                    result = HaskellQCConf.RUNHUGS_RE.sub('', result)
        
                except Exception, e:
                    msg = 'Internal error during semantic check: %s: %s' % \
                          (sys.exc_info()[0], e)
                                  
                    self.log.error(msg)
                    return BackendResult(-230, msg)

                # an error occured
                # FIXME: os.EX_OK is available only for Macintosh and Unix!
                if exitcode != EX_OK:
                    result = "\nYour submission failed. Test " \
                             "case was: '%s' (%s)" \
                             "\n\n Received result: %s"\
                             % (t, test.getName(), 
                                self._postProcessCheckSemantic(test, result))

                    return BackendResult(False, result)
                        
                # has the students' solution passed this test?
                else:
                    #self.log.debug(result)
                    failed = HaskellQCConf.FAILED_RE.findall(result)
                    passed = HaskellQCConf.PASSED_RE.findall(result)
                    
                    #if len(passed) != 0:
                    if passed:
                        feedback += "\nYour submission %s (%s)." \
                            % (''.join(passed), t)
                        # go to next property
                        
                    #elif failed != None:
                    elif failed:
                        #failed_data_str = ''
                        
                        #for d in failed_data:
                        #    failed_data_str += d + ' ';
                    
                        feedback = '\nYour submission failed. Test case was: %s (%s)' \
                            % (''.join(failed), t)
                        
                        solved = False                    
                        break # means: end testing right now
                    # end elif
                #end else                    
            # end inner for loop
        #end out for loop 

        # TODO: i18n
        feedback = feedback

        return BackendResult(solved, feedback)


# -- do some testing ----------------------------------------------------------

if __name__ == "__main__":
    """
    """
    import socket

    host = socket.getfqdn()
    port = 15060
    spooler = 'http://%s:%d' % (socket.getfqdn(), 15050)
    auth = {"username":"demo", "password":"foobar"}
    
    params = {'host': host, 'port': port, 'spooler': spooler, 'auth': auth, }

    jobdata = {
        'backend':'haskellqc', 
        'submission':'square x = x * x',
        'properties': 'prop_square x = (square x) == (x * x)\n  where types= x::Int',
        'tests': ['default'],
    }

    job = BackendJob(data=jobdata)

    result = HaskellQC(params)._process_checkSemantics(job)
    
    print result.getValue()
    print result.getMessage()