# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-University, Magdeburg
#
# This file is part of ECSpooler.
import sys, os, re
import logging

from types import StringTypes

# local imports
from backends.haskell.Haskell import Haskell
from backends.haskell.HaskellQCConf import HaskellQCConf
from backends.haskell.HaskellQCConf import HaskellQCConf

from lib.data.BackendResult import BackendResult
from lib.AbstractProgrammingBackend import AbstractProgrammingBackend
from lib.AbstractProgrammingBackend import EX_OK


class HaskellQuickCheck(Haskell):
    """
    Backend for testing Haskell programs using QuickCheck.
    """
        
    id = 'haskellqc'
    name = 'Haskell QuickCheck'
    version = '1.0'
    
    schema = HaskellQCConf.inputSchema
    testSchema = HaskellQCConf.tests

    def _process_checkSemantics(self, job):
        """
        Test a Haskell programs using QuickCheck.
        
        @return: a BackendResult object with result code and value
        """
        # test for available test specs
        testSpecs = self._getTests(job)

        if len(testSpecs) == 0:
            msg = 'No test specification selected.'
            logging.warn('%s, %s' % (msg, job.getId()))
            return BackendResult(-217, msg)

        # get model solution (maybe unused later) 
        modelSource = job.get('modelSolution', '')
        # get student's submission
        studentSource = job.getSubmission()
        # get QuickCheck properties
        propertySource = job.get('properties', '')
        
        resultStr = ''

        # write model solution, students' solution and wrapper files
        # 1. model solution file
        mSModuleName = getUniqueModuleName('Model')
        mSFileName = getTempFileName(mSModuleName, '.hs')
        mSSource = 'module ' + mSModuleName + ' where\n\n' + modelSource
        writeFile(mSSource, mSFileName)
                    
        # 2. students' solution
        sSModuleName = getUniqueModuleName('Student')
        sSFileName = getTempFileName(sSModuleName, '.hs')
        sSSource = 'module ' + sSModuleName + ' where\n\n' + studentSource
        writeFile(sSSource, sSFileName)
        
        # 3. write wrapper and execute it
        # get all property names first
        propertyNames = re.findall('prop_\S*', propertySource)
        propertyNames = unique(propertyNames)
        
        # remember all properties which are passed without error
        solvedProperties = [];
        
        # for each property write a wrapper 
        for propertyName in propertyNames:
            # 3.1 module definition and imports
            wSource = 'module Main where\n\n' + \
                      'import QuickCheck\n' + \
                      'import ' + mSModuleName + '\n' + \
                      'import ' + sSModuleName + '\n\n'
                   
            # 3.2 QC properties
            propertySource = propertySource.replace(DEFAULT_STUDENT_MODULE_NAME, sSModuleName)
            propertySource = propertySource.replace(DEFAULT_MODEL_MODULE_NAME, mSModuleName)
            wSource += propertySource + '\n\n'
            # main function
            wSource += 'main = quickCheck ' + propertyName
            #self.writeFile(wSource, wFileName)
        
            # TODO: use sand-box environment
            # 4. copy files to jail using ssh
            #executeOsCmd(sCommandCopy % (mSFilename + '.hs'))
            #executeOsCmd(sCommandCopy % (sSFilename + '.hs'))
            #executeOsCmd(sCommandCopy % (wFilename + '.hs'))
        
            # 5. execute wrapper file
            #print "TEST PROGRAM:\n%s"%test_code
            data = self._runHaskell(wSource)

            try:
                (exitcode, response) = data
                assert type(response) == list
                
                #print repr(''.join(response))
                # remove all special characters wirtten by runhugs/haskell 
                message = re.sub(REGEX_RUNHUGS_SPECIALS, '', ''.join(response))
                assert type(message) == type('')
                #print "EXIT CODE: %d" % exitcode
                #print "RESULT TEXT:\n%s" % output

            except:
                print "Internal error during Haskell execution: %s" % repr(data)
                return (-1, 'Internal error: Haskell execution failed.')

            # an error occured
            if exitcode != os.EX_OK:
                # delete temp files because we leave this method right now
                os.remove(mSFileName)
                os.remove(sSFileName)

                # find line number in message
                m = re.findall('.hs":(\d+)', message)
                # set line number minus two lines and return
                return (exitcode,
                    re.sub('.hs":(\d+)', '.hs":%s' % (int(m[0])-2), message))
                    
            # check wether or not a students' solution passed all tests
            else:
                failed = re.search(REGEX_FAILED, message)
                passed = re.findall(REGEX_PASSED_TESTNUMBER, message)
                
                if failed != None:
                    failed_data = re.findall(REGEX_FAILED_TESTDATA, message)
                    failed_data_str = ''
                    
                    for d in failed_data:
                        failed_data_str += d + ' ';
                    
                    resultStr += '\nYour submission failed. Test case was: %s (%s)' \
                            % (failed_data_str, propertyName)
                            
                    # get expected result for this test case
                    #expectedResult = self._getResultFor(sMModuleName, failed_data_str)
                    #studentResult = self._getResultFor(sSModuleName, failed_data_str)

                elif len(passed) != 0:
                    resultStr += '\nYour submission passed all %s tests. (%s)' \
                            % (passed[0], propertyName)
                    
                    solvedProperties.append(propertyName)

            #os.remove(wFileName)

        # 6. delete temp files
        os.remove(mSFileName)
        os.remove(sSFileName)

        # solved or not?
        if (len(solvedProperties) == len(propertyNames)):
            solved = 0
        else:
            solved = 1

        return (solved, resultStr)


    def _getResultFor(self, moduleName, test):
        """
        Returns the result of a haskell program for the given test data
        """
        
        wrapper = 'module Main where\n\nimport %s\n\nmain = putStr(show(%s))' \
                    % (moduleName, test)
        
        try:
            (exitcode, response) = self._runHaskell(wrapper)
            assert type(response) == list
                
            # remove all special characters wirtten by runhugs/haskell 
            result = re.sub(REGEX_RUNHUGS_SPECIALS, '', ''.join(response))
            assert type(result) == type('')

        except:
            return 'Internal error: %s' % result

        return result
