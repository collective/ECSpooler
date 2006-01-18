# -*- coding: UTF-8 -*-
# $Id$
#
# Copyright (c) 2005 Otto-von-Guericke-University, Magdeburg
#
# This file is part of ECSpooler.

import os, re, popen2, tempfile
import threading

from data import *
from util.utils import *

import checker

# resourcestring
PROCESS_WAIT_TIME = 15
HASKELL_INTERPRETER = 'systrace -a -f /opt/ecspooler/.systrace/opt_hugs_bin_runhugs /opt/hugs/bin/runhugs'

REGEX_FAILED = '(?m)Falsifiable, after \d+ tests?:'
REGEX_FAILED_TESTDATA = '\n(-?.+)'
REGEX_PASSED_TESTNUMBER = 'passed (\d+)'
REGEX_LINENUMBER = ':\d+'
DEFAULT_MODEL_MODULE_NAME = '#Model#'
DEFAULT_STUDENT_MODULE_NAME =  '#Student#'

REGEX_RUNHUGS_SPECIALS = 'Type checking\n?|Parsing\n?|[Parsing]*[Dependency analysis]*[Type checking]*[Compiling]*\x08 *'

ID = 'haskell_qc'
NAME = 'Haskell QuickCheck'

class HaskellQCChecker(checker.Checker):
    """
    Simple checker class for checking Haskell programs using QuickCheck.
    This class is inherited from Checker (see checker.py):
    """
    

    def checkSyntax(self):
        """
        Checks syntax of a Haskell function.
        @return A CheckResult object with error code and message text
        """
        assert self._job

        sSSource = 'module Main where\n\n' + self._job["student_solution"] + \
                   '\n\n' + 'main = putStr(\"\")'

        data = self._runHaskell(sSSource)
        
        try:
            (exitcode, response) = data
            assert type(response) == list

            #print repr(''.join(response))
            # remove all special characters wirtten by runhugs/haskell 
            message = re.sub(REGEX_RUNHUGS_SPECIALS, '', ''.join(response))
            assert type(message) == type('')

        except:
            return checkresult.CheckResult(20,
                   "Implementation Error - _runHaskell returns %s" % repr(data))

        # consider exit code
        if exitcode != os.EX_OK:
            # find line number in message
            m = re.findall('.hs":(\d+)', message)
            # set line number minus two lines and return
            return checkresult.CheckResult(exitcode,
                    re.sub('.hs":(\d+)', '.hs":%s' % (int(m[0])-2), message))
        else:
            return checkresult.CheckResult(0, "Haskell syntax check succeeded.")


    def checkSemantics(self):
        """
        Checks syntax and semantic of Haskell programs.
        @return A CheckResult object with error code and message text
        """
        assert self._job
        
        resultStr = ''
        
        modelSource = self._job["sample_solution"]
        studentSource = self._job["student_solution"]
        propertySource = self._job["testdata"]

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
        
            # 5. exceute wrapper file
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
                print "Internal Error during Haskell execution: %s" % repr(data)
                return checkresult.CheckResult(
                    -1, "Haskell execution failed - internal error.")

            # an error occured
            if exitcode != os.EX_OK:
                # delete temp files because we leave this method right now
                os.remove(mSFileName)
                os.remove(sSFileName)

                # find line number in message
                m = re.findall('.hs":(\d+)', message)
                # set line number minus two lines and return
                return checkresult.CheckResult(exitcode,
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

        return checkresult.CheckResult(solved, resultStr)


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
            return 'Internal Error: %s' % result

        return result


    def _runHaskell(self, program, suffix='.hs'):
        """
        Runs a program in prolog.

        @param program the source code to run
        @return (exitcode, output lines)
        """
        
        fname = tempfile.mktemp(suffix = suffix)

        fd = open(fname, "w")
        fd.write(program)
        fd.write("\n")
        fd.close()

        # TODO: use sand-box environment
        # copy files to jail using ssh
        #executeOsCmd(sCommandCopy % (mSFilename + '.hs'))
        #executeOsCmd(sCommandCopy % (sSFilename + '.hs'))
        #executeOsCmd(sCommandCopy % (wFilename + '.hs'))

        # Popen4 will provide both stdout and stderr on handle.fromchild
        handle = popen2.Popen4('%s %s' % (HASKELL_INTERPRETER, fname))
        handle.tochild.close()
        # we don't expect to send on stdin; instead we just wait for the process 
        # to end, or kill it.

        def interruptProcess():
            print 'Aborting ' + HASKELL_INTERPRETER + ' : SIGTERM -> %i' % handle.pid
            os.kill(handle.pid, 15)

        timer = threading.Timer(PROCESS_WAIT_TIME, interruptProcess)
        timer.start()
        exitcode = handle.wait()
        timer.cancel()

        if exitcode == 15:
            # process has been interrupted by timer
            os.remove(fname)
            return (15, ['Program aborted after %i seconds' % PROCESS_WAIT_TIME])
            
        buf = handle.fromchild.readlines()
        handle.fromchild.close()
        os.remove(fname)

        return (handle.poll(), buf)


# -- Test section --------------------------------------------------------------
if __name__ == "__main__":
    x = HaskellQCChecker({
            "name":"haskell_qc",
            "host":"lls.cs.uni-magdeburg.de",
            "port":8002,
            "capeserver":"http://lls.cs.uni-magdeburg.de:8000",
            "srv_auth":{"username":"demo", "password":"foobar"}})

    x.execute(
        {"username":"demo", "password":"foobar"},
        {'student_solution': 'add3 :: Int -> Int-> Int -> Int\n', 
         'checker': 'haskell_qc', 
         'comparator': 'prop_One :: Int -> Int -> Int -> Bool\nprop_One x y z = #Model#.add3 x y z == #Student#.add3 x y z\n\nprop_Two :: Int -> Int -> Int -> Bool\nprop_Two x y z = #Student#.add3 x y z == x + y + z', 
         'id': '1127830545.5163281', 
         'sample_solution': 'add3 x y z = (+) x ((+) y z)'})
