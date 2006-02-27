# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.

import os, re, popen2, tempfile
import threading

from data import *
from util.utils import *

from AbstractFPBackend import AbstractFPBackend

# FIXME: Put in Schema or get from  a config file.
INTERPRETER = '/usr/local/bin/runhugs'

TEMPLATE_SYNTAX = \
"""module Main where
%s
main = putStr(\"\")
"""

TEMPLATE_MODULE= \
"""module %s where
%s
"""

REGEX_RUNHUGS_SPECIALS = 'Type checking\n?|Parsing\n?|[Parsing]*[Dependency analysis]*[Type checking]*[Compiling]*\x08 *'

class AbstractHaskellBackend(AbstractFPBackend):
    """
    This abstract class can be used to derive subclasses for checking 
    Haskell programs.
    """
    
    def checkSyntax(self):
        """
        Checks syntax of a Haskell programm.
        @return A CheckResult object with error code and message text
        """
        assert self._job

        source = TEMPLATE_SYNTAX % (self._job["student_solution"],)

        try:
            (exitcode, response) = self._runHaskell(source)
            assert type(response) == list
            
            # remove all special characters wirtten by runhugs/haskell 
            result = re.sub(REGEX_RUNHUGS_SPECIALS, '', ''.join(response))
            assert type(result) == type('')

        except Exception, e:
            # FIXME: use log
            import traceback
            traceback.print_exc()

            print "Internal Error during syntax check: %s" % repr(e)
            return checkresult.CheckResult(20, repr(e))

        # consider exit code
        if exitcode != os.EX_OK:
            # find line number in result
            m = re.findall('.hs":(\d+)', result)
            # set line number minus two lines and return
            
            return checkresult.CheckResult(exitcode,
                    re.sub('.hs":(\d+)', '.hs":%s' % (int(m[0])-1), result))
        else:
            return checkresult.CheckResult(0, 'Syntax check succeeded.')


    def checkSemantics(self):
        """
        Checks semantic of a Haskell programs.
        
        @return A CheckResult object with error code and message text
        """
        # overwrite this method
        raise NotImplementedError("Method checkSemantics must be "
                                  "implemented by subclass")


    def _createModule(self, prefix='', source=''):
        """
        Creates a Haskell module file using a temporary name. 
        Returns module name and path to the file.
        """
        moduleName = getUniqueModuleName(prefix)
        fileName = getTempFileName(moduleName, '.hs')
        source = TEMPLATE_MODULE % (moduleName, source,)
        writeFile(source, fileName)
        
        return {'module':moduleName, 'file':fileName}


    def _runHaskell(self, source):
        """
        Runs a Haskell interpreter.

        @param source The source code to run.
        @return A tupel: (exitcode, output lines)
        """
        
        return self._runInterpreter(INTERPRETER, source, '.hs')


# -- Test section --------------------------------------------------------------
if __name__ == "__main__":
    print None