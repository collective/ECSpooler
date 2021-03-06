# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2011 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
from backends import *

#from lib.data.BackendJob import BackendJob
from lib.data.BackendResult import BackendResult

from BackendTestCase import BackendTestCase

class ProgrammingBackendTestCase(BackendTestCase):
    """
    """

    def testManageCheckSyntaxResultType(self):
        """_manage_checkSyntax should always return an instance of 'BackendResult'"""
        result = None

        self.job['tests'] = ['simple']
        
        exec 'result = %s(self.params)._manage_checkSyntax(self.job)' % self.backendClassName
        
        if result:
            self.assertEqual(result.__class__, BackendResult,
                            "Result is not an instance of BackendResult; %s" % type(result))
        else:
            self.assertFalse("Result should not be 'None'")


    def testManageCheckSemanticsResultType(self):
        """_manage_checkSemantics should always return an instance of 'BackendResult'"""
        result = None
        
        self.job['tests'] = ['simple']

        exec 'result = %s(self.params)._manage_checkSemantics(self.job)' % self.backendClassName
        
        if result:
            self.assertEqual(result.__class__, BackendResult,
                            "Result is not an instance of BackendResult; %s" % type(result))
        else:
            self.assertFalse("Result should not be 'None'")

