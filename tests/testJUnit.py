# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
import sys
import unittest

from backends.junit import JUnit
from lib.data.BackendJob import BackendJob
from ProgrammingBackendTestCase import ProgrammingBackendTestCase

class testJUnit(ProgrammingBackendTestCase):
    """
    """
    
    backendClassName = 'junit.JUnit.JUnit'

    # -- individual tests -----------------------------------------------------
    unitTests = \
"""
@Test public void encodeTestSingleChar()
{
    ${CLASS} submission = new ${CLASS}();
    assertEquals('b', submission.encode('a',1));
}

@Test public void encodeEncode()
{
    ${CLASS} submission = new ${CLASS}();
    assertEquals('a', submission.encode(submission.encode('a',5),-5));
}

@Test public void codeTestSingleString()
{
    ${CLASS} submission = new ${CLASS}();
    String str = "abc";
    assertEquals("bcd", submission.code(str,1));
}

@Test public void codeCode()
{
    ${CLASS} submission = new ${CLASS}();
    String str = "Test";
    assertEquals(str, submission.code(submission.code(str,4),-4));
}
"""

    submission = \
"""
public class Caesar 
{
    public char encode(char buchstabe, int n)
    {
        buchstabe += n;
        return buchstabe;
    }

    public String code(String text, int n)
    {
        String txt = new String("");
        for(int i = 0;i < text.length(); i ++)
        {
            txt += encode(text.charAt(i),n);
        }
        return txt;
    }
}
"""

    jobdata = {'backend': 'junit', 
               'submission': submission,
               'tests': ['JUnitTest'],
               'unitTests': unitTests,
               }


    def testSyntaxFail(self):
        """_manage_checkSyntax should return False for a syntactical incorrect program"""

        self.jobdata['submission'] = \
"""
public class Caesar 
{
    public char encode(char buchstabe, int n)
    {
        buchstabe += n;
        return buchstabe;
    }

    public String code(String text, int n)
    {
        // Error: ';' expected
        String txt = new String("")
        for(int i = 0;i < text.length(); i ++)
        {
            txt += encode(text.charAt(i),n);
        }
        return txt;
    }
}
"""
        job = BackendJob(data=self.jobdata)

        backend = JUnit.JUnit(self.params)
        result = backend._manage_checkSyntax(job)
        
        if result:
            self.assertEqual(result.getValue(), False, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSyntaxSuccess(self):
        """_manage_checkSyntax should return True for a correct program"""
        
        self.jobdata['submission'] = self.submission

        job = BackendJob(data=self.jobdata)

        backend = JUnit.JUnit(self.params)
        result = backend._manage_checkSyntax(job)
        
        if result:
            self.assertEqual(result.getValue(), True, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSemanticFailure(self):
        """_manage_checkSemantic should return False for an incorrect program"""
        
        self.jobdata['submission'] = \
"""
public class Caesar 
{
    // Error: character moved one to offen!
    public char encode(char buchstabe, int n)
    {
        buchstabe += n+1;
        return buchstabe;
    }
    
    public String code(String text, int n)
    {
        String txt = new String("");
        for(int i = 0;i < text.length(); i ++)
        {
            txt += encode(text.charAt(i),n);
        }
        return txt;
    }
}
"""

        job = BackendJob(data=self.jobdata)

        backend = JUnit.JUnit(self.params)

        # 1. we check syntax first to have a compiled class files
        result = backend._manage_checkSyntax(job)
        
        if not result:
            self.assertFalse('No result from syntax check: %s' % repr(result))

        # 2. lets do the semantic check
        result = backend._manage_checkSemantics(job)
        
        if result:
            self.assertEqual(result.getValue(), False, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))


    def testSemanticSuccess(self):
        """_manage_checkSemantic should return True for a correct program"""
        
        self.jobdata['submission'] = self.submission

        job = BackendJob(data=self.jobdata)

        backend = JUnit.JUnit(self.params)
        
        # 1. we check syntax first to have a compiled class files
        result = backend._manage_checkSyntax(job)
        
        if not result:
            self.assertFalse('No result from syntax check: %s' % repr(result))

        # 2. lets do the semantic check
        result = backend._manage_checkSemantics(job)

        if result:
            self.assertEqual(result.getValue(), True, result.getMessage())
        else:
            self.assertFalse('No result: %s' % repr(result))



def test_suite():
    """
    """
    from unittest import TestSuite, makeSuite

    suite = TestSuite()
    suite.addTest(makeSuite(testJUnit))
    
    return suite


# -- main ---------------------------------------------------------------------
if __name__ == '__main__':
    TestRunner = unittest.TextTestRunner
    suite = test_suite()

    TestRunner().run(suite)
