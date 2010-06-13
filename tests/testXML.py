# -*- coding: utf-8 -*-
# $Id: $
#
# Copyright (c) 2010 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
import unittest

from backends.xml import XML
from lib.data.BackendJob import BackendJob
from BackendTestCase import BackendTestCase

class testXML(BackendTestCase):
    """
    """
    
    backendClassName = 'xml.XML.XML'

    jobdata = {'backend': 'xml', 
               'tests': ['default'],
               }
    
    def testWellformedInactive(self):
        """_wellFormedCheck should return nothing when not having requested it to run. Additionally, no feedback should be generated."""
        
        self.jobdata['wellFormed'] = False
        self.jobdata['submission'] = r'''<?xml version="1.0" standalone="no"?>
<!DOCTYPE hallo SYSTEM "asdkfj.dtd">
<hallo>Hallo Welt!</hallo>'''

        job = BackendJob(data=self.jobdata)
        
        backend = XML.XML(self.params)
        feedbackLength = len(backend.feedback)
        
        result = backend._wellFormedCheck(job)
        if result:
            self.fail('_wellFormedCheck should not return anything when not having specified it to run.')
        else:
            self.assertEqual(feedbackLength, len(backend.feedback), 'Feedback was generated errornously.')
    
    def testWellformedFail(self):
        """_wellFormedCheck should return a false Backendresult for a non-well-formed XML file."""
        
        self.jobdata['wellFormed'] = True
        self.jobdata['submission'] = r'''<?xml version="1.0" standalone="no"?>
<!DOCTYPE hallo SYSTEM "asdkfj.dtd">
<hallo>Hallo Welt!</hallo'''

        job = BackendJob(data=self.jobdata)
        
        backend = XML.XML(self.params)
        
        result = backend._wellFormedCheck(job)
        if result:
            self.assertEqual(result.getValue(), False, result.getMessage())
        else:
            self.fail('No result: %s' % repr(result))
            
    def testWellformedSuccess(self):
        """_wellFormedCheck should return nothing for a well-formed XML file."""
        
        self.jobdata['wellFormed'] = True
        self.jobdata['submission'] = r'''<?xml version="1.0" standalone="no"?>
<!DOCTYPE hallo SYSTEM "asdkfj.dtd">
<hallo>Hallo Welt!</hallo>'''

        job = BackendJob(data=self.jobdata)
        
        backend = XML.XML(self.params)
        feedbackLength = len(backend.feedback)
        
        result = backend._wellFormedCheck(job)
        if result:
            self.fail('_wellFormedCheck should not return anything when running on a well-formed XML file.')
        else:
            self.assertTrue(feedbackLength < len(backend.feedback), 'No feedback was generated when running on a well-formed XML file.')
            
    def testValidationFail(self):
        """_validationCheck should return false for a non-valid XML file."""
        
        self.jobdata['dtd'] = r'''<!ELEMENT hallo (#PCDATA)>'''
        
        self.jobdata['submission'] = r'''<?xml version="1.0" standalone="no"?>
<!DOCTYPE hallo SYSTEM "asdkfj.dtd">
<hola>Hallo Welt!</hola>'''

        job = BackendJob(data=self.jobdata)
        
        backend = XML.XML(self.params)
        
        result = backend._validationCheck(job)
        if result:
            self.assertEqual(result.getValue(), False, result.getMessage())
        else:
            self.fail('No result: %s' % repr(result))
            
    def testValidationSuccess(self):
        """_validationCheck should return nothing for a valid XML file."""
        
        self.jobdata['dtd'] = r'''<!ELEMENT hallo (#PCDATA)>'''
        
        self.jobdata['submission'] = r'''<?xml version="1.0" standalone="no"?>
<!DOCTYPE hallo SYSTEM "asdkfj.dtd">
<hallo>Hallo Welt!</hallo>'''

        job = BackendJob(data=self.jobdata)
        
        backend = XML.XML(self.params)
        feedbackLength = len(backend.feedback)
        
        result = backend._validationCheck(job)
        if result:
            self.fail('_validationCheck should not return anything when running on a valid XML file.')
        else:
            self.assertTrue(feedbackLength < len(backend.feedback), 'No feedback was generated when running on a valid XML file.')
            
    def testValidationDTDEmpty(self):
        """_validationCheck should return nothing when running without DTD. Additionally, no feedback should be generated"""
        
        self.jobdata['dtd'] = r''
        
        self.jobdata['submission'] = r'''<?xml version="1.0" standalone="no"?>
<!DOCTYPE hallo SYSTEM "asdkfj.dtd">
<hola>Hallo Welt!</hola>'''

        job = BackendJob(data=self.jobdata)
        
        backend = XML.XML(self.params)
        feedbackLength = len(backend.feedback)
        
        result = backend._validationCheck(job)
        if result:
            self.fail('_validationCheck should not return anything when running without a DTD.')
        else:
            self.assertEqual(feedbackLength, len(backend.feedback), 'Feedback was generated errornously.')
            
    def testXPathFail(self):
        """_xPathCheck should return false if the requested node sets or values differ."""
        
        self.jobdata['modelSolution'] = r'''<?xml version="1.0" standalone="no"?>
<hallo>Hallo Welt!</hallo>'''

        self.jobdata['xpath'] = r'''count(${DOC})
        ${DOC}/hallo'''
        
        self.jobdata['submission'] = r'''<?xml version="1.0" standalone="no"?>
<hallo>Guten Tag Welt!</hallo>'''

        job = BackendJob(data=self.jobdata)
        
        backend = XML.XML(self.params)
        
        result = backend._xPathCheck(job)
        if result:
            self.assertEqual(result.getValue(), False, result.getMessage())
        else:
            self.fail('No result: %s' % repr(result))
            
    def testXPathSuccess(self):
        """_xPathCheck should return nothing if the requested node sets or values do not differ."""
        
        self.jobdata['modelSolution'] = r'''<?xml version="1.0" standalone="no"?>
<hallo>Hallo Welt!</hallo>'''

        self.jobdata['xpath'] = r'''count(${DOC})
        ${DOC}/hallo'''
        
        self.jobdata['submission'] = r'''<?xml version="1.0" standalone="no"?>
<hallo>Hallo Welt!</hallo>'''

        job = BackendJob(data=self.jobdata)
        
        backend = XML.XML(self.params)
        feedbackLength = len(backend.feedback)
        
        result = backend._xPathCheck(job)
        if result:
            self.fail('_xPathCheck should not return anything when running on a XML file semantically equivalent to the model solution.')
        else:
            self.assertTrue(feedbackLength < len(backend.feedback), 'No feedback was generated.')
            
    def testXPathModelEmpty(self):
        """_xPathCheck should return nothing when running without a model solution. Additionally, no feedback should be generated"""
        
        self.jobdata['modelSolution'] = r''''''

        self.jobdata['xpath'] = r'''count(${DOC})
        ${DOC}/hallo'''
        
        self.jobdata['submission'] = r'''<?xml version="1.0" standalone="no"?>
<hallo>Hallo Welt!</hallo>'''

        job = BackendJob(data=self.jobdata)
        
        backend = XML.XML(self.params)
        feedbackLength = len(backend.feedback)
        print backend.feedback
        
        result = backend._xPathCheck(job)
        if result:
            self.fail('_xPathCheck should not return anything when running without a model solution.')
        else:
            print backend.feedback
            self.assertEqual(feedbackLength, len(backend.feedback), 'Feedback was generated errornously.')
            
    def testXPathXPathEmpty(self):
        """_xPathCheck should return nothing when running without a model solution. Additionally, no feedback should be generated"""
        
        self.jobdata['modelSolution'] = r'''<?xml version="1.0" standalone="no"?>
<hallo>Hallo Welt!</hallo>'''

        self.jobdata['xpath'] = r''''''
        
        self.jobdata['submission'] = r'''<?xml version="1.0" standalone="no"?>
<hallo>Hallo Welt!</hallo>'''

        job = BackendJob(data=self.jobdata)
        
        backend = XML.XML(self.params)
        feedbackLength = len(backend.feedback)
        
        result = backend._xPathCheck(job)
        if result:
            self.fail('_xPathCheck should not return anything when running without xpath expressions.')
        else:
            self.assertEqual(feedbackLength, len(backend.feedback), 'Feedback was generated errornously.')
            


def test_suite():
    """
    """
    from unittest import TestSuite, makeSuite

    suite = TestSuite()
    suite.addTest(makeSuite(testXML))
    
    return suite


# -- main ---------------------------------------------------------------------
if __name__ == '__main__':
    TestRunner = unittest.TextTestRunner
    suite = test_suite()

    TestRunner().run(suite)
