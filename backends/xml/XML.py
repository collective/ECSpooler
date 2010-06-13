# -*- coding: utf-8 -*-
# $Id: $
#
# Copyright (c) 2010 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.

################################################################################
#                                Changelog                                     #
################################################################################
#
# 16.06.2010, chbauman:
#        Fixed typo, removed unnecessary imports.
#        Added comments to functions.

import sys, os, re
import logging

from types import UnicodeType

# local imports
from backends.xml import config

from lib.data.BackendResult import BackendResult
from lib.util.BackendSchema import InputField
from lib.util.BackendSchema import RepeatField
from lib.util.BackendSchema import Schema
from lib.util.BackendSchema import TestEnvironment
from lib.AbstractProgrammingBackend import AbstractProgrammingBackend
from lib.AbstractProgrammingBackend import EX_OK

# define default logging
log = logging.getLogger('backends.xml')

# This is the name we use to store the dtd on the server:
dtdName = 'dtd'

# These variables indicate how we name our xml files internally:
modelSelector = '$model'
submissionSelector = '$submission'

# Wrapper template for xQuery tests:
xqlWrapperTemplate = \
'''(: Include both xml files: :)
let %s := doc("modelSolution.xml")
let %s := doc("submission.xml")

(: return-part sequence automatically inserted by the XML backend :)
return (%s)
''' % (modelSelector, submissionSelector, '%s')

# input schema
inputSchema = Schema((
    InputField(
        "wellFormed",
        format = "boolean",
        label = "Test form",
        description = "If checked, the student's submission will be tested for well-formedness.",
        i18n_domain = "EC",
    ),
    
    InputField(
        'dtd', 
        label = 'DTD',
        description = "Enter a DTD the student's submission will be automatically validated against." +
                      "If empty no validation will be performed.",
        i18n_domain = "EC",
    ),
    
    InputField(
        "modelSolution", 
        label = "Model solution",
        description = "If you want to run XPath statements on the student's submission, enter a " +
                      "model solution for this task. If no XML file is specified here, it will be " +
                      "ignored.",
        i18n_domain = "EC",
    ),
    
    RepeatField(
        "xpath", 
        label = "XPath statements",
        description = "Enter a list of XPath statements the student's submission should be " +
                      "automatically tested with.",
        i18n_domain = "EC",
    ),
))

# testSchema
tests = Schema((
    TestEnvironment(
        'default',
        label = 'Default',
        description = 'Perform tests on a XML document.',
        interpreter = config.XMLLINT,
    ),
))

class XML(AbstractProgrammingBackend):
    """
    A backend for checking XML programs by comparing
    student and model solution on given test data.
    """
    
    id = 'xml'
    name = 'XML'

    srcFileSuffix = '.xml'
    dtdFileSuffix = '.dtd'
    xqlFileSuffix = '.xql'

    schema = inputSchema
    testSchema = tests
    
    # Indicates whether the student's submission has already been written to a file.
    submissionModule = None
    
    # String used to store the test stage outcomes:
    feedback = ''
    
    def __init__(self, params, versionFile=__file__, logger = None):
        """
        This constructor is needed to reset the logging environment.
        """
        if not logger: logger = log

        AbstractProgrammingBackend.__init__(self, params, versionFile, logger)
        
        
    def _process_execute(self, job):
        """
        Overriding AbstractBackend._process_execute
        @see: AbstractBackend._process_execute(self, job)
        """
        
        try:
            log.info('Executing tests (%s)' % job.getId())
            result = self._process_doTests(job)
        except Exception, e:
            msg = 'Internal error: %s: %s' % (sys.exc_info()[0], e)
            log.error(msg)
            
            result = BackendResult(-200, msg)
            
        return result
    
    def _process_doTests(self, job):
        """
        This function is used to delegate the processing tasks.
        It is responsible for clearing old feedbacks.
        """
        
        # Clear global variables:
        self.feedback = ''
        self.submissionModule = None
        
        # Do some error handling:
        testSpecs = self._getTests(job)
        
        if len(testSpecs) == 0:
            msg = 'No test specification selected.'
            log.warn('%s, %s' % (msg, job.getId()))
            return BackendResult(-217, msg)
        
        # We ensure that we defined our schema correctly:
        inputFields = self.schema.filterFields(type='InputField')
        
        assert inputFields, 'No InputField found.'
        assert len(inputFields) == 3, 'Did not find exactly 3 InputFields, as expected. (Found %s)' % len(inputFields)
        
        repeatFields = self.schema.filterFields(type='RepeatField')
        
        assert repeatFields, 'No RepeatField found.'
        assert len(repeatFields) == 1, 'Did not find exactly 1 RepeatField, as expected. (Found %s)' % len(repeatFields)
        
        # Store the result and report it if we have a return value:
        result = self._wellFormedCheck(job)
        if result:
            return result
        
        result = self._validationCheck(job)
        if result:
            return result
        
        result = self._xPathCheck(job)
        if result:
            return result
        
        # Look at self.feedback: If it is non-empty tests were run. If not, none were run.
        if len(self.feedback) != 0:
            return BackendResult(True, self.feedback)
        else:
            return BackendResult(True, 'No test was run.')
    
    def _writeXML(self, name, content, suffix, id, dtdName='dtd'):
        """
        Uses _writeModule to write an XML file, replacing all DOCTYPE declarations using SYSTEM that
        are not inside of comments.
        
        @return: Dictionary with keys module and file
        """
        
        # We have to ensure to replace all DOCTYPE declarations using SYSTEM that are not inside
        # of comments:
        nonCommentedXML = re.sub('(?ms)<!--.*?-->', '', content) # (?ms) => multiline, dotall matching
        correctedXML = re.sub('SYSTEM\s+".*">', 'SYSTEM "%s%s">' % (dtdName, self.dtdFileSuffix), nonCommentedXML)
        
        return self._writeModule(name, correctedXML, suffix, id)
    
        
    def _wellFormedCheck(self, job):
        """
        This function checks whether the submission is well-formed, if the InputField 'wellFormed'
        is checked (because it is a boolean field and rendered as a check box).
        
        @return: If job['wellFormed'] is checked and the submission is well-formed, nothing will be
        returned, but a message will be appended to self.feedback.
        If job['wellFormed'] is checked and the submission is not well-formed, a false BackendResult
        will be returned.
        If job['wellFormed'] is not checked, nothing will be returned.
        """
        
        wellFormedCheck = job['wellFormed']
        if wellFormedCheck:
            log.info('Testing well-formedness of XML file.')
            
            submission = job['submission']
            
            assert submission and type(submission) in (str, UnicodeType), \
                "Tests require valid 'submission' (%s)" % submission
                
            try:
                # Write the module:
                if not self.submissionModule:
                    self.submissionModule = self._writeXML('submission', submission, self.srcFileSuffix, job.getId(), dtdName)
                    
                # Perform well-formed test:
                exitcode, result = self._runInterpreter(config.XMLLINT, 
                                     os.path.dirname(self.submissionModule['file']),
                                     os.path.basename(self.submissionModule['file']))
            except Exception, e:
                msg = 'Internal error during well-formed-check: %s: %s' % \
                      (sys.exc_info()[0], e)
                              
                self.log.error(msg)
                
                return BackendResult(-230, msg)
            
            if exitcode != EX_OK:
                result = "Your submission failed. It is not well-formed.\n\nRecieved result: %s" % result
                
                return BackendResult(False, result)
            else:
                # This test was passed - append a success indicator to the global feedback:
                self.feedback += "Your submission is well-formed.\n"
        else:
            log.info('Not testing well-formedness of XML file.')
    
    
    def _validationCheck(self, job):
        """
        This function checks whether the submission can be validated against a given DTD.
        
        @return: If job['dtd'] is not empty and the submission can successfully be validated against
        the DTD, nothing will be returned, but a message will be appended to self.feedback.
        If job['dtd'] is not empty and the submission cannot successfully be validated against the
        DTD, a false BackendResult will be returned.
        If job['dtd'] is empty, nothing will be returned.
        """
        
        # Check if the InputField for the DTD is non-empty. If it is, proceed, otherwise return.
        dtd = job['dtd']
        submission = job['submission']
        
        assert submission and type(submission) in (str, UnicodeType), \
        "Tests require valid 'submission' (%s)" % submission
        
        if len(dtd) != 0:
            log.info("Validating student's submission against a DTD.")
            
            try:
                dtdModule = self._writeModule(dtdName, dtd, '.dtd', job.getId())
                
                # Write the submission:
                if not self.submissionModule:
                    self.submissionModule = self._writeXML('submission', submission, self.srcFileSuffix, job.getId(), dtdName)
                    
                # Perform well-formed test:
                exitcode, result = self._runInterpreter(config.XMLLINT, 
                                     os.path.dirname(self.submissionModule['file']),
                                     os.path.basename(self.submissionModule['file']), 
                                     ('--dtdvalid', os.path.basename(dtdModule['file'])))
            except Exception, e:
                msg = 'Internal error during dtd-check: %s: %s' % \
                      (sys.exc_info()[0], e)
                              
                self.log.error(msg)
                
                return BackendResult(-230, msg)
            
            if exitcode != EX_OK:
                result = "Your submission failed. It is not valid.\n\nRecieved result: %s" % result
                
                return BackendResult(False, result)
            else:
                # This test was passed - append a success indicator to the global feedback:
                self.feedback += "Your submission was successfully validated.\n"
        else:
            log.info('Not testing against a DTD, since it was not specified.')
    
    
    def _xPathCheck(self, job):
        """
        This function checks whether the submission returns the same node sets requested by a XPath
        expression, as a model solution.
        
        @return: If both job['modelSolution'] and job['xpath'] are not empty and all stated XPath
        expressions return the same node sets in both files, nothing will be returned, but a message
        will be appended to self.feedback.
        If both job['modelSolution'] and job['xpath'] are not empty and a XPath expression does not
        return the same node sets in both files, a false BackendResult will be returned.
        If either job['modelSolution'] or job['xpath'] is empty, nothing will be returned.
        """
        
        modelSolution = job['modelSolution']
        submission = job['submission']
        xpath = job['xpath']
        
        assert submission and type(submission) in (str, UnicodeType), \
            "Tests require valid 'submission' (%s)" % submission
            
        # Perform test only if modelSolution and xpath are non-empty:
        if len(modelSolution) != 0 and len(xpath) != 0:
            log.info('Testing XPath expressions.')
            
            try:
                # Get the testData:
                repeatField = self.schema.filterFields(type='RepeatField')[0]
                testData = repeatField.getAccessor()(job[repeatField.getName()])

                if len(testData) == 0:
                    msg = 'No test data defined.'
                    self.log.warn('%s, %s' % (msg, job.getId()))
                    return BackendResult(-216, msg)
                
                # Create the test sequence:
                testSequence = ', '.join(['concat(%s = %s, "%s")' % (test.replace('${DOC}', modelSelector), test.replace('${DOC}', submissionSelector), testData.index(test)) for test in testData])
                
                # Write the XQuery file and the model solution, as well as the submission:
                xqlWrapperModule = self._writeModule('xqlWrapper', xqlWrapperTemplate % testSequence, self.xqlFileSuffix, job.getId())
                modelSolution = self._writeModule('modelSolution', job['modelSolution'], self.srcFileSuffix, job.getId())
                
                # Eventually write the submission:
                if not self.submissionModule:
                    self.submissionModule = self._writeXML('submission', submission, self.srcFileSuffix, job.getId(), dtdName)
                    
                # Perform XPath tests:
                exitcode, result = self._runInterpreter(config.SAXON, 
                                     os.path.dirname(xqlWrapperModule['file']),
                                     os.path.basename(xqlWrapperModule['file']))
            except Exception, e:
                msg = 'Internal error during xpath-check: %s: %s' % \
                      (sys.exc_info()[0], e)
                              
                self.log.error(msg)
                
                return BackendResult(-230, msg)
            
            if exitcode != EX_OK:
                result = "Your submission failed.\n\nRecieved result:\n%s" % result
                
                return BackendResult(False, result)
            elif result.find('false') != -1:
                # Ooops - we found that at least one test did not run correctly:
                matcher = re.search('(?<=false)\d+', result)
                
                # Determine which test failed and return it, deleting ${DOC}.
                falsePosition = int(matcher.group())
                result = "Your submission failed. A XPath expression evaluated against a sample solution and compared to your submission yielded a different result.\nTest case was:\n\n%s" % testData[falsePosition].replace('${DOC}', '')
                
                return BackendResult(False, result)
            else:
                # This test was passed - append a success indicator to the global feedback:
                self.feedback += "Your submission was successfully tested.\n"
        else:
            log.info('Not testing XPath expressions, since either modelSolution or xpath were empty.')
        
    
        
        
        
        
        
        
        
        
        
        
        
        
        