'''
Created on 28.02.2012

@author: amelung
'''
from lib.Backend import Backend
from lib.data.BackendResult import BackendResult
from lib.util.BackendSchema import Schema, TestEnvironment, InputField

import logging

LOG = logging.getLogger()

# input schema
inputSchema = Schema((
    InputField(
        'inverted',
        format = 'boolean',
        label = 'Inverted matching',
        description = '???',
        i18n_domain = 'EC',
    ),
))

# test environsments
tests = Schema((
    TestEnvironment(
        'regulare',
        label = '1',
        description = '1',
    ),
))

class Echo(Backend):
    """
    Class comment
    """

    id = 'echo'
    name =  'Echo von sh und ma'
    schema = inputSchema
    testSchema = tests
    

    def _process_execute(self, job):
        """
        @param job 
        """
        
        submission = job['submission']
        
        return BackendResult(True, submission)
        