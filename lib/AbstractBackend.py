# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.

import os, threading, signal
import socket, xmlrpclib
import logging

from types import StringType, UnicodeType, IntType, DictionaryType

# local imports
from AbstractServer import AbstractServer
from lib.data import BackendJob, BackendResult

from util.BackendSchema import TestEnvironment, InputField, Schema

class AbstractBackend(AbstractServer):
    """
    AbstractBackend is the basic class of all backends. It
    implements an XMLRPC server, which is waiting for check
    jobs.

    Backend implementations should be derived from this class and *must*
    implement the following method(s):

      execute(self, authdata, jobdata):) -> CheckResult
    """
    
    # must be set in subclasses !!
    id = ''
    name =  ''
    schema = None
    testSchema = None
    version = None
    
    def __init__(self, params):
        """
        @params dict with all parameters which must be set for a backend
        """
        AbstractServer.__init__(self, params['host'], params['port'])

        # FIXME:
        #self.spooler = options.get('spooler')
        self.spooler = params['capeserver']
        self.auth = params['srv_auth']
        
        assert self.id != '', 'A id is required for this backend.'
        assert self.name != '', 'A name is required for this backend.'
        assert self.version != '', 'A version is required for this backend.'

        assert self.schema != None, \
            'A input schema is required for this backend'
        assert self.testSchema != None, \
            'A test schema is required for this backend'
        
        assert self.spooler and type(self.spooler) in (StringType, UnicodeType), \
            "Backend requires a correct 'spooler' option"

        #assert self.auth and type(self.auth) == type({}), \
        assert self.auth and type(self.auth) == DictionaryType, \
            "Backend requires a correct 'srv_auth' option"

        # set spoolerID to None; it will be set if backend was successfully
        # registered to a spooler
        self._spoolerId = None


    def _registerFunctions(self):
        """
        """
        self._server.register_function(self.execute)

        #self._server.register_function(self.getId)
        #self._server.register_function(self.getName)
        self._server.register_function(self.getStatus)
        self._server.register_function(self.getInputSchema)
        self._server.register_function(self.getTestSchema)
        self._server.register_function(self.getInputFields)
        self._server.register_function(self.getTestFields)

        self._server.register_function(self.shutdown)


    def _manageBeforeStart(self):
        """Registers the backend using the given spooler data.
        """
        # FIXME:
        #return 1
        
        registered = False

        try:
            spooler = xmlrpclib.Server(self.spooler)

            logging.debug("Registering backend '%s (%s)' at spooler on '%s'" % 
                          (self.id, self.version, self.spooler))

            (code, msg) = spooler.addBackend(self.auth, 
                                             self.id, 
                                             self.name,
                                             self.version, 
                                             'http://%s:%i' % 
                                             (self.host, self.port))

            if code != 0:
                logging.error("Can't add backend to spooler: %s (%i)" % 
                              (msg, code))
            elif not msg:
                logging.error("Internal error: Spooler returned an "
                              "invalid id")
            else:
                self._spoolerId = msg
                registered = True

        except socket.error, serr:
            logging.error("Socket error: %s (%s)" % (serr, self.spooler))

        except xmlrpclib.Fault, err:
            logging.error("XMLRPC error: %s (%s)" % (err, self.spooler,))
        
        
        if not registered:
            logging.error("Can't add backend to spooler")
            #os._exit(1)
            
        return registered

    
    def _manageBeforeStop(self):
        """
        Do nothing here right now.
        """
        try:
            logging.debug("Removing backend '%s' from spooler '%s'" % 
                          ('%s %s (%s)' % (self.name, self.version, self.id), 
                           self.spooler,))

            spooler = xmlrpclib.Server(self.spooler)
            spooler.removeBackend(self.auth, self.id, self.version)

        except socket.error, serr:
            logging.error("Socket error: %s (%s)" % (serr, self.spooler))

        except xmlrpclib.Fault, err:
            logging.error("XMLRPC error: %s (%s)" % (err, self.spooler,))


    def shutdown(self, auth):
        """
        Shutting down backend; called from spooler or other client
        """
        self._authenticate(auth)
        # start a new thread to stop the backend but let this method some time
        # to return a value to the spooler or other calling client
        #logging.debug('Waiting 0.5s before stopping backend.')
        sT = threading.Timer(0.5, self._stop, (signal.SIGTERM, None))
        sT.start()

        return (1, '')


    # -- methods used by the frontends ----------------------------------------
    def getStatus(self, auth):
        """Returns a dictionary with some status information about 
        this backend. In case the authentication fails, a 
        AuthorizationFailedException will be raised.
        
        @param auth authorization information
        @return a dictionary with id, name, version, host, and port infos
        """
        self._authenticate(auth)

        return (1, {
            'pid':     os.getpid(),
            'id':      self.id,
            'name':    self.name,
            'version': self.version,
            'host':    self.host,
            'port':    self.port,
        })


    def getInputSchema(self, auth):
        """Return the input schema defined for this backend.
        @return the input schema as string
        """
        return (1, str(self.schema))
    
    
    def getTestSchema(self, auth):
        """Return the test schema defined for this backend.
        @return the test schema as string
        """
        return (1, str(self.testSchema))

        
    def getInputFields(self, auth):
        """
        Returns a dictionary where keys are the field names and the values
        are all the properties defined by the input schema.
        """
        
        result = {}
        
        for field in self.schema.fields():
            #logging.debug('Processing field: %s' % field)
            result[field.getName()] = field.getAllProperties()

        #logging.debug('Return: %s' % retval)
        return  (1, result)


    def getTestFields(self, auth):
        """
        Returns a dictionary where keys are the ids and the values are the 
        label of all definied tests in testSchema.
        
        """
        result = {}

        for test in self.testSchema.fields():
            result[test.getName()] = test.label

        return (1, result)


    # -- internal methods used by subclasses ----------------------------------
    def execute(self, authdata, jobdata):
        """This method first checks the authentication and then 
        calls _process_execute which executes the job.

        @param authdata authorization information.
        @param jobdata all relevant job data
        @return CheckResult object or tupel consisting of result code and message
        """
        # check authentication
        self._authenticate(authdata)
        
        # check the job
        return self.process_execute(jobdata)


    def process_execute(self, jobdata):
        """Executes a check job using the given job data. This method 
        must be overridden in subclasses.

        @param jobdata all relevant job data
        @return CheckResult object or tupel consisting of result code and message
        """

        raise NotImplementedError("Method 'execute' must be "
                                  "implemented by subclass")


    def _getTests(self, job):
        """
        @return a list of test specifications depending on the ids given
                in the job data
        """
        result = []
        
        #logging.debug('job: %s' % job.getData())

        if job.has_key('tests'):
            # user has selected one or more tests
            testIds = job['tests']

            for id in testIds:
                result.extend(self.testSchema.filterFields(__name__=id))
        else:
            # not tests selected by the user, taking all available
            result = self.testSchema.fields()
            
        #logging.debug('Following tests will be used %s: ' % result)
        return result


    def _authenticate(self, data):
        """
        Checks the authorization information. 
        
        @return True if given authorization data is valid, otherwise False.
        """
        #return True

        if self._spoolerId == None: 
            msg = "Authorization failed: Invalid spooler connection settings"
            logging.error(msg)
            return False

        if not data or type(data) != dict or data['srv_id'] != self._spoolerId:
            s = "Authorization failed: Invalid data"
            logging.error(s)
            return False

        return True
