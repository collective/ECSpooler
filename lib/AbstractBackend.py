# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2011 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
#
import traceback

import sys, os, threading, signal
import logging
import socket, xmlrpclib

from os.path import join, dirname, abspath
from types import StringTypes, DictionaryType

# local imports
from lib.AbstractServer import AbstractServer
from lib.data.BackendJob import BackendJob
from lib.data.BackendResult import BackendResult

from lib.util import errorcodes

class AbstractBackend(AbstractServer):
    """
    AbstractBackend is the basic class of all backends.  It implements an 
    XMLRPC server, which is waiting for test jobs.

    Backend implementations should be derived from this class by implementing 
    the _process_execute method and defining their own input and test schema.
    """
    # resourcestring
    #ERROR_AUTH_FAILED = "Authorization failed"
    
    # set in subclasses !!
    id = ''
    name = ''
    schema = None
    testSchema = None
    version = ''
    
    def __init__(self, params, versionFile=__file__, logger=None):
        """
        @params dict with all parameters which must be set for a backend
        """
        global LOG
        
        if logger:
            LOG = logger
        else:
            LOG = logging.getLogger(self.id)

        AbstractServer.__init__(self,
                                params.get('host', None),
                                params.get('port', None),
                                LOG)
        
        
        try:
            # set version from backend's version.txt
            self.version = file(join(abspath(dirname(versionFile)), 'version.txt'), 'r').read()
        except IOError, ioe:
            # write warn message, but do nothing else
            LOG.warn(ioe)
            
        assert self.id != '', 'A valid ID is required for this backend.'
        assert self.name != '', 'A valid name is required for this backend.'
        #assert self.version != '', 'A valid version is required for this backend.'

        # we use current machine’s IP address
        self.url = 'http://%s:%d' % (socket.gethostbyname(socket.gethostname()), self.port) #(self.host, self.port))

        assert self.schema != None, \
            'A input schema is required for this backend'
        assert self.testSchema != None, \
            'A test schema is required for this backend'
        
        self.spooler = params.get('spooler', None)
        self.auth = params.get('auth', None)

        assert self.spooler and type(self.spooler) in StringTypes, \
            "Backend requires a correct 'spooler' option"

        #assert self.auth and type(self.auth) == type({}), \
        assert self.auth and type(self.auth) == DictionaryType, \
            "Backend requires a correct 'auth' option"

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
        try:
            spooler = xmlrpclib.Server(self.spooler)

            LOG.debug("Registering backend '%s (%s)' at '%s'" % 
                           (self.id, self.version, self.spooler))
            
            spoolerId = spooler.addBackend(self.auth,
                                           self.id,
                                           self.name,
                                           self.version,
                                           self.url)

            self._spoolerId = spoolerId
            
            return True

        except Exception, e:
            LOG.error("Can't add backend to spooler: %s: %s" % 
                           (sys.exc_info()[0], e))
            
        return False

    
    def _manageBeforeStop(self):
        """
        @see: AbstractServer._manageBeforeStop()
        """
        try:
            LOG.debug("Removing backend '%s (%s)' from ECSpooler '%s'" % 
                          (self.id, self.url, self.spooler,))

            spooler = xmlrpclib.Server(self.spooler)
            spooler.removeBackend(self.auth, self.id, self.url)

        except socket.error, serr:
            LOG.warn("Socket error: %s (%s)" % (serr, self.spooler))
            #traceback.print_exc()

        except xmlrpclib.Fault, err:
            LOG.warn("XMLRPC error: %s (%s)" % (err, self.spooler,))
            #traceback.print_exc()


    # -- public methods used by backendctl -------------------------------------

    # xmlrpc
    def shutdown(self, auth, backendId):
        """
        Shutting down the backend.  This method is called from spooler or 
        another client. Authentication is required.
        
        @return: a tuple
        """
        if not self._authenticate(auth):
            #return (-210, self.ERROR_AUTH_FAILED)
            raise Exception(errorcodes.ERROR_AUTH_FAILED)
        
        backendId = backendId.lower()
        
        if backendId != self.id:
            msg = "No such backend '%s' at '%s:%d'" % \
                   (backendId, self.host, self.port)
            
            LOG.warn(msg)
            raise Exception(msg)

        
        LOG.debug("Calling 'self._stop(%s, %s)'" % (signal.SIGTERM, None))
 
        # start a new thread to stop the backend but let this method some time
        # to return a value to the spooler or other calling client
        sT = threading.Timer(0.5, self._stop, (signal.SIGTERM, None))
        sT.start()

        return "Signal '%s' sent" % signal.SIGTERM


    # -- public methods (for spoolers/frontends) ------------------------------
    
    def getStatus(self, auth, backendId):
        """
        Returns a dictionary with some status information about 
        this backend. In case the authentication fails, a 
        AuthorizationFailedException will be raised.
        
        @param: auth authorization information
        @return: a dictionary with backendId, name, version, host, and port infos
        """
        # FIXME: dirty hack!
        if auth and not self._authenticate(auth):
            #return(-210, self.ERROR_AUTH_FAILED)
            raise Exception(errorcodes.ERROR_AUTH_FAILED)
        
        backendId = backendId.lower()
        
        if backendId != self.id:
            msg = "No such backend '%s' at '%s:%d'" % \
                   (backendId, self.host, self.port)
            
            LOG.warn(msg)
            raise Exception(msg)

        return {
            'pid':     os.getpid(),
            'id':      self.id,
            'name':    self.name,
            'version': self.version,
            'host':    self.host,
            'port':    self.port,
            'spooler': self.spooler,
        }


    def getInputSchema(self, auth):
        """
        Return the input schema defined for this backend.
        
        @return: the input schema as string
        """
        # FIXME: add authentification
        return str(self.schema)
    
    
    def getTestSchema(self, auth):
        """
        Return the test schema defined for this backend.
        
        @return: the test schema as string
        """
        # FIXME: add authentification
        return str(self.testSchema)

        
    def getInputFields(self, auth):
        """
        Returns a dictionary where keys are the field names and the values
        are all the properties defined by the input schema.
        """
        # FIXME: add authentification
        
        result = {}
        
        for field in self.schema.fields():
            #log.debug('Processing field: %s' % field)
            result[field.getName()] = field.getAllProperties()

        #log.debug("Return value: %s" % result)
        return result


    def getTestFields(self, auth):
        """
        Returns a dictionary where keys are the ids and the values are the 
        label of all definied tests in testSchema.
        """
        # FIXME: add authentification
        
        result = {}

        for test in self.testSchema.fields():
            result[test.getName()] = test.label

        return result


    def execute(self, authdata, jobdata):
        """
        Call _process_execute which executes the job.

        @param: authdata: authorization information
        @param: jobdata: all relevant job data
        @return: a dictionary with at least result value and message
        """
        # check authentication
        if not self._authenticate(authdata):
            #return(-210, self.ERROR_AUTH_FAILED)
            raise Exception(errorcodes.ERROR_AUTH_FAILED)
        
        # create a test job with the given data
        try:
            job = BackendJob(data=jobdata)

            # start testing
            result = self._process_execute(job)
            
        except Exception, e:
            LOG.error(traceback.format_exc())
            msg = 'Internal error: %s: %s' % (sys.exc_info()[0], e)
            result = BackendResult(-200, msg)

            
        return result.getData()


    # -- methodes which must be overwritten in subclasses ---------------------
    def _process_execute(self, job):
        """
        Executes a test job with the given test data.  This method must be 
        overridden in subclasses.

        @param: job: a BackendJob object with all relevant test data
        @return: a BackendResult object with at least result value and message
        """

        raise NotImplementedError("Method 'process_execute' must be "
                                  "implemented by subclass")


    # -- internal or private methodes (used or overwritten in subclasses) -----
    def _getTests(self, job):
        """
        @return: a list of test specifications depending on the ids given
                in the job data
        """
        result = []
        
        #log.debug('job: %s' % job.getData())

        if job.has_key('tests'):
            # user has selected one or more tests
            testIds = job['tests']

            for testId in testIds:
                result.extend(self.testSchema.filterFields(__name__=testId))
        #else:
        #    # not tests selected by the user, taking all available
        #    result = self.testSchema.fields()
            
        #log.debug('Following tests will be used %s: ' % result)
        return result


    def _authenticate(self, data):
        """
        Checks the authorization information. 
        
        @return: True if given authorization data is valid, otherwise False.
        """
        #return True

        if self._spoolerId == None: 
            LOG.error("Authorization failed: Invalid spooler connection settings")
            return False

        if not data or (type(data) != dict):
            LOG.error("Authorization failed: Invalid data")
            return False

        if data.has_key('srv_id') and data['srv_id'] == self._spoolerId:  
            return True
        
        if (data == self.auth):
            return True

        return False
