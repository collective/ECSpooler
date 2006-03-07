# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.

import os, sys, re, popen2, tempfile, threading, signal
import socket, xmlrpclib
import logging

from types import StringType, IntType, DictionaryType

# local imports
from AbstractServer import AbstractServer
from data import checkjob, checkresult
from data.exceptions import AuthorizationFailedException
from util.BackendSchema import TestEnv, InputField, Schema

class AbstractBackend(AbstractServer):
    """
    AbstractBackend is the basic class of all backends. It
    implements an XMLRPC server, which is waiting for check
    jobs requested using the execute method.

    Backend implementations *should* be derived from this class and
    implement the following method:

      execute(self, authdata, jobdata):) -> CheckResult

    This method can access the CheckJob data instance via self._job
    """
    
    # must be set in subclasses !!
    id = ''
    name =  ''
    schema = None
    
    def __init__(self, options):
        """
        """
        AbstractServer.__init__(self, options['host'], options['port'])

        # FIXME:
        #self.spooler = options.get('spooler')
        self.spooler = options['capeserver']
        self.auth = options['srv_auth']
        
        assert self.id != '', 'A Id is required for this backend.'
        assert self.name != '', 'A name is required for this backend.'
        assert self.schema != None, 'A schema is required for this backend'
        
        assert self.spooler and type(self.spooler) == StringType, \
            "Backend requires a correct 'spooler' option."

        #assert self.auth and type(self.auth) == type({}), \
        assert self.auth and type(self.auth) == DictionaryType, \
            "Backend requires a correct 'srv_auth' option."

        # set spoolerID to None; it will be set if backend was successfully
        # registered to a spooler
        self._spoolerID = None


    def _registerFunctions(self):
        """
        """
        self._server.register_function(self.execute)

        self._server.register_function(self.getId)
        self._server.register_function(self.getName)
        self._server.register_function(self.getSchema)
        self._server.register_function(self.getStatus)
        
        self._server.register_function(self.shutdown)


    def _manageBeforeStart(self):
        """Registers the backend using the given spooler data.
        """
        
        registered = False

        try:
            spooler = xmlrpclib.Server(self.spooler)

            (code, msg) = spooler.addBackend(self.auth, self.id, self.name,
                            'http://%s:%i' % (self.host, self.port))

            if code != 0:
                logging.error("Can't add backend to spooler: %s (%i)" % 
                              (msg, code))
            elif not msg:
                logging.error("Internal error: Spooler returned an "
                              "invalid id.")
            else:
                self._spoolerID = msg
                registered = True

        except socket.error, serr:
            logging.error("Socket error: %s (%s)" % (serr, self.spooler))

        except xmlrpclib.Fault, err:
            logging.error("XMLRPC error: %s (%s)" % (err, self.spooler,))
        
        
        if not registered:
            sys.exit(1)
            
    
    def _manageBeforeStop(self):
        """
        Do nothing here right now.
        """
        try:
            spooler = xmlrpclib.Server(self.spooler)
            spooler.removeBackend(self.auth, self.id)

        except socket.error, serr:
            logging.error("Socket error: %s (%s)" % (serr, self.spooler))

        except xmlrpclib.Fault, err:
            logging.error("XMLRPC error: %s (%s)" % (err, self.spooler,))


    def shutdown(self, authdata):
        """
        Shutting down backend; called from spooler or other client
        """
        self._authenticate(authdata)
        self._stop(signal.SIGTERM, None)


    def getId(self):
        """Returns the id of this backend.
        @return The backend's id
        """
        return self.id


    def getName(self):
        """Returns the name of this backend.
        @return The backend's name
        """
        return self.name


    def getSchema(self):
        """Returns the schema defined for this backend.
        @return A Schema object
        """
        return self.schema


    def getStatus(self, authdata):
        """Returns a dictionary with some status information about 
        this backend. In case the authentication fails, a 
        AuthorizationFailedException will be raised.
        
        @param authdata The authorization information
        @return A dictionary with id, name, host, and port settings
        """
        
        self._authenticate(authdata)

        return {
            'pid':  os.getpid(),
            'id':   self.id,
            'name': self.name,
            'host': self.host,
            'port': self.port,
        }


    def execute(self, authdata, jobdata):
        """Executes a check job using the fiven job data.

        @param authdata The authorization information.
        @param jobdata All relevant job data, see class CheckJob for details
        @return A CheckResult object
        """

        raise NotImplementedError("Method execute must be "
                                  "implemented by subclass.")


    def _authenticate(self, data):
        """Checks the authorization information. 
        @return True if given authorization data are valid, otherwise False.
        """
        # FIXME: 
        #return 1

        if self._spoolerID == None: 
            s = "Authorization failed: Invalid spooler connection settings."
            logging.error(msg)
            raise AuthorizationFailedException(s)

        if not data or type(data) != dict or data['srv_id'] != self._spoolerID:
            s = "Authorization failed: Invalid data."
            logging.error(s)
            raise AuthorizationFailedException(s)

        return 1
