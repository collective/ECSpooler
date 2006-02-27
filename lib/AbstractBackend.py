# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.

import os, re, popen2, tempfile, threading
import socket, xmlrpclib
import logging

from types import StringType, IntType, DictionaryType

from XMLRPCServer import XMLRPCServer
from data import checkjob, checkresult
from data.exceptions import AuthorizationFailedException
from util.BackendSchema import TestEnv, InputField, Schema

class AbstractBackend(XMLRPCServer):
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

        self.host = options.get('host')
        self.port = options.get('port')
        # FIXME:
        #self.spooler = options.get('spooler')
        self.spooler = options.get('capeserver')
        self.auth = options.get("srv_auth")
        
        assert self.id != '', 'A Id is required for this backend.'
        assert self.name != '', 'A name is required for this backend.'
        assert self.schema != None, 'A schema is required for this backend'
        
        assert self.host and type(self.host) == StringType, \
            "Backend requires a correct 'host' option."

        assert self.port and type(self.port) == IntType, \
            "Backend requires a correct 'port' option."

        assert self.spooler and type(self.spooler) == StringType, \
            "Backend requires a correct 'spooler' option."

        #assert self.auth and type(self.auth) == type({}), \
        assert self.auth and type(self.auth) == DictionaryType, \
            "Backend requires a correct 'srv_auth' option."

        # set srv_id to None because it will be set in __init__ of XMLRPCServer
        self._srv_id = None

        XMLRPCServer.__init__(self, (self.host, self.port), logRequests=0)

        self.register_function(self.getId)
        self.register_function(self.getName)
        self.register_function(self.getSchema)
        self.register_function(self.getStatus)

        self.register_function(self.stop)
        self.register_function(self.execute)

        self.register_introspection_functions()


    def _authenticate(self, data):
        """Checks the authorization information. 
        @return True if given authorization data are valid, otherwise False.
        """
        # FIXME: 
        return 1

        if self._srv_id == None: 
            s = "Authorization failed: Invalid spooler connection settings."
            logging.error(msg)
            raise AuthorizationFailedException(s)

        if not data or type(data) != dict or data.get("srv_id") != self._srv_id:
            s = "Authorization failed: Invalid data."
            logging.error(s)
            raise AuthorizationFailedException(s)

        return True


    def run(self):
        """Registers the backend using the given spooler data and 
        runs it 'forever'.
        @return False in case of an error, True instead.
        """

        # 1. register this backend at ecspooler
        try:
            spooler = xmlrpclib.Server(self.spooler)

            (code, msg) = spooler.addChecker(self.auth, self.id, self.name,
                            'http://%s:%i' % (self.host, self.port))

            if code != 0:
                logging.error("Can't add backend to spooler: %s (%i)" % (msg, code))
                return 0
            elif not msg:
                logging.error("Internal Error: Spooler returned an invalid id.")
                return 0
            else:
                self._srv_id = msg

        except socket.error, e:
            logging.error("Socket Error: %s (%s)" % (e, self.spooler))
            return 0

        except xmlrpclib.Fault, e:
            logging.error("XMLRPC Error: %s (%s)" % (e, self.spooler,))
            return 0

        # 2. run this backend
        self.serve_forever()
        return 1


    def stop(self, authdata):
        """Stops the backend. In case the authentication fails, a 
        AuthorizationFailedException will be raised.
        
        @param authdata The authorization information
        """
        self._authenticate(authdata)

        self.server_close()
        return 1

    
    def getId(self):
        """Returns the id of this backend.
        @return The backend's id
        """
        return self.id + self.get

    def getName(self):
        """Returns the name of this backend.
        @return The backend's name
        """
        return self.name + self.get

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
            'id': self.id,
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
