# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2009 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
#
import os
import random
import threading
import signal
import time
import logging

from types import IntType, StringTypes
from SimpleXMLRPCServer import SimpleXMLRPCServer, SimpleXMLRPCRequestHandler

try:
    import hashlib
except ImportError:
    import md5 as hashlib

# local imports
import config

class AbstractServer:
    """
    This is the abstract server class which will be used for spooler and 
    backend implementation.
    """

    def __init__(self, host, port, logger=None):
        """
        Creates a new XML-RPC server instance at the given host and port.
        
        @param: host: host name
        @param: port: port number
        """
        
        if logger:
            self.log = logger
        else:
            self.log = logging.getLogger()
        
        # set server identification
        self._srvId = \
            hashlib.md5(repr(random.Random().random())).hexdigest()


        self._className = self.__class__.__name__
        
        assert host and type(host) in StringTypes, \
            "%s requires a correct 'host' option" % self._className

        assert port and type(port) == IntType, \
            "%s requires a correct 'port' option" % self._className

        # set class varia
        self.host = host
        self.port = port
        
        #self.log.info('host: %s' % host)
        #self.log.info('port: %d' % port)

        self._serverThread = None

        # create a server instance, but do not run it
        self._server = SimpleXMLRPCServer((self.host, self.port), 
                                           SimpleXMLRPCRequestHandler, False)
                                           
        # register functions (must be implemented in subclasses)
        self._registerFunctions()


    def _XMLRPCThread(self):
        """
        Runs a XML-RPC server.
        """
        self._server.register_introspection_functions()
        self._server.serve_forever()


    def start(self):
        """
        Runs this XML-RPC server instance, but do so in a different thread.
        The main thread simply sleeps so that it can respond to signals.
        """
        
        if self._manageBeforeStart():

            self.log.info('Starting server thread (%s)...' % (self._className, ))

            self._serverThread = threading.Thread(target=self._XMLRPCThread)
            self._serverThread.setDaemon(1)
            self._serverThread.start()
            
            try:
                signal.signal(signal.SIGTERM, self._stop)
                #signal.signal(signal.SIGHUP, self._reconfig)
            except AttributeError:
                self.log.warn("signal.SIGTERM is not defined - skipping it.")
    
            
            if os.name == 'nt':
                try:
                    signal.signal(signal.SIGBREAK, signal.default_int_handler)
                except AttributeError:
                    self.log.warn('signal.SIGBREAK is not defined - skipping it.')
    
            try:
                # TODO: Green-IT: don't waste cpu cycles using while true: sleep(0.1)
                #signal.pause()
                while 1:
                    time.sleep(0.1)

            except KeyboardInterrupt:
                self.log.info('Received keyboard interrupt.')
                self._stop(signal.SIGTERM, None)

        else:
            return "Couldn't start server thread. See log for more details"


    def _stop(self, signal, stack):
        """
        Shuts down this XML-RPC server instance.
        
        @param: signal: the signal (TERM or KILL)
        @param: stack:
        """
        self.log.info('Received signal %s, shutting down...' % (signal, ))

        self._manageBeforeStop()

        # stop server thread
        self.log.info('Stopping server thread...')
        self._server.server_close()
        self.log.info('Exiting.')
        # TODO: os_exit doesn't clean up used sockets properly
        os._exit(0)

        
    def _reconfig(self, signal, stack):
        """
        Does nothing yet.
        
        @param: signal: the signal (?)
        @param: stack:
        """
        return


    def _registerFunctions(self):
        """
        Register functions to self._server
        """
        raise NotImplementedError("Method _registerFunctions must be "
                                  "implemented by subclass")


    def _manageBeforeStart(self):
        """
        Do some necessary stuff before starting the server.
        """
        raise NotImplementedError("Method _manageBeforeStart must be "
                                  "implemented by subclass")


    def _manageBeforeStop(self):
        """
        Do some necessary stuff before stopping the server.
        """
        raise NotImplementedError("Method _manageBeforeStop must be "
                                  "implemented by subclass")
