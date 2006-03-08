# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.
import os, time, random, md5, thread, threading, signal
import socket, xmlrpclib
import logging

from types import IntType, StringType
from SimpleXMLRPCServer import SimpleXMLRPCServer, SimpleXMLRPCRequestHandler

# local imports
from config import *


class AbstractServer:
    """
    This is the abstract server class which will be used for spooler and 
    backend implementation.
    """
    
    def __init__(self, host, port):

        # set server identification
        self._srvID = \
            md5.md5(repr(random.Random().random())).hexdigest()

        self._classNAME = self.__class__.__name__

        assert host and type(host) == StringType, \
            "%s requires a correct 'host' option." % self._classNAME

        assert port and type(port) == IntType, \
            "%s requires a correct 'port' option." % self._classNAME

        # set class varia
        self.host = host
        self.port = port

        self._serverThread = None

        # create a server instance, but do not run it
        self._server = SimpleXMLRPCServer((self.host, self.port), 
                                           SimpleXMLRPCRequestHandler, False)
                                           
        # register functions (must be implemented in subclasses)
        self._registerFunctions()


    def _XMLRPCThread(self):
        """
        Run a server.
        """
        self._server.register_introspection_functions()
        self._server.serve_forever()


    def start(self):
        """Run a XMLRPCServer, but do so in a different thread.
        The main thread simply sleeps so that it can respond to signals.
        """
        
        self._manageBeforeStart()

        logging.info('Starting server thread (%s)...' % self._classNAME)
        self._serverThread = threading.Thread(target=self._XMLRPCThread)
        self._serverThread.setDaemon(1)
        self._serverThread.start()

        try:
            signal.signal(signal.SIGTERM, self._stop)
            #signal.signal(signal.SIGHUP, self._reconfig)
        except AttributeError, aerr:
            logging.warn('Maybe signal.SIGTERM is not defined - skipping.')

        
        if os.name == 'nt':
            try:
                signal.signal(signal.SIGBREAK, signal.default_int_handler)
            except AttributeError, aerr:
                logging.warn('Maybe signal.SIGBREAK is not defined - skipping.')

        try:
            while 1:
                time.sleep(0.1)
            #end while
        except KeyboardInterrupt, ki:
            logging.debug('Receiving keyboard interrupt.')
            self._stop(signal.SIGTERM, None)


    def _stop(self, signal, stack):
        """
        """
        logging.debug('Receiving signal %s, shutting down (%s).' % 
                       (signal, self._classNAME))

        self._manageBeforeStop()

        # stop server thread
        logging.info('Stopping server thread (%s)...' % self._classNAME)
        self._server.server_close()
        
        os._exit(0)

        
    def _reconfig(self, signal, stack):
        """
        Does nothing yet.
        """
        return


    def _registerFunctions(self):
        """
        Register functions to self._server
        """
        raise NotImplementedError("Method _registerFunctions must be "
                                  "implemented by subclass.")

    def _manageBeforeStart(self):
        """
        Do some necessary stuff before starting server thread.
        """
        raise NotImplementedError("Method _manageBeforeStart must be "
                                  "implemented by subclass.")


    def _manageBeforeStop(self):
        """
        Do some necessary stuff before stopping server thread.
        """
        raise NotImplementedError("Method _manageBeforeStop must be "
                                  "implemented by subclass.")
        