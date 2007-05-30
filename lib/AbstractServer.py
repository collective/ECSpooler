# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
import os, time, random, md5, thread, threading, signal
import socket, xmlrpclib
import logging

from types import IntType, StringTypes
from SimpleXMLRPCServer import SimpleXMLRPCServer, SimpleXMLRPCRequestHandler

# local imports
from config import *


class AbstractServer:
    """
    This is the abstract server class which will be used for spooler and 
    backend implementation.
    """
    
    def __init__(self, host, port):
        """
        Creates a new XML-RPC server instance at the given host and port.
        
        @param host: host name
        @param port: port number
        """

        # set server identification
        self._srvId = \
            md5.md5(repr(random.Random().random())).hexdigest()

        self._className = self.__class__.__name__

        assert host and type(host) in StringTypes, \
            "%s requires a correct 'host' option" % self._className

        assert port and type(port) == IntType, \
            "%s requires a correct 'port' option" % self._className

        # set class varia
        self.host = host
        self.port = port
        
        logging.info('host: %s' % host)
        #print >> sys.stdout, 'host:', host
        logging.info('port: %d' % port)
        #print >> sys.stdout, 'port:', port

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

            logging.info('Starting server thread (%s)...' % self._className)
            self._serverThread = threading.Thread(target=self._XMLRPCThread)
            self._serverThread.setDaemon(1)
            self._serverThread.start()
    
            try:
                signal.signal(signal.SIGTERM, self._stop)
                #signal.signal(signal.SIGHUP, self._reconfig)
            except AttributeError, err:
                logging.warn('Maybe signal.SIGTERM is not defined - skipping.')
    
            
            if os.name == 'nt':
                try:
                    signal.signal(signal.SIGBREAK, signal.default_int_handler)
                except AttributeError, err:
                    logging.warn('Maybe signal.SIGBREAK is not defined - skipping.')
    
            try:
                while 1:
                    time.sleep(0.1)
                #end while
            except KeyboardInterrupt, ki:
                logging.info('Receiving keyboard interrupt.')
                self._stop(signal.SIGTERM, None)

        else:
          return "Couldn't start server thread. See log for more details"


    def _stop(self, signal, stack):
        """
        Shuts down this XML-RPC server instance.
        
        @param signal: the signal (TERM or KILL)
        @param stack:
        """
        logging.info('Receiving signal %s, shutting down (%s).' % 
                       (signal, self._className))

        self._manageBeforeStop()

        # stop server thread
        logging.info('Stopping server thread (%s)...' % self._className)
        self._server.server_close()
        
        os._exit(0)

        
    def _reconfig(self, signal, stack):
        """
        Does nothing yet.
        
        @param signal: the signal (?)
        @param stack:
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
        