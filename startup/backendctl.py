#!/usr/bin/env python
# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.

import os, sys, time
import xmlrpclib

import ConfigParser

PORT_CONFIG = os.path.dirname(__file__) + '/backend.ports'
BACKEND_HOST = 'localhost'

def _startBackend(spooler_host, spooler_port, backend_name, AUTH):
    """
    """
    
    backend_host = BACKEND_HOST
    backend_port = _getBackendPort(backend_name, spooler_port)
     
    pid = os.fork()
    if pid == 0:
        print 'Starting backend %s on http://%s:%d.........' % \
            (backend_name, backend_host, backend_port)
        
        backend = None
        
        if backend_name == 'python':
            backend = python_checker.PythonChecker({
                "id": python_checker.ID,
                "name": python_checker.NAME,
                "host": backend_host,
                "port": backend_port,
                "capeserver": 'http://%s:%d' % (spooler_host, spooler_port),
                "srv_auth": AUTH})

        elif backend_name == 'haskell':
            backend = haskell_checker.HaskellChecker({
                "id": haskell_checker.ID,
                "name": haskell_checker.NAME,
                "host": backend_host,
                "port": backend_port,
                "capeserver": 'http://%s:%d' % (spooler_host, spooler_port),
                "srv_auth": AUTH})

        elif backend_name == 'haskell_qc':
            backend = haskell_qc_checker.HaskellQCChecker({
                "id": haskell_qc_checker.ID,
                "name": haskell_qc_checker.NAME,
                "host": backend_host,
                "port": backend_port,
                "capeserver": 'http://%s:%d' % \
                                (spooler_host, spooler_port),
                "srv_auth": AUTH})
            
        if backend:
            backend.run()
        
    else:
        print 'pid (backendctl): ', pid


def _stopBackend(spooler_host, spooler_port, backend_name, AUTH):
    """
    """
    backend_host = BACKEND_HOST
    backend_port = _getBackendPort(backend_name, spooler_port)

    print 'Stopping backend %s on http://%s:%d.........' % \
        (backend_name, backend_host, backend_port)

    handle = xmlrpclib.ServerProxy("http://%s:%d" % (spooler_host, spooler_port))

    handle.removeChecker(AUTH, backend_name, 'http://%s:%d' % \
                          (backend_host, backend_port))

    #_deleteBackendPort(backend_name)


def _getConfig(filename):

    config = ConfigParser.ConfigParser()
    config.read(filename)
    
    host = config.get('SPOOLER', 'host')
    port = config.getint('SPOOLER', 'port')
    usr = config.get('SPOOLER', 'usr')
    pwd = config.get('SPOOLER', 'pwd')

    auth = {"username":usr, "password":pwd}

    return (host, port, auth)


def _getBackendPort(backend, spooler_port):

    config = ConfigParser.ConfigParser()
    config.read(PORT_CONFIG)

    if config.has_option(backend, 'port'):
        port = config.getint(backend, 'port')
        return port
    else:
        port = spooler_port + 1
        ports = []

        for section in config.sections():
            ports.append(config.getint(section, 'port'))
        
        while port in ports:
            port += 1
        
        _setBackendPort(backend, port)
        
        return port


def _setBackendPort(backend, port):

    config = ConfigParser.ConfigParser()
    config.read(PORT_CONFIG)
    
    if not config.has_section(backend):
        config.add_section(backend)

    config.set(backend, 'port', port)
    config.write(file(PORT_CONFIG, 'w'))

    return port
    

def _deleteBackendPort(backend):

    config = ConfigParser.ConfigParser()
    config.read(PORT_CONFIG)
    
    config.remove_section(backend)
    config.write(file(PORT_CONFIG, 'w'))


# -- Main ----------------------------------------------------------------------
if __name__ == "__main__":
    
    if len(sys.argv) == 5:
        
        homePath = sys.argv[1]
        configFile = sys.argv[2]
        backend_name = sys.argv[3]
        cmd = sys.argv[4]

        # set path for import the backends
        sys.path.insert(0, homePath + '/lib/')
        import haskell_qc_checker
        import haskell_checker
        import python_checker

        (spooler_host, spooler_port, auth) = _getConfig(configFile)

        assert spooler_host and type(spooler_host) == type(''),\
            "Backend requires a correct 'spooler host' option."
        assert spooler_port and type(spooler_port) == int,\
            "Backend requires a correct 'spooler port' option."


        try:
            if cmd == 'start':
                _startBackend(spooler_host, spooler_port, backend_name, auth)
    
            elif cmd == 'stop':
                _stopBackend(spooler_host, spooler_port, backend_name, auth)
                
            elif cmd == 'restart':
                _stopBackend(spooler_host, spooler_port, backend_name, auth)
                time.sleep(2)
    
                _startBackend(spooler_host, spooler_port, backend_name, auth)
    
            else:
                print 'unknown command ', cmd
    
        except Exception, e:
            import traceback
            traceback.print_exc()

            print "Error: %s" % e
    else:
        print "Usage: python backendctl.py home_path config_file backend_name start|stop|restart"
        