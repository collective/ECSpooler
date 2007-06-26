#!/usr/bin/env python
# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.
"""
This module contains functions to start/stop a backend or get status 
information from a backend. __main__ reads the command lines arguments and
processes them. A configuration file must be avaiable for each backend 
with entries about spooler server's host and port as well as base port 
for the backend. If a port is already in use, we will try another one.
"""
import os, sys, time, socket, xmlrpclib
import traceback
import getopt
import logging

# add parent directory to the system path
sys.path.append(os.path.join(os.path.dirname(__file__), os.pardir))

from config import LOGLEVEL, NOBODY_UID, NOBODY_GID
from backends import *

# Run process as nobody
try:
    os.setuid(NOBODY_UID)
except os.error, e:
    print >> sys.stderr, "Cannot change uid:", e

MAX_TRIALS = 15
BACKEND_HOST = socket.getfqdn()

def _startBackend(backendId, backendPort, spoolerHost, spoolerPort, 
                   spoolerAuth):
    """
    Starts a backend with the given parameters.
    """
    backendHost = BACKEND_HOST

    try:
        exec 'id = %s.id' % (backendId,)
    except NameError, ne:
        print >> sys.stderr, "No such backend: '%s'" % (backendId,)
        raise ne
    except AttributeError, ae:
        print >> sys.stderr, "No such backend: '%s'.  Did you mean '%s'?" \
              % (backendId, backendId.capitalize(),)
        raise ae
    
    exec 'version = %s.version' % (backendId,)

    print "Starting backend '%s %s (%s)' on %s ..." % (backendId, version, id, backendHost)

    try:
        if sys.platform in ['unixware7']:
            cpid = os.fork1()
        else:
            cpid = os.fork()
    except AttributeError, aerr:
        logging.warn('os.fork not defined - skipping.')
        cpid = 0

    if cpid == 0:
        # child process
        (backend, p)= _getBackendInstance(backendId, backendHost, backendPort, 
                                     spoolerHost, spoolerPort, spoolerAuth)
        
        if backend:
            #print 'name=%s-%s' % (backend.id, backend.version)
            print 'port=%d' % p
            print backend.start()
        else:
            print 'Finally failed. See log for more information.'

    else:
        # parent process
        time.sleep(1)
        print 'pid=%d' % cpid


def _getBackendInstance(backendId, backendHost, backendPort, spoolerHost, 
                        spoolerPort, spoolerAuth):
    """
    Returns an instance of the backend class if one could be created.
    """
    
    # put together the backend module and class name using backendId
    moduleName = '%s' % \
        backendId
        #(backendId.lower()[0].upper() + backendId.lower()[1:])
    

    for i in range(backendPort, backendPort + MAX_TRIALS, 1):
    
        # get a instance, e.g., PythonBackend
        # FIXME: use spooler instead of spooler
        instanceCreateStmt = \
        "backend = %s({ \
                    'host': '%s', \
                    'port': %d, \
                    'spooler': 'http://%s:%d', \
                    'auth': %s})" \
                % (moduleName, backendHost, i, \
                   spoolerHost, spoolerPort, repr(spoolerAuth),)
                
        #print instanceCreateStmt
                   
        retval = _tryGetBackendInstance(moduleName, instanceCreateStmt)
        
        if retval: 
            return (retval, i)
        else:
            time.sleep(0.1)
    
    return None
    

def _tryGetBackendInstance(moduleName, instanceCreateStmt):
    """
    Executes the import statement for the backend class, creates an instance 
    and returns this instance.
    
    An exception occures if the port is allready in use. In this case we will
    return None.

    @param: moduleName Name of the module containing the backend class definition.
    @param: instanceCreateStmt Statement which will be executed to create a new
                              instance of the backend class.
    @return: The backend instance or None if the instance couldn't be created.
    """
    try:
        logging.debug("Trying to create instance of backend '%s'" % moduleName)
        #logging.debug(instanceCreateStmt)
        # e.g. import PythonBackend.PythonBackend
        #exec('import %s' % (moduleName,))
        exec(instanceCreateStmt)

        return backend

    # FIXME: This doesn't work on windows os -> no socket.error is thrown if
    #        port is allready in use!
    except socket.error, msg:
        return None
        

def _stopBackend(backendId, spoolerHost, spoolerPort, spoolerAuth):
    """
    Stops the backend.
    """
    backendHost = BACKEND_HOST
    
    exec 'id = %s.id' % (backendId,)
    exec 'version = %s.version' % (backendId,)

    print "Stopping backend '%s %s (%s)' on %s ..." % (backendId, version, id, backendHost)

    
    spooler = xmlrpclib.ServerProxy("http://%s:%d" % (spoolerHost, spoolerPort))
    retval = spooler.stopBackend(spoolerAuth, id)
    
    if retval:
        if retval[0]:
            print 'Done.'
        else:
            print retval[1]


def _getStatus(backendId, spoolerHost, spoolerPort, spoolerAuth):
    """
    Returns some status information of the backend.
    """
    spooler = xmlrpclib.ServerProxy("http://%s:%d" % (spoolerHost, spoolerPort))
    result = spooler.getBackendStatus(spoolerAuth, backendId)
    
    if result and result[0]:
        return result[1]
    else:
        return result


def usage():
    print "Usage: python backendctl.py [-H SPOOLER_HOSTNAME] " \
          "[-P SPOOLER_PORT] -u USERNAME -p PASSWORD " \
          "[-B BACKEND_PORT] BACKEND start|stop|restart|status"


def authError(cmd):
    print "%s requires username and password" % cmd


def main():
    """
    """

    try:
        opts, args = getopt.getopt(sys.argv[1:], "H:P:u:p:B:h", 
                               ["spoolerhost=", "spoolerport=", 
                                "user=", "password=", 
                                "backendport=",
                                "help"])
    except getopt.GetoptError:
        # print help information and exit:
        usage()
        sys.exit(2)

    spoolerHost = None
    spoolerPort = None
    user = None
    password = None
    backendPort = None
    
    for o, a in opts:
        if o in ("-h", "--help"):
            usage()
            sys.exit()

        if o in ("-H", "--spoolerhost"):
            spoolerHost = a

        if o in ("-P", "--spoolerport"):
            try:
                spoolerPort = int(a)
            except ValueError:
                usage()
                sys.exit(2)
                
        if o in ("-u", "--user"):
            user = a

        if o in ("-p", "--password"):
            password = a

        if o in ("-B", "--backendport"):
            try:
                backendPort = int(a)
            except ValueError:
                usage()
                sys.exit(2)
    
    if len(args) != 2:
        usage()
        sys.exit(2)

    else:
        backendId, cmd = args
        auth = {"username":user, "password":password}

        if not spoolerHost: spoolerHost = socket.getfqdn()
        if not spoolerPort: spoolerPort = 5050
        if not backendPort: backendPort = 5060

        if not user or not password:
            authError(cmd)
            usage()
            sys.exit(2)
        else:
            try:
                # check cmd option and do the indicated action
                if cmd == 'start':
                    _startBackend(backendId, backendPort, spoolerHost, 
                                 spoolerPort, auth)
    
                elif cmd == 'stop':
                    _stopBackend(backendId, spoolerHost, spoolerPort, auth)
                
                elif cmd == 'restart':
                    _stopBackend(backendId, spoolerHost, spoolerPort, auth)
                    time.sleep(2)
        
                    _startBackend(backendId, backendPort, spoolerHost, 
                                 spoolerPort, auth)

                elif cmd == 'status':
                    #raise NotImplementedError('Getting status information is ' +
                    #                          'not implemented yet.')
                    print _getStatus(backendId, spoolerHost, spoolerPort, auth)
    
                else:
                    print 'Unknown command %s' % cmd
                    usage()
                    
            except Exception, e:
                if LOGLEVEL == logging.DEBUG:
                    traceback.print_exc()
                    #print type(e)     # the exception instance
                    #print e.args      # arguments stored in .args
                    #print e           # __str__ allows args to printed directly
                    #print sys.exc_info()[0]

                print "Error (%s): %s" % (sys.exc_info()[0], e)

# -- Main ----------------------------------------------------------------------
if __name__ == "__main__":
    main()
