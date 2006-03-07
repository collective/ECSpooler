#!/usr/bin/env python
# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.

import os, sys, time, signal, socket, xmlrpclib, getopt
import logging

sys.path.insert(0, os.path.join(os.path.dirname(__file__),  '..', 'lib'))
import config

def _startSpooler(host, port, pwdFile):
    """
    """
    print "Starting ECSpooler on %s port %d ........." % (host, port)

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
        import Spooler

        spooler = Spooler.Spooler(host, int(port), pwdFile)
        spooler.start()
    else:
        # parent process
        time.sleep(1)
        print 'pid=%d' % cpid


def _stopSpooler(host, port, auth):
    """
    """
    print "Stopping ECSpooler on %s port %d ........." % (host, port)
    spooler = xmlrpclib.ServerProxy("http://%s:%s" % (host, port))
    pid = spooler.getStatus(auth)['pid']
    
    try:
        # stops the spooler sending kill -15 process-id
        os.kill(pid, signal.SIGTERM)         
    except AttributeError, aerr:
        logging.warn('os.kill and/or signal.SIGTERM not defined. '
                      'Trying to stop spooler elsewhere .')
        # FIXME:
        #spooler.shutdown(auth)


def _getSpoolerStatus(host, port, auth):
    """
    """
    spooler = xmlrpclib.Server("http://%s:%s" % (host, port))
    return spooler.getStatus(auth)


def usage():
    print "Usage: python spoolerctl.py [-H HOSTNAME] [-P PORT] [-u USERNAME] " \
          "[-p PASSWORD] start|stop|restart|status"


def authError(cmd):
    print "Command '%s' requires username and password." % cmd


def main():    
    """
    """
    try:
        opts, args = getopt.getopt(sys.argv[1:], "H:P:u:p:h", 
                               ["host=", "port=", "user=", "password=", "help"])
    except getopt.GetoptError:
        # print help information and exit:
        usage()
        sys.exit(2)

    host = None
    port = None
    user = None
    password = None
    
    for o, a in opts:
        if o in ("-h", "--help"):
            usage()
            sys.exit()

        if o in ("-H", "--host"):
            host = a

        if o in ("-P", "--port"):
            try:
                port = int(a)
            except ValueError:
                usage()
                sys.exit(2)
                
        if o in ("-u", "--user"):
            user = a

        if o in ("-p", "--password"):
            password = a

    
    if len(args) != 1:
        usage()
        sys.exit(2)

    else:
        command = args[0]
        
        if not host: host = socket.getfqdn()
        if not port: port = 5050
        
        pwdFile = os.path.join(os.path.dirname(__file__), '..', 'etc', 'passwd')
        
        
        try:
            if command == 'start':
                _startSpooler(host, port, pwdFile)
    
            else:
                if not user or not password:
                    authError(command)
                    usage()
                    sys.exit(2)
                else:
                    auth = {"username":user, "password":password}
                
                    if command == 'stop':
                        _stopSpooler(host, port, auth)
    
                    elif command == 'restart':
                        _stopSpooler(host, port, auth)
                        time.sleep(2)
                        _startSpooler(host, port, pwdFile)
    
                    elif command == 'status':
                        print _getSpoolerStatus(host, port, auth)
    
                    else:
                        print 'Unknown command %s' % cmd
                        usage()

        except (socket.error, xmlrpclib.Fault), exc:
            if config.LOGLEVEL == logging.DEBUG:
                import traceback
                traceback.print_exc()

            print "Server error: %s: %s" % (sys.exc_info()[0], exc)

        except Exception, e:
            if config.LOGLEVEL == logging.DEBUG:
                import traceback
                traceback.print_exc()
                #print type(e)     # the exception instance
                #print e.args      # arguments stored in .args
                #print e           # __str__ allows args to printed directly
                #print sys.exc_info()[0]
            print "Error (%s): %s" % (sys.exc_info()[0], e)


# -- Main ----------------------------------------------------------------------
if __name__ == "__main__":
    main()
            