#!/usr/bin/env python
# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.

import os, sys, time, signal, socket, xmlrpclib
import ConfigParser

def _startSpooler(host, port, pwdFile):
    """
    """
    pid = os.fork()
    if pid == 0:
        print "Starting ECSpooler on %s port %d ........." % (host, port)
        spooler = capeserver.CapeServer({'host':host, 'port':int(port), 'pwd_file':pwdFile})
        spooler.run()
    else:
        print 'pid (spoolerctl): ', pid


def _stopSpooler(host, port, AUTH):
    """
    """
    print "Stopping ECSpooler on %s port %d ........." % (host, port)
    spooler = xmlrpclib.ServerProxy("http://%s:%s" % (host, port))
    pid = spooler.getStatus(AUTH)['pid']
    
    #spooler.stop()
    os.kill(pid, signal.SIGTERM)         


def _getSpoolerStatus(host, port, auth):
    spooler = xmlrpclib.Server("http://%s:%s" % (host, port))
    return spooler.getStatus(auth)


def _getConfig(filename):

    config = ConfigParser.ConfigParser()
    config.read(filename)
    
    host = config.get('SPOOLER', 'host')
    port = config.getint('SPOOLER', 'port')
    usr = config.get('SPOOLER', 'usr')
    pwd = config.get('SPOOLER', 'pwd')
    pwdFile = homePath + '/' + config.get('SPOOLER', 'pwdFile')

    auth = {"username":usr, "password":pwd}

    return (host, port, auth, pwdFile)

# -- Main ----------------------------------------------------------------------
if __name__ == "__main__":

    if len(sys.argv) == 4:
        
        homePath = sys.argv[1]
        configFile = sys.argv[2]
        command = sys.argv[3]
        
        sys.path.insert(0, homePath + '/lib/')
        import capeserver

        (host, port, auth, pwdFile) = _getConfig(configFile)
        
        assert host and type(host) == type(''),\
            "ECSpooler requires a correct 'host' option."
        assert port and type(port) == int,\
            "ECSpooler requires a correct 'port' option."
            

        try:
            if command == 'start':
                _startSpooler(host, port, pwdFile)
    
            elif command == 'stop':
                _stopSpooler(host, port, auth)
    
            elif command == 'restart':
                _stopSpooler(host, port, auth)
                time.sleep(2)
                _startSpooler(host, port, pwdFile)
    
            elif command == 'status':
                print _getSpoolerStatus(host, port, auth)
    
            else:
                print 'Unknown command ', command

        except (socket.error, xmlrpclib.Fault), exc:
            print "Server error: %s" % exc
        except Exception, e:
            #import traceback
            #traceback.print_exc()

            print "Error: %s" % e

    else:
        print "Usage: python spoolerctl.py home_path config_file start|stop|restart|status"
