# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2013 Otto-von-Guericke University Magdeburg
#
# This file is part of ECSpooler.
#
"""
Created on 28.03.2013

@author: amelung
"""
import os
import sys
import time

DEFAULT_HOST = '0.0.0.0'
RESTART_WAIT_TIME = 3

class ServiceControl(object):
    """
    """
    
    @staticmethod
    def usage():
        """
        """
        raise NotImplementedError("Please Implement this method")
        

    def __init__(self, name, port, username=None, password=None):
        """
        """
        self.name = name
        self.host = DEFAULT_HOST
        self.port = port
        self.username = username
        self.password = password
        
    def _get_auth(self):
        """
        """
        return {"username":self.username, "password":self.password}

    def _get_uri(self):
        """
        """
        return "http://%s:%i" % (self.host, self.port)
    
    def process(self, cmd):
        """
        @param cmd: 
        """
        try:
            if cmd == 'start':
                self.start()
            elif cmd == 'stop':
                self.stop()
            elif cmd == 'restart':
                self.stop()
                time.sleep(RESTART_WAIT_TIME)
                self.start()
            elif cmd == 'status':
                self.status()
            else:
                raise ValueError("Unknown command: %s" % cmd)

        except ValueError, e:
            print >> sys.stderr, e
            self.usage()
            #sys.exit(2)

        except Exception, e: 
            print >> sys.stderr, ("ERROR - %s:%s" % 
                                  (sys.exc_info()[0], e))
            #sys.exit(99)
        
    def start(self):
        """
        """
        print >> sys.stdout, "Starting %s at %s" % (self.name, time.asctime()) 
        print >> sys.stdout, "    Hostname: %s" % self.host
        print >> sys.stdout, "    Port: %i" % self.port

        self._fork()
        
    def _fork(self):
        """
        """
        try:
            if sys.platform in ['unixware7']:
                cpid = os.fork1()
            else:
                cpid = os.fork()
        except AttributeError:
            print >> sys.stdout, 'os.fork not defined - skipping it.'
            cpid = 0

        if cpid == 0:
            # child process
            self._process_child_start()
        else:
            # parent process
            self._process_parent_start()
            print >> sys.stdout, 'pid=%d' % cpid

    def _process_child_start(self):
        """
        """
        raise NotImplementedError("Please Implement this method")

    def _process_parent_start(self):
        """
        """
        raise NotImplementedError("Please Implement this method")

    def stop(self):
        """
        """
        print >> sys.stdout, "Stopping %s at %s:%d" % (self.name, self.host, self.port)
        
        self._process_stop()
        
    def _process_stop(self):
        """
        """
        raise NotImplementedError("Please Implement this method")

    def status(self):
        """
        """
        print >> sys.stdout, self._process_status()
    
    
    def _process_status(self):
        """
        """
        raise NotImplementedError("Please Implement this method")
