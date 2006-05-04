# -*- coding: utf-8 -*-
# $Id$
"""
Tests the spooler.
"""

import sys, time, socket
import xmlrpclib

# set authentication values
auth = {"username":"demo", "password":"foobar"}

spooler = xmlrpclib.Server("http://%s:%d" % ('aix', 5050))

print spooler.getBackendFields(auth, 'python')