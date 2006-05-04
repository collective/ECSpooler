# -*- coding: utf-8 -*-
# $Id$
"""
Test Python backend standalone - that means directly and without a spooler.
"""

import time
import xmlrpclib, socket

AUTH = {"username":"demo", "password":"foobar"}

BACKEND = 'python'

CORRECT = 'def add3(x, y, z): return (x + y + z)'
SEMANTIC_ERROR = 'def add3(x, y, z): return (x + y)'
SYNTAX_ERROR = 'def add3(x, y, z) = return (x + y)'

TEST_FUNCTION = ''

TEST_DATA = \
"""
add3(1,2,3)
add3(-1,3,0)
"""

jobData = {
    'id':              repr(time.time()),
    "backend":         BACKEND,
    "modelSolution":   CORRECT,
    "studentSolution": SEMANTIC_ERROR,
    "testFunction":    TEST_FUNCTION,
    "testData":        TEST_DATA,
    }

backend = xmlrpclib.Server("http://%s:5061" % 'sylt') #socket.getfqdn())

print backend.getStatus('')
print backend.execute('', jobData)

#print backend.shutdown('')
