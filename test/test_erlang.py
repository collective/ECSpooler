# -*- coding: utf-8 -*-
# $Id$
"""
Test Python backend standalone - that means directly and without a spooler.
"""

import time
import xmlrpclib, socket

AUTH = {"username":"demo", "password":"foobar"}

BACKEND = 'erlang'

WITHOUT_ERROR = \
"""-module(fac).
-export([fac/1]).

fac(0) -> 1;
fac(N) -> N * fac(N-1);
"""

SYNTAX_ERROR = \
"""-module(fac).
-export([fac/1]).

fac(0) -> 1;
fac(N) -> N * fac(N-1);
"""

SEMANTIC_ERROR = \
"""-module(fac).
-export([fac/1]).

fac(N) -> N * fac(N-1).
"""

TEST_FUNCTION = ''

TEST_DATA = \
"""
fac:fac(2)
fac:fac(3)
"""

job = {
    'id':              repr(time.time()),
    "backend":         BACKEND,
    "modelSolution":   WITHOUT_ERROR,
    "studentSolution": WITHOUT_ERROR,
    "testFunction":    TEST_FUNCTION,
    "testData":        TEST_DATA,
    'tests':           ['simpleTest'],
    }

backend = xmlrpclib.Server("http://%s:%d" % ('sylt', 5060))

#print backend.getStatus('')
print backend.execute('', job)

#print backend.shutdown('')
