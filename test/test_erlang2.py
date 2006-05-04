# -*- coding: utf-8 -*-
# $Id$
"""
Test Python backend standalone - that means directly and without a spooler.
"""

import sys, time
import xmlrpclib

# set number of tests
NUM_TESTS = 1
# set authentication values
auth = {"username":"demo", "password":"foobar"}
backendName = 'erlang'

WITHOUT_ERROR = \
"""-module(fac).
-export([fac/1]).

fac(0) -> 1;
fac(N) -> N * fac(N-1).
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
fac(2)
fac(3)
"""

# lets start some tests   
try:
    result = {}
    jobIdPool = []

    spooler = xmlrpclib.Server('http://%s:%d' % ('aix', 5050))

    for i in range(NUM_TESTS):
        enq = spooler.enqueue(auth, 
            {
             'backend':         backendName,
             'modelSolution':   WITHOUT_ERROR,
             'studentSolution': SEMANTIC_ERROR,
             'tests':           ['simpleTest'],
             'testFunction':    TEST_FUNCTION,
             'testData':        TEST_DATA,
             
            }
        )
    
        if enq[0] == 0:
            print "new job enqueued: %s" % repr(enq)
            jobIdPool.append(enq[1])
        else:
            print "couldn't enqueue new job: %s" % enq[1]

  
    while len(jobIdPool) != 0:
        
        time.sleep(2)

        for jobId in jobIdPool:
            result = spooler.pollResult(auth, jobId)
            print "polling result: %s" % repr(result)

            if result.has_key(jobId):
                #print 'result for job: %s' % jobId
                #print 'exitcode: ', result[jobId][0]
                #print 'result:\n', result[jobId][1]
                
                jobIdPool.remove(jobId)
                
            else:
                print 'no result for job: %s' % jobId
        # end for

    # end while
except Exception, e:
    import traceback
    traceback.print_exc()
    print >> sys.stderr, str(e)
