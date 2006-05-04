# -*- coding: utf-8 -*-
# $Id$
"""
Test Python backend using spooler.
"""

import sys, time, socket
import xmlrpclib

# set number of tests
NUM_TESTS = 1
# set authentication values
AUTH = {"username":"demo", "password":"foobar"}

# define needed values for test cases
WITHOUT_ERROR = 'def add3(x, y, z): return (x + y + z)'
SEMANTIC_ERROR = 'def add3(x, y, z): return (x + y)'
SYNTAX_ERROR = 'def add3(x, y, z) = return (x + y)'

TEST_DATA= \
"""
add3(1,2,3)
add3(-1,3,0)
"""


# lets start some tests   
try:
    result = {}
    jobIdPool = []

    spooler = xmlrpclib.Server("http://%s:%d" % ('aix', 5050))

    for i in range(NUM_TESTS):
        enq = spooler.enqueue(AUTH, 
            {
             'backend':         'python',
             'modelSolution':   WITHOUT_ERROR,
             'studentSolution': SYNTAX_ERROR,
             'tests':           ['simpleTest'],
             'testFunction':    '',
             'testData':        TEST_DATA,
             
            }
        )
    
        if enq[0] == 0:
            print "new job enqueued: %s" % repr(enq)
            jobIdPool.append(enq[1])
        else:
            print "couldn't enqueue new job: %s" % enq[1]

  
    while len(jobIdPool) != 0:
        
        time.sleep(1)

        for jobId in jobIdPool:
            result = spooler.pollResult(AUTH, jobId)
            #print "polling result: %s" % repr(result)
            print result

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
    print >> sys.stderr, str(e)
