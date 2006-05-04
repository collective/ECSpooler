# -*- coding: utf-8 -*-
# $Id$
"""
Test Haskell backend using spooler.
"""

import sys, time
import xmlrpclib

# set number of tests
NUM_TESTS = 3
# set authentication values
AUTH = {"username":"demo", "password":"foobar"}

# define needed values for test cases
CORRECT_SOLUTION=\
"""
add3:: Int -> Int -> Int -> Int
add3 x y z = x + y + z
"""

INCORRECT_SOLUTION=\
"""
add3:: Int -> Int -> Int -> Int
add3 x y z : x + z
"""

TESTDATA= \
"""
add3 1 2 3
"""


# lets start some tests   
try:
    result = {}
    jobIdPool = []

    spooler = xmlrpclib.Server('http://%s:%d' % ('aix', 5050))

    for i in range(NUM_TESTS):
        enq = spooler.enqueue(AUTH, 
            {
             'backend':         'haskell',
             'modelSolution':   CORRECT_SOLUTION,
             'studentSolution': CORRECT_SOLUTION,
             'tests':           ['simpleTest'],
             'testFunction':    '',
             'testData':        TESTDATA,
             
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
