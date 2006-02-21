# -*- coding: utf-8 -*-
# $Id$

import sys, time
import xmlrpclib

# set number of tests
NUM_TESTS = 2
# set authentication values
AUTH = {"username":"demo", "password":"foobar"}

# define needed values for test cases
CORRECT_STUDENT_SOLUTION=\
"""
add3:: Int -> Int -> Int -> Int
add3 x y z = x + y + z
"""

INCORRECT_STUDENT_SOLUTION=\
"""
add3:: Int -> Int -> Int -> Int
add3 x y z = x + z
"""

TESTDATA=\
"""
prop_One :: Int -> Int -> Int -> Bool
prop_One x y z = #Model#.add3 x y z == #Student#.add3 x y z

prop_Two :: Int -> Int -> Int -> Bool
prop_Two x y z = #Student#.add3 x y z == x + y + z
"""

# lets start some tests   
try:
    result = {}
    jobIdPool = []

    handle = xmlrpclib.Server("http://aix.cs.uni-magdeburg.de:5050")

    for i in range(NUM_TESTS):
        enq = handle.enqueue(AUTH, 
            {
             'checker':          'haskellqc',
             'student_solution': INCORRECT_STUDENT_SOLUTION,
             'sample_solution':  CORRECT_STUDENT_SOLUTION,
             'comparator':       '',
             'testdata':         TESTDATA,
             
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
            result = handle.pollResult(AUTH, jobId)
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
    print >> sys.stderr, str(e)
