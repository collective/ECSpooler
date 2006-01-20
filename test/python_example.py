#!/usr/bin/env python

import sys, time
import xmlrpclib

# set number of tests
NUM_TESTS = 3
# set authentication values
AUTH = {"username":"demo", "password":"foobar"}

# define needed values for test cases
CORRECT_STUDENT_SOLUTION=\
"""
def add3(x, y, z): return x + y + z
"""

INCORRECT_STUDENT_SOLUTION=\
"""
def add3(x, y, z): return x + y
"""

TESTDATA='add3(1,2,3)'


# lets start some tests   
try:
    result = {}
    jobIdPool = []

    handle = xmlrpclib.Server("http://xian.cs.uni-magdeburg.de:6000")

    for i in range(NUM_TESTS):
        enq = handle.enqueue(AUTH, {
            'checker':          'python',
            'student_solution': INCORRECT_STUDENT_SOLUTION,
            'sample_solution':  CORRECT_STUDENT_SOLUTION,
            'comparator':       '',
            'testdata':         TESTDATA,
        })
    
        print "new job enqueued: %s" % repr(enq)
    
        jobIdPool.append(enq[1])
        
  
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
