# -*- coding: utf-8 -*-
# $Id$
"""
Test Scheme backend using spooler.
"""

import sys, time
import xmlrpclib
import socket

# set number of tests
NUM_TESTS = 1
# set authentication values
AUTH = {"username":"demo", "password":"foobar"}

# define needed values for test cases
CORRECT_SOLUTION=\
"""
(define (square x) (* x x))

(define (integers-starting-from n)
    (stream-cons n (integers-starting-from (+ 1 n))))

(define integers (integers-starting-from 1))
"""

INCORRECT_SOLUTION=\
"""
(define (square x) (* x 2))

(define (integers-starting-from n)
    (stream-cons n (integers-starting-from (+ 1 n))))

(define integers (integers-starting-from 1))
"""

TESTDATA= \
"""
square 0
square 2
square 3
square 4
square 5
"""


# start some tests   
try:
    result = {}
    jobIdPool = []

    spooler = xmlrpclib.Server('http://%s:%d' % (socket.getfqdn(), 5050))

    for i in range(NUM_TESTS):
        enq = spooler.appendJob(AUTH, 
            {
             'backend':         'scheme',
             'modelSolution':   CORRECT_SOLUTION,
             'studentSolution': INCORRECT_SOLUTION,
             'tests':           ['simple'],
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
            result = spooler.popResult(AUTH, jobId)
            #print "polling result: %s" % repr(result)

            if result.has_key(jobId):
                #print 'result for job: %s' % jobId
                #print 'exitcode: ', result[jobId][0]
                #print 'result:\n', result[jobId][1]
                print 'exitcode: ', result[jobId].get('code')
                print 'result:', result[jobId].get('value')
                
                jobIdPool.remove(jobId)
                
            else:
                print 'no result for job: %s \n' % jobId
        # end for

    # end while
except Exception, e:
    import traceback
    traceback.print_exc()
    print >> sys.stderr, str(e)
