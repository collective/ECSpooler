#! /usr/bin/env python

import glob
#import os, os.path, string
from distutils.core import setup

DESCRIPTION = """\
ECSpooler is a Python XML-RPC service which provides the automatic 
assessment of submissions for ECAutoAssessmentBox.  It manages a 
submission queue and several backends.
"""

version = open('version.txt').readline()

###############################################################################

#tests = glob.glob('tests/*')

backends = ['backends/__init__.py']

backend_keywords = glob.glob('backends/keywords/*')

docs = glob.glob('*.txt')

etc = glob.glob('etc/*')

log = glob.glob('log/*')

var = glob.glob('var/*')

tests = ['tests/__init__.py',
         'tests/testKeywords.py',
         'tests/BackendTestCase.py',
         'tests/ProgrammingBackendTestCase.py',
         'tests/README.txt',
         'tests/runtests',
         'tests/testSpooler.py',
         'tests/testTestJob.py',
         'tests/testTestResult.py',
        ]

###############################################################################

setup (name="ECSpooler",
       version = version,
       license = "GPL",
       author = "Mario Amelung and Michael Piotrowski",
       author_email = "https://listserv.uni-magdeburg.de/mailman/listinfo/educomponents",
       url = "http://wdok.cs.uni-magdeburg.de/software/ecspooler",
       description = "A service for checking student submissions",
       long_description = DESCRIPTION,
       packages = ['', 'lib', 'lib/data', 'lib/util'],
       scripts = ['bin/spoolerctl', 'bin/backendctl'],
       data_files = [('backends', backends),
                     ('backends/keywords', backend_keywords),
                     ('doc', docs),
                     ('etc', etc),
                     ('log', log),
                     ('var', var),
                     ('tests', tests)]
       )
