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
#tests.remove('tests/CVS')

backends = ['backends/__init__.py']

backend_keywords = glob.glob('backends/keywords/*')
backend_keywords.remove('backends/keywords/CVS')

docs = glob.glob('*.txt')

etc = glob.glob('etc/*')
etc.remove('etc/CVS')

log = glob.glob('log/*')
log.remove('log/CVS')

var = glob.glob('var/*')
var.remove('var/CVS')

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
                     ('var', var)]
       )
