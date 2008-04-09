# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2005 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
import sys, os
import logging.handlers

try:
    import pwd
except ImportError:
    pwd = None


# -- project properties -------------------------------------------------------
PROJECTNAME = 'ECSpooler'


# -- i18n properties ----------------------------------------------------------
I18N_DOMAIN = 'eduComponents'


# -- storage properties -------------------------------------------------------
JOB_QUEUE_STORAGE = os.path.join(os.path.dirname(__file__), 'var', 'data.fs')
RESULT_QUEUE_STORAGE = os.path.join(os.path.dirname(__file__), 'var', 'result.fs')


# -- log properties -----------------------------------------------------------
LOGLEVEL = logging.DEBUG

# get directory for log files and create it if necessary
logDir = os.path.join(os.path.dirname(__file__), 'log')

if not os.path.exists(logDir): os.makedirs(logDir)

# create log handlers for console, file, and syslog
cH  = logging.StreamHandler(sys.stdout)
fH  = logging.handlers.RotatingFileHandler(os.path.join(logDir, 'event.log'),
                                           'a', 1000000, 7)

longFmt  = logging.Formatter('%(asctime)s %(levelname)s: %(message)s ' \
                             '(%(filename)s:%(lineno)d)')

shortFmt = logging.Formatter('%(asctime)s %(levelname)s %(message)s')

cH.setFormatter(longFmt)
fH.setFormatter(longFmt)

# get the root logger, set log level and handlers
logging.getLogger('').setLevel(LOGLEVEL)
logging.getLogger('').addHandler(fH)
#logging.getLogger('').addHandler(cH)

# define special handlers for logging spooler and backend messages
spoolerLogHandler = logging.handlers.RotatingFileHandler(
                        os.path.join(logDir, 'spooler.log'), 'a', 1000000, 7)
spoolerLogHandler.setFormatter(longFmt)

backendLogHandler = logging.handlers.RotatingFileHandler(
                        os.path.join(logDir, 'backends.log'), 'a', 1000000, 7)
backendLogHandler.setFormatter(longFmt)

logging.getLogger('lib.Spooler').setLevel(LOGLEVEL)
logging.getLogger('lib.Spooler').addHandler(spoolerLogHandler)
#logging.getLogger('lib.Spooler').addHandler(fH)
                                            
logging.getLogger('lib.AbstractBackend').setLevel(LOGLEVEL)
logging.getLogger('lib.AbstractBackend').addHandler(backendLogHandler)
#logging.getLogger('lib.AbstractBackend').addHandler(fH)


# -- uid and gid for nobody ---------------------------------------------------
if pwd:
    NOBODY_UID, NOBODY_GID = pwd.getpwnam('nobody')[2:4]
else:
    NOBODY_UID, NOBODY_GID = 42, 42


# -- additional properties ----------------------------------------------------
# Remove temporary files and directories created by some backends
#CLEANUP = True 
CLEANUP = False
