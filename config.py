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

logger = logging.getLogger('')  # root logger
logger.setLevel(LOGLEVEL)

# set directory for log file and create it if necessary
logDir = os.path.join(os.path.dirname(__file__), 'log')

if not os.path.exists(logDir):
    os.makedirs(logDir)

# create log handlers for console, file, and syslog
cH  = logging.StreamHandler(sys.stdout)
fH  = logging.handlers.RotatingFileHandler(os.path.join(logDir, 'messages'),
                                           'a', 1000000, 7)
#sLH = logging.handlers.SysLogHandler()

longFmt  = logging.Formatter('%(asctime)s %(levelname)s: %(message)s ' \
                             '(%(filename)s:%(lineno)d)')

shortFmt = logging.Formatter('%(asctime)s %(levelname)s %(message)s')

cH.setFormatter(longFmt)
fH.setFormatter(longFmt)
#sLH.setFormatter(shortFmt)

#logger.addHandler(cH)
logger.addHandler(fH)
#logger.addHandler(sLH)

# -- uid and gid for nobody ---------------------------------------------------
if pwd:
    NOBODY_UID, NOBODY_GID = pwd.getpwnam('nobody')[2:4]
else:
    NOBODY_UID, NOBODY_GID = 42, 42

# -- additional properties ----------------------------------------------------
CLEANUP = True # Remove the temporary directories created by some backends

