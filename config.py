# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2005-2009 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
#
import sys, os
import logging.handlers

# -- project properties -------------------------------------------------------
PROJECTNAME = 'ECSpooler'


# -- i18n properties ----------------------------------------------------------
I18N_DOMAIN = 'eduComponents'


# -- storage properties -------------------------------------------------------
DATA_DIR = os.path.join(os.path.dirname(__file__), 'var')

JOB_QUEUE_STORAGE = os.path.join(DATA_DIR, 'data.fs')
RESULT_QUEUE_STORAGE = os.path.join(DATA_DIR, 'result.fs')


# -- log properties -----------------------------------------------------------
LOG_DIR = os.path.join(os.path.dirname(__file__), 'log')
LOG_FORMAT  = logging.Formatter('%(asctime)s %(levelname)s: [%(name)s] %(message)s ' \
                                '(%(filename)s:%(lineno)d)')
#LOG_FORMAT = logging.Formatter('%(asctime)s %(levelname)s %(message)s')
LOG_LEVEL = logging.DEBUG
LOG_FILES = 7
LOG_FILESIZE = 1024 * 1024 


# -- additional properties ----------------------------------------------------
# Remove temporary files and directories created by some backends
#CLEANUP = True 
CLEANUP = False



# -- initialize ---------------------------------------------------------------

# get directory for log files and create it if necessary
if (not os.path.exists(LOG_DIR) and not os.path.isdir(LOG_DIR)):
    print "log directory '%s' does not exist - trying to create it." % (LOG_DIR)
    os.makedirs(LOG_DIR)

# set the root logger (level, format and handlers)
rch = logging.StreamHandler(sys.stdout)
rfh = logging.handlers.RotatingFileHandler(os.path.join(LOG_DIR, 'event.log'),
                                           'a', 
                                           LOG_FILESIZE, 
                                           LOG_FILES)

rch.setFormatter(LOG_FORMAT)
rfh.setFormatter(LOG_FORMAT)

logging.getLogger('').setLevel(LOG_LEVEL)
logging.getLogger('').addHandler(rfh)
#logging.getLogger('').addHandler(rch)

# define special handlers for logging spooler messages
sch = logging.StreamHandler(sys.stdout)
sfh = logging.handlers.RotatingFileHandler(os.path.join(LOG_DIR, 'spooler.log'), 
                                           'a', 
                                           LOG_FILESIZE, 
                                           LOG_FILES)

sch.setFormatter(LOG_FORMAT)
sfh.setFormatter(LOG_FORMAT)

logging.getLogger('lib.Spooler').setLevel(LOG_LEVEL)
logging.getLogger('lib.Spooler').addHandler(sfh)
#logging.getLogger('lib.Spooler').addHandler(sch)

# define special handlers for logging backend messages
bch = logging.StreamHandler(sys.stdout)
bfh = logging.handlers.RotatingFileHandler(os.path.join(LOG_DIR, 'backends.log'), 
                                           'a', 
                                           LOG_FILESIZE, 
                                           LOG_FILES)

bch.setFormatter(LOG_FORMAT)
bfh.setFormatter(LOG_FORMAT)

logging.getLogger('backends').setLevel(LOG_LEVEL)
logging.getLogger('backends').addHandler(bfh)
#logging.getLogger('backends').addHandler(bch)


# -- uid and gid for nobody ---------------------------------------------------
try:
    import pwd
    NOBODY_UID, NOBODY_GID = pwd.getpwnam('nobody')[2:4]
except ImportError:
    NOBODY_UID, NOBODY_GID = 42, 42
