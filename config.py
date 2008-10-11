# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2005 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
import sys, os, string
import logging.handlers

try:
    import pwd
except ImportError:
    pwd = None


# -- project properties -------------------------------------------------------
PROJECTNAME = 'ECSpooler'


# -- i18n properties ----------------------------------------------------------
I18N_DOMAIN = 'eduComponents'

# -- all available backends (import names) ------------------------------------
ALL_BACKENDS = 'erlang,haskell,haskellqc,haskellext,haskellio,java,junit,python,scheme,prolog,cl,javare,keywords'

# -- default storage properties -----------------------------------------------
if os.getenv('EC_SRVC') == 'SPOOLER':
     __vdir = os.path.join(os.path.dirname(__file__), 'var')
     __tmp = os.getenv('VARDIR')
     if __tmp:
         if os.path.exists(__tmp) and os.path.isdir(__tmp):
	     	__vdir = __tmp
         else:
             print "WARNING: var directory '%s' does not exist - using default '%s'" % (__tmp, __vdir)
    
     JOB_QUEUE_STORAGE = os.path.join(__vdir, 'data.fs')
     RESULT_QUEUE_STORAGE = os.path.join(__vdir, 'result.fs')


# -- default log properties ---------------------------------------------------
LOGLEVEL = logging.DEBUG
__tmp = os.getenv('LOGLEVEL')
if __tmp:
    __lvl = __tmp.upper()
    if __lvl == "CRITICAL" or __lvl == "FATAL":
        LOGLEVEL = logging.CRITICAL
    elif __lvl == "ERROR":
        LOGLEVEL = logging.ERROR
    elif __lvl == "WARNING" or __lvl == "WARN":
        LOGLEVEL = logging.WARNING
    elif __lvl == "INFO":
        LOGLEVEL = logging.INFO
    elif __lvl == "DEBUG":
        LOGLEVEL = logging.DEBUG
    elif __lvl == "NOTSET" or __lvl == "NONE":
        LOGLEVEL = logging.NOTSET
    else:
        print "WARNING: Invalid loglevel %s ignored." % (__tmp)

# get directory for log files and create it if necessary
logDir = os.path.join(os.path.dirname(__file__), 'log')
__tmp = os.getenv('LOGDIR')
if __tmp:
    if os.path.exists(__tmp) and os.path.isdir(__tmp):
		logDir = __tmp
    else:
        print "WARNING: var directory '%s' does not exist - using default '%s'" % (__tmp, logDir);
	
if not os.path.exists(logDir): os.makedirs(logDir)

# create log handlers for console, file, and syslog
#cH  = logging.StreamHandler(sys.stdout)
#fH  = logging.handlers.RotatingFileHandler(os.path.join(logDir, 'event.log'),
#                                           'a', 1000000, 7)

longFmt  = logging.Formatter('%(asctime)s %(levelname)s: %(message)s ' \
                             '(%(filename)s:%(lineno)d)')

shortFmt = logging.Formatter('%(asctime)s %(levelname)s %(message)s')

#cH.setFormatter(longFmt)
#fH.setFormatter(longFmt)

# get the root logger, set log level and handlers
logging.getLogger('').setLevel(LOGLEVEL)
# no need to duplicate messages
#logging.getLogger('').addHandler(fH)
#logging.getLogger('').addHandler(cH)

# define special handlers for logging spooler and backend messages
__tmp = os.getenv('EC_SRVC')
if not __tmp:
	print "WARNING: environment variable EC_SRVC needs to be set"
elif __tmp == 'SPOOLER':
    spoolerLogHandler = logging.handlers.RotatingFileHandler(
                        os.path.join(logDir, 'spooler.log'), 'a', 1000000, 7)
    spoolerLogHandler.setFormatter(longFmt)
    logging.getLogger('lib.Spooler').setLevel(LOGLEVEL)
    logging.getLogger('lib.Spooler').addHandler(spoolerLogHandler)
else:
	# TODO: should we make this fool proof (i.e. allow simple basenames only)
    backendLogHandler = logging.handlers.RotatingFileHandler(
                   os.path.join(logDir, os.getenv('EC_SRVC').lower() + '.log'),
                   'a', 1000000, 7)
    backendLogHandler.setFormatter(longFmt)
    logging.getLogger('lib.AbstractBackend').setLevel(LOGLEVEL)
    logging.getLogger('lib.AbstractBackend').addHandler(backendLogHandler)

# -- uid and gid for nobody ---------------------------------------------------
if pwd:
    NOBODY_UID, NOBODY_GID = pwd.getpwnam('nobody')[2:4]
else:
    NOBODY_UID, NOBODY_GID = 42, 42


# -- additional properties ----------------------------------------------------
# Remove temporary files and directories created by some backends
CLEANUP = True

__tmp = os.getenv('CLEANUP');
if __tmp and __tmp.upper() == "FALSE":
    CLEANUP = False; 
