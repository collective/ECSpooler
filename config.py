# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2005 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
import sys, os, string, tempfile
import logging.handlers

PROJECTNAME = 'ECSpooler'
I18N_DOMAIN = 'eduComponents'

# ------ default spooler properties -------------------------------------------
DATA_DIR = os.path.join(os.path.dirname(__file__), 'var')
JOB_QUEUE_STORAGE = os.path.join(DATA_DIR, 'data.fs')
RESULT_QUEUE_STORAGE = os.path.join(DATA_DIR, 'result.fs')

# ------ logging --------------------------------------------------------------
LOG_DIR = os.path.join(os.path.dirname(__file__), 'log')
LOG_FORMAT  = logging.Formatter('%(asctime)s %(levelname)s: %(message)s ' \
                             '(%(filename)s:%(lineno)d)')
#LOG_FORMAT = logging.Formatter('%(asctime)s %(levelname)s %(message)s')
LOG_LEVEL = logging.DEBUG
LOG_FILES = 7
LOG_FILESIZE = 10 * 1024 * 1024 

# ------ backend properties ---------------------------------------------------
# Remove temporary files and directories created by some backends
CLEANUP = True

RUN_AS = 'nobody'	# if started with uid change euid to this user
RUN_AS_UID = 42		# fallback, if there is no pwd module to determine UID

# -- all available backends (import names) ------------------------------------
ALL_BACKENDS = 'erlang,haskell,haskellqc,haskellext,haskellio,java,junit,python,scheme,prolog,cl,javare,keywords'


log = None

##
# Set the default data dir to use.
# @param dir	default directory to use for storing jobs and results files. 
#               Ignored if None or doesn't exist.
def setDataDir(dir):
     if not dir:
         return

     global DATA_DIR, JOB_QUEUE_STORAGE, RESULT_QUEUE_STORAGE
     if os.path.exists(dir) and os.path.isdir(dir):
         DATA_DIR = dir;
         JOB_QUEUE_STORAGE = os.path.join(DATA_DIR, 'data.fs')
         RESULT_QUEUE_STORAGE = os.path.join(DATA_DIR, 'result.fs')
     else:
         log.warn("var directory '%s' does not exist - using default '%s'" % (dir, DATA_DIR))

##
# Get the logging.level for the given name.
# @param level	level name to convert.
# @return None if invalid, the logging.level otherwise.
def getLogLevel(level):
    if not level:
         return None

    __lvl = level.upper()
    if __lvl == "CRITICAL" or __lvl == "FATAL":
        return logging.CRITICAL
    elif __lvl == "ERROR":
        return logging.ERROR
    elif __lvl == "WARNING" or __lvl == "WARN":
        return logging.WARNING
    elif __lvl == "INFO":
        return logging.INFO
    elif __lvl == "DEBUG":
        return logging.DEBUG
    elif __lvl == "NOTSET" or __lvl == "NONE":
        return logging.NOTSET
    else:
        return None
##
# Set the default log dir to use.
# @param dir	default directory to use for storing log files. Will be created
#               if it does not already exist. Ignored if None.
def setLogDir(dir):
    if not dir:
        return

    global LOG_DIR
    if os.path.exists(dir) and os.path.isdir(dir):
	    LOG_DIR = dir
    else:
        log.warn("var directory '%s' does not exist - using default '%s'" % (dir, LOG_DIR));

    if not os.path.exists(LOG_DIR): os.makedirs(LOG_DIR)

##
# Initialize logging.
# @param level	log level to set.  Default: LOG_LEVEL
# @param fname	basename of the logfile. If None, stderr will be used instead.
# @param dir    directory, where to store the log.  Default: LOG_DIR
# @param fsize	max. size of the rotating logfile. 
# @param fcount	max. number of logfiles.  Default: LOG_FILES
# @param format	log format to use.  Default: LOG_FORMAT
# @return the log handler used
def initLogging(level = None, fname = None, dir = None, fsize = None, fcount = None, format = None):
    __log = logging.getLogger('')
    if not level: level = LOG_LEVEL
    __log.setLevel(level)
    if not dir: dir = LOG_DIR
    if not fsize: fsize = LOG_FILESIZE
    if not fcount: fcount = LOG_FILES
    if not format: format = LOG_FORMAT
    if fname:
        __handler = logging.handlers.RotatingFileHandler(
                        os.path.join(dir, fname + '.log'), 'a', fsize, fcount)
    else:
        __handler = logging.StreamHandler()

    __handler.setFormatter(format)
    __log.addHandler(__handler)
    return __handler

def cleanup(enable):
    global CLEANUP
    CLEANUP = enable and enable.upper() == "TRUE"

##
# Try to set the UID of this process to the uid of the given user/uid.
# @param user	user name to use to determine the uid
# @param uid	uid to use, if uid can't be determined from user
def runAs(user, uid):
    if os.geteuid() != 0:
        return
    try:
        import pwd
        _uid, _gid = pwd.getpwnam(user)[2:4]
    except ImportError:
        _uid, _gid = 42, 42 
   
    try:
        os.setuid(_uid)
    except os.error, e:
        log.warn("Cannot change uid: %s" % e)

##
# Set the temp directory to use. If the application is not running as root,
# this method tries to chmod 0700 the directory. Also it alters the environment
# variable TMPDIR.
# @param tmpdir		name of the directory to use for tempfiles. Ignored if None.
def setTempDir(tmpdir):
    if not tmpdir:
        return
    os.environ['TMPDIR'] = tmpdir
    if not os.path.exists(tmpdir):
        try:
            os.makedirs(tmpdir, 0755)
        except Exception, e:
            log.warn("Unable to create directory: %s - using the default '%s' instead" % (e, tempfile.gettempdir()))
        if os.geteuid() != 0 and tmpdir == tempfile.gettempdir():
            """ Too much trouble one could cause as root """
            try:
                os.chmod(tmpdir, 0711)
            except Exception, e:
                log.warn("Unable to chmod 0711 %s: %s" % (tmpdir, e))

def setWorkingDir(dir):
    if not dir:
        return
    try:
        os.chdir(dir)
    except Exception, e:
        log.warn("Unable to change working directory to '%s': %s" % (e, dir))
