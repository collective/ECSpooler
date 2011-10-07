# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2011 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
#

import os, sys
import logging
from logging.handlers import RotatingFileHandler

from config import LOG_LEVEL, LOG_DIR, LOG_FILESIZE, LOG_FILES, LF_LONG

# uid and gid for nobody
try:
    import pwd
    NOBODY_UID, NOBODY_GID = pwd.getpwnam('nobody')[2:4]
except ImportError:
    NOBODY_UID, NOBODY_GID = 42, 42


# init logging
def init_logging(level=None, fname=None, dir=None, fsize=None, fcount=None, format=None):
    """Initialize logging.
    
    @param level: log level to set.  Default: LOG_LEVEL
    @param fname: basename of the logfile. If None, stdout will be used instead.
    @param dir: directory, where to store the log.  Default: LOG_DIR
    @param fsize: max. size of the rotating logfile. 
    @param fcount: max. number of logfiles.  Default: LOG_FILES
    @param format: log format to use.  Default: LOG_FORMAT
    @return:  the log handler used
    """
    
    # get logger
    __log = logging.getLogger()

    # log level
    if not level: level = LOG_LEVEL
    __log.setLevel(level)

    # log dir
    if not dir: dir = LOG_DIR

    # get directory for log files and create it if necessary
    if (not os.path.exists(dir) and not os.path.isdir(dir)):
        print "Log directory '%s' does not exist - trying to create it." % (dir)
        os.makedirs(dir)

    # size, count, format
    if not fsize: fsize = LOG_FILESIZE
    if not fcount: fcount = LOG_FILES
    if not format: format = LF_LONG

    # log file name and context
    if fname:
        __handler = RotatingFileHandler(os.path.join(dir, fname + '.log'), 'a', fsize, fcount)
    else:
        __handler = logging.StreamHandler(sys.stdout)

    __handler.setFormatter(format)
    __log.addHandler(__handler)
    
    return __handler

