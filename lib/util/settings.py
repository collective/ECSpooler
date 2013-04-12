# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2013 Otto-von-Guericke-Universität Magdeburg
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
def init_logging(name=None):
    """
    Initialize logging using root logger and specified name for log file.
    
    @param name: specified logger name which will be used as basename for log file.
    """
    
    # use root logger
    get_logger(None, name)


def get_logger(name, filename=None, level=LOG_LEVEL, maxBytes=LOG_FILESIZE, backupCount=LOG_FILES, fmt=LF_LONG):
    """
    Return logger with the given parameters
    
    @param name: specified logger name which will be used as basename of the logfile.
    @param level: log level to set.  Default: LOG_LEVEL
    @param maxBytes: max. size of the rotating logfile.  Default: LOG_FILESIZE
    @param backupCount: max. number of rotating logfiles.  Default: LOG_FILES
    @param fmt: log format to use.  Default: LOG_FORMAT
    @return:  the log handler used
    """
    log = logging.getLogger(name)
    
    # set log level
    log.setLevel(level)

    # set log file
    if filename:
        if (not os.path.exists(LOG_DIR) and not os.path.isdir(LOG_DIR)):
            os.makedirs(LOG_DIR)
        path = os.path.join(LOG_DIR, filename + '.log')
        hdlr = RotatingFileHandler(path, maxBytes=maxBytes, backupCount=backupCount)
    else:
        hdlr = logging.StreamHandler(sys.stdout)

    # set log format
    hdlr.setFormatter(fmt)
    
    # add handler
    log.addHandler(hdlr)
    
    return log