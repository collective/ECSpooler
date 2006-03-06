# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2005 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
import sys, os, logging, logging.handlers

# -- project properties -------------------------------------------------------
PROJECTNAME = 'ECSpooler'
PRODUCT_NAME = 'ECSpooler'


# -- i18n properties ----------------------------------------------------------
I18N_DOMAIN = 'eduComponents'


# -- log properties -----------------------------------------------------------
LOGLEVEL = logging.DEBUG

logger = logging.getLogger('')  #root logger
logger.setLevel(LOGLEVEL)

# set directory for log file and create it if necessary
logDir = os.path.join(os.path.dirname(__file__), '..', 'log')

if not os.path.exists(logDir):
    os.makedirs(logDir)

# create log handlers for console, file, and syslog
cH  = logging.StreamHandler(sys.stdout)
fH  = logging.FileHandler(os.path.join(logDir, 'messages'))
sLH = logging.handlers.SysLogHandler()

longFmt  = logging.Formatter('%(asctime)s %(levelname)s: %(message)s ' \
                             '(%(filename)s:%(lineno)d)')

shortFmt = logging.Formatter('%(asctime)s %(levelname)s %(message)s')

cH.setFormatter(longFmt)
fH.setFormatter(longFmt)
sLH.setFormatter(shortFmt)

#logger.addHandler(cH)
logger.addHandler(fH)
logger.addHandler(sLH)

# defining log settings for (later) usage in a config file
#[handler_hand02]
#class=FileHandler
#level=DEBUG
#formatter=form02
#args=('status.log', 'a')
#filename=status.log
#mode=a
#
#[handler_hand05]
#class=handlers.SysLogHandler
#level=ERROR
#formatter=form05
#args=(('localhost', handlers.SYSLOG_UDP_PORT), handlers.SysLogHandler.LOG_USER)
#host=localhost
#port=SYSLOG_UDP_PORT
#facility=LOG_USER
#
#
#[formatter_form02]
#format=F2 %(asctime)s %(pathname)s(%(lineno)d): %(levelname)s %(message)s
#datefmt=
#
#[formatter_form05]
#format=F5 %(asctime)s %(levelname)s %(message)s
#datefmt=

# -- additional properties ----------------------------------------------------

