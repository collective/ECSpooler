# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2005 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
import os, logging, logging.handlers

# set up project
PROJECTNAME = 'ECSpooler'
PRODUCT_NAME = 'ECSpooler'

# set up i18n
I18N_DOMAIN = 'eduComponents'

# set up logging
LOGLEVEL = logging.DEBUG

logger = logging.getLogger("")  #root logger
logger.setLevel(LOGLEVEL)


# set directory for log file and create it if necessary
logDir = os.path.join(os.path.dirname(__file__), '..', 'log')

#if not os.path.exists(logDir):
#    try:
#        os.makedirs(logDir)
#    except OSError, ose:
#        pass

# create log handlers for file and syslog
fH = logging.FileHandler(os.path.join(logDir, 'status.log'))

#sLH = logging.handlers.SysLogHandler(('localhost', handlers.SYSLOG_UDP_PORT), handlers.SysLogHandler.LOG_USER)
sLH = logging.handlers.SysLogHandler()

fHFmt = logging.Formatter('%(asctime)s %(levelname)s: %(message)s ' \
                           '(%(filename)s:%(lineno)d)')

sLHFmt = logging.Formatter('%(asctime)s %(levelname)s %(message)s')

fH.setFormatter(fHFmt)
sLH.setFormatter(sLHFmt)

logger.addHandler(fH)
logger.addHandler(sLH)

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


