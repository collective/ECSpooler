# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2013 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
#
import os
import logging

# -- project properties -------------------------------------------------------
PROJECTNAME = 'ECSpooler'


# -- i18n properties ----------------------------------------------------------
I18N_DOMAIN = 'eduComponents'


# -- storage properties -------------------------------------------------------
# Set path to job and result storage
DATA_DIR = os.path.join(os.path.dirname(__file__), 'var')

JOB_QUEUE_STORAGE = os.path.join(DATA_DIR, 'data.fs')
RESULT_QUEUE_STORAGE = os.path.join(DATA_DIR, 'result.fs')
RETRY_QUEUE_STORAGE = os.path.join(DATA_DIR, 'retry.fs')


# -- log properties -----------------------------------------------------------
# Set log dir and format
LOG_DIR = os.path.join(os.path.dirname(__file__), 'logs')
LF_LONG  = logging.Formatter('%(asctime)s %(levelname)-7s: [%(name)s] %(message)s (%(filename)s:%(lineno)d)')
LF_SHORT = logging.Formatter('%(asctime)s %(levelname)-7s: %(message)s')
LF_MESSAGE_ONLY = logging.Formatter('%(message)s')

LOG_LEVEL = logging.DEBUG#logging.INFO
LOG_FILES = 7
LOG_FILESIZE = 1024 * 1024 


# -- additional properties ----------------------------------------------------
# Remove temporary files and directories created by some backends [True|False]
CLEANUP = True 

