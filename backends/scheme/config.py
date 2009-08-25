# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.

from os.path import join, dirname, abspath


#INTERPRETER = join(abspath(dirname(__file__)), 'mzscheme+systrace')
INTERPRETER = join(abspath(dirname(__file__)), 'mzscheme.sh')

# construct path for stream lib
STREAM_LIB = 'streams.scm'
STREAM_LIB_PATH = join(abspath(dirname(__file__)), 'scheme_libs', STREAM_LIB)
