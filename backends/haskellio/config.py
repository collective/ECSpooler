# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2011 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.

from os.path import join, dirname, abspath

INTERPRETER = join(abspath(dirname(__file__)), 'runhugs.sh')
#INTERPRETER = join(abspath(dirname(__file__)), 'runhugs+systrace')
