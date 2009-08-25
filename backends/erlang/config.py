# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.
from os.path import join, dirname, abspath

#COMPILER = join(abspath(dirname(__file__)), 'erlc')
COMPILER = join(abspath(dirname(__file__)), 'erlc.sh')

#INTERPRETER = join(abspath(dirname(__file__)), 'erl+systrace')
INTERPRETER = join(abspath(dirname(__file__)), 'erl.sh')
