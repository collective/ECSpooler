# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.
from os.path import join, dirname, abspath

INTERPRETER = join(abspath(dirname(__file__)), 'pl+systrace')
#INTERPRETER = join(abspath(dirname(__file__)), 'pl.sh')

COMPILER = INTERPRETER
