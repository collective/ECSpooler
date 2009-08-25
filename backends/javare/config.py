# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.
from os.path import join, dirname, abspath

#COMPILER = join(abspath(dirname(__file__)), 'javac')
COMPILER = join(abspath(dirname(__file__)), 'javac.sh')

#INTERPRETER = join(abspath(dirname(__file__)), 'java+systrace')
INTERPRETER = join(abspath(dirname(__file__)), 'java.sh')
