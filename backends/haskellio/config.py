# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.

################################################################################
#                                Changelog                                     #
################################################################################
#
# 08.05.2009, chbauman:
#       renamed method 'test' (in simpleTest.hs etc.) to 'haskell_backend_internal_equality_test'

from backends.haskell import config

INTERPRETER = config.INTERPRETER
#INTERPRETER = join(abspath(dirname(__file__)), 'runhugs+systrace')
#INTERPRETER = join(abspath(dirname(__file__)), 'runhugs.sh')
