# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2008 Otto-von-Guericke-University, Magdeburg
#
# This file is part of ECSpooler.

################################################################################
#                                Changelog                                     #
################################################################################
#
# 18.03.2009, chbauman:
#       'propsTemplate' now imports module 'QuickCheck' -> conditional properties can be used.
# 20.03.2009, chbauman:
#       'FAILED_RE' now matches the whole last result of QuickCheck
# 24.03.2009, chbauman:
#       corrected comments
#       corporate design: FAILED_TEST_MESSAGE and PASSED_ALL_TESTS_MESSAGE

from os.path import join, dirname, abspath

INTERPRETER = join(abspath(dirname(__file__)), 'runhugs.sh')
#INTERPRETER = join(abspath(dirname(__file__)), 'runhugs+systrace')
