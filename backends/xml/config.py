# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2011 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.

from os.path import join, dirname, abspath

# Used to perform checks for well-formedness and validity:
XMLLINT = join(abspath(dirname(__file__)), 'xmllint.sh')

# Used to run XPath statements wrapped in a XQuery program:
SAXON = join(abspath(dirname(__file__)), 'saxon.sh')