# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität, Magdeburg
#
# This file is part of ECSpooler.
import sys, logging

from lib.util.BackendSchema import InputField
from lib.util.BackendSchema import RepeatField
from lib.util.BackendSchema import Schema
from lib.util.BackendSchema import TestEnvironment

class KeywordsConf:
    """
    Defines all properties used by backend Keywords.
    """

    # input schema
    inputSchema = Schema((
        InputField(
            'inverted',
            format = 'boolean',
            label = 'Inverted matching',
            description = 'Invert the sense of matching, i.e., ' +
                          'the keywords must not appear in the text and '
                          'matches result in a lower score.',
            i18n_domain = 'EC',
        ),
        RepeatField(
            'keywords',
            #accessor = # must return a list; default is one element per line
            required = True, 
            label = 'Keywords',
            description = 'Enter one or more keywords/regexps. '+ 
                          'One keyword/regexp per line.',
            i18n_domain = 'EC',
        ),
    ))

    # testSchema
    tests = Schema((
        TestEnvironment(
            'keywords',
            label = 'Keywords',
            description = 'Exact matching of keywords.',
        ),
        TestEnvironment(
            'regexp',
            label = 'Regular Expressions',
            description = 'Regexp matching.',
        ),
    ))
