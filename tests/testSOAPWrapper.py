# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2012 Otto-von-Guericke University Magdeburg
#
# This file is part of ECSpooler.

import traceback
import threading
import time

from suds import WebFault
from suds.client import Client

from spyne.model.complex import Array
from spyne.model.primitive import Unicode

# ==============================================================================

if __name__=='__main__':
    """
    """
    ns = 'org.educomponents.spooler.ws.dto'
    
    host = 'localhost' #'127.0.0.1'
    port = 3050
    url = 'http://%s:%i/?wsdl' % (host, port)

    try:
        c = Client(url, cache=None)
        #print c.service.getState('demo', 'foobar')
        print 'Backend Status: python'
        print c.service.getBackendStatus('demo', 'foobar', 'python')
        print ' '
        print 'Get Backends'
        print c.service.getBackends('demo', 'foobar')
        print ''
        print 'Get PID'
        print c.service.getPID('root', 'bazquux')
        print ''
        print 'Input Fields: Keywords'
        print c.service.getBackendInputFields('demo', 'foobar', 'keywords')
        print ''
        print 'Test Fields: Keywords'
        print c.service.getBackendTestFields('demo', 'foobar', 'keywords')
        
    except Exception, e:
        print traceback.format_exc()
