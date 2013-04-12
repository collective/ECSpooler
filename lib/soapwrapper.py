# -*- coding: utf-8 -*-
# $Id$
#
import os
#import sys
#import time
import logging
#import threading
#from threading import Thread
#from urllib2 import URLError

#from copy import deepcopy

#from types import StringTypes

# Python build-in xmlrpclib
import xmlrpclib

# spyne
from spyne.decorator import srpc, rpc
from spyne.service import ServiceBase
from spyne.model.complex import Array
#from spyne.model.primitive import Integer
from spyne.model.primitive import Unicode
#from spyne.model.primitive import String
#from spyne.model.primitive import AnyDict
#from spyne.model.primitive import Mandatory

from spyne.util.simple import wsgi_soap11_application

try:
    from wsgiref.simple_server import make_server
except ImportError:
    print("Error: example server code requires Python >= 2.5")
    raise

# local imports
from lib.data.dto import TARGET_NAMESPACE
from lib.data.dto import SpoolerState, State
#from lib.data.dto import BackendStatus
from lib.data.dto import Backend
from lib.data.dto import BackendInputField
from lib.data.dto import BackendTestField
from lib.data.dto import DictElem
from lib.data.dto import Answer

def set_spooler_host(host):
    """
    """
    global __host
    __host = host

def set_spooler_port(port):
    """
    """
    global __port
    __port = port

def _get_spooler_proxy():
    """
    """
    return xmlrpclib.ServerProxy("http://%s:%s" % (__host, __port))


def _get_user_auth_dict(username, password):
    """
    """
    return {"username":username, "password":password}

class SoapWrapper(ServiceBase):
    """
    """

    @rpc(Unicode, Unicode, _returns=State)
    def getPID(self, username, password):
        """
        Returns the process ID

        @param authdata: username and password for authentication
        @return: current process ID
        """
        #sp = _get_spooler_proxy()
        #state = sp.getStatus(_get_user_auth_dict(username, password))

        result = State()
        result.pid = os.getpid()

        return result
    

    @srpc(Unicode, Unicode, _returns=SpoolerState)
    def getState(username, password): #@NoSelf
        """
        Returns status information such aus PID, number of tests and
        result in the queue as well as a list with IDs of all registered 
        backends.
        
        @param username: 
        @param password:
        
        @throws Exception:
        """
        sp = _get_spooler_proxy()
        state = sp.getStatus(_get_user_auth_dict(username, password))
        
        result = SpoolerState()
        result.backends = state.get('backends', [])
        result.queueCount = state.get('queue', 0)
        result.resultCount = state.get('results', 0)

        return result

    
    @srpc(Unicode, Unicode, _returns=Array(Backend))
    #@srpc(Unicode, Unicode, _returns=AnyDict)
    def getBackends(username, password): #@NoSelf        
        
        sp = _get_spooler_proxy()
        backends = sp.getBackends(_get_user_auth_dict(username, password))
        
        result = []
    
        for key, values in backends.iteritems():
            #print name
            #print values
            backend = Backend()
            backend.name    = values['name']
            #backend.url     = values['url'] 
            #backend.isBusy  = values['isBusy']
            backend.version = values['version']
            backend.id      = values['id']
            result.append(backend)
            
        return result
   

    @srpc(Unicode, Unicode, Unicode, _returns=Array(BackendInputField))
    def getBackendInputFields(username, password, backendId): #@NoSelf
        
        sp = _get_spooler_proxy()
        fields = sp.getBackendInputFields(_get_user_auth_dict(username, password), backendId)
    
        result = []
        
        for key, values in fields.iteritems():
            inputField = BackendInputField()
            
            inputField.name                = key
            inputField.description         = values['description']
            inputField.format              = values['format']
            inputField.required            = values['required']
            inputField.label               = values['label']
            inputField.languageIndependent = values['languageIndependent']
            inputField.i18n_domain         = values['i18n_domain']
            inputField.type                = values['type']
            if values.has_key('accessor'):
                inputField.accessor        = values['accessor']
            else:
                inputField.accessor        = ''
                
            result.append(inputField)
            
        return result
    
    @srpc(Unicode, Unicode, Unicode, _returns=Array(BackendTestField))
    def getBackendTestFields(username, password, backendId): #@NoSelf
    
        sp = _get_spooler_proxy()
        fields = sp.getBackendTestFields(_get_user_auth_dict(username, password), backendId)
    
        result = []
        
        for key, value in fields.iteritems():
            testField = BackendTestField()
            
            testField.id   = key
            testField.name = value
            
            result.append(testField)
            
        return result
    
    @srpc(Unicode, Unicode, _returns=Array(Answer))
    def pullJobs(username, password): #@NoSelf
        
        sp = _get_spooler_proxy()
        results = sp.getResults(_get_user_auth_dict(username, password))
    
        answers = []
        
        for key, value in results.iteritems():
            answer = Answer()
            answer.id      = value['id']
            answer.message = value['message']
            answer.value   = value['value']
            answers.append(answer)
            
        return answers
    
    
    @srpc(Unicode, Unicode, Unicode, _returns=Answer)
    def pull(username, password, jobId): #@NoSelf
        
        sp = _get_spooler_proxy()
        result = sp.pull(_get_user_auth_dict(username, password), jobId)
        
        answer = Answer()
        
        for key, value in result.iteritems():
            answer.id      = value['id']
            answer.message = value['message']
            answer.value   = value['value']
        
        return answer
    
    
    
    @srpc(Unicode, Unicode, Unicode, Unicode, Array(Unicode), Array(DictElem), _returns=Unicode)
    def push(username, password, submission, backend, tests, options): #@NoSelf
        
        sp = _get_spooler_proxy()
        
        # create job data dict with mandatory keys
        jobdata = {'backend' : backend,
                   'submission' : submission, 
                   'tests' : tests
                   }
        
        # add options 
        for x in options:
            jobdata.update({x.key : x.value})
        
        append = sp.push(_get_user_auth_dict(username, password), jobdata)
        
        return append
    
    
# ==============================================================================
if __name__ == '__main__':
    """
    """
    #logging.basicConfig(level=logging.INFO)
    #logging.getLogger('spyne.protocol.xml').setLevel(logging.INFO)
    
    host = '0.0.0.0'
    port = 3050
    
    set_spooler_host("0.0.0.0")
    set_spooler_port(5050)


    logging.info("listening to http://%s:%d" % (host, port))
    logging.info("wsdl is at: http://localhost:%d/?wsdl" % (port))

    wsgi_app = wsgi_soap11_application([SoapWrapper], TARGET_NAMESPACE)
    server = make_server(host, port, wsgi_app)
    
    logging.info("pid = %s" % os.getpid())
    
    server.serve_forever()
