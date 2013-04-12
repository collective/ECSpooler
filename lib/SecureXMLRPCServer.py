# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2011 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
#
"""Simple XML RPC server supporting SSL.

Based on this article: http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/81549
"""

import os

__CERT_PATH = os.path.join(os.path.abspath(os.path.dirname(__file__)), '..', 'certs') 
            
KEYFILE = os.path.join(__CERT_PATH, 'key.pem')
CERTFILE = os.path.join(__CERT_PATH, 'cert.pem')

import socket
import SocketServer
import BaseHTTPServer
#import SimpleHTTPServer
import SimpleXMLRPCServer

from OpenSSL import SSL

class SecureXMLRPCServer(BaseHTTPServer.HTTPServer, SimpleXMLRPCServer.SimpleXMLRPCDispatcher):
    """Secure XML-RPC server.

    It is very similar to SimpleXMLRPCServer but it uses HTTPS 
    for transporting XML data.
    """

    def __init__(self, server_address, HandlerClass, logRequests=True):
        """
        """
        self.logRequests = logRequests

        SimpleXMLRPCServer.SimpleXMLRPCDispatcher.__init__(self)
        try:
            SimpleXMLRPCServer.SimpleXMLRPCDispatcher.__init__(self)
        except TypeError:
            # An exception is raised in Python 2.5 as the prototype of 
            # the __init__ method has changed and now has 3 arguments 
            # (self, allow_none, encoding)
            SimpleXMLRPCServer.SimpleXMLRPCDispatcher.__init__(self, False, None)

        SocketServer.BaseServer.__init__(self, server_address, HandlerClass)
        ctx = SSL.Context(SSL.SSLv23_METHOD)
        ctx.use_privatekey_file(KEYFILE)
        ctx.use_certificate_file(CERTFILE)
        self.socket = SSL.Connection(ctx, socket.socket(self.address_family,
                                                        self.socket_type))
        self.server_bind()
        self.server_activate()


class SecureXMLRpcRequestHandler(SimpleXMLRPCServer.SimpleXMLRPCRequestHandler):
    """Secure XML-RPC request handler.

    It is very similar to SimpleXMLRPCRequestHandler but it uses HTTPS 
    for transporting XML data.
    """
    
    def setup(self):
        """
        """
        self.connection = self.request
        self.rfile = socket._fileobject(self.request, "rb", self.rbufsize)
        self.wfile = socket._fileobject(self.request, "wb", self.wbufsize)

    '''        
    def do_POST(self):
        """Handles the HTTPS POST request.

        It was copied out from SimpleXMLRPCServer.py and modified to shutdown the socket cleanly.
        """

        try:
            # get arguments
            data = self.rfile.read(int(self.headers["content-length"]))
            # In previous versions of SimpleXMLRPCServer, _dispatch
            # could be overridden in this class, instead of in
            # SimpleXMLRPCDispatcher. To maintain backwards compatibility,
            # check to see if a subclass implements _dispatch and dispatch
            # using that method if present.
            response = self.server._marshaled_dispatch(
                    data, getattr(self, '_dispatch', None)
                )
        except: # This should only happen if the module is buggy
            # internal error, report as HTTP server error
            self.send_response(500)
            self.end_headers()
        else:
            # got a valid XML RPC response
            self.send_response(200)
            self.send_header("Content-type", "text/xml")
            self.send_header("Content-length", str(len(response)))
            self.end_headers()
            self.wfile.write(response)

            # shut down the connection
            self.wfile.flush()
            self.connection.shutdown() # Modified here!
    '''

'''    
def test(HandlerClass = SecureXMLRpcRequestHandler,ServerClass = SecureXMLRPCServer):
    """Test xml rpc over https server"""
    class xmlrpc_registers:
        def __init__(self):
            import string
            self.python_string = string
            
        def add(self, x, y):
            return x + y
    
        def mult(self,x,y):
            return x*y
    
        def div(self,x,y):
            return x//y
        
    # Configure below
    LISTEN_HOST='127.0.0.1' # You should not use '' here, unless you have a real FQDN.
    LISTEN_PORT=5443

    server_address = (LISTEN_HOST, LISTEN_PORT) # (address, port)
    server = ServerClass(server_address, HandlerClass)    
    server.register_instance(xmlrpc_registers())    
    sa = server.socket.getsockname()
    print "Serving HTTPS on", sa[0], "port", sa[1]
    server.serve_forever()


if __name__ == '__main__':
    test()


# Here is the client for testing:
import xmlrpclib

server = xmlrpclib.Server('https://localhost:5443')
print server.add(1,2)
print server.div(10,4)
'''