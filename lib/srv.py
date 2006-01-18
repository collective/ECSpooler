from SimpleXMLRPCServer import SimpleXMLRPCServer
import sys, time, socket, syslog


class XMLRPCServer(SimpleXMLRPCServer):
    """
This class is a SimpleXMLRPCServer with following modifications:

- Improved shutdown behavior
"""
        
    def server_bind(self):
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        #self.socket.settimeout(1)
        SimpleXMLRPCServer.server_bind(self)

    def serve_forever(self):
        self._log("Starting XMLRPCServer...")
        while True:
            if self.handle_request() < 0: break

    def server_close(self):
        self._log("Shutting down XMLRPCServer.")
        SimpleXMLRPCServer.server_close(self)
        


    def handle_request(self):
        try:
            request, client_address = self.get_request()
            # PROBLEM: 
            # das get_request() im SimpleXMLRPCServer fuehrt ein 
            # socket.accept() aus -> dadurch ist programmablauf
            # bis zum naechsten socket-zugriff blockiert -> sogar,
            # wenn, wenn die socket beendet wird
            # -> wir muessen ein timeout einfuehren
            self._log("Connection from %s" % client_address[0])

        except socket.error, se:
            if se.args[0] == "timed out": 
                #print "timeout"
                return 0
            else:
                #self._log("Dying with socket.error: %s"%repr(se.args))
                return -1

        if self.verify_request(request, client_address):
            try:
                self.process_request(request, client_address)
            except:
                self.handle_error(request, client_address)
                self.close_request(request)
        return 0


    def verify_request(self, request, client_address):
        # client_address: (ip, port)
        return 1


    def _log(self, msg):
        #t = time.localtime()
        #print("[%d-%02d-%02d %02d:%02d:%02d] %s"\
        #    %(t[0], t[1], t[2], t[3], t[4], t[5], msg))
        """
        """
        #sys.argv[0] = 'spooler'
        syslog.syslog('%s: %s' % (self.__class__.__name__, msg))
