# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
from types import IntType

from lib.util.QueueItem import QueueItem

# TODO: rename to TestResult
class BackendResult(QueueItem):
    """
    Represents values returned from a backend.  Each result has an ID which is
    given from the original job.
    
    Required keys:
    
      - value: True/False to indicate success/failure (e.g., Haskell backend)
               or 0..100 to indicate percentage/scores (e.g., Keywords backends)
               or negative integer to indicate exceptions or errors
          
      - message: detailed information generated by the backend 
    """
    
    UNDEFINED = -42
    UNKNOWN = '?'

    def __init__(self, value=None, message=None,  data=None, id=None):
        """
        """
        if not data: data = {}
        
        if value != None: data['value'] = value
        
        if message != None: data['message'] = message
        
        QueueItem.__init__(self, data, id)
        

    def _validate(self):
        """
        Tests if all required keys are given.
        
        @see QueueItem.validate
        @return True, if everything is ok, otherwise an assertion 
        """

        assert self._data.has_key('value'), \
                "BackendResult requires a valid 'value' entry"

        assert self._data.get('message', None), \
                "BackendResult requires a valid 'message' entry"

        return True


    def setValue(self, value):
        """
        @param value: new value string
        """
        self._data['value'] = value

    
    def getValue(self):
        """
        @return result value or UNDEFINED if it is not set.
        """
        return self._data.get('value', self.UNDEFINED)


    def setMessage(self, msg):
        """
        @param msg: new message string
        """
        self._data['message'] = msg


    def getMessage(self):
        """
        @return esult message or '' if it is not set.
        """
        return self._data.get('message', self.UNKNOWN)
    

    def hasFailed(self):
        """
        Returns wether or not a job failed
        """
        
        value = self.getValue()
        
        if (type(value) == IntType):
            return (value < 0 and value != self.UNDEFINED)
        else:
            return False

# -- main ---------------------------------------------------------------------
if __name__ == "__main__":
    r1 = BackendResult(True, "Your submission passed all tests.")

    print r1
    #print r1.getValue()
    #print r1.getMessage()
    print r1.getData()
    #print r1.hasFailed()
    

#    r2 = BackendResult(-100, "Internal error")
#    print r2.hasFailed()
#
#    r3 = BackendResult(-42, "xxx")
#    print r3.hasFailed()

    r4 = BackendResult(data = {'value':'80', 'message':'You have 80%.'})
    print r4.getData()

    r5 = BackendResult(data={'value':'80', 'message':'You have 80%.'}, id='0815')
    print r5.getData()
    