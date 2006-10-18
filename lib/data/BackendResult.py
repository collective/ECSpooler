# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universit�t Magdeburg
#
# This file is part of ECSpooler.

from lib.util.QueueItem import QueueItem

class BackendResult(QueueItem):
    """
    Represents the values returned from a backend:
    
    code:  = 1 -> everthing is fine, e.g. a solution passed all tests
           = 0 -> something went wrong, e.g. a test cases was missed
           < 0 -> internal error
              
    value: a string with some more information about the result
    """
    
    PASSED = 1
    FAILED = 0
    UNDEFINED = -42

    def validate(self):
        """
        Tests if a backend is given or not.
        
        @see QueueItem.validate
        @return True, if everything is ok, otherwise an assertion 
        """
        #assert self._data['code'], \
        #        "BackendResult requires a valid 'code' entry"

        assert self._data['value'], \
                "BackendResult requires a valid 'value' entry"

        return True

    def getResultCode(self):
        """
        @return the result code or -42 if it is not set.
        """
        return self._data.get('code', self.UNDEFINED)
    

    def getResultValue(self):
        """
        @return the result value or '' if it is not set.
        """
        return self.get('value', '')


    def isFailure(self):
        """
        Returns wether or not a test has failed
        """
        return (self.getResultCode() == self.FAILED) or (self.getResultCode() < 0)
