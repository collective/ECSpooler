# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.

from lib.util.QueueItem import QueueItem

#TODO: rename to TestJob
class BackendJob(QueueItem):
    """
    Represents a job which will be managed by the spooler and handed
    over to the specified backend (getBackend).  Each job has an ID.
    
    Required keys:
    
      - backend: a backend's name
      
      - submission: text, program etc. which will be tested
      
    All other keys are specified by the backend's input schema.
    """

    UNDEFINED = -42
    UNKNOWN = '?'

            
    def __init__(self, backend=None, submission=None,  data=None, id=None):
        """
        """
        if not data: data = {}
        
        if backend: data['backend'] = backend
        
        if submission: data['submission'] = submission
        
        QueueItem.__init__(self, data, id)


    def _validate(self):
        """
        Tests if a backend's name and submission ist given.
        
        @see QueueItem.validate
        @return True, if everything is ok, otherwise an assertion is thrown
        """
        assert self._data.get('backend', None), \
                "BackendJob requires a valid 'backend' entry"

        assert self._data.get('submission', None), \
            "BackendJob requires valid 'submission'"

        return True

    
    def setBackend(self, backend):
        """
        @param backend:  a backend's name
        """
        if backend:
            self._data['backend'] = backend


    def getBackend(self):
        """
        @return: name of the backend
        """
        return self._data.get('backend', self.UNDEFINED)
    
    
    def setSubmission(self, submission):
        """
        @param submission: term which will be tested (e.g., text or sourcecode)
        """
        if submission:
            self._data['submission'] = submission

    
    def getSubmission(self):
        """
        @return: a string
        """
        return self._data.get('submission', self.UNKNOWN)


# -- main ---------------------------------------------------------------------
#if __name__ == "__main__":
#    j1 = BackendJob("haskell", "map f xs = xs")
#
#    print j1
#    print j1.getBackend()
#    print j1.getSubmission()
#    #print j1.getData()