# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.

from lib.util.QueueItem import QueueItem

class BackendJob(QueueItem):
    """
    This class represents a job which will be managed by the spooler and handed
    over to the specified backend. The method getBackend ist
    """

    def getBackend(self):
        """
        @return String with the name of the backend.
        """
        return self._data['backend']


    def validate(self):
        """
        Tests if a backend is given or not.
        
        @see QueueItem.validate
        @return True, if everything is ok, otherwise an assertion 
        """
        assert self._data['backend'], \
                "BackendJob requires a valid 'backend' entry"

        return True
