# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2011 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
from lib.util import uuid

class QueueItem:
    """
    An class which represents items for a queue.
    """

    def __init__(self, data, oid=None):
        """
        Creates a new QueueItem object with the given data. If oid is None we
        will create a new unique oid using time.
        
        @param: data a dictionary including job data, e.g. backend and students' solution
        @param: oid a unique ID; if not set, a unique ID will be created
        """
        assert type(data) == dict, 'data in QueueItem must be a dictionary.'

        # if oid is None...
        if not oid: 
            # ...use oid in data if available
            if data.has_key('id'):
                oid = data.get('id')
            # ...or create a new unique oid
            else:
                oid = str(uuid.uuid1())

        self._oid = oid
        self._data = {}
        
        self.setData(data)


    def getId(self):
        """
        """
        return self._oid


    def setData(self, data):
        """
        Replaces the complete data with new ones.
        """
        self._data = data.copy()
        # add oid to the data dictionary because it will be used later
        self._data['id'] = self._oid

        # valide the inputs; an exception can be thrown
        self._validate()


    def getData(self):
        """
        Returns the transport data structure which can be transmitted via XMLRPC.
        """
        #return self._data.copy()
        return self._data


    def _validate(self):
        """
        Validates the data. Will be called from setData. Overwrite this 
        method in subclasses if needed.
        """
        return True


    def has_key(self, key):
        """
        Returns True if a value for the given key exists.
        """
        return self._data.has_key(key)


    def __getitem__(self, key):
        """
        Returns the value for the given key.
        """
        return self._data.get(key)


    def __setitem__(self, key, value):
        """
        Sets a value for the given key.
        """
        self._data[key] = value
        self._validate()


# -- test section -------------------------------------------------------------
#if __name__ == "__main__":
#    """
#    """
#    oid = uuid.uuid1()
#    print oid