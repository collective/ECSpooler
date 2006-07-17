# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
import time
#from lib.data import exceptions
import exceptions

class CheckJob:
    """
    """

    def __init__(self, d, createID=0):
        """
        """
        self._data = d != None and d.copy()

        if self._data != None and createID:
            self._data["id"] = repr(time.time())
            
        self.validate() # throws an exception if necessary


    def getId(self):
        """
        """
        return self._data['id']


    def getBackend(self):
        """
        """
        return self._data['backend']


    def getData(self):
        """
        Returns the transport data structure which can be transmitted via XMLRPC.
        """
        return self._data.copy()


    def validate(self):
        """
        Tests CheckJob data.
        """
        try:
            assert type(self._data) == dict, \
                'New check job must be of type data dictionary.'

            assert self._data['id'], \
                "New check job requires a valid 'id'"

            assert self._data['backend'], \
                "New check job requires a valid 'backend'"

            assert self._data['studentSolution'], \
                "New check job requires valid 'student solution'"

            #assert self._data['modelSolution'], \
            #    "New check job requires valid 'model solution'"

            #assert self._data['comparator'], "data requires comparator"
            #assert self._data['testdata'], "data requires testdata"

        except AssertionError, err:
            raise exceptions.InvalidDataException(err)
    
        return 1


    def has_key(self, key):
        return self._data.has_key(key)

    def __getitem__(self, name):
        """
        """
        return self._data.get(name)


    def __setitem__(self, name, value):
        """
        """
        self._data[name] = value
        self.validate()
        
