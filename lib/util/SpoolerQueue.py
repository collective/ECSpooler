# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2011 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
#
import os
import sys
import pickle

from lib.Spooler import LOG
from lib.util.QueueItem import QueueItem

class SpoolerQueue:
    """
    A thread safe simple queue which stores queue items using pickle.
    """
    
    def __init__(self, filename):
        """
        Sets the name of the file for saving the entries and initializes the 
        queue as a dictionary.
        """

        self._filename = filename

        # initialize the queue
        self._queue = {}

        if not os.path.exists(os.path.dirname(filename)): 
            os.makedirs(os.path.dirname(filename))

        if os.path.exists(filename): 
            # try to load a queue from file
            self._load()

 
    def _load(self):
        """
        Reads the whole queue from a file.
        """
        try:
            #log.debug("loading queue from '%s'" % (self._filename,))
            self._queue = pickle.load(open(self._filename, 'r'))
        except Exception, e:
            msg = '%s: %s' % (sys.exc_info()[0], e)
            LOG.warn(msg)


    def _save(self):
        """
        Saves the whole queue to a file.
        """
        try:
            pickle.dump(self._queue, open(self._filename, 'w'), pickle.HIGHEST_PROTOCOL)
        except Exception, e:
            msg = '%s: %s' % (sys.exc_info()[0], e)
            LOG.error(msg)


    def isEmpty(self):
        """
        Returns whether or not the queue is empty.
        """
        return len(self._queue) == 0


    def getSize(self):
        """
        Returns the number of objects in the queue.
        """
        return len(self._queue)

 
    def enqueue(self, item):
        """
        Adds a single instance of QueueItem to the queue.
        
        @param: obj an object to enqueue
        """
        assert isinstance(item, QueueItem), \
                "Illegal Argument, item must be an instance of class QueueItem"
        
        id = item.getId()
        #log.debug('adding new item to queue: %s' % id)
        
        assert (not self._queue.has_key(id)), \
            "Item '%s' already exists in queue" % (id,)

        self._queue[id] = item

        # store changes in file on disk
        self._save();
        
        return id

    
    def dequeue(self, id=None):
        """
        Returns the first item or the item for the given id and removes it 
        from the queue.
        
        @return: a QueueItem
        """
        try:
            #log.debug('id: %s' % id)
            
            if not id:
                (id, item) = self._queue.popitem()
            else:
                item = self._queue.pop(id, None)
        
            #log.debug("pop item '%s'" % (id,))

            # store changes in file on disk
            self._save();
            
            return item

        except KeyError, e :
            LOG.warn(str(e))
            return None


# -- some tests ---------------------------------------------------------------
#if __name__ == "__main__":
#    """
#    """
#    from lib.data.BackendJob import BackendJob
#    
#    data = {}
#    data['backend'] = "checker_val"
#    data["submission"] = "student_val"
#    data["model"] = "sample_val"
#    data["comparator"] = "comp_val"
#    
#    data2 = {}
#    data2['backend'] = "checker_val2"
#    data2["submission"] = "student_val2"
#    data2["model"] = "sample_val2"
#    data2["comparator"] = "comp_val2"
#
#    j = BackendJob(data)
#    j2 = BackendJob(data2)
#        
#    q = SpoolerQueue(os.path.join(os.path.dirname(__file__), 
#                                  '..', '..', 'var', 'test.fs'))
#    
#    print q.getSize()
#    
#    q.enqueue(j)
#    q.enqueue(j2)
#
#    print q.getSize()
