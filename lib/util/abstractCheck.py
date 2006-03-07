# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.
import os, thread
import logging

try:
    from pysqlite2 import dbapi2 as sqlite
except ImportError, ierr:
    logging.error('Module pysqlite2 not found!')
    #raise ierr
    sqlite = None

class AbstractCheck(object):
    
    def __init__(self):
        """
        """
        dbDir = os.path.join(os.path.dirname(__file__), '..', '..', 'var')
        if not os.path.exists(dbDir): 
            os.makedirs(dbDir)
        
        self.dbFile = os.path.join(dbDir, 'data.fs')

        self.lock = thread.allocate_lock()
        self.possiblyCreateTable()
        self.initFromDatabase()


    def initFromDatabase(self):
        raise NotImplementedError("initFromDatabase() needs to be "
                                  "implemented by subclasses")
        

    def withConnection(self, fun):
        def dictFactory(cursor, row):
            d = {}
            for idx, col in enumerate(cursor.description):
                d[col[0]] = row[idx]
            return d
        
        self.lock.acquire()
        try:
            connection = sqlite.connect(self.dbFile)
            connection.row_factory = dictFactory
            try:
                return fun(connection)
            finally:
                connection.close()
        finally:            
            self.lock.release()
        
        
    def possiblyCreateTable(self):
        """
        checks whether needed table exists in databases.
        if it does not exist, it will be created.
        """
        def fun(connection):            
            cursor = connection.cursor()
            cursor.execute("SELECT name FROM sqlite_master WHERE TYPE='table'")
            rows = cursor.fetchall()
            tableExists = len([row for row in rows
                               if row["name"] == self.tableName]) > 0
            if not tableExists:
                cursor.execute("CREATE TABLE %s (%s)" % (self.tableName,
                                                    self.tableSchema))
                connection.commit()
            cursor.close()
        
        return self.withConnection(fun)
