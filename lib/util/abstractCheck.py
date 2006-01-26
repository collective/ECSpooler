from pysqlite2 import dbapi2 as sqlite
import thread


class AbstractCheck(object):
    
    def __init__(self):        
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
            connection = sqlite.connect("cape")
            connection.row_factory = dictFactory
            try:
                return fun(connection)
            finally:
                connection.close()
        finally:
            self.lock.release()
        
        
    def tableExists(self):        
        def fun(connection):
            cursor = connection.cursor()
            cursor.execute("SELECT name FROM sqlite_master WHERE TYPE='table'")
            rows = cursor.fetchall()
            cursor.close()
            return len([row for row in rows
                        if row["name"] == self.tableName]) > 0
        
        return self.withConnection(fun)

        
    def possiblyCreateTable(self):
        """
        checks whether needed table exists in databases.
        if it does not exist, it will be created.
        """
        def fun(connection):
            if not self.tableExists():
                cursor = connection.cursor()
                cursor.execute("CREATE TABLE %s (%s)" % (self.tableName,
                                                    self.tableSchema))
                cursor.close()
                connection.commit()
        
        return self.withConnection(fun)
