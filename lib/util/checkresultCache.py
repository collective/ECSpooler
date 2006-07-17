from lib.data.checkresult import CheckResult
import time
import thread

# wfenske 2006-01-21
from abstractCheck import AbstractCheck


class CheckResultCache(AbstractCheck):
    """
    A thread safe checkresult cache which stores the results in an SQL table
    """
    
    tableName   = "results"
    tableSchema = "id      text primary key, " \
                  "result  int, " \
                  "message text"
    DEBUG = 0
    _results = {}

    
    def getSize(self):
        """
        Returns the number of cached CheckResults.
        """
        return len(self._results)


    def addResult(self, checkResult, jobid):
        """
        adds a checkresult
        @param checkResult the checkresult to add (must be of type CheckResult)
        @param jobid the jobid
        """
        def fun(connection):
            assert type(jobid) == str, \
                    "Illegal Argument, expected jobid of type str"
            assert self._results.get(jobid) == None, \
                    "Illegal Argument, jobid is already assigned"
            assert checkResult.__class__ == CheckResult, \
                    "Illegal Argument, checkResult must be a CheckResult"


            cursor = connection.cursor()
            data = checkResult.getData()
            cursor.execute("INSERT INTO %s (id,result,message) "
                           "VALUES (?,?,?)" % self.tableName,
                           (jobid, data[0], data[1]))
            cursor.close()
            connection.commit() # wfenske 2006-01-21

            self._results[jobid] = checkResult
            
        return self.withConnection(fun)
                    
    
    def initFromDatabase(self):
        """
        Reads all queue items from the database into our cache
        """
        def fun(connection):
            cursor = connection.cursor()
            cursor.execute("SELECT id, result, message "
                           "FROM %s" % self.tableName)
            rows = cursor.fetchall()
            cursor.close()
            
            for row in rows:
                id = row["id"]
                result = row["result"]
                message = row["message"]
                r = CheckResult(int(result), message)
                self._results[id] = r
            
        return self.withConnection(fun)
                
    
    def pollResults(self):
        """
        Removes all results and returns them
        @return All checkResults as {id: checkResult}
        """
        def fun(connection):
            obj = self._results.copy() # shallow copy is sufficient
            self._results.clear()

            cursor = connection.cursor()
            cursor.execute("DELETE FROM %s" % self.tableName)
            cursor.close()
            connection.commit() # wfenske 2006-01-21
            return obj
            
        return self.withConnection(fun)

            
    def pollResult(self, id):
        """
        Removes one result and returns it
        @return One checkResult as {id: checkResult}
        """
        def fun(connection):
            try:
                # get and delete CheckResult if one exists
                result = self._results[id]
                del self._results[id]

                # delete CheckResult from database
                cursor = connection.cursor()
                cursor.execute("DELETE FROM %s WHERE id=?" % self.tableName,
                               (id,))
                cursor.close()
                connection.commit() # wfenske 2006-01-21
                
            except Exception, e:
                result = None

            return result
            
        return self.withConnection(fun)


if __name__ == "__main__":
    result = CheckResult(0, "message")
    cache = CheckResultCache()
    cache.addResult(result, time.time())
    for x in cache.pollResults():
        print x[0] + " " +repr(x[1].getData())
    
