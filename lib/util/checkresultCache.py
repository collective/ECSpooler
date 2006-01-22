# wfenske 2006-01-21
#import MySQLdb
from pysqlite2 import dbapi2 as sqlite

from data.checkresult import CheckResult
import time
import thread

# wfenske 2006-01-21
from checkjobQueue import dict_factory, possibly_create_table, get_connection

class CheckResultCache:
  
  def __init__(self):
    """
    Simple constructor for a thread safe checkresult cache which stores the results in a sql table
    """
    self.DEBUG = 0
    self._results = {}
    # wfenske 2006-01-21
    #self._conn = MySQLdb.connect(user="cape", passwd="cape..",
    #                             host="localhost", db="cape")
    self._conn = sqlite.connect("cape")
    self._conn.row_factory = dict_factory
    
    self.checkForTables()
    self.initResultsFromDatabase()
    self._lock = thread.allocate_lock();

  
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
    assert type(jobid) == type(""), \
        "Illegal Argument, expected jobid of type str"
    assert self._results.get(jobid) == None, \
        "Illegal Argument, jobid is already assigned"
    assert checkResult.__class__ == CheckResult, \
        "Illegal Argument, checkResult must be a CheckResult"
    
    self._lock.acquire()
    # wfenske 2006-01-21
    #cursor = self._conn.cursor(MySQLdb.cursors.DictCursor)
    self._conn = get_connection()
    cursor = self._conn.cursor()
    data = checkResult.getData()
    # wfenske 2006-01-21
    '''
    sql = []
    sql.append("insert into results (id,result,message) values ('")
    sql.append(jobid)
    sql.append("','")
    sql.append(str(data[0]))
    sql.append("','")
    sql.append(data[1].replace("'","\\'"))
    sql.append("')")
    if self.DEBUG: print ">>> %s"%("".join(sql))
    '''
    # wfenske 2006-01-21
    #cursor.execute("".join(sql))
    cursor.execute("insert into results (id,result,message) values (?,?,?)",
                   (jobid, data[0], data[1]))
    cursor.close()
    self._conn.commit() # wfenske 2006-01-21
    
    self._results[jobid] = checkResult
    self._lock.release()
    
  def checkForTables(self):
    """
    checks whether needed tables exists in databases.
    if it does not exist it will be created.
    altering is not supported.
    """
    # wfenske 2006-01-21
    '''
    c = self._conn.cursor(MySQLdb.cursors.DictCursor)
    c.execute("show tables")      
    rows = c.fetchall()
    c.close()
    
    ok = 0
    for row in rows
      if row.get("Tables_in_cape") == "results":
        ok = 1
    if ok == 0:
      c = self._conn.cursor(MySQLdb.cursors.DictCursor)
      c.execute("create table results (id varchar(255) primary key, result int(2), message text)")
    '''
    self._conn = get_connection()
    possibly_create_table(self._conn, "results",
                          "id text primary key, result int, message text")
      
  
  def initResultsFromDatabase(self):
    """
    Reads all queue items from the database into our cache
    """
    # wfenske 2006-01-21
    #c = self._conn.cursor(MySQLdb.cursors.DictCursor)
    self._conn = get_connection()
    c = self._conn.cursor()
    c.execute("select id, result, message from results")
    rows = c.fetchall()
    c.close()
    for row in rows:
      id = row["id"]
      result = row["result"]
      message = row["message"]
      r = CheckResult(int(result), message)
      self._results[id] = r
  
  def pollResults(self):
    """
    Removes all results and returns them
    @return All checkResults as {id: checkResult}
    """
    self._lock.acquire()
    obj = self._results.copy() # shallow copy is sufficient
    self._results.clear()

    # wfenske 2006-01-21
    #c = self._conn.cursor(MySQLdb.cursors.DictCursor)
    self._conn = get_connection()
    c = self._conn.cursor()
    # wfenske 2006-01-21
    #sql = "delete from results"
    #c.execute(sql)
    #c.execute("delete from results")
    c.close()
    self._conn.commit() # wfenske 2006-01-21
    self._lock.release()
    return obj
      
  def pollResult(self, id):
    """
    Removes one result and returns it
    @return One checkResult as {id: checkResult}
    """
    self._lock.acquire()

    try:
        # get and delete CheckResult if one exists
        result = self._results[id]
        del self._results[id]

        # delete CheckResult from database
        # wfenske 2006-01-21
        #c = self._conn.cursor(MySQLdb.cursors.DictCursor)
        self._conn = get_connection()
        c = self._conn.cursor()
        # wfenske 2006-01-21
        #sql = "delete from results where id = %s" % (id,)
        #c.execute(sql)
        #c.execute("delete from results where id=?", (id,))
        c.close()
        self._conn.commit() # wfenske 2006-01-21

    except Exception, e:
        # let us return None
        result = None

    self._lock.release()
    
    return result

if __name__ == "__main__":
  result = CheckResult(0, "message")
  cache = CheckResultCache()
  cache.addResult(result, time.time())
  for x in cache.pollResults():
    print x[0] + " " +repr(x[1].getData())
  
