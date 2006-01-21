# wfenske 2006-01-21
#import MySQLdb
from pysqlite2 import dbapi2 as sqlite
from data.checkjob import CheckJob
import time
import thread


# wfenske 2006-01-21
def dict_factory(cursor, row):
    d = {}
    for idx, col in enumerate(cursor.description):
        d[col[0]] = row[idx]
    return d


# wfenske 2006-01-21
def has_table(connection, name):
  c = connection.cursor()
  c.execute("SELECT name FROM sqlite_master WHERE type='table'")
  rows = c.fetchall()
  c.close()
  return len([row for row in rows if row["name"] == name]) > 0


# wfenske 2006-01-21
def possibly_create_table(connection, name, spec):
  if not has_table(connection, name):
    c = connection.cursor()
    c.execute("create table %s (%s)" % (name, spec))
    c.close()
    connection.commit()


class CheckJobQueue:
  
  def __init__(self):
    """
    Simple constructor for a thread safe checkjob queue which stores the jobs
    in a sql table
    """
    self._queue = []
    self._objs = {}
    # wfenske 2006-01-21
    #self._conn = MySQLdb.connect(user="cape", passwd="cape..",
    #                             host="localhost", db="cape")
    self._conn = sqlite.connect("cape")
    self._conn.row_factory = dict_factory

    self.checkForTables()
    self.initQueueFromDatabase()
    self._lock = thread.allocate_lock()

 
  def isEmpty(self):
    """
    Returns whether or not the queue is empty.
    """
    return len(self._queue) == 0
	

  def getSize(self):
    """
    Returns the number of CheckJobs in the queue.
    """
    return len(self._queue)

 
  def enqueue(self, checkJob):
    """
    enqueues a checkjob
    @param checkJob the checkjob to enqueue (must be of type CheckJob)
    """
    assert checkJob.__class__ == CheckJob, \
       "Illegal Argument, checkJob must be a checkjob"
    
    self._lock.acquire()
    # wfenske 2006-01-21
    #cursor = self._conn.cursor(MySQLdb.cursors.DictCursor)
    cursor = self._conn.cursor()

    data = checkJob.getData()

    # wfenske 2006-01-21
    '''
    for k,v in data.iteritems():
      if type(v) == type(""):
        data[k] = v.replace("'","\\'")
      else:
        data[k] = str(v)
    '''

    if data['id']:
        key = data['id']
    else:
        key = repr(time.time())

    # wfenske 2006-01-21
    '''
    sql = []
    sql.append("insert into queue (id,checker,student_solution, sample_solution,comparator) values ('")
    sql.append(key)
    sql.append("','")
    sql.append(data.get("checker"))
    sql.append("','")
    sql.append(data.get("student_solution"))
    sql.append("','")
    sql.append(data.get("sample_solution"))
    sql.append("','")
    sql.append(data.get("comparator"))
    sql.append("')")
    cursor.execute("".join(sql))
    '''
    cursor.execute("insert into queue "
                   "(id,checker,student_solution,sample_solution,comparator) "
                   "values (?,?,?,?,?)",
                   (key, data.get("checker"), data.get("student_solution"),
                    data.get("sample_solution"), data.get("comparator")))
    cursor.close()
    self._conn.commit() # wfenske 2006-01-21
    
    self._queue.append(key)
    self._objs[key] = checkJob
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
    for row in rows:
      if row.get("Tables_in_cape") == "queue":
        ok = 1
    if ok == 0:
      c = self._conn.cursor(MySQLdb.cursors.DictCursor)
      c.execute("create table queue (id varchar(255) primary key, checker varchar(255), student_solution text, sample_solution text, comparator text)")
    '''
    possibly_create_table(self._conn, "queue",
                          "id text primary key, "
                          "checker text, "
                          "student_solution text, "
                          "sample_solution text, "
                          "comparator text")
  
  def initQueueFromDatabase(self):
    """
    Reads all queue items from the database into our cache
    """
    # wfenske 2006-01-21
    #c = self._conn.cursor(MySQLdb.cursors.DictCursor)
    c = self._conn.cursor()
    c.execute("select id, checker, student_solution, sample_solution, comparator from queue")
    rows = c.fetchall()
    c.close()
    for row in rows:
      id = row["id"]
      jobDict = {}
      # fix of bug xy (2005-09-28, ma)
      jobDict["id"] = str(id)
      jobDict["checker"] = row["checker"]
      jobDict["student_solution"] = row["student_solution"]
      jobDict["sample_solution"] = row["sample_solution"]
      jobDict["comparator"] = row["comparator"]
      j = CheckJob(jobDict)
      self._objs[id] = j
      self._queue.append(id)
  
  def dequeue(self):
    """
    Removes the first job from the queue.
    @return CheckJob, the first job
    """
    self._lock.acquire()
    if self._queue.__len__() < 1:
      self._lock.release()
      return None
    
    key = self._queue[0]
    val = self._objs[key]
    del(self._objs[key])
    self._queue.remove(key)
    
    # wfenske 2006-01-21
    #c = self._conn.cursor(MySQLdb.cursors.DictCursor)
    c = self._conn.cursor()
    # wfenske 2006-01-21
    #sql = "delete from queue where id='%s'" % key
    #c.execute(sql)
    c.execute("delete from queue where id=?", (key,))
    c.close()
    self._conn.commit() # wfenske 2006-01-21
    self._lock.release()
    return val
      
if __name__ == "__main__":
  jobDict = {}
  jobDict["checker"] = "checker_val"
  jobDict["student_solution"] = "student_val"
  jobDict["sample_solution"] = "sample_val"
  jobDict["comparator"] = "comp_val"
  j = CheckJob(jobDict)
  q = CheckJobQueue()
#  q.enqueue(j)
  print q.dequeue().getData()
#  print q.dequeue()
