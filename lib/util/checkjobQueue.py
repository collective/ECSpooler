from data.checkjob import CheckJob
import time

# wfenske 2006-01-21
from abstractCheck import AbstractCheck


class CheckJobQueue(AbstractCheck):
    """
    A thread safe checkjob queue which stores the jobs in an SQL table
    """
    
    tableName   = "queue"
    tableSchema = "id               text primary key, " \
                  "checker          text, " \
                  "student_solution text, " \
                  "sample_solution  text, " \
                  "comparator       text"
    _queue = []
    _objs = {}

 
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
        def fun(connection):
            assert checkJob.__class__ == CheckJob, \
                   "Illegal Argument, checkJob must be a checkjob"
        
            cursor = connection.cursor()
            data = checkJob.getData()
            if data['id']:
                key = data['id']
            else:
                key = repr(time.time())

            cursor.execute("INSERT INTO %s "
                           "(id,checker,student_solution,sample_solution,"
                           "comparator) "
                           "VALUES (?,?,?,?,?)" % self.tableName,
                           (key,
                            data.get("checker"),
                            data.get("student_solution"),
                            data.get("sample_solution"),
                            data.get("comparator")))
            cursor.close()
            connection.commit() # wfenske 2006-01-21
        
            self._queue.append(key)
            self._objs[key] = checkJob
            
        return self.withConnection(fun)

        
    def initFromDatabase(self):
        """
        Reads all queue items from the database into our cache
        """
        def fun(connection):
            cursor = connection.cursor()
            cursor.execute("SELECT id, checker, student_solution, "
                           "sample_solution, comparator "
                           "FROM %s" % self.tableName)
            rows = cursor.fetchall()
            cursor.close()

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
                
        return self.withConnection(fun)

    
    def dequeue(self):
        """
        Removes the first job from the queue.
        @return CheckJob, the first job
        """
        def fun(connection):
            if self.isEmpty():
                return None

            key = self._queue[0]
            val = self._objs[key]
            del(self._objs[key])
            self._queue.remove(key)

            cursor = connection.cursor()
            cursor.execute("DELETE FROM %s WHERE id=?" % self.tableName,
                           (key,))
            cursor.close()
            connection.commit() # wfenske 2006-01-21
            
            return val
        
        return self.withConnection(fun)        


if __name__ == "__main__":
    jobDict = {}
    jobDict["checker"] = "checker_val"
    jobDict["student_solution"] = "student_val"
    jobDict["sample_solution"] = "sample_val"
    jobDict["comparator"] = "comp_val"
    j = CheckJob(jobDict)
    q = CheckJobQueue()
#   q.enqueue(j)
    print q.dequeue().getData()
#   print q.dequeue()
