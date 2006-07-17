from lib.data.checkjob import CheckJob
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
                  "studentSolution text, " \
                  "modelSolution  text, " \
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

#            cursor.execute("INSERT INTO %s "
#                           "(id,checker,studentSolution,modelSolution,"
#                           "comparator) "
#                           "VALUES (?,?,?,?,?)" % self.tableName,
#                           (key,
#                            data.get('backend'),
#                            data.get('studentSolution'),
#                            data.get('backendSolution'),
#                            data.get('testFunction')))
#            cursor.close()
#            connection.commit() # wfenske 2006-01-21
        
            self._queue.append(key)
            self._objs[key] = checkJob
            
        return self.withConnection(fun)

        
    def initFromDatabase(self):
        """
        Reads all queue items from the database into our cache
        """
        def fun(connection):
#            cursor = connection.cursor()
#            cursor.execute("SELECT id, checker, studentSolution, "
#                           "modelSolution, comparator "
#                           "FROM %s" % self.tableName)
#            rows = cursor.fetchall()
#            cursor.close()
#
#            for row in rows:
#                id = row["id"]
#                jobDict = {}
#                # fix of bug xy (2005-09-28, ma)
#                jobDict["id"] = str(id)
#                jobDict['backend'] = row['backend']
#                jobDict['studentSolution'] = row['studentSolution']
#                jobDict['modelSolution'] = row["modelSolution"]
#                jobDict['testFunction'] = row["testFunction"]
#                j = CheckJob(jobDict)
#                self._objs[id] = j
#                self._queue.append(id)
            pass
                
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

#            cursor = connection.cursor()
#            cursor.execute("DELETE FROM %s WHERE id=?" % self.tableName,
#                           (key,))
#            cursor.close()
#            connection.commit() # wfenske 2006-01-21
            
            return val
        
        return self.withConnection(fun)        


if __name__ == "__main__":
    jobDict = {}
    jobDict['backend'] = "checker_val"
    jobDict["studentSolution"] = "student_val"
    jobDict["modelSolution"] = "sample_val"
    jobDict["comparator"] = "comp_val"
    j = CheckJob(jobDict)
    q = CheckJobQueue()
#   q.enqueue(j)
    print q.dequeue().getData()
#   print q.dequeue()
