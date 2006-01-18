import time
from data import exceptions

class CheckJob:

    def __init__(self, d, createID=0):
        
        self._data = d != None and d.copy()

        if self._data != None and createID:
            self._data["id"] = repr(time.time())
            
        self.validate() # throws an exception if necessary


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
            assert type(self._data) == dict, "CheckJob requires a data dictionary"

            assert self._data.get("id"), "data requires id"
            assert self._data.get("checker"), "data requires checker id"
            assert self._data.get("student_solution"), "data requires student_solution"
            assert self._data.get("sample_solution"), "data requires sample_solution"
            #assert self._data.get("comparator"), "data requires comparator"
            #assert self._data.get("testdata"), "data requires testdata"

        except AssertionError, err:
            raise exceptions.InvalidDataException(err)
    
        return 1
    

    def __getitem__(self, name):
        """
        """
        return self._data.get(name)


    def __setitem__(self, name, value):
        """
        """
        self._data[name] = value
        self.validate()
        
