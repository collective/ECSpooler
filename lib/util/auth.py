# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2006 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.

from types import StringType, UnicodeType, DictionaryType
import logging, md5, shelve, time

try:
    import crypt
except ImportError, ierr:
    crypt = None
    
# authorization levels
SHUTDOWN = 'le1'
ADD_BACKEND = 'le2'
STOP_BACKEND = 'le3'
REMOVE_BACKEND = 'le4'
APPEND_JOB = 'le5'
POP_RESULT = 'le6'
GET_STATUS = 'le7'
GET_BACKEND_INFO = 'le8'
UNDEFINED = 'le9'

class AuthorizationBackend:
    """
    """

    _userAuthFailSleep = 3.0 # this will prevent dictionary attacks 

    def __init__(self):
        """
        """
        pass

    def test(self, args, level=UNDEFINED):
        """
        """
        raise NotImplementedError("Method 'test' must be "
                                  "implemented by subclass")


class UserAuth(AuthorizationBackend):
    """User/password authentification with database containing
    (encrypted) passwords (UNIX-like)
    """
    
    def __init__(self, userFile='passwd'):
        """
        """
        self.db = shelve.open(userFile)

        if crypt:
            self.crypt = crypt.crypt
        else:
            self.crypt = lambda x, y: x
            logging.warn('Module crypt not found - using cleartext passwords!')

    def test(self, args, level=UNDEFINED):
        """
        @param level unused/ignored
        @return True if username and password are correct, otherwise False
        """
        try:
            assert type(args) == DictionaryType, \
                "Invalid data structure (%s)" % str(type(args))

            username = args.get("username")
            password = args.get("password")
        
            # do some parameter testing
            assert username and type(username) in (StringType, UnicodeType), \
                "Missing or invalid 'username'"
            assert password and type(password) in (StringType, UnicodeType), \
                "Missing or invalid 'password'"

            return self.authorize(username, password)

        except AssertionError, err:
            logging.info("Authorization failed: %s" % err)
            return False

    def authorize(self, username, password):
        """
        @return True if username and password are correct, otherwise False
        """
        ans = self.db.has_key(username) and \
               self.db[username] == self.crypt(password, password[:2])

        if not ans:
            time.sleep(self._userAuthFailSleep)

        return ans

    def addUser(self, username, password):
        """
        """
        self.db[username] = self.crypt(password, password[:2])

    def delUser(self, username):
        """
        @param username
        """
        del self.db[username]

    def users(self):
        """
        @return A list of all users
        """
        return self.db.keys()


class UserAuthMD5(AuthorizationBackend):
    """User/password authentification with a file containing
    md5 encrypted passwords (UNIX-like)
    """
    
    def __init__(self, userFile='passwd'):
        """
        @param userFile Absolute path to a file containing usernames and
                        md5 encrypted passwords
        """
        self.db = self._load(userFile)

    def test(self, args, level=UNDEFINED):
        """
        @param args A dictionary witj keys and values for username and password
        @param level 
        @return True if username and password are correct, otherwise False
        """
        try:
            assert type(args) == DictionaryType, \
                "Invalid data structure (%s)" % str(type(args))

            username = args.get("username")
            password = args.get("password")
        
            # do some parameter testing
            assert username and type(username) in (StringType, UnicodeType), \
                "Missing or invalid 'username'"
            assert password and type(password) in (StringType, UnicodeType), \
                "Missing or invalid 'password'"

            return self.authorize(username, password)

        except AssertionError, err:
            logging.info("Authorization failed: %s" % err)
            return False


    def authorize(self, username, password):
        """
        @return True if username and password are correct, otherwise False
        """
        ans = self.db.has_key(username) and \
               self.db[username] == md5.md5(password).hexdigest()

        if not ans:
            logging.warn('Wrong password. Waiting %d seconds to prevent ' 
                         'dictionary attacks' % self._userAuthFailSleep)
            time.sleep(self._userAuthFailSleep)

        return ans

    def users(self):
        """
        @return A list with all usernames
        """
        return self.db.keys()

    def _load(self, userFile):
        """
        @return A dictionary containing username-password pairs
        """
        lines = []
        usrdb = {}
        
        try:
            pwdFile = open(userFile, "r")
            lines = pwdFile.readlines()
            pwdFile.close()
        except IOError, ioe:
            logging.error(ioe)
            logging.warn('No user accounts available.')

        for line in lines:
            
            if line.startswith("#"): continue
            if not line.strip(): continue

            if line.find(":") != -1:
                username = line[0:line.find(":")].strip()
                usrdb[username] = line[line.find(":")+1:].strip()
                #logging.debug("Added user '%s'" % username)

        
        return usrdb


# -- --------------------------------------------------------------------------
if __name__ == '__main__': 

    # testing UserAuth

    #ua = UserAuth('passwd_db')
    #ua.addUser('demo', 'foobar')
    #print ua.authorize('test', 'SuPPe')
    #print ua.authorize('demo', 'foobar')


    # testing UserAuthMD5
    uamd5 = UserAuthMD5('passwd_md5')
    print uamd5.authorize('test', 'SuPPe')
    print uamd5.authorize('demo', 'foobar')
