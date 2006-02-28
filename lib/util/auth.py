# -*- coding: utf-8 -*-
# $Id$
import shelve, time, logging
from types import StringType

try:
    import crypt
except ImportError, ierr:
    crypt = None
    
userAuthFailSleep = 3.0 # prevent dictionary attacks 

def initAuthBackend(opts):
    # return your custom auth backend implementation
    return SimpleAuthorizationBackend(opts)


class AuthorizationBackend:

    # authorization levels
    ENQUEUE = POLL_RESULTS = 1
    GET_STATUS  = 2
    ADD_CHECKER = 3

    
    def __init__(self, opts):
        pass
    

    def test(self, args, level):
        raise "This method is abstract"


class UserAuth(AuthorizationBackend):
    """User/password authentification with database containing
    (encrypted) passwords (UNIX-like)
    
    @see http://www.python-forum.de/viewtopic.php?t=231&sid=514dfaa0a9a6b78b64c49403204d5af9
    """
    
    def __init__(self, userFile='.passwd'):
        self.db = shelve.open(userFile)
        if crypt:
            self.crypt = crypt.crypt
        else:
            self.crypt = lambda x,y: x
            logging.warn('crypt module not found - using cleartext passwords!')

    def test(self, args, level):
        """
        """
        username = args.get("username")
        password = args.get("password")
        
        return self.authorize(username, password)


    def authorize(self, username, password):
        ans = self.db.has_key(username) and \
               self.db[username] == self.crypt(password, password[:2])
        if not ans:
            time.sleep(userAuthFailSleep)
        return ans


    def addUser(self, username, password):
        self.db[username] = self.crypt(password, password[:2])

    
    def delUser(self, username):
        del self.db[username]


    def users(self):
        return self.db.keys()


class SimpleAuthorizationBackend(AuthorizationBackend):
    """
    The SimpleAuthorizationBackend is a lightweight implementation
    for user authorization. It stores all users in a htpasswd-style
    file, with standard crypt ciphers. It does not take different
    levels of authorization into account.
    """

    def __init__(self, opts):
        AuthorizationBackend.__init__(self,opts)
        #self._fname = opts and opts.get("filename") or "../etc/passwd"
        self._fname = opts.get("filename")
        self._users = {}
        self._load()

    def test(self, args, level):

        try:
            assert type(args) == type({}), "Invalid data structure"
            assert args.get("username") and type(args["username"]) == type(""), \
                "Missing or invalid 'username' attribute"
            assert args.get("password") and type(args["password"]) == type(""), \
                "Missing or invalid 'passwd' attribute"

            assert self._users.get(args["username"]), "No such user: %s"%args["username"]

            cipher = self._users[args["username"]]

            assert crypt.crypt(args["password"], cipher[0:2]) == cipher, \
                "Crypt mismatch for user '%s'"%d["username"]

            return 1

        except AssertionError, err:
            logging.info("[SimpleAuthorizationBackend] test failed: %s" % err)
            return 0


    def _load(self):
        try:
            fd = open(self._fname, "r")
            lines = fd.readlines()
            fd.close()
            for line in lines:
                if line.startswith("#"): continue
                if not line.strip(): continue

                assert line.find(":") > 0, \
                    "invalid passwd entry '%s'"%(line.strip())

                uid = line[0:line.find(":")].strip()
                self._users[uid] = line[line.find(":")+1:].strip()
                #logging.debug("Added User '%s'" % uid)

            assert len(self._users) > 0, "at least one account is required"

        except Exception, exc:
            raise "Unable to load passwd database (%s: %s)"%(exc.__class__.__name__,exc)



if __name__ == '__main__': 

    ua = UserAuth()
    #ua.addUser('demo', 'foobar')
    print ua.authorize('test', 'SuPPe')
