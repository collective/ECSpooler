import crypt

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
			print "[SimpleAuthorizationBackend] test failed: %s"%err
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
				#print "Added User '%s'"%uid

			assert len(self._users) > 0, "at least one account is required"

		except Exception, exc:
			raise "Unable to load passwd database (%s: %s)"%(exc.__class__.__name__,exc)
