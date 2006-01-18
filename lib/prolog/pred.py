__doc__="""
This module is a tiny prolog parser.
"""

def getPredicates(code):
	"""
Returns a list of predicates in the given prolog code.
"""
	parser = PrologParser(code)
	preds = []
	while 1:
		pred = parser.nextPredicate()
		if not pred: break
		if not pred in preds: preds.append(pred)

	return preds


class PrologParser:
	
	def __init__(self, code):
		assert type(code) == type("")
		self._code = code.replace("\r","")
		self._idx = 0
		self._lineno = 1

	def nextPredicate(self):
		# RULES:
		# - '%': ignore until '\n'
		# - '/*': ignore until '*/'
		# - a sentence must end with a '.'
		# - a predicate ends at ':-', or if there aint such, at '.'
		# - if there is a '(' in the predicate, we have 1 + count(',') arguments
		# - if there is no '(', we have 0 arguments

		if self.head(1) == None: # end of code
			return None

		#print ">>> position: %i    head: %s"%(self._idx, repr(self.head(5)))

		self.skipWhitespaces()

		#print ">>> position: %i    head: %s"%(self._idx, repr(self.head(5)))

		if self.head(2) == "/*":
			#print ">>> skipped /**/ comment"
			self.readUntil("*/")
			return self.nextPredicate()

		elif self.head(1) == "%":
			#print ">>> skipped % comment"
			self.readUntil("\n")
			return self.nextPredicate()

		elif self.head(1) == None:
			# finished
			return 

		else:
			#print ">>> will read sentence, we are at %i, facing %s"%(self._idx, repr(self.head(1)))
			# IMHO we're facing a predicate
			sentence = self.readUntil(".")
			#print ">>> read sentence: %s"%repr(sentence)
			if not sentence:
				raise "ParserError: expected to find a '.' beyond index %i (line %i)"\
					%(self._idx, self._lineno)

			if sentence.find(":-") > 0:
				pred = sentence[0 : sentence.find(":-")].strip()
			else:
				pred = sentence[0 : sentence.find(".")].strip()

			if pred.find("(") < 0:
				# predicate without arguments
				return "%s/0"%pred

			else:
				pred_num_args = pred.count(",") + 1
				pred_name = pred[0 : pred.find("(")].strip()
				return "%s/%i"%(pred_name, pred_num_args)
			


	def head(self, length=1):
		if self._idx + length >= len(self._code):
			return None

		return self._code[self._idx : self._idx+length]


	def readUntil(self, s):
		pos = self._code.find(s, self._idx) # find occurence starting at current index
		if pos < 0: return None

		ret = self._code[self._idx : pos + len(s)] # extract token
		self._idx = pos + len(s) # consume

		self._lineno += ret.count("\n")
		return ret
	
	def skipWhitespaces(self):
		try:
			while self._code[self._idx] in ( '\t','\r','\n',' '):
				if self._code[self._idx] == '\n': self._lineno += 1
				self._idx += 1

		except IndexError, err:
			pass



if __name__ == "__main__":
	import sys

	if len(sys.argv) > 1:
		# filename given
		fd = open(sys.argv[1])
		code = fd.read()
		fd.close()
	else:
		# test code
		code = """
% ignore this line
/*
ignore this line too
*/
test.
tier(hund).
tier(katze).
aehnlich(hund,katze).
freunde(X,Y) :- tier(X), tier(Y), aehnlich(X,Y).
"""
	print getPredicates(code)

