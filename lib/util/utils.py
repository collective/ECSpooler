# -- some hepler methods -------------------------------------------------------
import os, time, random, md5, socket, tempfile

def getUniqueModuleName(prefix=''):
    """ 
    Generates a unique identifier. The prefix can be set or left blank.
    @param prefix The identifier's prefix (e.g. Student for student's Haskell module).
    @return A unique identifier with or without a prefix
    """
    return prefix + uuid()

    
def getTempFileName(moduleName, suffix=''):
    """ 
    Generates a absolute path string including the path to main temp dir, 
    the name of the Haskel module and a suffix.
    @param moduleName The content of this file.
    @param suffix The file's suffix (e.g. extension). Default is .hs
    @return A string with absolute file path
    """
    return os.path.join(tempfile.gettempdir(), moduleName + suffix)


def writeFile(content, filename):
    """ 
    Writes a file with the given *absolute* filepath and content.
    @param content The content of this file.
    @param filename The absolut e file path.
    @return nothing
    """
    
    try:
        dir = os.path.dirname(filename)
    
        if not os.path.exists(dir):
            os.makedirs(dir)
        
        #file = open(filename, 'w+')
        file = open(filename, 'w')
        file.write(content)
        file.write('\n')
    
        file.flush()
        file.close()
        
        return 1
    except Exception, e:
        # FIXME: return error message or through a special exception
        return 0


def delDir(path):
    """
    Removes entire directory trees. Be careful with it, since it deletes
    a lot of stuff. It is a recursive function.
    
    @param path absolute path
    @deprecated
    """
    try:
        for file in os.listdir(path):
    
            file_or_dir = os.path.join(path,file)
    
            if os.path.isdir(file_or_dir) and not os.path.islink(file_or_dir):
                
                # it's a directory reucursive call to function again
                delDir(file_or_dir)
    
            else:
    
                # it's a file, delete it
                os.remove(file_or_dir) 
    
        # delete the directory here
        os.rmdir(path)
        
        return 1
        
    except Exception, e:
        import traceback
        traceback.print_exc()
        # FIXME: return error message
        return e

        
def uuid(*args):
    """
    Generates a universally unique Id. 
    Any arguments only create more randomness.
    """

    t = long(time.time() * 1000)
    r = long(random.random() * 100000000000000000L)
    try:
        a = socket.gethostbyname(socket.gethostname())
    except:
        # if we can't get a network address, just imagine one
        a = random.random() * 100000000000000000L
  
    data = str(t) + ' ' + str(r) + ' ' + str(a) + ' ' + str(args)
    data = md5.md5(data).hexdigest()
    return data


def unique(seq, idfun=None):
    """Returns a list with no duplicate items."""

    if idfun is None:
        def idfun(x): return x
    seen = {}
    result = []
    for item in seq:
        marker = idfun(item)
        # in old Python versions:
        # if seen.has_key(marker)
        # but in new ones:
        if marker in seen: continue
        seen[marker] = 1
        result.append(item)
    return result
