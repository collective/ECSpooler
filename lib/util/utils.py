# -*- coding: utf-8 -*-
# $Id$
#
# Copyright (c) 2007-2011 Otto-von-Guericke-Universität Magdeburg
#
# This file is part of ECSpooler.

"""
Some helper methods
"""

import os 
import time
import random
import socket
import tempfile
import errno

try:
    import hashlib
except ImportError:
    import md5 as hashlib

def getUniqueModuleName(prefix=''):
    """ 
    Generates a unique identifier. The prefix can be set or left blank.
    @param: prefix The identifier's prefix (e.g. Student for student's Haskell module).
    @return: A unique identifier with or without a prefix
    """
    return prefix + uuid()

    
def getTempFileName(name, suffix=''):
    """ 
    Generates a absolute path string including the path to main temp dir, 
    the name of the module and a suffix.
    @param: name The content of this file.
    @param: suffix The file's suffix (e.g. extension). Default is ''
    @return: A string with absolute file path
    """
    return os.path.join(tempfile.gettempdir(), name + suffix)


def writeFile(content, filename, encoding='utf-8'):
    """ 
    Writes a file with the given *absolute* filepath and content.
    
    @param: content The content of this file.
    @param: filename The absolut e file path.
    @return: nothing
    """
    
    d = os.path.dirname(filename)

    if not os.path.exists(d):
        os.makedirs(d)
    
    #f = open(filename, 'w+')
    f = open(filename, 'w')
    f.write(content.encode(encoding, 'replace'))
    f.write('\n')

    f.flush()
    f.close()

def removeDirectory(path):
    """
    Removes entire directory trees. Be careful with it, since it deletes
    a lot of stuff. It is a recursive function.
    
    @param: path absolute path
    @deprecated
    """
    try:
        for f in os.listdir(path):
    
            file_or_dir = os.path.join(path, f)
    
            if os.path.isdir(file_or_dir) and not os.path.islink(file_or_dir):
                
                # it's a directory reucursive call to function again
                removeDirectory(file_or_dir)
    
            else:
    
                # it's a file, delete it
                os.remove(file_or_dir) 
    
        # delete the directory here
        os.rmdir(path)
        
        return True
        
    except Exception:
        #LOG.error(traceback.format_exc())
        return False

        
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
    #data = md5.md5(data).hexdigest()
    data = hashlib.md5(data).hexdigest()
    return data


def unique(seq, idFunction=None):
    """Returns a list with no duplicate items"""

    if idFunction is None:
        def idFunction(x): return x
    
    seen = {}
    result = []
    
    for item in seq:
        marker = idFunction(item)
        # in old Python versions:
        # if seen.has_key(marker)
        # but in new ones:
        if marker in seen: continue
        seen[marker] = 1
        result.append(item)
    
    return result


def write_pid_file(pid_filename):
    """ 
    Write the PID in the named PID file.

    Get the numeric process ID (“PID”) of the current process
    and write it to the named file as a line of text.

    @param pid_filename: Absolute path to PID file
    """
    open_flags = (os.O_CREAT | os.O_EXCL | os.O_WRONLY)
    open_mode = 0644
    
    pid_file_fd = os.open(pid_filename, open_flags, open_mode)
    pid_file = os.fdopen(pid_file_fd, 'w')

    # According to the FHS 2.3 section on PID files in /var/run:
    #
    #   The file must consist of the process identifier in
    #   ASCII-encoded decimal, followed by a newline character. For
    #   example, if crond was process number 25, /var/run/crond.pid
    #   would contain three characters: two, five, and newline.

    pid = os.getpid()
    line = "%(pid)d\n" % vars()
    pid_file.write(line)
    pid_file.close()


def read_pid_file(pid_filename):
    """ 
    Read the PID recorded in the named PID file.

    Read and return the numeric PID recorded as text in the named
    PID file. If the PID file cannot be read, or if the content is
    not a valid PID, return ``None``.

    @param pid_filename: Absolute path to PID file
    @return: PID or None
    """

    pid = None
    try:
        pid_file = open(pid_filename, 'r')
    except IOError:
        pass
    else:
        # According to the FHS 2.3 section on PID files in /var/run:
        #
        #   The file must consist of the process identifier in
        #   ASCII-encoded decimal, followed by a newline character.
        #
        #   Programs that read PID files should be somewhat flexible
        #   in what they accept; i.e., they should ignore extra
        #   whitespace, leading zeroes, absence of the trailing
        #   newline, or additional lines in the PID file.

        line = pid_file.readline().strip()
        try:
            pid = int(line)
        except ValueError:
            pass
        
        pid_file.close()

    return pid


def remove_pid_file(pid_filename):
    """ 
    Remove the named PID file if it exists.

    Removing a PID file that doesn't already exist puts us in the
    desired state, so we ignore the condition if the file does not
    exist.

    @param pid_filename: Absolute path to PID file
    """
    try:
        os.remove(pid_filename)
    except OSError, exc:
        if exc.errno == errno.ENOENT:
            pass
        else:
            raise

