<!-- -*- coding: utf-8 -*- -->

Overview

Description

  The Python backend provides - in tandem with ECSpooler - automatic 
  testing of Python programs.  Therefore the backend compares the
  output of a program with the output of a model solution for a set of 
  test data.
    
Prerequisites

  * A UNIX or UNIX-like operating system, e.g., NetBSD, Solaris, Mac
    OS X, or Linux.

    Prolog backend does run on Windows (with some restrictions), but
    this is neither supported nor recommended.

  * "Python":http://python.org/
  
  * "ECSpooler":http://wdok.cs.uni-magdeburg.de/software/ecspooler
  
  * ECSpooler is primarily intended to be used via the
    "ECAutoAssessmentBox":http://plone.org/products/ecautoassessmentbox
    Plone product.  So you will normally have a Plone installation
    with ECAutoAssessmentBox.
  
Installation

  * Unzip this archive under the 'backends' directory of your 
    ECSpooler installation (for example '/opt/ESpooler/backends/').  
    
  * Adjust value for INTERPRETER in the script 
    'backends/python/python.sh' to your system environemnt.
    
  * Make sure that the script 'backends/python/python.sh' is
    executable.

Security

  When executing unknown code, you have to take security precautions, 
  e.g., by using chroot, jails, or systrace.  These mechanisms are, 
  however, system-dependent, so we cannot give detailed installation
  instructions.
  
  If you are using "systrace":http://www.systrace.org/, adjust the 
  shell scripts 'backends/python/python+systrace' to your system
  environment and set *INTERPRETER* in  
  'backends/python/config.py' to this script.
  
  Please be aware of the risks if you are *not* able to deal with the 
  configuration issues.
  
Quick Start

  After installation first start ECSpooler (see the ECSpooler
  documentation for details).  Then you should be able to start the 
  Java backend using the following command (you may have to specify 
  the path)::

    backendctl -u root -p bazquux Python start

  If you run this command as root, the process will be started as user
  "nobody".

  Once the backend is running, check the status of the backend using::

    spoolerctl -u root -p bazquux status        

  and get a response similar to::

    {'queue': 0, 'backends': ['python'], 'pid': 26437, 'results': 0}

  For administrative tasks such as starting/stopping backends 
  authentication is required.  Therefore ECSpooler ships with a 
  password file ('etc/passwd') containing a user root with default 
  password "bazquux".

  Testing a Python program with the Python backend requires a model 
  solution and a set of test data, e.g.,  
  
  * Model solution::
 
    def square(x): return x * x
    
  * Test data (one test call per line)::
  
    square(0)
    square(2)
    square(3)
    
  The Python backend supports TODO compare modes:
  
  * simple - the results of the student and model solutions for same
    test data must be equal
    
  * permutation - accept student solution if the returned list is a 
    permutation of the result from the model solution

    
Support

  For questions and discussions about backends, please join the
  "eduComponents mailing
  list":https://listserv.uni-magdeburg.de/mailman/listinfo/educomponents.

Credits

  The Python backend was written by "Mario 
  Amelung":mailto:mario.amelung@gmx.de.
  
License

  The Python backend is licensed under the
  "GPL":http://opensource.org/licenses/gpl-license.

  Copyright © 2007 Otto-von-Guericke-Universität Magdeburg

  The Python backend is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License as
  published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  The Python backend is distributed in the hope that it will be
  useful, but WITHOUT ANY WARRANTY; without even the implied warranty
  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with ECSpooler; if not, write to the Free Software Foundation,
  Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
