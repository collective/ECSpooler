<!-- -*- coding: utf-8 -*- -->

Overview

Description

  The HaskellIO backend is a Python XML-RPC service, which provides -  
  in tandem with ECSpooler - automatic testing of Haskell programs.  
  Therefore the backend compares the output of a program with the 
  output of a model solution for a set of test data.
  
  The HaskellIO backend is derived from the Haskell backend and 
  should be used for testing I/O code in Haskell.

Prerequisites

  * A UNIX or UNIX-like operating system, e.g., NetBSD, Solaris, Mac
    OS X, or Linux.

    The HaskellIO backend does run on Windows (with some 
    restrictions), but this is neither supported nor recommended.

  * "Python":http://python.org/
  
  * "Hugs":http://www.haskell.org/hugs/
    
  * "ECSpooler":http://wdok.cs.uni-magdeburg.de/software/ecspooler
    
  * ECSpooler is primarily intended to be used via the
    "ECAutoAssessmentBox":http://plone.org/products/ecautoassessmentbox
    Plone product.  So you will normally have a Plone installation
    with ECAutoAssessmentBox.

  * Haskell backend since HaskellIO is derived from it.
  
Installation

  * Unzip this archive under the 'backends' directory of your 
    ECSpooler installation (for example '/opt/ESpooler/backends/').
    
  * Even if the Haskell backend is already installed, please 
    check the following twice  
    
    * Adjust values for INTERPRTER and OPTIONS in the script
      'backends/haskell/runhugs.sh' to your system environemnt.
    
    * Make sure that the script 'backends/haskell/runhugs.sh' is
      executable.

Security

  Please read the security section in the Haskell backend's README 
  file and be be aware of the risks if you are *not* able to deal with 
  the configuration issues.
  
Quick Start

  After installation, first start ECSpooler (see the ECSpooler
  documentation for details).  Then you should be able to start the 
  HaskellIO backend using the following command (you may have to 
  specify the path)::

    backendctl -u root -p bazquux HaskellIO start

  If you run this command as root, the process will be started as user
  "nobody".

  Once the backend is running, check the status of the backend using::

    spoolerctl -u root -p bazquux status
    
  or        

    backendctl -u root -p bazquux HaskellIO status

  and get a response similar to::

    {'queue': 0, 'backends': ['haskellio'], 'pid': 2642, 'results': 0}

  For administrative tasks such as starting/stopping backends
  authentication is required.  Therefore ECSpooler ships with a
  password file ('etc/passwd') containing a user "root" with default
  password "bazquux".  See the ECSpooler documentation if you would
  like to add/change users and passwords.
 
  Testing a Haskell program with the HaskellIO backend requires a 
  model solution and a set of test data, e.g.,  
  
  TODO:
  
  * Model solution::
 
    import IO
 
    sumInts :: FilePath -> IO String
    printFile path = catch (do cnt <- readFile path
                         return (show cnt))
                     ) 
                     (\_ -> return ("No such file"))
                             
  * Test data (one test call per line)::
  
    printFile "/tmp/foobar.txt"
    printFile "/tmp/123.txt"
    
  The HaskellIO backend currently supports three compare modes:
  
  * simple - the results of the student and model solutions for same
    test data must be equal
    
  * permutation - accept student solution if the returned list is a 
    permutation of the result from the model solution
  
  * tolerance - result of student and model solution must be equal to
    15 positions after decimal point
  
Support

  For questions and discussions about backends, please join the
  "eduComponents mailing
  list":https://listserv.uni-magdeburg.de/mailman/listinfo/educomponents.

Credits

  The HaskellIO backend was written by "Mario 
  Amelung":http://wdok.cs.uni-magdeburg.de/Members/amelung.
  
License

  The HaskellIO backend is licensed under the
  "GPL":http://opensource.org/licenses/gpl-license.

  Copyright © 2007 Otto-von-Guericke-Universität Magdeburg

  The HaskellIO backend is free software; you can redistribute it 
  and/or modify it under the terms of the GNU General Public License as
  published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  The HaskellIO backend is distributed in the hope that it will be
  useful, but WITHOUT ANY WARRANTY; without even the implied warranty
  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with ECSpooler; if not, write to the Free Software Foundation,
  Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
