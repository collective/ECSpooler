<!-- -*- coding: utf-8 -*- -->

Overview

Description

  The Haskell QuickCheck backend is a Python XML-RPC service, which 
  provides - in tandem with ECSpooler - automatic testing of Haskell 
  programs using "QuicCheck":http://www.cs.chalmers.se/~rjmh/QuickCheck/.  
  
  The Haskell QuickCheck backend is derived from Haskell backend.

Prerequisites

  * A UNIX or UNIX-like operating system, e.g., NetBSD, Solaris, Mac
    OS X, or Linux.

    The Haskell QuickCheck backend does run on Windows (with some 
    restrictions), but this is neither supported nor recommended.

  * "Python":http://python.org/
  
  * "Hugs":http://www.haskell.org/hugs/
    
  * "ECSpooler":http://wdok.cs.uni-magdeburg.de/software/ecspooler
    
  * ECSpooler is primarily intended to be used via the
    "ECAutoAssessmentBox":http://plone.org/products/ecautoassessmentbox
    Plone product.  So you will normally have a Plone installation
    with ECAutoAssessmentBox.

  * Haskell backend since Haskell QuickCheck is derived from it.
  
Installation

  * Unzip this archive under the 'backends' directory of your 
    ECSpooler installation (for example '/opt/ESpooler/backends/').
    
  * Adjust values for INTERPRTER and OPTIONS in the script
    'backends/haskellqc/runhugs.sh' to your system environemnt.
    
  * Make sure that the script 'backends/haskellqc/runhugs.sh' is
    executable.

Security

  Please read the security section in the Haskell backend's README 
  file and be be aware of the risks if you are *not* able to deal with 
  the configuration issues.
  
Quick Start

  After installation, first start ECSpooler (see the ECSpooler
  documentation for details).  Then you should be able to start the 
  Haskell QuickCheck backend using the following command (you may have 
  to specify the path)::

    backendctl -u root -p bazquux HaskellQC start

  If you run this command as root, the process will be started as user
  "nobody".

  Once the backend is running, check the status of the backend using::

    spoolerctl -u root -p bazquux status
    
  or        

    backendctl -u root -p bazquux HaskellQC status

  and get a response similar to::

    {'queue': 0, 'backends': ['haskellqc'], 'pid': 2642, 'results': 0}

  For administrative tasks such as starting/stopping backends
  authentication is required.  Therefore ECSpooler ships with a
  password file ('etc/passwd') containing a user "root" with default
  password "bazquux".  See the ECSpooler documentation if you would
  like to add/change users and passwords.
 
  Testing a Haskell program with the Haskell QuickCheck backend 
  requires at least one QuickCheck property, e.g.::  
  
    TODO:
    prop_


Support

  For questions and discussions about backends, please join the
  "eduComponents mailing
  list":https://listserv.uni-magdeburg.de/mailman/listinfo/educomponents.

Credits

  The Haskell QuickCheck backend was written by "Mario 
  Amelung":http://wdok.cs.uni-magdeburg.de/Members/amelung.
  
License

  The Haskell QuickCheck backend is licensed under the
  "GPL":http://opensource.org/licenses/gpl-license.

  Copyright © 2008 Otto-von-Guericke-Universität Magdeburg

  The Haskell QuickCheck backend is free software; you can redistribute 
  it and/or modify it under the terms of the GNU General Public License 
  as published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  The Haskell QuickCheck backend is distributed in the hope that it 
  will be useful, but WITHOUT ANY WARRANTY; without even the implied 
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See 
  the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with ECSpooler; if not, write to the Free Software Foundation,
  Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
