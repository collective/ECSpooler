<!-- -*- coding: utf-8 -*- -->

Overview

  ECSpooler is a Python XML-RPC service, which provides the automatic 
  testing of submissions for ECAutoAssessmentBox.  It manages a 
  submission queue and several backends.

Download

  * "Project page":http://wdok.cs.uni-magdeburg.de/software/ecspooler

Prerequisites

  * "Python":http://python.org

  * A UNIX or UNIX-like operating system, e.g., NetBSD, Solaris, Mac
    OS X, or Linux.

    ECSpooler does run on Windows (with some restrictions), but this
    is neither supported nor recommended.

  * ECSpooler is primarily intended to be used via the
    "ECAutoAssessmentBox":http://plone.org/products/ecautoassessmentbox
    Plone product.  So you will normally have a Plone installation
    with ECAutoAssessmentBox.
  
Installation

  * If necessary, edit the installation directories in 'setup.cfg'.
    By default, ECSpooler will be installed in '/opt/ECSpooler'.

  * Run 'python setup.py install' (installation may require
    appropriate privileges for writing to the specified installation
    directory)

  * Be sure that 'ECSpooler/var' and 'ECSpooler/log' are writeable by
    the user who is starting the spooler (cf. below if you run the spooler
    as root).

Quick Start

  After installation, you should then be able to start the spooler (on
  the default port 5050) using the command (you may have to specify
  the path)::

    spoolerctl start

  and the Keywords backend using::

    backendctl -u root -p bazquux Keywords start

  If you check the status using::

    spoolerctl -u root -p bazquux status        

  and get a response similar to::

    {'queue': 0, 'backends': ['keywords'], 'pid': 26946, 'results': 0}

  If you run these commands as root, the processes will be started
  as user "nobody".

  Once ECSpooler and the backend are running, go to your Plone site,
  log in as Manager, and go to the "Site Setup", where you should
  find "Auto Assessment Settings".  Make the spooler connection
  settings.  You should the see the available backends; select the
  Keywords backend.

  You can then start to create Auto Assessment Boxes.  See the
  ECAutoAssessmentBox documentation for details.

Description

  ECSpooler provides automatic testing of submissions for
  "ECAutoAssessmentBox":http://plone.org/products/ecautoassessmentbox.
  ECAutoAssessmentBox allows students to submit their assignments via
  the Web at any time during the submission period.  A typical case
  are programming assignments, where students have to submit programs.
  Submitted programs are automatically checked and students get
  immediate feedback on whether their programs are syntactically
  correct and yield the expected results.

  When a student submits a program, it is first sent to ECSpooler.
  ECSpooler is a Web service which manages a submission queue and
  several backends.  Backends provide syntax checking and testing for
  a specific programming language, usually in conjunction with the
  corresponding compiler and/or interpreter.  The results of the tests
  performed by the backend are immediately returned and are displayed
  by ECAutoAssessmentBox.

  Backends

    We have implemented backends for Haskell (using Hugs), Scheme
  (using MzScheme), Erlang, Prolog (using SWI-Prolog), Python, and
  Java.  Backends can also be used to implement different approaches
  for testing: For example, we have implemented one backend for
  Haskell which compares the output of the student solution with the
  output of a model solution for a set of test data, and, as an
  alternative, we have implemented another backend for Haskell which
  uses QuickCheck for testing based on formal specifications of
  properties required for a correct solution.

  However, ECSpooler only ships with a demo backend, Keywords, which
  checks whether a submission contains certain keywords specified by
  the creator of the assignment.  We do not include other backends
  because, when executing student code, you have to take security
  precautions, e.g., by using chroot, jails, or systrace.  These
  mechanisms are, however, system-dependent, so we cannot include
  anything that runs out of the box *and* is secure.  Furthermore,
  paths to interpreters or compilers vary across installations.  If
  you are aware of the risks and if you are able to deal with the
  configuration issues, we are happy provide you with our backends on
  request.

  Users

    For job submissions and for administrative tasks authentication is
  required.  ECSpooler uses a simple user/password authentication
  scheme.  Note that a user normally corresponds to a Plone site, not
  to an individual person.

  ECSpooler ships with a password file ('etc/passwd') containing two
  users: root (default password "bazquux") and demo (default password
  "foobar").

  The root user is only intended for administrative tasks.  Only the
  root user can start and stop backends, thus you have to provide the
  username "root" and the corresponding password to spoolerctl and
  backendctl.

  The root user should not be used for job submissions.  In a
  production environment you should also definitely change the
  passwords.

  To create a new user, add a new line into the password file,
  consisting of the user name and an MD5 hash of the password,
  separated by a colon.  If your system doesn't provide an md5
  command, you can use the following command line to create the hash::

    python -c 'import md5; print md5.new("password").hexdigest()'

Support

  For questions and discussions about ECSpooler, please join the
  "eduComponents mailing
  list":https://listserv.uni-magdeburg.de/mailman/listinfo/educomponents.

Credits

  ECSpooler was written by
  "Mario Amelung":http://wdok.cs.uni-magdeburg.de/Members/amelung and
  "Michael Piotrowski":http://wdok.cs.uni-magdeburg.de/Members/mxp.
  
  Additional programming by Wolfram Fenske.

License

  ECSpooler is licensed under the
  "GPL":http://opensource.org/licenses/gpl-license.

  Copyright © 2007-2011 Otto-von-Guericke-Universität Magdeburg

  ECSpooler is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the
  Free Software Foundation; either version 2 of the License, or (at
  your option) any later version.

  ECSpooler is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with ECSpooler; if not, write to the Free Software Foundation,
  Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
