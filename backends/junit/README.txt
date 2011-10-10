<!-- -*- coding: utf-8 -*- -->

Overview

Description

  The JUnit backend is a Python XML-RPC service, which provides - in  
  tandem with ECSpooler - automatic testing of Java programs.  
  Therefore the backend uses JUnit tests, which have to be specified
  by the teacher.

Prerequisites

  * A UNIX or UNIX-like operating system, e.g., NetBSD, Solaris, Mac
    OS X, or Linux.

    JUnit backend does run on Windows (with some restrictions), but
    this is neither supported nor recommended.

  * "Python":http://python.org/
  
  * "Java":http://java.sun.com/
  
  * "JUnit (at least Version 4)":http://www.junit.org/
    
  * "ECSpooler":http://wdok.cs.uni-magdeburg.de/software/ecspooler
  
  * ECSpooler is primarily intended to be used via the
    "ECAutoAssessmentBox":http://plone.org/products/ecautoassessmentbox
    Plone product.  So you will normally have a Plone installation
    with ECAutoAssessmentBox.
  
Installation

  * Unzip this archive under the 'backends' directory of your 
    ECSpooler installation (for example '/opt/ESpooler/backends/').  
    
  * Adjust JAVA_HOME, JVM_HOME and PATH variables in 
    'backends/junit/javac.sh' to your system environemnt.
    
  * Adjust JAVA_HOME, JVM_HOME and PATH variables in 
    'backends/junit/java.sh' to your system environemnt.

Security

  When executing unknown code, you have to take security precautions, 
  e.g., by using chroot, jails, or systrace.  These mechanisms are, 
  however, system-dependent, so we cannot give detailed installation
  instructions.
  
  If you are using "systrace":http://www.systrace.org/, adjust the 
  shell scripts 'backends/junit/javac' and 'backends/junit/java+systrace' 
  to your system environment and also edit the variables *compiler* and 
  *interpreter* in 'backends/junit/JUnitConf.py'
  
  Please be aware of the risks if you are *not* able to deal with the 
  configuration issues.
  
Quick Start

  After installation first start ECSpooler (see the ECSpooler
  documentation for details). Then you should be able to start the 
  JUnit backend using the following command (you may have to specify 
  the path)::

    backendctl -u root -p bazquux JUnit start

  If you run this command as root, the process will be started as user
  "nobody".

  Once the backend is running, check the status of the backend using::

    spoolerctl -u root -p bazquux status        

  and get a response similar to::

    {'queue': 0, 'backends': ['junit'], 'pid': 6427, 'results': 0}

  For administrative tasks such as starting/stopping backends 
  authentication is required.  Therefore ECSpooler ships with a 
  password file ('etc/passwd') containing a user root with default 
  password "bazquux".
 
  Testing a Java program with the JUnit backend requires a Unit Test, e.g.,  
  
  * Unit Test::
 
	@Test public void encodeTestSingleChar(){
		${CLASS} submission = new ${CLASS}();
		assertEquals('b',submission.encode('a',1));
	}
	
	@Test public void encodeEncode(){
		${CLASS} submission = new ${CLASS}();
		assertEquals('a',submission.encode(submission.encode('a',5),-5));
	}
	
	@Test public void codeTestSingleString(){
		${CLASS} submission = new ${CLASS}();
		String str = "abc";
		assertEquals("bcd",submission.code(str,1));
	}
	
	@Test public void codeCode(){
		${CLASS} submission = new ${CLASS}();
		String str = "Test";
		assertEquals(str, submission.code(submission.code(str,4),-4));
	}
 
Support

  For questions and discussions about backends, please join the
  "eduComponents mailing
  list":https://listserv.uni-magdeburg.de/mailman/listinfo/educomponents.

Credits

  The JUnit backend was written by 
  "Christian Baumann":mailto:christian.baumann@teleos-web.de and 
  "Julia Preuße".
  
License

  The JUnit backend is licensed under the
  "GPL":http://opensource.org/licenses/gpl-license.php.

  Copyright © 2007-2011 Otto-von-Guericke-Universität Magdeburg

  The JUnit backend is free software; you can redistribute it and/or 
  modify it under the terms of the GNU General Public License as
  published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  The JUnit backend is distributed in the hope that it will be
  useful, but WITHOUT ANY WARRANTY; without even the implied warranty
  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with ECSpooler; if not, write to the Free Software Foundation,
  Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
