<!-- -*- coding: utf-8 -*- -->

Overview

Description

  The XML backend is a Python XML-RPC service, which provides - in  
  tandem with ECSpooler - automatic testing of XML files (i.e. test whether a
  XML file is valid, whether it can be validated against a given DTD and 
  whether XPath-Queries can be run successfully).  

Prerequisites

  * A UNIX or UNIX-like operating system, e.g., NetBSD, Solaris, Mac
    OS X, or Linux.

    The XML backend does run on Windows (with some restrictions),
    but this is neither supported nor recommended.

  * "Python":http://python.org/
  
  * "xmllint":http://www.xmlsoft.org/index.html
    
  * "ECSpooler":http://wdok.cs.uni-magdeburg.de/software/ecspooler
  
  * ECSpooler is primarily intended to be used via the
    "ECAutoAssessmentBox":http://plone.org/products/ecautoassessmentbox
    Plone product. So you will normally have a Plone installation
    with ECAutoAssessmentBox.
  
Installation

  * Unzip this archive under the 'backends' directory of your 
    ECSpooler installation (for example '/opt/ESpooler/backends/').  
    
  * Adjust values for INTERPRETER and OPTIONS in the script
    'backends/XML/xmllint.sh' and 'backends/XML/saxon.sh' to your system environemnt.
    
  * Make sure that the scripts 'backends/XML/xmllint.sh' and 'backends/XML/saxon.sh' are
    executable.

Security

  When executing unknown code, you have to take security precautions, 
  e.g., by using chroot, jails, or systrace.  These mechanisms are, 
  however, system-dependent, so we cannot give detailed installation
  instructions.
  
  If you are using "systrace":http://www.systrace.org/, create and
  adjust the script 'backends/XML/xml+systrace' to your system
  environment and set *INTERPRETER* in	
  'backends/XML/config.py' to this script.
  
  Please be aware of the risks if you are *not* able to deal with the
  configuration issues.
  
Quick Start

  After installation, first start ECSpooler (see the ECSpooler
  documentation for details).  Then you should be able to start the 
  XML backend using the following command (you may have to specify 
  the path)::

    backendctl -u root -p bazquux XML start

  If you run this command as root, the process will be started as user
  "nobody".

  Once the backend is running, check the status of the backend using::

    spoolerctl -u root -p bazquux status        

  and get a response similar to::

    {'queue': 0, 'backends': ['XML'], 'pid': 26425, 'results': 0}

  For administrative tasks such as starting/stopping backends
  authentication is required.  Therefore ECSpooler ships with a
  password file ('etc/passwd') containing a user "root" with default
  password "bazquux".  See the ECSpooler documentation if you would
  like to add/change users and passwords.

  Testing a XML program with the XML backend is very easy.
  Have a look at the documentation in 'backends/XML/doc/XMLHandbuch.pdf' (german only) for a
  detailed instruction.
  
  If you want to check whether a submission is
    - well-formed, simply check the checkbox 'Test form'.
    - valid, provide the text field 'DTD' with a DTD.
    - equivalent to a model solution by using XPath expressions, simply provide the text fields
      'Model solution' and 'XPath statements' with a model XML file and linewise XPath expressions,
      respectively.
      
  You can try these values (omit the quotation marks):
    - Test form: Checked
    
    - DTD:
    "<!ELEMENT hallo (#PCDATA)>"
    
    - Model solution: 
    "<?xml version="1.0" standalone="no"?>
    <!DOCTYPE hallo SYSTEM "dtd.dtd">
    <hallo>Hallo Welt!</hallo>"
    
    - XPath expressions:
    "count(${DOC})
    ${DOC}/hallo"

Support

  For questions and discussions about backends, please join the
  "eduComponents mailing
  list":https://listserv.uni-magdeburg.de/mailman/listinfo/educomponents.

Credits

  The XML backend was written by "Christian Baumann":mailto:christian.baumann@st.ovgu.de.
  The saxon XSLT and XQuery processor was originally developed by Michael Kay. See 
  'backends/xml/bin/saxon' for further information.
  
License

  The XML backend is licensed under the
  "GPL":http://opensource.org/licenses/gpl-license.

  Copyright © 2010-2011 Otto-von-Guericke-Universität Magdeburg

  The XML backend is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License as
  published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  The XML backend is distributed in the hope that it will be
  useful, but WITHOUT ANY WARRANTY; without even the implied warranty
  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with ECSpooler; if not, write to the Free Software Foundation,
  Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
