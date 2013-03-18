@echo off
set JAVA_PATH="C:\Programme\Java\jre1.6.0_07\bin"
set LIB_PATH="C:\Dokumente und Einstellungen\biermann\dbUnitworkspace\ECSpooler\backends\sql\lib"

%JAVA_PATH%\java -cp %LIB_PATH%\dbunit-2.4.8.jar;%LIB_PATH%\SQLAssessment.jar;%LIB_PATH%\hsqldb.jar;%LIB_PATH%\slf4j-api-1.6.4.jar;%LIB_PATH%\slf4-jcl-1.6.4.jar SQLAssessment.Main 