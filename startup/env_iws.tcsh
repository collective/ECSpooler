# source this file before running tests/runalltests --> no need to adjust
# backends/**/*.sh
setenv TMPDIR /tmp/backends
rm -rf $TMPDIR
mkdir -p $TMPDIR
setenv ERL_HOME /local/usr/lib/erlang/erts-5.5.4
setenv HUGS_HOME /local/usr
setenv HUGSDIR /local/usr/lib/hugs
setenv PYTHONHOME /usr
setenv CL_HOME /local/usr
setenv SCHEME_HOME /local/usr
setenv PL_HOME /local/usr
if ( $?JAVA_HOME == 0 ) setenv JAVA_HOME /opt/java
