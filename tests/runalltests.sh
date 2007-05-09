#! /bin/sh

cd `dirname $0`

WORK_DIR=`pwd`
WORK_DIR=${WORK_DIR%%/tests}

PYTHON="/opt/python/bin/python"
#PYTHON="/usr/local/bin/python2.4"

PYTHONPATH="$WORK_DIR"; export PYTHONPATH

exec "$PYTHON" runalltests.py
