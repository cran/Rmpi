#!/bin/sh 
if [ $# -lt 3 ]; then
	echo "Need 3 arguments" > err.log
	exit 1
fi

Rscript=$1
if [ ! -r $Rscript ]; then
	echo $Rscript "does not exist or is not readable!" > err.log
	exit 1
fi

if [ $3 == "needlog" ]; then
	hn=`hostname -s`
	R --no-init-file --slave --no-save < $1 > $hn.$2.$$.log
else
	R --no-init-file --slave --no-save < $1 > /dev/null
fi
