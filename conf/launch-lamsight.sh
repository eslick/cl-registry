#!/bin/bash

umask 022

#LISP=/usr/local/bin/sbcl
LISP=/usr/local/bin/ccl
BASE=/usr/local/lamsight-rf
SOURCE=$BASE/src/cl-registry

rm -f $BASE/log/detachtty.log $BASE/run/lamsight.* $BASE/run/lamsight-dribble

REGPORT=8080
REGADDR="localhost"
REGCONFIG="lamsight-production"
export REGPORT REGCONFIG REGADDR

# SBCL
# detachtty --dribble-file $BASE/run/lamsight-dribble \
#     --log-file $BASE/log/detachtty.log \
#     --pid-file $BASE/run/lamsight.pid \
#     $BASE/run/lamsight.sock \
#     $LISP --eval "(load \"$SOURCE/registry-loader.lisp\")" \
#     	  --eval '(in-package :registry-loader)' \
# 	  --eval '(when (find-package :registry) (in-package :registry))' \
# 	  --eval "(registry-loader:load-swank 4005)"

# CCL
detachtty --dribble-file $BASE/run/lamsight-dribble \
    --log-file $BASE/log/detachtty.log \
    --pid-file $BASE/run/lamsight.pid \
    $BASE/run/lamsight.sock \
    $LISP -e "(load \"$SOURCE/registry-loader.lisp\")" \
    	  -e '(in-package :registry-loader)' \
	  -e '(when (find-package :registry) (in-package :registry))' \
	  -e "(registry-loader:load-swank 4005)"

