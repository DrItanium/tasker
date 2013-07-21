#!/bin/bash
# Simple tasker execution script
pushd .
export $TaskerFSRoot=$HOME/.tasker/
cd $TaskerFSRoot
bin/electron -f2 bin/tasker.clp
popd 
