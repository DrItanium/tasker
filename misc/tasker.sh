#!/bin/bash
# Simple tasker execution script
pushd .
export $ElectronFSRoot=$HOME/.tasker/
cd $ElectronFSRoot
bin/electron -f2 bin/tasker.clp
popd 
