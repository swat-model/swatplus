#!/bin/bash

# check if Intel setvars.sh exists
if [ -f "/opt/intel/oneapi/setvars.sh" ]; then
    # if exists, load it to set Intel environment
    source /opt/intel/oneapi/setvars.sh
fi

# "exec" will use CMD (or user-specified executable in "docker run" command)
eval "exec $@"