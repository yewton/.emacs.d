#!/bin/bash -xe
DIR=$(dirname "$(greadlink -f "$0")")
gfind ${DIR} -name '*~' -delete
gfind ${DIR}/lisp -name '*.elc' -delete
gfind ${DIR}/init.d -name '*.elc' -delete
