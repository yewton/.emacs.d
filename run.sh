#!/bin/bash
DIR=$(dirname "$(greadlink -f "$0")")
open -n -a "Emacs" --args -q --debug-init -l ${DIR}/init.el
