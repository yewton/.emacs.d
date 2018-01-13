#!/bin/bash
DIR=$(dirname "$(greadlink -f "$0")")
open -n -a "Emacs" --args -Q -l ${DIR}/init.el
