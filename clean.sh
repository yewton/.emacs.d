#!/bin/bash
DIR=$(dirname "$(greadlink -f "$0")")
gfind ./lisp -name '*.elc' -delete
