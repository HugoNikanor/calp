#!/bin/sh

guild use2dot-all \
	--engine fdp \
	--output graph.pdf \
	--default-module '(calp main)' \
	--remove '((datetime) (vcomponent) (hnh util))' \
	module
