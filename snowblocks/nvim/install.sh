#!/bin/sh

command -v nvim > /dev/null
if ! [ $? == 0 ]; then
	echo "nvim not installed, aborting"
	exit 1
fi
