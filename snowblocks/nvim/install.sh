#!/bin/sh

command -v nvim > /dev/null
if ! [ $? == 0 ]; then
	echo "nvim not installed, aborting"
	exit 1
fi

command -v npm > /dev/null
if ! [ $? == 0 ]; then
	echo "npm not installed, required for coc.nvim aborting"
	exit 1
fi


