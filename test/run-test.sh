#!/bin/bash

emacsen="emacs23 emacs22 emacs21"

for emacs in $(echo ${emacsen}); do
    if ! which $emacs 1> /dev/null 2> /dev/null; then
	echo "No such executable found: ${emacs}: skipped."
	continue
    fi
    $emacs -q --no-site-file --batch --load $(dirname $0)/el-test-runner.el
    if [ $? != 0 ]; then
	echo "Test failed on $(${emacs} --version | head -n 1)"
	exit 1
    fi
done
