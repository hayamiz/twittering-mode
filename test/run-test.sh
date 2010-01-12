#!/bin/bash

emacsen="emacs emacs23 emacs22 emacs21"

continue_on_error=
if [ $# -gt 0 ]; then
    continue_on_error="yes"
fi
exit_status=0

for emacs in $(echo ${emacsen}); do
    if ! which $emacs 1> /dev/null 2> /dev/null; then
	echo "No such executable found: ${emacs}: skipped."
	continue
    fi
    $emacs -q --no-site-file --batch --load $(dirname $0)/el-test-runner.el
    exit_status=$?
    if [ $exit_status != 0 ]; then
	echo "Test failed on $(${emacs} --version | head -n 1)"
	if [ -z "${continue_on_error}" ]; then
	    exit 1
	else
	    echo "sleep 5 secs..."
	    sleep 5
	fi
    fi
done

exit $exit_status
