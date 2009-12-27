#!/bin/bash

emacsen="emacs23 emacs22 emacs21"

for emacs in $(echo ${emacsen}); do
    $emacs -q --no-site-file --batch --load $(dirname $0)/el-test-runner.el
done
