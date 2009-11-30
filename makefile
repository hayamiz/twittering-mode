
.PHONY: unit-test clean

EMACS ?= emacs

# batchtest : twittering-mode.el
# 	$(EMACS21) --no-init-file --batch -f batch-byte-compile twittering-mode.el
# 	rm twittering-mode.elc
# 	$(EMACS22) --no-init-file --batch -f batch-byte-compile twittering-mode.el
# 	rm twittering-mode.elc
# 
# test : twittering-mode.el
# 	$(EMACS21) -q -l local.test-emacs21.el &
# 	$(EMACS22) -q -l local.test-emacs22.el &

unit-test: test/run-test.el
	$(EMACS) --batch -Q -l $<

clean : 
	rm twittering-mode.elc

