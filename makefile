
EMACS21 = /home/haya/usr/bin/emacs-21.4
EMACS22 = /home/haya/usr/bin/emacs-22.1

batchtest : twittering-mode.el
	$(EMACS21) --no-init-file --batch -f batch-byte-compile twittering-mode.el
	rm twittering-mode.elc
	$(EMACS22) --no-init-file --batch -f batch-byte-compile twittering-mode.el
	rm twittering-mode.elc

test : twittering-mode.el
	$(EMACS21) -q -l local.test-emacs21.el &
	$(EMACS22) -q -l local.test-emacs22.el &

clean : 
	rm twittering-mode.elc

