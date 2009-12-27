
EMACS ?= emacs

.PHONY: all clean update-po check

all:

update-po:
	$(MAKE) -C doc update-po

check:
	./test/run-test.sh

clean : 
	rm twittering-mode.elc
