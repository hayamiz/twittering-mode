
EMACS ?= emacs

DISTRIB_FILES = twittering-mode.el README INSTALL win-curl

.PHONY: all check clean update-po release

all:

update-po:
	$(MAKE) -C doc update-po

check:
	./test/run-test.sh

clean : 
	rm -f twittering-mode.elc README

DISTRIB_DIR = twittering-mode-$$(cat VERSION)

release: $(DISTRIB_FILES)
	@([ -d $(DISTRIB_DIR) ] && rm -rf $(DISTRIB_DIR)) || true
	mkdir $(DISTRIB_DIR)
	cp README.markdown $(DISTRIB_DIR)/
	cp -r -t $(DISTRIB_DIR)/ $(DISTRIB_FILES)
	zip -r $(DISTRIB_DIR).zip $(addprefix $(DISTRIB_DIR)/,$(DISTRIB_FILES))
	tar czvf $(DISTRIB_DIR).tar.gz $(addprefix $(DISTRIB_DIR)/,$(DISTRIB_FILES))
	rm -rf $(DISTRIB_DIR)

