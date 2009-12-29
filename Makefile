
EMACS ?= emacs

DISTRIB_FILES = twittering-mode.el \
		README README.ja \
		NEWS NEWS.ja \
		INSTALL INSTALL.ja \
		win-curl

.PHONY: all check clean update-po release release-upload

all:

update-po:
	$(MAKE) -C doc update-po

check:
	./test/run-test.sh -y

clean : 
	rm -f twittering-mode.elc README *.zip *.tar.gz

VERSION = $$(cat VERSION)
DISTRIB_DIR = twittering-mode-$(VERSION)

README: README.markdown
	cp $< $@
release: $(DISTRIB_FILES)
	@(! [ -z "$${SF_USERNAME}" ] || (echo "Environmental variable 'SF_USERNAME', which is a username of sf.net, is required."; false))
	@echo -n "updated VERSION and LAST-VERSION files? [y or n]: "; read ans; [ "$${ans}" = "y" ]
	@echo -n "wrote NEWS file? [y or n]: "; read ans; [ "$${ans}" = "y" ]
	ruby misc/vernum-updater.rb \
	  --prev-version=$$(cat LAST-VERSION) --next-version=$$(cat VERSION) \
	  VERSION twittering-mode.el doc/web/index.html
	git commit -a
	@([ -d $(DISTRIB_DIR) ] && rm -rf $(DISTRIB_DIR)) || true
	mkdir $(DISTRIB_DIR)
	cp -r -t $(DISTRIB_DIR)/ $(DISTRIB_FILES)
	zip -r $(DISTRIB_DIR).zip $(addprefix $(DISTRIB_DIR)/,$(DISTRIB_FILES))
	tar czvf $(DISTRIB_DIR).tar.gz $(addprefix $(DISTRIB_DIR)/,$(DISTRIB_FILES))
	rm -rf $(DISTRIB_DIR)

release-upload: release
	(echo "cd /home/frs/project/t/tw/twmode/"; \
	 echo "-rm $(DISTRIB_DIR)/*"; \
	 echo "-rmdir $(DISTRIB_DIR)"; \
	 echo "mkdir $(DISTRIB_DIR)"; \
	 echo "cd $(DISTRIB_DIR)"; \
	 echo "put $(DISTRIB_DIR).zip"; \
	 echo "put $(DISTRIB_DIR).tar.gz") > upload.bat
	sftp -b upload.bat $${SF_USERNAME},twmode@web.sourceforge.net
	rm -f upload.bat

