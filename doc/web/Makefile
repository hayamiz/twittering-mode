
RM ?= rm -f
PUBLISH := ./publish.rb

HTML_DOCS := index.html

.PHONY: all clean upload

.SUFFIXES: .text .html

all: $(HTML_DOCS)

.text.html: $(PUBLISH)
	$(PUBLISH) $<

upload: upload.bat
	@(! [ -z "$${SF_USERNAME}" ] || (echo "Environmental variable 'SF_USERNAME', which is a username of sf.net, is required."; false))
	sftp -b $< $${SF_USERNAME},twmode@web.sourceforge.net

upload.bat: $(HTML_DOCS) Makefile
	(echo "cd htdocs"; echo "-mkdir images"; ls *.html *.css images/*.jpg images/*.gif images/*.png | while read file; do echo "put $${file} $${file}"; done) > $@

clean:
	$(RM) *.html upload.bat
