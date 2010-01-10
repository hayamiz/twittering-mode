
CATALOGS := ja.po
LANGS := en ja
MARKDOWN_DOCS :=				\
	index.text				\
	how-to-contribute.text			\
	screenshot.text

PUBLISH := ./publish.rb
RM ?= rm -f
HTML_DOCS := $(subst .text,.html,$(MARKDOWN_DOCS))
HTML_ASSETS := style.css images

.PHONY: all clean upload update-po publish

.SUFFIXES: .text .html

all: publish

.text.html: $(PUBLISH) header.rhtml footer.rhtml
	$(PUBLISH) -H header.rhtml -F footer.rhtml \
	  -d lang=\"en\" -d langs="\"$(LANGS)\".split" -d file=\"$@\" \
	  -o $@ $<

upload: upload.bat
	@(! [ -z "$${SF_USERNAME}" ] || (echo "Environmental variable 'SF_USERNAME', which is a username of sf.net, is required."; false))
	sftp -b $< $${SF_USERNAME},twmode@web.sourceforge.net

upload.bat: $(HTML_DOCS) Makefile publish
	files=$$(find -name "*.html"; \
	         find -name "*.html.??"; \
	         find -name ".htaccess"; \
	         find -name "*.css"; \
	         find -name "*.png"; \
	         find -name "*.jpg"); \
	dirs=$$(echo $$files|xargs -n1 dirname|sort|uniq); \
	(echo "cd htdocs"; \
	 echo $$dirs | xargs -n 1 echo "-mkdir"; \
	 for file in $$files; do echo "put $${file} $${file}"; done) > $@

twmode.pot: $(HTML_DOCS)
	xml2po -k -o $@ $(HTML_DOCS)
	for catalog in $(CATALOGS); do \
	  [ -f $$catalog ] || cp $@ $$catalog; \
	done

update-po: twmode.pot Makefile
	for catalog in $(CATALOGS); do \
	  xml2po -k -m xhtml -u $$catalog $(HTML_DOCS); \
	done

publish: $(HTML_DOCS) $(CATALOGS)
	for catalog in $(CATALOGS); do \
	  lang=$$(echo $$catalog|sed 's/.po$$//'); \
	  [ -d $$lang ] || mkdir $$lang; \
	  cp -t $$lang -r $(HTML_ASSETS); \
	  for text in $(MARKDOWN_DOCS); do \
	    html=$$(echo $$text|sed 's/.text$$/.html/'); \
	    $(PUBLISH) -H header.rhtml -F footer.rhtml \
	      -d lang=\"$$lang\" -d langs="\"$(LANGS)\".split" \
	      -d file=\"$$html\" --po-file=$$catalog \
	      -o $$lang/$$html $$text; \
	  done; \
	done

clean:
	$(RM) *.html *.html upload.bat
	for catalog in $(CATALOGS); do \
	  lang=$$(echo $$catalog|sed 's/.po$$//'); \
	  $(RM) $$lang/*.html; \
	done
