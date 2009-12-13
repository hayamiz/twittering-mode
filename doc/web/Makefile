
RM ?= rm -f
PUBLISH := ./publish.rb

.PHONY: all clean

.SUFFIXES: .text .html

all: index.html

.text.html:
	$(PUBLISH) $<

clean:
	$(RM) *.html
