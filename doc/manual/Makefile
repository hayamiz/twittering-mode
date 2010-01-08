
.PHONY: all update-po clean

all: twmode/twmode.html

update-po:

twmode/twmode.html: twmode.texinfo
	texi2html --split chapter $<

clean:
	rm -rf twmode
