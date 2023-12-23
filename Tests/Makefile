PAS := $(sort $(wildcard *.pas) $(wildcard */*.pas))
OUT = $(PAS:.pas=.out.xml)

.PHONY: all parse clean

%.out.xml: %.pas
	git diff -U0 --word-diff=color --word-diff-regex='[A-Za-z0-9]' --no-index -- $(@:.out.xml=.shouldbe.xml) $@

all: parse $(OUT)

parse: $(PAS)
	../build/ParseFile $(PAS)

clean:
	rm -f **/*.res
