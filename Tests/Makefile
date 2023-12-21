PAS := $(sort $(wildcard *.pas) $(wildcard */*.pas))
RES = $(PAS:.pas=.res)

.PHONY: all parse clean

%.res: %.pas
	git diff -U0 --word-diff=color --word-diff-regex='[A-Za-z]' --no-index -- $@.expect $@
#	diff --color $@.expect $@

all: parse $(RES)

parse: $(PAS)
	../build/ParseFile $(PAS)

clean:
	rm -f **/*.res
