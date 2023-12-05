PAS := $(sort $(wildcard *.pas) $(wildcard */*.pas))
RES = $(PAS:.pas=.res)

.PHONY: all parse clean

%.res: %.pas
	diff --color $@.expect $@

all: parse $(RES)

parse: $(PAS)
	../build/ParseFile $(PAS)

clean:
	rm -f **/*.res
