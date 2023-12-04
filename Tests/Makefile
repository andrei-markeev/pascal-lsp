PAS := $(sort $(wildcard *.pas) $(wildcard */*.pas))
RES = $(PAS:.pas=.res)

.PHONY: all clean

%.res: %.pas
	../build/ParseFile $<
	diff --color $@.expect $@

all: $(RES)

clean:
	rm -f **/*.res
