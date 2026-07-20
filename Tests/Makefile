PAS := $(sort $(wildcard *.pas) $(wildcard */*.pas))

.PHONY: all parse clean

all: parse
	@passed=0; failed=0; \
	for file in $(PAS); do \
		base=$${file%.pas}; \
		diff_out=$$(git -c core.safecrlf=false diff -U0 --word-diff=color --word-diff-regex='[A-Za-z0-9]' --no-index -- "$$base.shouldbe.xml" "$$base.out.xml" 2>&1); \
		if [ $$? -eq 0 ]; then \
			passed=$$((passed + 1)); \
		else \
			failed=$$((failed + 1)); \
			echo "FAIL: $$file"; \
			echo "$$diff_out"; \
			echo ""; \
		fi; \
	done; \
	echo "----------------------------------------"; \
	echo "Test Results: $$passed passed, $$failed failed ($$((passed + failed)) total)"; \
	if [ $$failed -gt 0 ]; then exit 1; fi

parse: $(PAS)
	@../build/ParseFile $(PAS) > /dev/null

clean:
	rm -f **/*.res

