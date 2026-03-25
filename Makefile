
.PHONY: antlr-clj build test testcode testjs testjar clean

antlr-clj:
	./script/gen-antlr.sh
	lein javac

build: antlr-clj buildjar.sh
	bash buildjar.sh
# Order is important because one command deletes build artifacts of the other.
# Also, not sure if this would stop correctly on error:
# echo $^ | xargs -n 1 bash

test: testcode-clj testcode-cljs testjar

testcode-clj: antlr-clj
	lein test

testcode-cljs:
	lein doo node once

# Test by executing parser on test file and expect at least one line of output (grep .)
testjar: build testjar.sh
	bash testjar.sh | grep .

# Filenames of :tangle files must be hardcoded :(
buildjs.sh buildjar.sh testjar.sh: README.org
	emacs --batch -l org $< -f org-babel-tangle

clean:
	$(RM) build*.sh
	$(RM) test*.sh
	$(RM) -r src/java/org_parser/antlr/
	$(RM) target/
