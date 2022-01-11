
.PHONY: build test testcode testjs testjar clean

build: buildjar.sh
	bash buildjar.sh
# Order is important because one command deletes build artifacts of the other.
# Also, not sure if this would stop correctly on error:
# echo $^ | xargs -n 1 bash

test: testcode-clj testcode-cljs testjar

testcode-clj:
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
	$(RM) target/
