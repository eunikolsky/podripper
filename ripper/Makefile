.PHONY:
check: check-build check-test
# FIXME add check-hlint

.PHONY:
check-build:
	stack --verbosity error build --fast

.PHONY:
check-test:
	stack --verbosity error test --fast --ta='-f silent' $(MAIN_TEST_TARGET)

.PHONY:
testd:
	@ghcid -c "stack ghci --test ripper:lib $(MAIN_TEST_TARGET) --ghci-options=-fobject-code" --test "main"

.PHONY:
testfw:
	@stack test --fast --file-watch $(MAIN_TEST_TARGET)

.PHONY:
buildd:
	@ghcid -c "stack ghci"

.PHONY:
buildfw:
	@stack build --fast --file-watch

.PHONY:
install-precommit-hook:
	# GNU ln supports the `-r` option to create a relative symlink
	gln -srv .git-pre-commit ../.git/hooks/pre-commit

MAIN_TEST_TARGET = ripper:test:ripper-test
