### build actions

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
	@# GNU ln supports the `-r` option to create a relative symlink
	@gln -srv .git-pre-commit .git/hooks/pre-commit

MAIN_TEST_TARGET = ripper:test:ripper-test

### packaging actions

.PHONY:
build-pkg: clean-pkg
	docker build --progress plain -t podripper-build -o . .

.PHONY:
rebuild-pkg: clean-build-linux build-ripper-linux build-pkg

.PHONY:
upload-pkg:
	scp podripper*pkg* podripper:'~'

.PHONY:
test-pkg:
	tar tvf podripper*pkg* usr/share/podripper/ >/dev/null

.PHONY:
clean-pkg:
	trash podripper*pkg.tar* || true

.PHONY:
clean-build-linux:
	trash .stack-work/install/x86_64-linux-* || true

.PHONY:
build-ripper-linux:
	stack --docker build

### local testing actions

.PHONY:
local-eradio:
	@export RIPPER="$$( stack path --local-install-root )/bin/ripper-exe" CONF_DIR="$$PWD/conf" SHAKE_DIR="$$PWD/_testing/.shake"; cd _testing; ../podripper.sh eradio
