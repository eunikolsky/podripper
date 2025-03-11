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
	@ghcid -c "HSPEC_FORMAT=failed-examples stack ghci --test ripper:lib $(MAIN_TEST_TARGET) --ghci-options=-fobject-code" -T ":main $${MATCH:+--match \"$${MATCH}\"} $${SEED:+--seed $${SEED}}"

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
	# this glob excludes the new `podripper-debug-*.pkg.*` files
	tar tvf podripper-[0-9]*pkg* usr/share/podripper/ >/dev/null

.PHONY:
clean-pkg:
	trash podripper*pkg.tar* || true

.PHONY:
clean-build-linux:
	trash .stack-work/install/x86_64-linux-* || true

.PHONY:
build-ripper-linux:
	@# clearing `SSH_AUTH_SOCK` helps with an error from docker (4.24.2) that has
	@# started appearing all of a sudden:
	@# `Error response from daemon: error while creating mount source path '/host_mnt/private/tmp/com.apple.launchd.abc/Listeners': mkdir /host_mnt/private/tmp/com.apple.launchd.abc/Listeners: operation not supported`
	@# `/private/…/Listeners` here is the value of `SSH_AUTH_SOCK`
	SSH_AUTH_SOCK= stack --docker build

### local testing actions

.PHONY:
local-stream:
	@CONF_DIR=$$PWD/conf stack run --cwd _testing -- run "$${STREAM:-test}" +RTS -s
