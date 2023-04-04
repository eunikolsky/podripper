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
	trash ripper/.stack-work/install/x86_64-linux-* || true

.PHONY:
build-ripper-linux:
	cd ripper && stack --docker build
