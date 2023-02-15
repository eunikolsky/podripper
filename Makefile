.PHONY:
build-pkg: clean-pkg
	docker build --progress plain -t rssgen-build -o . .

.PHONY:
rebuild-pkg: clean-build-linux build-rssgen-linux build-ripper-linux build-pkg

.PHONY:
upload-pkg:
	scp podripper*pkg* podripper:'~'

.PHONY:
clean-pkg:
	trash podripper*pkg.tar* || true

.PHONY:
clean-build-linux:
	trash {rssgen,ripper}/.stack-work/install/x86_64-linux-* || true

.PHONY:
build-rssgen-linux:
	cd rssgen && stack --docker build

.PHONY:
build-ripper-linux:
	cd ripper && stack --docker build
