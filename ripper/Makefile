.PHONY:
sbf:
	@stack build --fast

.PHONY:
sbfw:
	@stack build --fast --file-watch

.PHONY:
ghcid-test:
	@ghcid --command "stack ghci --test --main-is ripper:test:ripper-test --ghci-options=-fobject-code" --test "main"

.PHONY:
ghcid:
	@ghcid --command "stack ghci"
