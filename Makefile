export EMACS ?= $(shell command -v emacs 2>/dev/null)
CASK_DIR := $(shell cask package-directory)

$(CASK_DIR): Cask
	cask install
	@touch $(CASK_DIR)

.PHONY: cask
cask: $(CASK_DIR)

.PHONY: compile
compile: cask
	cask emacs -batch -L . -L test  \
          --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile org-nix-shell.el; \
	  (ret=$$? ; cask clean-elc && exit $$ret)

.PHONY: test
test: compile
	cask emacs --batch -L . -L test -l org-nix-shell-test.el -f ert-run-tests-batch

.PHONY: lint
lint: cask
	cask emacs --batch -L . -l package-lint -f package-lint-batch-and-exit org-nix-shell.el

.PHONY: bench
bench: compile
	hyperfine 'cask emacs --batch -L . -L test -l bench.el -f org-nix-shell--run-bench'
