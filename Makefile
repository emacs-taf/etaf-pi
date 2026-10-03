.DEFAULT_GOAL := check
.NOTPARALLEL:
EMACS ?= emacs

# Providers live beside the application (standalone) or two levels up (workspace).
PROVIDER_ROOT := $(if $(wildcard ../etaf/etaf.el),..,$(if $(wildcard ../../etaf/etaf.el),../..,))

LOAD_PATH = -L . -L lisp -L tests -L $(PROVIDER_ROOT)/etaf -L $(PROVIDER_ROOT)/etaf-ui \
            -L $(PROVIDER_ROOT)/ebox -L $(PROVIDER_ROOT)/ecss

SOURCES = $(wildcard lisp/*.el) etaf-pi.el

.PHONY: compile load test check structure-check clean acceptance api-check setup-hooks

compile:
	rm -f *.elc lisp/*.elc tests/*.elc
	$(EMACS) -Q --batch $(LOAD_PATH) \
		--eval '(setq load-prefer-newer t byte-compile-error-on-warn t)' \
		--eval '(unless (file-exists-p "$(PROVIDER_ROOT)/etaf/etaf.el") (error "找不到 ETAF 检出（../etaf 或 ../../etaf）"))' \
		--eval '(dolist (file (list $(foreach file,$(SOURCES),"$(file)"))) (unless (byte-compile-file file) (kill-emacs 1)))'

load: compile
	$(EMACS) -Q --batch $(LOAD_PATH) \
		--eval '(require (quote etaf-pi))' \
		--eval '(princ "etaf-pi load OK\n")'

test: acceptance

acceptance:
	python3 scripts/run-acceptance.py --emacs "$(EMACS)"

api-check:
	$(EMACS) -Q --batch $(LOAD_PATH) --eval '(setq load-prefer-newer t)' -l scripts/check-api.el

check: structure-check compile acceptance api-check

structure-check:
	python3 scripts/check-repository.py

setup-hooks:
	git config --local core.hooksPath .githooks

clean:
	rm -f *.elc lisp/*.elc tests/*.elc
