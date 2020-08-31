# see https://gist.github.com/sighingnow/deee806603ec9274fd47#gistcomment-2970470
OS_NAME := $(shell uname -s | tr A-Z a-z)
FIND := find
RUNEMACS := emacs
ORGS := $(wildcard ./lisp/*.org)
INITS := toncs-bootstrap.el early-init.el init.el
ELS := $(ORGS:.org=.el)
ELCS := $(ELS:.el=.elc)
STATUS := ./var/el-get/.status.el
ERROR_ON_WARN ?= nil

tangle = emacs --quick --batch --load "ob" --eval "(org-babel-tangle-file \"$<\")"

ifeq ($(OS_NAME),darwin)
	FIND := gfind
    RUNEMACS := open -n -a "Emacs" --args
endif

.PHONY: all gc clean run

all: $(INITS) $(STATUS) $(ELCS)

$(INITS): %.el: README.org
	$(tangle)

$(ELS): lisp/%.el: lisp/%.org
	$(tangle)

$(STATUS): $(addsuffix .el,toncs-bootstrap $(addprefix lisp/,toncs-deps toncs-stdlib toncs-el-get))
	emacs --quick --batch --load toncs-bootstrap.el --load toncs-el-get --funcall toncs-el-get-install

$(ELCS): lisp/%.elc: lisp/%.el
	emacs --quick --batch --load toncs-bootstrap.el --eval "(setq byte-compile-error-on-warn $(ERROR_ON_WARN))" --funcall batch-byte-compile $<

gc:
	rm -vf $(ELCS)
	$(FIND) . -name '*~' -delete -print

clean: gc
	rm -vfr ./var $(INITS) $(ELS) $(ELCS)

run: all
	$(RUNEMACS) --no-init-file --chdir $(PWD) --debug-init -l $(PWD)/early-init.el -l $(PWD)/init.el >/dev/null 2>&1 &

run-nw: all
	emacs --no-init-file --no-window-system --chdir $(PWD) --debug-init -l $(PWD)/early-init.el -l $(PWD)/init.el
