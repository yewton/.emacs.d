# see https://gist.github.com/sighingnow/deee806603ec9274fd47#gistcomment-2970470
OS_NAME := $(shell uname -s | tr A-Z a-z)
FIND := find
RUNEMACS := emacs
ORGS := $(wildcard ./lisp/*.org)
INITS := toncs-bootstrap.el early-init.el init.el
ELS := $(ORGS:.org=.el)
ELCS := $(ELS:.el=.elc)
DICT_DIR := ./var/skk-jisyo/
DICTS := $(addprefix SKK-JISYO.,L jinmei geo station propernoun)
DICT_PATHS := $(addsuffix .utf8,$(addprefix $(DICT_DIR),$(DICTS)))
KAOMOJI_DICT := $(DICT_DIR)kaomoji.skk.utf8
KAOMOJI_DICT_URL := https://raw.githubusercontent.com/yewton/dicts/master/kaomoji.skk.utf8
STATUS := ./var/el-get/.status.el
ERROR_ON_WARN ?= nil

ifeq ($(OS_NAME),darwin)
	FIND := gfind
    RUNEMACS := open -n -a "Emacs" --args
endif

MAKEFLAGS += --no-builtin-rules
.SUFFIXES:
.PHONY: all gc clean run test borg

all: $(INITS) $(STATUS) $(ELCS) $(DICT_PATHS) $(KAOMOJI_DICT)

$(INITS): README.org
$(ELS): %.el: %.org

$(INITS) $(ELS):
	emacs --quick --batch --load "ob" --eval "(org-babel-tangle-file \"$<\")"
	touch "$@"

$(STATUS): $(addsuffix .el,toncs-bootstrap $(addprefix lisp/toncs-,deps stdlib el-get))
	emacs --quick --batch --load toncs-bootstrap.el --load toncs-el-get --funcall toncs-el-get-install

lisp/toncs-el-get.elc: lisp/toncs-stdlib.elc lisp/toncs-deps.elc
lisp/toncs-config-org.elc:  lisp/toncs-stdlib.elc

lib/borg/borg.el:
	make -f borg.mk bootstrap-borg
	make -f borg.mk bootstrap # 本当は要らないけど elc 作成に必要になるのでついでにやっちゃう

lisp/%.elc: lisp/%.el toncs-bootstrap.el lisp/toncs-deps.el | lib/borg/borg.el
	emacs --quick --batch --load toncs-bootstrap.el --eval "(setq byte-compile-error-on-warn $(ERROR_ON_WARN))" \
	-L lib/borg --load borg --funcall borg-initialize --funcall batch-byte-compile $<

$(DICT_DIR):
	mkdir -p $@

$(DICT_PATHS): | $(DICT_DIR)
	curl --silent "https://skk-dev.github.io/dict/$(basename $(@F)).gz" | gunzip -c | iconv -f euc-jisx0213 -t utf8 | sed 's/coding: euc-jp/coding: utf-8/' > $@

$(KAOMOJI_DICT): | $(DICT_DIR)
	curl --silent "$(KAOMOJI_DICT_URL)" > $@

gc:
	rm -vf $(ELCS)
	$(FIND) . -name '*~' -delete -print

clean: gc
	rm -vfr $(INITS) $(ELS)

run: all
	$(RUNEMACS) --no-init-file --chdir $(PWD) --debug-init -l $(PWD)/early-init.el -l $(PWD)/init.el >/dev/null 2>&1 &

test: all
	emacs --batch --load ert --load test/init-test.el -f ert-run-tests-batch-and-exit

run-nw: all
	emacs --no-init-file --no-window-system --chdir $(PWD) --debug-init -l $(PWD)/early-init.el -l $(PWD)/init.el
