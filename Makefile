# see https://gist.github.com/sighingnow/deee806603ec9274fd47#gistcomment-2970470
OS_NAME := $(shell uname -s | tr A-Z a-z)
FIND := find
RUNEMACS := emacs
ORGS := $(wildcard ./lisp/*.org)
INITS := toncs-bootstrap.el early-init.el init.el
ELS := $(ORGS:.org=.el)
DICT_DIR := ./var/skk-jisyo/
DICTS := $(addprefix SKK-JISYO.,L jinmei geo station propernoun)
DICT_PATHS := $(addsuffix .utf8,$(addprefix $(DICT_DIR),$(DICTS)))
KAOMOJI_DICT := $(DICT_DIR)kaomoji.skk.utf8
KAOMOJI_DICT_URL := https://raw.githubusercontent.com/yewton/dicts/master/kaomoji.skk.utf8
ERROR_ON_WARN ?= nil
TEST_FILES := $(wildcard test/*-test.el)

ifeq ($(OS_NAME),darwin)
	FIND := gfind
    RUNEMACS := open -n -a "Emacs" --args
endif

MAKEFLAGS += --no-builtin-rules
.SUFFIXES:
.PHONY: all gc clean run test borg lisp

all: lisp $(DICT_PATHS) $(KAOMOJI_DICT)

$(INITS): README.org
$(ELS): %.el: %.org

$(INITS) $(ELS):
	emacs --quick --batch --load "ob" --eval "(org-babel-tangle-file \"$<\")"
	touch "$@"

# drone (lib/ 配下の submodule) が index の記録とずれているときだけ同期する。
# git diff / git submodule update はどちらも index と比較するので、
# 意図的に進めた drone は `git add lib/<drone>` でステージしておけば同期対象にならない。
borg:
	@sync=0; \
	git diff --quiet --ignore-submodules=dirty -- lib 2>/dev/null || sync=1; \
	for p in lib/*/; do [ -e "$$p.git" ] || sync=1; done; \
	if [ "$$sync" = 1 ]; then \
		printf "\n=== Syncing drones ===\n\n"; \
		git submodule update --init; \
		$(MAKE) -f borg.mk build-fast; \
	fi

lisp: borg $(ELS) toncs-bootstrap.el
	emacs --quick --batch --load toncs-bootstrap.el --eval "(setq byte-compile-error-on-warn $(ERROR_ON_WARN))" \
	-L lib/borg --load borg --funcall borg-initialize --eval "(batch-byte-recompile-directory 0)" lisp

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
	emacs --batch --load ert $(addprefix --load ,$(TEST_FILES)) -f ert-run-tests-batch-and-exit

run-nw: all
	emacs --no-init-file --no-window-system --chdir $(PWD) --debug-init -l $(PWD)/early-init.el -l $(PWD)/init.el
