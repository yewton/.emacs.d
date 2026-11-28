DRONES_DIR = $(shell git config "borg.drones-directory" || echo "lib")
INIT_FILES = borg-init.el
MAKE = make -f borg.mk

-include $(DRONES_DIR)/borg/borg.mk

bootstrap-borg:
	@git submodule--helper clone --name borg --path $(DRONES_DIR)/borg \
	--url https://github.com/emacscollective/borg.git
	@cd $(DRONES_DIR)/borg; git symbolic-ref HEAD refs/heads/main
	@cd $(DRONES_DIR)/borg; git reset --hard HEAD

# up-to-date なら再コンパイルしない build。drone ごとに別プロセスでビルドする
# (理由は etc/borg/build-fast.el を参照)
build-fast:
	$(Q)$(EMACS_BATCH) $(BORG_ARGS) --load etc/borg/build-fast.el \
	--eval '(setq toncs-borg-build-fast-make "$(MAKE)")' \
	--funcall toncs-borg-build-fast $(INIT_FILES) 2>&1

# build-fast から drone ごとに呼ばれる
build-fast/%: .FORCE
	$(Q)$(EMACS_BATCH) $(BORG_ARGS) --load etc/borg/build-fast.el \
	--eval '(borg-build "$*")' 2>&1

bootstrap-fast:
	$(Q)printf "\n=== Running 'git submodule init' ===\n\n"
	$(Q)git submodule init
	$(Q)printf "\n=== Running '$(BORG_DIR)borg.sh clone' ===\n"
	$(Q)$(BORG_DIR)borg.sh clone
	$(Q)printf "\n=== Running '$(BORG_DIR)borg.sh checkout' ===\n"
	$(Q)$(BORG_DIR)borg.sh checkout
	$(Q)printf "\n=== Running 'make rebuild' ===\n\n"
	$(Q)$(MAKE) build-fast
	$(Q)printf "\n=== Bootstrapping finished ===\n\n"
	$(Q)git submodule status
