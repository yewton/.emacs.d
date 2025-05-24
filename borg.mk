DRONES_DIR = $(shell git config "borg.drones-directory" || echo "lib")
INIT_FILES = borg-init.el
EMACS_ARGUMENTS = -Q --batch --load toncs-bootstrap.el
MAKE = make -f borg.mk

-include $(DRONES_DIR)/borg/borg.mk

bootstrap-borg:
	@git submodule--helper clone --name borg --path $(DRONES_DIR)/borg \
	--url https://github.com/emacscollective/borg.git
	@cd $(DRONES_DIR)/borg; git symbolic-ref HEAD refs/heads/main
	@cd $(DRONES_DIR)/borg; git reset --hard HEAD

# up-to-date なら再コンパイルしない build
build-fast:
	$(Q)$(EMACS) $(EMACS_ARGUMENTS) $(EMACS_EXTRA) $(SILENCIO) \
	$(BORG_ARGUMENTS) \
    --eval "(setq borg-compile-function (lambda (file) (byte-recompile-file file nil 0)))" \
	--eval "(advice-add 'borg-clean :around #'ignore)" \
	--eval "(advice-add 'borg--remove-autoloads :around #'ignore)" \
	--funcall borg-batch-rebuild $(INIT_FILES) 2>&1

bootstrap-fast:
	$(Q)printf "\n=== Running 'git submodule init' ===\n\n"
	$(Q)git submodule init
	$(Q)printf "\n=== Running '$(BORG_DIR)borg.sh clone' ===\n"
	$(Q)$(BORG_DIR)borg.sh clone
	$(Q)printf "\n=== Running '$(BORG_DIR)borg.sh checkout' ===\n"
	$(Q)$(BORG_DIR)borg.sh checkout --reset-hard
	$(Q)printf "\n=== Running 'make rebuild' ===\n\n"
	$(Q)$(MAKE) build-fast
	$(Q)printf "\n=== Bootstrapping finished ===\n\n"
	$(Q)git submodule status
