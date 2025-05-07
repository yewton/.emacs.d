DRONES_DIR = $(shell git config "borg.drones-directory" || echo "lib")
INIT_FILES = borg-init.el
EMACS_ARGUMENTS = -Q -L . --load toncs-bootstrap.el

-include $(DRONES_DIR)/borg/borg.mk

bootstrap-borg:
	@git submodule--helper clone --name borg --path $(DRONES_DIR)/borg \
	--url https://github.com/emacscollective/borg.git
	@cd $(DRONES_DIR)/borg; git symbolic-ref HEAD refs/heads/main
	@cd $(DRONES_DIR)/borg; git reset --hard HEAD
