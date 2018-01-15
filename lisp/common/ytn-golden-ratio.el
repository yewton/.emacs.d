;;; ytn-golden-ratio.el --- Golden ratio -*- lexical-binding: t -*-

;; Copyright (C) 2018 Yuto SASAKI
;; Author: Yuto SASAKI <yewton@gmail.com>

;;; Commentary:

;; Automatic resizing of Emacs windows to the golden ratio

;;; Code:

(require 'golden-ratio)

(setq golden-ratio-exclude-modes
      '("bs-mode"
        "calc-mode"
        "dired-mode"
        "ediff-mode"
        "gdb-breakpoints-mode"
        "gdb-disassembly-mode"
        "gdb-frames-mode"
        "gdb-inferior-io-mode"
        "gdb-inferior-io-mode"
        "gdb-locals-mode"
        "gdb-memory-mode"
        "gdb-registers-mode"
        "gdb-threads-mode"
        "gud-mode"
        "gud-mode"
        "restclient-mode"
        "speedbar-mode"))

(setq golden-ratio-extra-commands
      '(windmove-left
        windmove-right
        windmove-down
        windmove-up
        buf-move-down
        buf-move-left
        buf-move-right
        buf-move-up
        winum-select-window-0
        winum-select-window-1
        winum-select-window-2
        winum-select-window-3
        winum-select-window-4
        winum-select-window-5
        winum-select-window-6
        winum-select-window-7
        winum-select-window-8
        winum-select-window-9
        xref-find-definitions))
(golden-ratio-mode)

(provide 'ytn-golden-ratio)
;;; ytn-golden-ratio.el ends here
