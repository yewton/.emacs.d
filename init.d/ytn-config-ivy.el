;; -*- lexical-binding: t; -*-
(require 'delight)
(require 'f)
(require 'no-littering)

(eval-when-compile (require 'use-package))

;; cf. https://github.com/abo-abo/swiper/wiki/FAQ#sorting-commands-by-frequency
(use-package smex
  :config
  (setq smex-save-file (f-join no-littering-var-directory "smex-items")
        smex-history-length 9))

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package ivy
  :delight
  :demand t
  :commands (ivy-mode)
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume)
         :map ivy-minibuffer-map
         ("C-r" . ivy-previous-line)
         ("C-l" . ivy-backward-delete-char))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-height 30
        enable-recursive-minibuffers t
        ivy-initial-inputs-alist '((org-refile . "")
                                   (org-agenda-refile . "")
                                   (org-capture-refile . "")
                                   (counsel-M-x . "^")
                                   (counsel-describe-function . "^")
                                   (counsel-describe-variable . "^")
                                   (counsel-org-capture . "^")
                                   (Man-completion-table . "^")
                                   (woman . "^"))))

(use-package ivy-hydra
  :after (ivy hydra))

(use-package counsel
  :commands (counsel-ag)
  :config
  (setq counsel-describe-function-preselect 'ivy-function-called-at-point
        counsel-find-file-ignore-regexp "\\(?:\\`[#.]\\)\\|\\(?:[#~]\\'\\)\\|\\(?:\\.elc\\'\\)")
  (defun ytn-open-junk-file (&optional arg)
    "Open junk file using counsel.

  When ARG is non-nil search in junk files."
    (interactive "P")
    (let* ((open-junk-file-format (f-join no-littering-var-directory "junk/%Y/%m/%d-%H%M%S."))
           (fname (format-time-string open-junk-file-format (current-time)))
           (rel-fname (file-name-nondirectory fname))
           (junk-dir (file-name-directory fname))
           (default-directory junk-dir))
      (if arg
          (counsel-ag nil junk-dir "" "[junk]")
        (mkdir junk-dir 'parents)
        (counsel-find-file rel-fname))))
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<help> b" . counsel-descbinds)
         ("<help> f" . counsel-describe-function)
         ("<help> v" . counsel-describe-variable)
         ("<help> l" . counsel-load-library)
         ("<help> a" . counsel-apropos)
         ("<help> S" . counsel-info-lookup-symbol)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-rg)
         ("C-x l" . counsel-locate)
         ("C-x C-r" . counsel-recentf)
         ("M-y" . counsel-yank-pop)
         ("C-x C-z". ytn-open-junk-file)
         :map read-expression-map
         ("C-r" . counsel-minibuffer-history)))

(use-package ivy-xref
  :demand t
  :commands (ivy-xref-show-xrefs)
  :config
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package counsel-projectile
  :after projectile
  :delight
  :commands (counsel-projectile-mode)
  :init
  (counsel-projectile-mode))

(use-package company
  :delight
  :commands (global-company-mode)
  :init
  (add-hook 'emacs-startup-hook #'global-company-mode)
  :config
  (bind-key [remap next-line] 'company-select-next company-active-map)
  (bind-key [remap previous-line] 'company-select-previous company-active-map)
  (bind-key "C-f" 'company-show-location company-active-map)
  (bind-key "M-f" 'company-show-doc-buffer company-active-map)
  (bind-key [remap next-line] 'company-select-next company-search-map)
  (bind-key [remap previous-line] 'company-select-previous company-search-map)
  (bind-key [remap complete-symbol] 'counsel-company company-mode-map)
  (bind-key [remap completion-at-point] 'counsel-company company-mode-map))

(use-package help-mode
  :bind (:map help-mode-map
              ("j" . next-line)
              ("k" . previous-line)
              ("H" . help-go-back)
              ("h" . backward-char)
              ("L" . help-go-forward)
              ("l" . forward-char)
              ("v" . recenter-top-bottom)
              ("c" . counsel-ace-link)))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
