;; -*- lexical-binding: t; -*-
(eval-when-compile (require 'use-package))

(declare-function projectile-project-p "projectile")
(declare-function treemacs-projectile "treemacs-projectile")
(declare-function treemacs "treemacs")

(defun ytn-treemacs ()
      "Project context-aware treemacs.

If called in a project it calls `treemacs-projectile', otherwise `treemacs'."
      (interactive)
      (let ((fun (if (projectile-project-p) #'treemacs-projectile #'treemacs)))
        (call-interactively fun)))

(use-package treemacs
  :bind (([f8] . ytn-treemacs)
         ("M-0" . treemacs-select-window)
         ("C-c 1" . treemacs-delete-other-windows))
  :config
  (setq treemacs-change-root-without-asking nil
        treemacs-collapse-dirs              (if (executable-find "python") 3 0)
        treemacs-file-event-delay           5000
        treemacs-follow-after-init          t
        treemacs-goto-tag-strategy          'refetch-index
        treemacs-indentation                2
        treemacs-indentation-string         " "
        treemacs-is-never-other-window      nil
        treemacs-never-persist              nil
        treemacs-no-png-images              nil
        treemacs-recenter-after-file-follow nil
        treemacs-recenter-after-tag-follow  nil
        treemacs-show-hidden-files          t
        treemacs-silent-filewatch           nil
        treemacs-silent-refresh             nil
        treemacs-sorting                    'alphabetic-desc
        treemacs-tag-follow-cleanup         t
        treemacs-tag-follow-delay           1.5
        treemacs-winum-number               10
        treemacs-width                      40
        treemacs-python-executable (executable-find "python3")))

(use-package treemacs-projectile
  :commands (treemacs-projectile)
  :after (treemacs)
  :config
  (setq treemacs-header-function 'treemacs-projectile-create-header))

(use-package treemacs-filewatch-mode
  :commands (treemacs-filewatch-mode)
  :after (treemacs)
  :config
  (treemacs-filewatch-mode t))

(use-package treemacs-follow-mode
  :after (treemacs)
  :commands (treemacs-follow-mode)
  :config
  (treemacs-follow-mode t))

(use-package treemacs-async
  :after (treemacs)
  :commands (treemacs-git-mode)
  :config
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'extended))
    (`(t . _)
     (treemacs-git-mode 'simple))))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
