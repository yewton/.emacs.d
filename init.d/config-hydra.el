;; -*- lexical-binding: t -*-
(require 'bind-key)
(require 'hydra)

(declare-function rectangle-previous-line "rect")
(declare-function rectangle-next-line "rect")
(declare-function rectangle-backward-char "rect")
(declare-function rectangle-forward-char "rect")
(declare-function rectangle-exchange-point-and-mark "rect")
(declare-function rectangle-number-lines "rect")

(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                     :color pink
                                     :hint nil
                                     :post (deactivate-mark))
  "
  ^_k_^       _w_ copy      _o_pen       _N_umber-lines            |\\     -,,,--,,_
_h_   _l_     _y_ank        _t_ype       _e_xchange-point          /,`.-'`'   ..  \-;;,_
  ^_j_^       _d_ kill      _c_lear      _r_eset-region-mark      |,4-  ) )_   .;.(  `'-'
^^^^          _u_ndo        _g_ quit     ^ ^                     '---''(./..)-'(_\_)
"
  ("k" rectangle-previous-line)
  ("j" rectangle-next-line)
  ("h" rectangle-backward-char)
  ("l" rectangle-forward-char)
  ("d" kill-rectangle)                    ;; C-x r k
  ("y" yank-rectangle)                    ;; C-x r y
  ("w" copy-rectangle-as-kill)            ;; C-x r M-w
  ("o" open-rectangle)                    ;; C-x r o
  ("t" string-rectangle)                  ;; C-x r t
  ("c" clear-rectangle)                   ;; C-x r c
  ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
  ("N" rectangle-number-lines)            ;; C-x r N
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)))
  ("u" undo nil)
  ("g" nil))
(bind-key [remap refctangle-mark-mode] #'hydra-rectangle/body)

(defhydra hydra-window-resize (global-map "C-x")
  "Enlarge/shrink window."
  ("{" shrink-window-horizontally)
  ("}" enlarge-window-horizontally)
  ("^" enlarge-window)
  ("&" shrink-window)
  ("+" balance-windows)
  ("-" shrink-window-if-larger-than-buffer)
  ("q" nil)
  ("g" nil))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
