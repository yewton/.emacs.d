;;; ytn-hydra-rectangle.el --- Hydra for rectangle -*- lexical-binding: t -*-

;; Copyright (C) 2018 Yuto SASAKI
;; Author: Yuto SASAKI <yewton@gmail.com>

;;; Commentary:

;; This file is intended to be autoloaded to lazy load along with `rect' package.

;;; Code:

(require 'rect)
(defun hydra-rectangle/body ()
  (interactive)
  
  (require 'hydra)
  (funcall (defhydra hydra-rectangle (:pre (rectangle-mark-mode 1)
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
             ("g" nil))))

(provide 'ytn-hydra-rectangle)
;;; ytn-hydra-rectangle.el ends here
