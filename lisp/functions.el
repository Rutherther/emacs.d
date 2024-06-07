(provide 'functions)

(eval-and-compile
  (defmacro my-use-package (&rest body)
    "Passes BODY to `use-package' and call it at comptime and runtime."
    (declare (indent defun))
    `(eval-and-compile
       ,(cons 'use-package body))))

(setq my-evil-state-maps '(evil-normal-state-map
                           evil-insert-state-map
                           evil-visual-state-map
                           evil-motion-state-map
                           evil-operator-state-map
                           evil-replace-state-map))

;; Function to unbind a key in all Evil state maps
(defun my-unbind-key-in-evil-states (key)
  (dolist (map my-evil-state-maps)
    (define-key (symbol-value map) (kbd key) nil)))
