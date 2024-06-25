;; Android early setup
(when (eq window-system 'android)
  (setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin" (getenv "PATH")))

  (push "/data/data/com.termux/files/usr/bin" exec-path)

  (customize-set-variable 'touch-screen-display-keyboard t)

  (with-eval-after-load 'evil
    (evil-define-key nil evil-normal-state-map
      [mouse-1] 'mouse-set-point
      [down-mouse-1] 'mouse-drag-region
      [drag-mouse-1] 'mouse-drag-region))

  (set-text-conversion-style nil)
  (add-hook
  'after-change-major-mode-hook
  (lambda () (set-text-conversion-style nil))))

(provide 'android-setup)
