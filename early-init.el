(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "stateful/eln-cache/" user-emacs-directory))))

(setq package-enable-at-startup nil)

;; Visual stuff init
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tab-bar-mode -1)
(blink-cursor-mode -1)
(setq ring-bell-function #'ignore)
