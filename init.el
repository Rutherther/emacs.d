;; use straight.el for package management
;;
;; load envs (exec-path-from-shell)
;;
;; Switch to elpaca
;; general.el
;;  SPC, like in DOOM
;; vertico config
;; lsp-mode? or eglot
;;      C, C++
;;      Rust
;;      VHDL, Verilog
;;      Nix
;; Company, Vertico

;; Straight setup

;; TODO: put to conditions, etc.

(eval-and-compile
  (defmacro my-use-package (&rest body)
    "Passes BODY to `use-package' and call it at comptime and runtime."
    (declare (indent defun))
    `(eval-and-compile
       ,(cons 'use-package body))))


(defconst straight-bootstrap
  (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))


(unless (file-exists-p straight-bootstrap)
    (with-current-buffer
	    (url-retrieve-synchronously
	    "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	    'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))

(load straight-bootstrap)
(require 'straight)

(straight-use-package 'use-package)

(require 'use-package)
(straight-use-package-mode t)

;; Basic keybindings, etc.
(setq evil-want-keybinding nil)

(my-use-package evil
  :straight t
  :demand t
  :custom
  (evil-undo-system 'undo-redo)
  :config
  (evil-mode))

(my-use-package evil-collection
  :after evil
  :straight t
  :demand t)

(my-use-package evil-easymotion
  :after evil
  :straight t
  :demand t
  :config
  (evilem-default-keybindings "\\")
)

(my-use-package evil-surround
  :after evil
  :straight t
  :demand t
  :config
  (global-evil-surround-mode))

(my-use-package evil-goggles
  :after evil
  :straight t
  :demand t
  :custom
  (evil-goggles-duration 0.1)
  :config
  (evil-goggles-mode))

(my-use-package evil-commentary
  :after evil
  :straight t
  :demand t
  :config
  (evil-commentary-mode))

(my-use-package evil-snipe
  :after evil
  :straight t
  :demand t
  :config
  (evil-snipe-mode))

(my-use-package which-key
  :straight t
  :demand t
  :custom
  (which-key-idle-delay 0.6)
  :config
  (which-key-mode))

(my-use-package nordic-night-theme
 :straight t
 :demand t
 :config
   (load-theme 'nordic-night t)
)

;; Vertico, consult, history
(my-use-package vertico
  :straight t
  :init
  (vertico-mode))

(my-use-package consult
  :straight t
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  :config
    (setq consult-narrow-key "<") ;; "C-+"
  )

(my-use-package savehist
  :straight t
  :init
  (savehist-mode))

(my-use-package emacs
  :init
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

;; Modeline
(my-use-package vs-modeline
  :straight (vs-modeline :type git
                         :host github
                         :repo "VojtechStep/vs-modeline.el")

  :demand t
  :config
  (vs-modeline-mode))

(my-use-package orderless
  :straight t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Git


;; TODO: some config.
;; Magit todos
(my-use-package magit
  :straight t
  :custom
  (magit-save-repository-buffers nil)
  (magit-diff-refine-hunk 'all)
  (evil-collection-magit-want-horizontal-movement t)
  :preface
  (declare-function evil-collection-magit-setup "modes/magit/evil-collection-magit")
  :init
  (with-eval-after-load 'magit-repos ; magit-repos does not load magit, so the evil-collection setup is not triggered
    (evil-collection-magit-setup)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lisp
(my-use-package elisp-mode
  :custom
  (lisp-indent-offset 2))

(defun my-use-package-indent (indent-point state)
  "Indent always by two"
  (goto-char (elt state 1))
  (+ 2 (current-column)))

(put 'my-use-package 'lisp-indent-function 'my-use-package-indent)


;; Some additional stuff
(add-hook 'write-file-hooks 'delete-trailing-whitespace nil t)

;; some visual configs
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil
      tab-bar-mode nil
      blink-cursor-mode nil
      ring-bell-function #'ignore)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(setq tab-width 2)
