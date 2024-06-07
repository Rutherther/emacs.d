; use elpaca.el for package management
;;
;; load envs (exec-path-from-shell)
;;
;; lsp-mode? or eglot
;;      C, C++
;;      Rust
;;      VHDL, Verilog
;;      Nix
;;      Latex + templates
;; flycheck
;; pdf tools

(add-to-list 'load-path (locate-user-emacs-file "lisp/"))
(require 'functions)
(require 'elpaca)

;; some visual configs
(my-use-package nordic-night-theme
 :ensure t
 :demand t
 :config
   (load-theme 'nordic-night t)
)

(setq-default inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;; Default editing configs
(setq tab-width 2
	evil-shift-width 2)

;; ENV
(my-use-package exec-path-from-shell
  :ensure t
  :demand t
  :custom
  (exec-path-from-shell-shell-name (getenv "SHELL"))
  (exec-path-from-shell-arguments nil)
  (exec-path-from-shell-variables
   '("PATH"
     "MANPATH"
     "CXX"
     "CC"
     "XDG_CONFIG_HOME"
     "XDG_CACHE_HOME"
     "XDG_DATA_HOME"
     "NIX_PATH"))
  :config
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                            KEYS                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(my-use-package which-key
  :ensure t
  :demand t
  :custom
  (which-key-idle-delay 0.6)
  :config
  (which-key-mode 1))

(my-use-package general
  :ensure t
  :config
  (general-create-definer my-leader
    :states '(motion normal)
    :keymaps 'override
    :prefix "SPC")
  (general-create-definer my-local-leader
    :states 'normal
    :keymaps 'override
    :prefix "SPC m")
  (my-leader
    "" '(nil :wk "global leader")
    "h" '(:keymap help-map :wk "Help")
    "C-g" '(keyboard-quit :wk "abort"))
)

(elpaca-wait)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                            EVIL                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(my-use-package evil
  :ensure t
  :demand t
  :general
  (my-leader
    "u" '(universal-argument :wk "Universal argument"))
  :bind
  (:map evil-window-map
    ("d" . evil-window-delete)
    ("o" . ace-window))
  :custom
  (evil-undo-system 'undo-redo)
  (evil-want-integration t)
  (evil-want-keybinding nil)
  :config
	(my-unbind-key-in-evil-states "C-.")

  (evil-mode))

(my-use-package evil-collection
  :after evil
  :ensure t
  :demand t
  :config
  (evil-collection-init))

(my-use-package evil-easymotion
  :after evil
  :ensure t
  :demand t
  :config
  (evilem-default-keybindings "\\")
)

(my-use-package evil-surround
  :after evil
  :ensure t
  :demand t
  :config
  (global-evil-surround-mode 1))

(my-use-package evil-goggles
  :after evil
  :ensure t
  :demand t
  :custom
  (evil-goggles-duration 0.1)
  :config
  (evil-goggles-mode))

(my-use-package evil-commentary
  :after evil
  :ensure t
  :demand t
  :config
  (evil-commentary-mode))

(my-use-package evil-snipe
  :after evil
  :ensure t
  :demand t
  :config
  (evil-snipe-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                VERTICO, CONSULT, EMBARK                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(my-use-package vertico
  :ensure t
  :general
  (my-leader "'" '(vertico-repeat :wk "Last search"))
  :init
  (vertico-mode))

(my-use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
	  ("C-h" . vertico-directory-up)))

(my-use-package vertico-quick
  :ensure nil
  :after vertico
  :bind (:map vertico-map
	  ("M-q" . vertico-quick-insert)
	  ("C-q" . vertico-quick-exit)))

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode))

(use-package embark
  :ensure t
  :general
  (my-leader
    "." '(embark-act :wk "Act")
    ";" '(embark-dwim :wk "Dwim"))
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :init

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(my-use-package consult
  :ensure t
  :general
  (my-leader
    "f" '(nil :wk "File")
    "f f" '(find-file :wk "Find file")
    "f s" '(save-buffer :wk "Save file")
    "f l" '(consult-locate : "Locate file")

    "b" '(nil :wk "Buffer")
    "b b" '(consult-buffer :wk "Switch buffer")
    "," '(consult-buffer :wk "Switch buffer")

    "c" '(nil :wk "Mode specific")
    "c h" '(consult-history :wk "History")
    "c k" '(consult-kmacro :wk "Kmacro")
    "c m" '(consult-man :wk "Man")
    "c i" '(consult-info :wk "Info")

    "p" '(consult-yank-pop :wk "Yank pop")

    ;; "g" '(nil :wk "Goto")
    ;; "g f" '(consult-flymake :wk "Goto flymake")
    ;; "g l" '(consult-goto-line :wk "Goto line")
    ;; "g o" '(consult-outline :wk "Goto outline")
    ;; "g m" '(consult-mark :wk "Goto mark")
    ;; "g k" '(consult-global-mark :wk "Goto global mark")
    ;; "g i" '(consult-imenu :wk "Goto imenu")
    ;; "g I" '(consult-imenu-multi :wk "Goto imenu multi")

    "s" '(:keymap search-map :wk "Search")
  )
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  :custom
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "<")
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (recentf-mode 1))

(my-use-package orderless
  :ensure t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
)

;;; NAVIGATION
(my-use-package ace-window
  :ensure t
  :general
  (my-leader
    "`" '(evil-switch-to-windows-last-buffer :wk "Switch to last buffer")
    "w" '(:keymap evil-window-map :wk "Windows")
  )
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-char-position 'left)
  (aw-leading-char-style 'char)
  (aw-scope 'frame)
  :bind (("M-o" . ace-window)))

(my-use-package savehist
  :init
  (savehist-mode 1))

(my-use-package emacs
  :hook
  (minibuffer-setup-hook . cursor-intangible-mode)
  :custom
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt)))

;; Help
;; TODO: helpful

;; Projects
(my-use-package projectile
  :ensure t
  :demand t
  :general
  (my-leader "p" '(:keymap projectile-command-map :wk "projectile"))
  :bind (:map projectile-mode-map
	  ("C-c p" . projectile-command-map))
  :config
  (projectile-mode 1))

;; Modeline
(my-use-package vs-modeline
  :ensure (vs-modeline :type git
                         :host github
                         :repo "VojtechStep/vs-modeline.el")

  :demand t
  :config
  (vs-modeline-mode))

;; File browser
(my-use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t))

;; Git
(my-use-package transient
  :ensure t)
(my-use-package magit
  :ensure t
  :hook
  (with-editor-mode-hook . evil-insert-state)
  :general
  (my-leader
    "g" '(nil :wk "Magit")
    "g g" '(magit-status :wk "Magit")
    "g /" '(magit-dispatch :wk "Dispatch"))
  :custom
  (magit-save-repository-buffers 'dontask)
  (magit-diff-refine-hunk 'all)
  :config
  ;; I don't know why, but if this is in :custom block,
  ;; magit-dispatch ends up in an error...
  (setq evil-collection-magit-want-horizontal-movement t)

  (evil-set-initial-state #'git-commit-mode 'insert))

(my-use-package hl-todo
  :ensure (:pin t :tag "v3.6.0"))
(my-use-package magit-todos
  :ensure t
  :after magit
  :config (magit-todos-mode 1))

;; Vterm
(use-package vterm
  :ensure t
  :config
  (add-to-list 'vterm-eval-cmds '("update-pwd" (lambda (path) (setq default-directory path))))

  (push (list "find-file-below"
            (lambda (path)
              (if-let* ((buf (find-file-noselect path))
                        (window (display-buffer-below-selected buf nil)))
                  (select-window window)
                (message "Failed to open file: %s" path))))
      vterm-eval-cmds))

;; MMM mode
(my-use-package mmm-mode
  :ensure t
  :config
  (setq mmm-global-mode 'maybe))

;; Completion
(my-use-package company
  :ensure t
  :custom
  (company-idle-delay nil)
  :bind
  ("C-SPC" . company-complete)
  :config
  (global-company-mode 1))

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

;; Nix
(my-use-package nix-mode
  :ensure (nix-mode :fetcher github
	    :repo "NixOS/nix-mode"
	    :files (:defaults (:exclude "nix-company.el")))
  :after mmm-mode
  :mode "\\.nix\\'"
  :init
  (load (expand-file-name "nix-mode-mmm.el" (file-name-directory (locate-library "nix-mode")))))

;; Some additional stuff
(add-hook 'write-file-hooks 'delete-trailing-whitespace nil t)

;; Last step - async
(elpaca-process-queues)
