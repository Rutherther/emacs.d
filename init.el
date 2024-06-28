;;; init.el --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs

(add-to-list 'load-path (locate-user-emacs-file "lisp/"))
(require 'android-setup nil t)
(require 'custom-setup nil t)
(require 'functions)
(require 'elpaca-loader)

;; Startup time
(defun efs/display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; Loaded early to prevent all littering
(my-use-package no-littering
  :ensure (:wait t)
  :demand t
  :init
  (setq no-littering-etc-directory (expand-file-name "stateful/config" user-emacs-directory))
  (setq no-littering-var-directory (expand-file-name "stateful/data" user-emacs-directory)))
(elpaca-wait) ; No littering

;; Loaded early to prevent loading unnecessary stuff
(my-use-package general
  :ensure (:wait t) ; Adds general use-package keyword
  :config
  (general-auto-unbind-keys)
  (general-create-definer my-leader
    :states '(motion normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC")
  (general-create-definer my-local-leader
    :states 'normal
    :keymaps 'override
    :prefix "\\")
  (my-leader
    "" '(nil :wk "global leader")
    "h" '(:keymap help-map :wk "Help")
    "C-g" '(keyboard-quit :wk "abort")
    "!" '(shell-command :wk "Shell command")
    "&" '(async-shell-command :wk "Async shell command")
    ;; "'" '()
    "x" '(execute-extended-command :wk "Execute extended command")
    ":" '(eval-expression :wk "Evaluate expression")))

;; Loaded early cause I genuinely am not able to use emacs much without evil
;; so if stuff brokes, it's good to have evil at hand
;; (general-def 'insert 'override
;;   "C-\\" 'evil-normal-state)
(my-use-package evil
  :ensure t
  :demand t
  :general
  (my-leader
    "u" '(universal-argument :wk "Universal argument"))
  (general-def 'insert 'override
   "C-\\" '(evil-normal-state :wk "Enter normal state"))
  :custom
  (evil-undo-system 'undo-redo)
  (evil-want-integration t)
  (evil-want-keybinding nil)
  :init
  ; evil-want-Y-yank-to-eol cannot be set by custom. Use this instead
  (setq evil-want-Y-yank-to-eol t)
  :config
	(my-unbind-key-in-evil-states "C-.")
  (evil-mode 1))

(my-use-package evil-collection
  :after evil
  :ensure t
  :demand t
  :config
  (evil-collection-init))
(elpaca-wait)

;; some visual configs
(my-use-package gruvbox-theme
 :ensure t
 :demand t
 :config
   (load-theme 'gruvbox-dark-hard t)
)

(my-use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode 1))

(setq-default inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

(setq-default scroll-step 1
              scroll-margin 3
              scroll-conservatively 101
              hscroll-step 1
              hscroll-margin 3)

(setq-default resize-mini-windows t)

;; Editing
(my-use-package whitespace
  :hook
  ((before-save . whitespace-cleanup)
   ((prog-mode text-mode) . whitespace-mode))
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

;; Default editing configs
(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq backup-directory-alist `(("." . ,(expand-file-name "saves/" no-littering-var-directory))))

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default sentence-end-double-space nil)
(setq-default truncate-lines t)

(defcustom my/indent-variable-mode-alist '()
  "Maps modes to their respective indent variable"
  :type '(alist :key-type (symbol :tag "Mode")
                :value-type (symbol :tag "Variable")))

(defmacro my/indent-variable-mode-alist-add (mode variable)
  `(add-to-list 'my/indent-variable-mode-alist '(,mode . ,variable)))

(defun my/hook-indent-adjust-shift-widths ()
  (when-let ((indent (symbol-value (cdr (assoc major-mode my/indent-variable-mode-alist)))))
    (setq-local tab-width indent))
  (setq-local evil-shift-width tab-width))

;; This doesn't work. Why?
;; it's not a good approach, but I don't understand it!
;; (defmacro my/adjust-shift-widths (variable)
;;   `(let
;;       ((shift-width ,variable))
;;     (setq-local evil-shift-width shift-width)
;;     (setq-local tab-width shift-width)))

;; (defmacro my/add-hook-adjust-shift-widths (mode variable)
;;   `(add-hook
;;     (intern (concat (symbol-name ',mode) "-hook"))
;;     (lambda (&rest args) (my/adjust-shift-widths ,variable))))

;; ENV
(my-use-package exec-path-from-shell
  :ensure t
  :demand t
  :custom
  (exec-path-from-shell-shell-name (getenv "SHELL"))
  ; (exec-path-from-shell-arguments "-l -i") ; Set by default for bash, zsh etc.
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
  (unless (memq system-type '(windows-nt android))
    (exec-path-from-shell-initialize)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                            KEYS                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Early keys stuff (general.el) is loaded earlier

(my-use-package which-key
  :ensure t
  :demand t
  :custom
  (which-key-idle-delay 0.6)
  :config
  (which-key-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                            EVIL                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Early evil stuff is loaded earlier

(my-use-package evil-easymotion
  :after evil
  :ensure t
  :demand t
  :general
  (my-leader
    "l" '(:keymap evilem-map :wk "Evilem"))
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

(my-use-package evil-numbers
  :ensure t
  :general
  (normal
   "g+" '(evil-numbers/inc-at-pt :wk "Increment at point")
   "g-" '(evil-numbers/dec-at-pt :wk "Decrement at point"))
  (visual
   "g+" '(evil-numbers/inc-at-pt-incremental :wk "Increment at point")
   "g-" '(evil-numbers/dec-at-pt-incremental :wk "Decrement at point"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                VERTICO, CONSULT, EMBARK                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(my-use-package vertico
  :ensure t
  :after savehist
  :hook
  (minibuffer-setup . vertico-repeat-save)
  :bind (:map vertico-map
	  ("M-P" . vertico-repeat-previous)
	  ("M-N" . vertico-repeat-next)
	  ("S-<prior>" . vertico-repeat-previous)
	  ("S-<next>" . vertico-repeat-next)

    ;; Exit without substituting selected
    ("M-RET" . vertico-exit-input)

	  ("?" . minibuffer-completion-help)
	  ("M-TAB" . minibuffer-complete))
  :general
  (my-leader "'" '(vertico-repeat :wk "Last search"))
  :init
  (vertico-mode 1)
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

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
  :config
  ; TODO: use general.el :general block
  (define-key embark-file-map     (kbd "o") (my/embark-ace-action find-file))
  (define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))
  (define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(my-use-package embark-consult
  :ensure t
  :hook
  ((embark-collect-mode . consult-preview-at-point-mode)))

(my-use-package consult
  :ensure t
  :general
  (my-leader
    "f" '(nil :wk "File")
    "f f" '(find-file :wk "Find file")
    "f F" '(consult-fd :wk "Consult fd")
    "f r" '(consult-recent-file :wk "Recent file")
    "f s" '(save-buffer :wk "Save file")
    "f l" '(consult-locate : "Locate file")

    "b" '(nil :wk "Buffer")
    "b r" '(revert-buffer :wk "Revert buffer")
    "b b" '(consult-buffer :wk "Switch buffer")
    "b k" '(kill-buffer :wk "Kill buffer")
    "b B" '(consult-project-buffer :wk "Switch project buffer")
    "," '(consult-buffer :wk "Switch buffer")

    "d h" '(consult-history :wk "Consult history")
    "d k" '(consult-kmacro :wk "Consult kmacro")
    "d m" '(consult-man :wk "Consult uan")
    "d i" '(consult-info :wk "Consult info")

    "j c" '(consult-flymake :wk "Consult flymake")

    "y" '(consult-yank-pop :wk "Yank pop")

    "s" '(:keymap search-map :wk "Search")
    "/" '(consult-ripgrep-all :wk "Search project")
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
  (:map search-map
              ("s" . consult-ripgrep-all)
              ("i" . consult-imenu)
              ("I" . consult-imenu-multi)
              ("f" . consult-flymake))
  :custom
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "<")
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (require 'consult-ripgrep-all)
  (recentf-mode 1))

(my-use-package orderless
  :ensure t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (orderless-matching-styles '(orderless-literal orderless-regexp))
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
)

(my-use-package wgrep
  :ensure t)

;;; NAVIGATION, Window managements
(my-use-package emacs
  :general
  (my-leader
    "t m" '(switch-to-minibuffer :wk "Switch to minibuffer"))
  :config
  (add-to-list 'display-buffer-alist
            '((or (major-mode . Info-mode)
                  (major-mode . help-mode)
                  (major-mode . helpful-mode))
              (display-buffer-reuse-window
                display-buffer-in-side-window)
              (reusable-frames . visible)
              (side . right)
              (window-width . 0.33)))

  (add-to-list 'display-buffer-alist
            '("\\*vterm\\*" nil
              (dedicated . t)))

  (defun switch-to-minibuffer ()
    "Switch to minibuffer window."
    (interactive)
    (if (active-minibuffer-window)
        (select-window (active-minibuffer-window))
      (error "Minibuffer is not active"))))

(my-use-package ace-window
  :ensure t
  :commands (aw-select ace-window ace-window-one-command)
  :general
  (my-leader
    "o" '(ace-window :wk "Ace window")
    "O" '(ace-window-one-command :wk "Ace window one command")
    "`" '(evil-switch-to-windows-last-buffer :wk "Switch to last buffer")
    "<TAB>" '(evil-switch-to-windows-last-buffer :wk "Switch to last buffer")
    "w" '(:keymap evil-window-map :wk "Windows"))
  :bind
  (("M-o" . ace-window)
   ("M-O" . ace-window-one-command))
  (:map evil-window-map
    ("d" . evil-window-delete)
    ("o" . ace-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-char-position 'top-left)
  (aw-leading-char-style 'char)
  (aw-scope 'global)
  (aw-dispatch-always t)
  :init
  ;; Thanks https://karthinks.com/software/fifteen-ways-to-use-embark/#open-any-buffer-by-splitting-any-window
  (eval-when-compile
    (defmacro my/embark-ace-action (fn)
      `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
        (interactive)
        (with-demoted-errors "%s"
        (require 'ace-window)
        (let ((aw-dispatch-always t))
          (aw-switch-to-window (aw-select nil))
          (call-interactively (symbol-function ',fn)))))))
  (defun other-window-mru ()
    "Select the most recently used window on this frame."
    (interactive)
    (when-let ((mru-window
                (get-mru-window
                nil nil 'not-this-one-dummy)))
      (select-window mru-window)))
  :config
  (add-to-list 'aw-dispatch-alist '(?i other-window-mru))

  ;; Thanks https://karthinks.com/software/emacs-window-management-almanac/
  (defun ace-window-one-command ()
    (interactive)
    (let ((win (aw-select " ACE")))
      (when (windowp win)
        (with-selected-window win
          (let* ((command (key-binding
                          (read-key-sequence
                            (format "Run in %s..." (buffer-name)))))
                (this-command command))
            (call-interactively command)))))))

(my-use-package golden-ratio
  :ensure t
  :demand t
  :custom
  (golden-ratio-exclude-buffer-regexp '("dape"))
  (golden-ratio-exclude-modes '("ediff-mode"))
  ;; Work with evil
  (golden-ratio-extra-commands '(
                                 ;; ace-window
                                 ace-window
                                 ace-swap-window
                                 ace-delete-window
                                 ace-select-window
                                 ;; magit
                                 ;; TODO: but there is something...
                                 ;; evil
                                 evil-window-left
                                 evil-window-right
                                 evil-window-up
                                 evil-window-down
                                 buf-move-left
                                 buf-move-right
                                 buf-move-up
                                 buf-move-down
                                 window-number-select
                                 select-window
                                 select-window-1
                                 select-window-2
                                 select-window-3
                                 select-window-4
                                 select-window-5
                                 select-window-6
                                 select-window-7
                                 select-window-8
                                 select-window-9))
  ;; Work with which-key
  (golden-ratio-inhibit-functions '(
                                    (lambda () (and which-key--buffer
                                                    (window-live-p (get-buffer-window which-key--buffer))))
                                    pl/ediff-comparison-buffer-p
                                    ))
  :hook
  (ediff-startup . my-ediff-startup-hook)
  :init
  (defun my-ediff-startup-hook ()
    "Workaround to balance the ediff windows when golden-ratio is enabled."
    ;; There's probably a better way to do it.
    (ediff-toggle-split)
    (ediff-toggle-split))

  (defun pl/ediff-comparison-buffer-p ()
  (and (boundp 'ediff-this-buffer-ediff-sessions)
       ediff-this-buffer-ediff-sessions))
  :config
  (golden-ratio-mode 1)
  )

(my-use-package savehist
  :ensure nil
  :init
  (save-place-mode 1)
  (savehist-mode 1))

(my-use-package emacs
  :hook
  (minibuffer-setup . cursor-intangible-mode)
  (after-change-major-mode . my/hook-indent-adjust-shift-widths)
  :general
  (my-leader
    "n" '(:keymap narrow-map :wk "Narrowing")
    "f R" '(revert-buffer :wk "Revert"))
  :bind
  (("C-x C-b" . ibuffer))
  :custom
  (use-short-answers t)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (use-dialog-box nil)
  :init
  (put 'narrow-to-region 'disabled nil)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt)))

(my-use-package autorevert
  :ensure nil
  :custom
  (global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode t))

;; Help
(my-use-package helpful
  :ensure t
  :general
  (my-leader
    "d d" '(helpful-at-point :wk "Helpful at point"))
  :bind (
    ([remap describe-function] . helpful-function)
    ([remap describe-variable] . helpful-variable)
    ([remap describe-key] . helpful-key)
    ([remap describe-command] . helpful-command)
    ([remap describe-symbol] . helpful-symbol)))

;; Projects
(my-use-package xref
  :ensure t)

(my-use-package project
  :ensure t
  :custom
  (vc-handled-backends '(Git))
  (project-switch-commands
   '(
     (consult-project-buffer "Find buffer" "b")
     (project-find-file "Find file" "f")
     (consult-ripgrep-all "Search" "s")
     (project-dired "Dired" "d")
     ;; (vterm-toggle "Vterm" "v") ;; TODO: make sure it opens inside of the project!
     (my/magit-current-window "Magit" "m"))) :general
  (my-leader
    "p" '(:keymap project-prefix-map :wk "Project"))
  :init
  (defun my/magit-current-window ()
    "Open Magit status in current window"
    (interactive)
    (same-window-prefix)
    (magit-status (project-root (project-current t))))
  )

;; Modeline
(my-use-package vs-modeline
  :ensure (vs-modeline :type git
                         :host github
                         :repo "VojtechStep/vs-modeline.el")

  :demand t
  :custom
  ((vs-modeline-left
   '("%e"
     (:eval (window-parameter (selected-window) 'ace-window-path))
     " "
     (:eval (vs-modeline-evil))
     mode-line-process
     (:eval (vs-modeline-project-el-name))
     (:eval (vs-modeline-buffer-name))
     (:eval (when (buffer-modified-p) "+"))
     (:eval (when buffer-read-only " RO"))))
    (vs-modeline-right
      '(
        (:eval (when which-function-mode which-func-format))
        (:eval (when which-function-mode " "))
        (:eval (vs-modeline-input-method))
        (:eval (when flymake-mode " "))
        (:eval (when flymake-mode flymake-mode-line-exception))
        (:eval (when flymake-mode flymake-mode-line-counters))
        (:eval (when flymake-mode " "))
        mode-name
        " "
        (:eval (vs-modeline-position-rel))
        (:eval (vs-modeline-position)))))
  :config

  ;; Why does project.el project-current slow down by a lot?
  (defvar project-project-name nil)
  (defun obtain-project-name ()
    (let ((project-info (project-current)))
      (setq-local project-project-name
                  (or (unless project-info "")
                      (file-name-nondirectory (directory-file-name (nth 2 project-info)))))))

  (defun obtain-project-name-once ()
    (if project-project-name
        project-project-name
      (obtain-project-name)))

  (vs-modeline-def-prop-segment project-el-name
    (when-let ((project-name (obtain-project-name-once)))
      (concat " " project-name))
    'vs-modeline-project)

  (vs-modeline-mode))

;; File browser
(my-use-package dired
  :ensure nil
  :general
  (my-leader
    "o d" '(dired-jump :wk "Dired"))
  :custom
  (dired-dwim-target t))

(my-use-package diredfl
  :ensure t
  :config
  (diredfl-global-mode))

(my-use-package fd-dired
  :ensure t
  :commands (fd-dired fd-grep-dired)
  :general
  (my-leader
    "s d" '(fd-dired :wk "Search dired")
    "s D" '(fd-grep-dired :wk "Search contents dired")))

(my-use-package dired-filter
  :ensure t)

(my-use-package dired-subtree
  :ensure t)

(my-use-package dired-ranger
  :ensure t
  :general
  (my-local-leader dired-mode-map
    :override t
    "y" '(dired-ranger-copy :wk "Ranger copy")
    "c" '(dired-ranger-copy :wk "Ranger copy")
    "m" '(dired-ranger-move :wk "Ranger move")
    "p" '(dired-ranger-paste :wk "Ranger paste")
    "b" '(dired-ranger-bookmark :wk "Ranger bookmark")
    "RET" '(dired-ranger-bookmark-visit :wk "Ranger bookmark visit"))
)

(my-use-package dired-narrow
  :ensure t)

;; There is a bug for dired-collapse + dired-subtree.
;; When expanding the last folder in a tree, it cannot
;; be reliably expanded / collapsed...
(my-use-package dired-collapse
  :ensure t
  :hook
  (dired-mode . dired-collapse-mode))

;; TODO: Use this?
;; (my-use-package dirvish
;;   :ensure t)

;; Git
(my-use-package transient
  :ensure t
  :custom
  (transient-levels-file (locate-user-emacs-file "transient-levels.el")))
(my-use-package magit
  :ensure t
  :hook
  (with-editor-mode . evil-insert-state)
  :general
  (my-leader
    "g" '(nil :wk "Git")
    "g g" '(magit-status :wk "Git status")
    "g c" '(magit-clone :wk "Git clone")
    "g i" '(magit-init :wk "Git init")
    "g ." '(magit-dispatch :wk "Dispatch")
    "g b" '(magit-blame :wk "Blame"))
  :custom
  (magit-save-repository-buffers 'dontask)
  (magit-diff-refine-hunk 'all)
  :config
  ;; I don't know why, but if this is in :custom block,
  ;; magit-dispatch ends up in an error...
  (setq evil-collection-magit-want-horizontal-movement t)

  (evil-set-initial-state #'git-commit-mode 'insert))

(my-use-package hl-todo
  :ensure (:pin t :tag "v3.6.0")
  :hook
  ((prog-mode text-mode) . hl-todo-mode)
  :general
  (my-local-leader hl-todo-mode-map
    "t j" '(hl-todo-next :wk "next TODO" :jump t)
    "t k" '(hl-todo-previous :wk "previous TODO" :jump t)))
(my-use-package magit-todos
  :ensure t
  :after magit
  :config (magit-todos-mode 1))

(my-use-package git-gutter
  :ensure t
  :general
  (my-leader
    "g p" '(git-gutter:previous-hunk :wk "Previous hunk")
    "g n" '(git-gutter:next-hunk :wk "Next hunk")
    "g C-g" '(git-gutter :wk "Git gutter")
    "g v =" '(git-gutter:popup-hunk :wk "Popup hunk")
    "g v s" '(git-gutter:stage-hunk :wk "Stage hunk")
    "g v r" '(git-gutter:revert-hunk :wk "Revert hunk")
    "g v SPC" '(git-gutter:mark-hunk :wk "Mark hunk"))
  :config
  (global-git-gutter-mode 1))

(my-use-package git-timemachine
  :ensure t
  :general
  (my-leader
    "g t" '(git-timemachine-toggle :wk "Git timemachine"))
  :config
  (evil-define-key 'normal git-timemachine-mode-map
    "?" 'git-timemachine-help
    "gtc" 'git-timemachine-show-commit)

  ;; Since the mapping for git-timemachine-help is not
  ;; updated by updating the map, update it manually.
  (transient-define-prefix git-timemachine-help ()
    "Show online help."
    ["Navigate"
      [("C-k" "show previous revision" git-timemachine-show-previous-revision)
        ("C-j" "show next revision" git-timemachine-show-next-revision)
        ("gtg" "show nth revision" git-timemachine-show-nth-revision)
        ("gtt" "show fuzzy revision" git-timemachine-show-revision-fuzzy)]]
    ["Kill current revision"
      [("gty" "kill abbreviated revision" git-timemachine-kill-abbreviated-revision)
        ("gtY" "kill revision" git-timemachine-kill-revision)]]
    ["Misc"
      [("gtb" "blame current revision" git-timemachine-blame)
        ("gtc" "show commit" git-timemachine-show-commit)
        ("?" "show help" git-timemachine-help)
        ("q" "quit" git-timemachine-quit)]])
  )

;; Editing
(my-use-package flymake
  :ensure t
  :general
  (my-leader
    "e n" '(flymake-goto-next-error :wk "Next error")
    "e p" '(flymake-goto-prev-error :wk "Prev error")
    "e j" '(flymake-goto-next-error :wk "Next error")
    "e k" '(flymake-goto-prev-error :wk "Prev error")

    "e R" '(flymake-running-backends :wk "Running backends")
    "e d" '(flymake-disabled-backends :wk "Disabled backends")
    "e r" '(flymake-reporting-backends :wk "Reported backends")

    "e l" '(flymake-show-buffer-diagnostics :wk "Show buffer diagnostics")
    "e L" '(flymake-show-project-diagnostics :wk "Show project diagnostics")
    ))
;; TODO: what about isearch-lazy-count variable?
;; Added in emacs 27, could be an alternative.
(my-use-package evil-anzu
  :ensure t
  :demand t
  :after evil
  :config
  (global-anzu-mode +1))

;; Vterm
(my-use-package vterm
  :ensure t
  :commands vterm
  :config
  (add-to-list 'vterm-eval-cmds '("update-pwd" (lambda (path) (setq default-directory path))))

  (push (list "find-file-below"
            (lambda (path)
              (if-let* ((buf (find-file-noselect path))
                        (window (display-buffer-below-selected buf nil)))
                  (select-window window)
                (message "Failed to open file: %s" path))))
      vterm-eval-cmds))

(my-use-package vterm-toggle
  :ensure t
  :commands vterm-toggle
  :custom
  (vterm-toggle-scope 'project)
  (vterm-toggle-project-root t)
  :general
  (my-leader
    "t t" '(vterm-toggle :wk "Toggle terminal")
    "t T" '(vterm :wk "Open terminal")))

;; MMM mode
;; (my-use-package mmm-mode
;;   :ensure t
;;   :config
;;   (setq mmm-global-mode 'maybe))

(my-use-package corfu
  :ensure t
  :demand t
  :bind
  ("C-SPC" . completion-at-point)
  (:map corfu-map
        ("M-m" . corfu-move-to-minibuffer)
        ("M-q" . corfu-quick-complete)
        ("C-q" . corfu-quick-complete)
        ("M-SPC" . corfu-insert-separator))
  :custom
  (corfu-cycle t)
  (corfu-quit-no-match 'separator)
  (completion-cycle-threshold 3)
  :init
  (defun corfu-move-to-minibuffer ()
      (interactive)
      (pcase completion-in-region--data
        (`(,beg ,end ,table ,pred ,extras)
        (let ((completion-extra-properties extras)
              completion-cycle-threshold completion-cycling)
          (consult-completion-in-region beg end table pred)))))
  :config
  (global-corfu-mode 1)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer))

(use-package cape
  :ensure t
  :demand t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  :custom
  (cape-dabbrev-min-length 2)
  (cape-dabbrev-check-other-buffers #'cape--buffers-major-mode)
  :config
  (defun kb/cape-capf-setup-git-commit ()
    (let ((result))
      (dolist (element '(cape-symbol cape-dabbrev) result)
        (add-to-list 'completion-at-point-functions element))))
  :general(:prefix "M-p"
            "p" 'completion-at-point
            "t" 'complete-tag   ; etags
            "d" 'cape-dabbrev   ; basically `dabbrev-completion'
            "h" 'cape-history
            "f" 'cape-file
            "k" 'cape-keyword
            "s" 'cape-symbol
            "a" 'cape-abbrev
            "i" 'cape-ispell
            "l" 'cape-line
            ":" 'cape-emoji
            "w" 'cape-dict
            "\\" 'cape-tex
            "_" 'cape-tex
            "^" 'cape-tex
            "&" 'cape-sgml
            "r" 'cape-rfc1345))

;; Programming
(my-use-package jsonrpc
  :ensure t)
(my-use-package eldoc
  :ensure t)

(my-use-package eglot
  :ensure t
  :commands (eglot eglot-ensure)
  :custom
  (eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (eglot-stay-out-of '(imenu)) ; I prefer the ts imenu for now
  :general
  (normal eglot--managed-mode
   :definer 'minor-mode
   "gR" '(eglot-rename :wk "Rename identifier")
   "g." '(eglot-code-actions :wk "Code actions")))

(my-use-package envrc
  :ensure t
  :demand t
  :config
  (envrc-global-mode 1))

(my-use-package fancy-compilation
  :ensure t
  :defer 3
  :custom
  (fancy-compilation-scroll-output 'first-error)
  :config
  (fancy-compilation-mode))

(my-use-package dape
  :ensure t
  :custom
  (dape-buffer-window-arrangement 'right))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PDF tools
(my-use-package pdf-tools
  :ensure nil ;; TODO: how to manage this?
              ;; I have this package installed via
              ;; my distribution. To get the executables
              ;; properly
  :hook
  (doc-view-mode . (lambda () (display-line-numbers-mode -1)))
  (pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :commands pdf-view-mode)

;; Treesit langs
(my-use-package emacs
  :ensure nil
  :custom
  (treesit-font-lock-level 4)
  :config
  (which-function-mode 1))

;; Nix
(my-use-package nix-mode
  :ensure t
  ;; :ensure (nix-mode :fetcher github
	;;     :repo "NixOS/nix-mode"
	;;     :files (:defaults (:exclude "nix-company.el")))
  ;; :after mmm-mode
  :mode "\\.nix\\'"
  :init
  ;; (load (expand-file-name "nix-mode-mmm.el" (file-name-directory (locate-library "nix-mode"))))
  )

;; Rust
(my-use-package rust-mode
  :ensure t
  :custom
  (rust-mode-treesitter-derive t)
  :config
  (my/indent-variable-mode-alist-add rust-ts-mode rust-ts-mode-indent-offset))

(my-use-package rustic
  :ensure t
  :mode
  ("\\.rs\\'" . rustic-mode))

;; CSharp

(my-use-package csharp-mode
  :ensure nil
  :after eglot
  :mode (("\\.cs\\'" . csharp-ts-mode))
  :config
  (my/indent-variable-mode-alist-add csharp-mode c-basic-offset)
  (my/indent-variable-mode-alist-add csharp-ts-mode csharp-ts-mode-indent-offset))

;; Sh, Bash

(my-use-package sh-script
  :ensure nil
  :custom
  (sh-basic-offset 2))

;; (my-use-package bash-ts-mode
;;   :ensure nil
;;   :mode "\\.sh\\'"
;;   :custom
;;   (bash-ts-mode-indent-offset 2))

;; VHDL
(my-use-package vhdl-mode
  :ensure nil
  :demand t
  :after eglot
  ;; :mode
  ;; Use vhdl-ts-mode instead
  ;; ("\\.vhdl?\\'" . vhdl-mode)
  :general
  (my-local-leader vhdl-mode-map
    "a" '(nil :wk "Alignment")
    "a a" '(vhdl-align-group :wk "Align group")
    "a c" '(vhdl-align-inline-comment-group :wk "Align comment group"))
  :hook
   ((vhdl-mode . vhdl-electric-mode)
    (vhdl-mode . vhdl-stutter-mode)
    (vhdl-mode . my/disable-eglot-completion))
  :custom
  (vhdl-clock-edge-condition 'function)
  (vhdl-clock-name "clk_i")
  (vhdl-reset-kind 'sync)
  (vhdl-reset-name "rst_in")
  (vhdl-basic-offset 2)
  (vhdl-end-comment-column 300)
  :init
  ;; VHDL lsp servers don't have good completion capabilities for now.
  ;; Remove this when they are ready.
  (defun my/disable-eglot-completion ()
    (make-local-variable 'eglot-ignored-server-capabilites)
    (add-to-list 'eglot-ignored-server-capabilities :completionProvider))
  :config
  (add-to-list 'eglot-server-programs
                '(vhdl-mode . ("vhdl_ls")))
  (my/indent-variable-mode-alist-add vhdl-mode vhdl-basic-offset)
)

(my-use-package vhdl-ts-mode
  :ensure (:host github :repo "Rutherther/vhdl-ts-mode")
  :after vhdl-mode
  :general
  (my-local-leader vhdl-ts-mode-map
    "f" '(nil :wk "Formatting")
    "f f" '(my/vhdl-ts/beautify-block-at-point :wk "Beautify block at point")
    "f b" '(my/vhdl-ts/beautify-buffer :wk "Beautify buffer"))
  :custom
  (vhdl-ts-indent-level tab-width)
  :mode
  ("\\.vhdl?\\'" . vhdl-ts-mode)
  :init
  (defun my/vhdl-ts/beautify-block-at-point ()
    (interactive)
    (vhdl-align-group)
    (vhdl-ts-beautify-block-at-point))

  (defun my/vhdl-ts/beautify-buffer ()
    (interactive)
    (vhdl-align-buffer)
    (vhdl-ts-beautify-block-at-point))
  :config
  (my/indent-variable-mode-alist-add vhdl-ts-mode vhdl-ts-indent-level)

  (defun my/vhdl-ts-special-node-identifier-name (node)
    "Return identifier name of NODE."
    (let (temp-node)
      (when node
        (cond ((string-match vhdl-ts-instance-re (treesit-node-type node))
              (cond ((setq temp-node (treesit-search-subtree node "\\_<component_instantiation\\_>"))
                      (treesit-node-text (treesit-node-child-by-field-name temp-node "component") :no-prop))
                    ((setq temp-node (treesit-search-subtree node "entity_instantiation"))
                      (treesit-search-subtree node "entity_instantiation")
                      (treesit-node-text (treesit-node-child temp-node 1) :no-props))
                    (t (error "Unexpected component_instantiation_statement subnode!"))))
              (t nil)))))

  (defun my/vhdl-ts-node-identifier-name (orig node)
    (when-let ((original-identifier-name (orig node)))
      (let* ((special-identifier-name (my/vhdl-ts-special-node-identifier-name node))
            (concat-identifier-name
              (when special-identifier-name
                (concat ": " special-identifier-name))))
        (concat original-identifier-name concat-identifier-name))))

  ;; (advice-add 'vhdl-ts--node-identifier-name :around #'my/vhdl-ts-node-identifier-name)
  )

;; (my-use-package hydra
;;   :ensure t)

;; Verilog

;; (my-use-package verilog-mode
;;   :ensure nil
;;   :mode
;;   ("\\.v\\'" . verilog-mode)
;;   ("\\.sv\\'" . verilog-mode)
;;   :custom
;;   (verilog-indent-lists nil)
;;   (verilog-indent-level 2)
;;   (verilog-indent-level-behavioral 2)
;;   (verilog-indent-level-declaration 2)
;;   (verilog-indent-level-module 2)
;;   (verilog-case-indent 2)
;;   (verilog-cexp-indent 2)
;;   (verilog-align-ifelse t)
;;   (verilog-auto-delete-trailing-whitespace t)
;;   (verilog-auto-newline nil)
;;   (verilog-auto-save-policy nil)
;;   (verilog-auto-template-warn-unused t)
;;   (verilog-tab-to-comment t)
;;   (verilog-highlight-modules t)
;;   (verilog-highlight-grouping-keywords t)
;; )

;; (my-use-package verilog-ext
;;   :ensure t
;;   :hook
;;   (verilog-mode . verilog-ext-mode)
;;   :custom
;;   (verilog-ext-feature-list
;;         '(font-lock
;;           xref
;;           capf
;;           hierarchy
;;           lsp
;;           navigation
;;           template
;;           formatter
;;           compilation
;;           imenu
;;           which-func
;;           hideshow
;;           typedefs
;;           time-stamp
;;           block-end-comments
;;           ports))
;;   :config
;;   (verilog-ext-mode-setup))

(setq warning-minimum-level :error)

;;; init.el ends here
