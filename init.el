;; Lang configs
;;      C, C++
;;      VHDL, Verilog
;;      Latex + templates
;;      Matlab
;;      org
;; Multiple cursors (evil-mc?)
;; super-save?
;; https://github.com/svaante/dape

;; eglot?

;; dired compatability of a/b and tab
;; formatting etc for vhdl-ts - local leader

;; debug vhdl lsp, what is the issue
;;  how to speed it up?

;; vhdl-ts
;;  wrong indentation for new line, but it gets fixed afterwards? figure out

;; flycheck errors jumping

(require 'custom-setup nil t)

(add-to-list 'load-path (locate-user-emacs-file "lisp/"))
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

(my-use-package no-littering
  :ensure (:wait t)
  :demand t
  :init
  (setq no-littering-etc-directory (expand-file-name "stateful/config" user-emacs-directory))
  (setq no-littering-var-directory (expand-file-name "stateful/data" user-emacs-directory)))
(elpaca-wait) ; No littering

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
  :ensure (:wait t) ; Adds general use-package keyword
  :config
  (general-auto-unbind-keys)
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
(my-use-package embark-consult
  :ensure t
  :bind (:map search-map
          ("s" . consult-ripgrep-all))
  :hook
  ((embark-collect-mode . consult-preview-at-point-mode))
  :init
  (require 'consult-ripgrep-all))

(my-use-package consult
  :ensure t
  :general
  (my-leader
    "f" '(nil :wk "File")
    "f f" '(find-file :wk "Find file")
    "f r" '(consult-recent-file :wk "Recent file")
    "f s" '(save-buffer :wk "Save file")
    "f l" '(consult-locate : "Locate file")

    "b" '(nil :wk "Buffer")
    "b b" '(consult-buffer :wk "Switch buffer")
    "b B" '(consult-project-buffer :wk "Switch project buffer")
    "," '(consult-buffer :wk "Switch buffer")

    "d h" '(consult-history :wk "Consult history")
    "d k" '(consult-kmacro :wk "Consult kmacro")
    "d m" '(consult-man :wk "Consult uan")
    "d i" '(consult-info :wk "Consult info")

    "y" '(consult-yank-pop :wk "Yank pop")

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
  (orderless-matching-styles '(orderless-literal orderless-regexp))
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
)

;;; NAVIGATION
; TODO: consider removing this. I don't use it
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

(my-use-package golden-ratio
  :ensure t
  :custom
  ;; Work with evil
  (golden-ratio-extra-commands '(
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
  (golden-ratio-inhibit-functions '((lambda () (and which-key--buffer
                                                  (window-live-p (get-buffer-window which-key--buffer))))))
  :config
  (golden-ratio-mode 1)
  )

(my-use-package savehist
  :init
  (save-place-mode 1)
  (savehist-mode 1))

(my-use-package emacs
  :hook
  (minibuffer-setup . cursor-intangible-mode)
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
(my-use-package projectile
  :ensure t
  :commands projectile-project-root
  :custom
  (projectile-switch-project-action 'projectile-dired)
  (projectile-completion-system 'default)
  (projectile-current-project-on-switch 'keep)
  (evil-shift-width tab-width)
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
    "g" '(nil :wk "Magit")
    "g g" '(magit-status :wk "Magit")
    "g /" '(magit-dispatch :wk "Dispatch")
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
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode 1))

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
    "o t" '(vterm-toggle :wk "Toggle terminal")
    "o T" '(vterm :wk "Open terminal")))

;; MMM mode
;; (my-use-package mmm-mode
;;   :ensure t
;;   :config
;;   (setq mmm-global-mode 'maybe))

(my-use-package corfu
  :ensure t
  :commands completion-at-point
  :bind
  ("M-m" . corfu-move-to-minibuffer)
  ("C-SPC" . completion-at-point)
  (:map corfu-map
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
  (global-corfu-mode 1)
  :config
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
(my-use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :after projectile
  )

(my-use-package envrc
  :ensure t
  :demand t
  :general
  (my-leader
    "e" '(:keymap envrc-command-map :wk "Direnv"))
  :config
  (envrc-global-mode 1))

(my-use-package fancy-compilation
  :ensure t
  :defer 3
  :custom
  (fancy-compilation-scroll-output 'first-error)
  :config
  (fancy-compilation-mode))

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
  :commands pdf-view-mode
  )

;; Treesit langs
(my-use-package emacs
  :ensure nil
  :custom
  (treesit-font-lock-level 4))

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
  :hook
  (((rust-mode rust-ts-mode) . eglot-ensure))
  :custom
  (rust-mode-treesitter-derive t))

(my-use-package rustic
  :ensure t
  :mode
  ("\\.rs\\'" . rustic-mode)
  :hook
  ((rustic-mode . eglot-ensure)))

;; VHDL
(my-use-package vhdl-mode
  :ensure nil
  :demand t
  :after (eglot flycheck)
  ;; :mode
  ;; Use vhdl-ts-mode instead
  ;; ("\\.vhdl?\\'" . vhdl-mode)
  :hook
  ((vhdl-mode . eglot-ensure)
   (vhdl-mode . vhdl-electric-mode)
   (vhdl-mode . vhdl-stutter-mode))
  :custom
  (vhdl-clock-edge-condition 'function)
  (vhdl-clock-name "clk_i")
  (vhdl-reset-kind 'sync)
  (vhdl-reset-name "rst_in")
  (vhdl-basic-offset 2)
  :config
  (add-to-list 'eglot-server-programs
                '(vhdl-mode . ("vhdl_ls")))

  ;; TODO: just append
  (setq-default flycheck-disabled-checkers '(vhdl-ghdl))
)

(my-use-package vhdl-ts-mode
  :ensure t
  :after vhdl-mode
  :general
  (my-local-leader vhdl-ts-mode-map
    "f" '(nil :wk "Formatting")
    "f f" '(vhdl-ts-beautify-block-at-point :wk "Beautify block at point")
    "f b" '(vhdl-ts-beautify-buffer :wk "Beautify buffer"))
  :custom
  (vhdl-ts-indent-level tab-width)
  :mode
  ("\\.vhdl?\\'" . vhdl-ts-mode))

(my-use-package hydra
  :ensure t)

(my-use-package vhdl-ext
  :ensure t
  :after vhdl-mode)

;; Verilog

(my-use-package verilog-mode
  :ensure nil
  :mode
  ("\\.v\\'" . verilog-mode)
  ("\\.sv\\'" . verilog-mode)
  :custom
  (verilog-indent-lists nil)
  (verilog-indent-level 2)
  (verilog-indent-level-behavioral 2)
  (verilog-indent-level-declaration 2)
  (verilog-indent-level-module 2)
  (verilog-case-indent 2)
  (verilog-cexp-indent 2)
  (verilog-align-ifelse t)
  (verilog-auto-delete-trailing-whitespace t)
  (verilog-auto-newline nil)
  (verilog-auto-save-policy nil)
  (verilog-auto-template-warn-unused t)
  (verilog-tab-to-comment t)
  (verilog-highlight-modules t)
  (verilog-highlight-grouping-keywords t)
)

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
