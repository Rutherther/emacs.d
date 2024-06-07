;; use elpaca.el for package management
;;
;; load envs (exec-path-from-shell)
;;
;; general.el
;;  SPC, like in DOOM
;; vertico config
;; lsp-mode? or eglot
;;      C, C++
;;      Rust
;;      VHDL, Verilog
;;      Nix
;; Company, Vertico

;; Elpaca setup

;; TODO: put to conditions, etc.

(eval-and-compile
  (defmacro my-use-package (&rest body)
    "Passes BODY to `use-package' and call it at comptime and runtime."
    (declare (indent defun))
    `(eval-and-compile
       ,(cons 'use-package body))))

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; Basic keybindings, etc.
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

(my-use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :custom
  (evil-undo-system 'undo-redo)
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
  (global-evil-surround-mode t))

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

(my-use-package which-key
  :ensure t
  :demand t
  :custom
  (which-key-idle-delay 0.6)
  :config
  (which-key-mode))

(my-use-package nordic-night-theme
 :ensure t
 :demand t
 :config
   (load-theme 'nordic-night t)
)

;; Vertico, consult, history
(my-use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

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
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (recentf-mode 1)
  (setq consult-narrow-key "<"))

(my-use-package ace-window
  :ensure t
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-char-position 'left)
  (aw-leading-char-style 'char)
  (aw-scope 'frame)
  :bind (("M-o" . ace-window)))

(my-use-package savehist
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

;; Help
;; TODO: helpful

;; Projects
(my-use-package projectile
  :ensure t
  :demand t
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

(my-use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
;; Git


;; TODO: some config.
;; Magit todos
(my-use-package transient
  :ensure t)
(my-use-package magit
  :ensure t
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
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tab-bar-mode -1)
(blink-cursor-mode -1)
(setq ring-bell-function #'ignore)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(setq tab-width 2
	evil-shift-width 2)

(elpaca-process-queues)
