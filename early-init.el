;;; early-init.el --- Early initialization file for Emacs
;;; Commentary: Early emacs Startup File --- initialization for Emacs

(eval-and-compile (setq load-prefer-newer t))

;; ------------------------------------------------------------------------------------------
;; Speed optimization taken from igloo.el, see https://github.com/VojtechStep/igloo.el
;; ------------------------------------------------------------------------------------------
;; Another important feature we throw out the window is Emacs builtin package management. It would otherwise initialize itself during startup, and that's just not groovy.

;; Many of the startup optimizations were inspired by [[https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly][how Doom does it]]. I recommend reading it, since not all the tricks are used here, only the ones that noticeably improved the loading times for me.
;; In order to reduce the startup time of Emacs, we can employ several techniques. The most important part is deferred package loading, to which we will get in [[*Package management][Package management]].
;; Other than that, we can start by looking into the garbage collector. The garbage collector runs when there is garbage to be picked up, that is when objects on the heap are being abandoned. We can influence when the garbage collection runs. If we wanted to have a lower memory footprint, we would want GC to run more frequently. However, in this case, we can live with a little memory spike if it provides us with a snappier experience.
;; Emacs garbage collector can run once the amount of allocated memory since the last GC run reaches a certain threshold, which is 8MB by default. This number is ridiculously small for most, so we increase it to about 100 megs.
;; Starting up is where a lot of garbage can be created, so it's easiest to pretty much disable garbage collection as soon as possible in the initialization, and re-enable it after Emacs starts. Another small-object sensitive workflows are minibuffer operations and company completion, so disable GC during those two too.
;; Furthermore, when enabling garbage collection, do so in a deferred manner, that is run it only after a second passes since the task (minibuffer action, completion) ends. This way, the thing running just after will still be free of garbage collection.

(defconst igloo--gc-threshold (* 100 1024 1024))
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      read-process-output-max (* 1024 1024))

(defun igloo--startup-cleanup ()
  "Reset settings disabled for faster startup."
  (setq gc-cons-threshold igloo--gc-threshold
        gc-cons-percentage 0.1))
(add-hook 'emacs-startup-hook #'igloo--startup-cleanup)


(defun igloo--gc-disable (&rest _)
  "Disable garbage collection."
  (setq gc-cons-threshold most-positive-fixnum))
(add-hook 'minibuffer-setup-hook #'igloo--gc-disable)
(add-hook 'company-completion-started-hook #'igloo--gc-disable)

(defun igloo--gc-enable ()
  "Enable garbage collection."
  (setq gc-cons-threshold igloo--gc-threshold))
(defun igloo--defer-gc-enable (&rest _)
  "Enable garbage collection, defered."
  (run-at-time 1 nil #'igloo--gc-enable))
(add-hook 'minibuffer-exit-hook #'igloo--defer-gc-enable)
(add-hook 'company-completion-finished-hook #'igloo--defer-gc-enable)

(advice-add #'x-apply-session-resources :override #'ignore)

(setq vc-handled-backends nil)

(setq package-enable-at-startup nil)

;; Emacs has support for bidirectional text, which I don't have a use case for, and disabling it can improve redisplay performance.

(setq bidi-inhibit-bpa t)
(setq-default bidi-paragraph-direction 'left-to-right)

;; Font rendering performance
(setq-default font-lock-support-mode 'jit-lock-mode)
(setq-default font-lock-multiline t)
(customize-set-variable
 'default-frame-alist
 '((font . "Hack")
   (background-color . "#282828")
   (foreground-color . "#ebdbb2")))

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "stateful/eln-cache/" user-emacs-directory))))

(setq package-enable-at-startup nil)

;; ------------------------------------------------------------------------------------------
;; End of speed optimizations
;; ------------------------------------------------------------------------------------------

;; Visual stuff init
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tab-bar-mode -1)
(blink-cursor-mode -1)
(setq ring-bell-function #'ignore)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage)

;;; init.el ends here
