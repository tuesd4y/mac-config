;;  -*- lexical-binding: t; -*-
;; Additional performance improvements
(setq read-process-output-max (* 4 1024 1024)) ; 4MB (default is much lower)
(setq process-adaptive-read-buffering nil)     ; Improves process communication

;; Native compilation support if available (Emacs 28+)
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (message "Native compilation is available")
  ;; Don't store .eln files in ~/.emacs.d/eln-cache/
  (when (boundp 'native-comp-eln-load-path)
    (setcar native-comp-eln-load-path
            (expand-file-name "eln-cache/" user-emacs-directory)))

  ;; Silence compilation warnings
  (setq native-comp-async-report-warnings-errors nil)

  ;; Set compilation jobs to half the cores available
  (setq native-comp-deferred-compilation t)
  (setq native-comp-async-jobs-number 2) ;; Adjust based on your CPU

  ;; Package compilation
  (setq package-native-compile t))

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; straight pacakge manager config
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; straight config

;; Install use-package
(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

;; Install use-package-ensure-system-package first to ensure keyword extensions work
;;(straight-use-package 'use-package-ensure-system-package)

;; Enable the :straight keyword for use-package
;; This is needed for package-specific recipes to work
;; (use-package use-package-ensure-system-package)

;; Configure straight to use use-package
(setq straight-use-package-by-default t)     ;; Use straight.el for use-package expressions
(setq straight-check-for-modifications nil)  ;; Don't check for modifications (faster startup)
(setq straight-vc-git-default-clone-depth 1) ;; Shallow clone (faster, less bandwidth)

;; Configure use-package behavior
(setq use-package-always-defer t)          ;; Always defer loading packages
(setq use-package-always-demand nil)       ;; Don't load packages immediately
(setq use-package-expand-minimally t)      ;; Make the expanded code as minimal as possible
(setq use-package-compute-statistics t)    ;; Gather stats to optimize package loading
(setq use-package-enable-imenu-support t)  ;; Make packages easier to find with imenu
(setq use-package-hook-name-suffix nil)    ;; Use :hook without -hook suffix

;; Configuration for straight.el specifics
(setq straight-recipes-gnu-elpa-use-mirror t)
(setq straight-vc-git-auto-fast-forward nil)

;; Define some repository sources
(straight-use-package 'org) ;; Important to install org via straight.el first

;; catppucin theme
(straight-use-package 'catppuccin-theme)
(load-theme 'catppuccin :no-confirm)

;; ui config
(load "~/.config/emacs/ui-config.el")

;; ivy (command search) config
(load "~/.config/emacs/ivy-config.el")

;; org mode config
(load "~/.config/emacs/orgmode-config.el")

;; random general settings
(setq inhibit-splash-screen t) ;; no splash screen
;; zooming
(defun djcb-zoom (n)
  "with positive N, increase the font size, otherwise decrease it"
  (set-face-attribute 'default (selected-frame) :height
                      (+ (face-attribute 'default :height) (* (if (> n 0) 1 -1) 10))))


(global-set-key (kbd "C-+")      '(lambda nil (interactive) (djcb-zoom 1)))
(global-set-key (kbd "C--")      '(lambda nil (interactive) (djcb-zoom -1)))

;; source for most parts of the config
;; https://justin.abrah.ms/blog/configs/

					; desktop saved windows and restore

(use-package desktop
  :defer 5  ;; Load even later in startup
  :init
  ;; Pre-configure before loading the package
  (setq desktop-restore-frames nil)  ;; Don't restore frames - big speedup
  (setq desktop-restore-eager 0)     ;; Don't eagerly load ANY buffers
  (setq desktop-auto-save-timeout 300) ;; Save desktop every 5 minutes

  ;; Skip buffers that cause slowdowns
  (setq desktop-buffers-not-to-save
        (concat "\\("
                "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS\\|"
                "\\.el\\.gz\\|\\.tar\\.gz\\|\\.tgz\\|\\.zip\\|\\.pdf\\|"
                "\\.png\\|\\.jpe?g\\|\\.svg\\|\\.gif\\|"
                "\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb\\|"
                "magit.*\\|\\.git/.*\\|"
                "\\.org#\\|\\*\\(scratch\\|dashboard\\|Messages\\|Warnings\\)\\*"
                "\\)$"))

  ;; Exclude more modes from save
  (setq desktop-modes-not-to-save
        '(dired-mode
          special-mode
          shell-mode
          term-mode
          eshell-mode
          magit-mode
          magit-process-mode
          magit-status-mode
          help-mode
          Info-mode
          calc-mode
          compilation-mode
          Info-mode
          info-lookup-mode
          fundamental-mode
          org-mode
          org-agenda-mode
          org-roam-mode
          ibuffer-mode))

  :config
  ;; Handle locked desktop gracefully
  (setq desktop-load-locked-desktop nil)  ;; Don't ask, just don't load

  ;; Lazily restore buffers on demand with a hook
  (add-hook 'desktop-after-read-hook
            (lambda ()
              ;; Let user know what happened
              (message "Desktop state loaded, but buffers will only restore when accessed.")))

  ;; Use deferred loading with a much more efficient function
  (defun my/desktop-lazy-load ()
    "Enable minimal desktop save mode that only loads buffers when they're accessed."
    (desktop-save-mode 1)
    ;; Disable after next save
    (remove-hook 'after-init-hook #'my/desktop-lazy-load))

  ;; Add hook to enable desktop-save-mode after init
  (add-hook 'after-init-hook #'my/desktop-lazy-load))
