;; -*- lexical-binding: t;-*-

;; Better font detection that doesn't crash  
(defun font-available-p (font-name)
  "Check if font FONT-NAME is available, safely."
  (find-font (font-spec :name font-name)))

(defun abrahms/set-font-safely (preferred-fonts face &optional weight height)
  "Set FACE to the first available font in PREFERRED-FONTS list.
Optionally specify WEIGHT and HEIGHT."
  (let ((available-font (cl-find-if #'font-available-p preferred-fonts)))
    (if available-font
        (set-face-attribute face nil
                            :font available-font
                            :weight (or weight 'light)
                            :height (or height 120))
      (message "None of the preferred fonts are available: %s" preferred-fonts))))

;; Set up fonts with fallbacks - prioritize modern programming fonts with Nerd Font icons
(abrahms/set-font-safely '("JetBrains Mono Nerd Font"
                           "Fira Code Nerd Font Mono"
                           "Cascadia Code PL"
                           "ProFont IIx Nerd Font Mono"
                           "Menlo"
                           "Consolas") 'default)
(abrahms/set-font-safely '("JetBrains Mono Nerd Font"
                           "Fira Code Nerd Font Mono"
                           "Cascadia Code PL"
                           "ProFont IIx Nerd Font Mono"
                           "Menlo"
                           "Consolas") 'fixed-pitch)
(abrahms/set-font-safely '("Helvetica Neue"
                           "Equity Text A"
                           "Baskerville"
                           "Georgia"
                           "Times New Roman") 'variable-pitch)

;; Disable GUI elements for a cleaner interface
(when (display-graphic-p)
  (setq use-dialog-box nil)      ;; no popups
  (menu-bar-mode -1)             ;; no menu bar
  (tool-bar-mode -1)             ;; no toolbar
  (scroll-bar-mode -1)           ;; no scrollbars
  (tooltip-mode -1))             ;; no tooltips

;; Prettier frame appearance
(set-frame-parameter (selected-frame) 'internal-border-width 15)
(setq default-frame-alist
      (append (list '(width  . 90)
                    '(height . 45)
                    '(vertical-scroll-bars . nil)
                    '(internal-border-width . 15)
                    '(left-fringe . 1)
                    '(right-fringe . 1))))

;; Line and text display improvements
(setq-default line-spacing 0.1)          ;; Slightly more space between lines
(setq x-underline-at-descent-line t)     ;; Prettier underlines
(setq widget-image-enable nil)           ;; No ugly button for checkboxes
(setq truncate-lines t)                  ;; Don't wrap lines by default

;; Cursor settings
(set-default 'cursor-type '(bar . 2))    ;; Thin bar cursor
(blink-cursor-mode 0)                    ;; No blinking

;; Silent operation
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Show matching parentheses
(show-paren-mode t)
(setq show-paren-delay 0.0)
(setq show-paren-style 'parenthesis)     ;; Highlight just the matching paren

;; Fringe settings for minimal visual clutter
(when (display-graphic-p)
  (fringe-mode '(4 . 4)))                ;; Small but visible fringes

;; Visual line mode settings for text
(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook #'visual-line-mode)

;; Hide minor modes from modeline to reduce clutter
;; (Now handled by doom-modeline)
(setq mode-line-percent-position nil)

;; Disable the conventional modeline since we'll use doom-modeline
;; This custom function is kept for reference but no longer used
(defun mode-line-render (left right)
  (let* ((available-width (- (window-width) (length left) )))
    (format (format "%%s %%%ds" available-width) left right)))

;; Time display format (now handled by doom-modeline)
(setq display-time-format "%l:%M %p")
(setq display-time-default-load-average nil)

;; Window divider settings
(setq window-divider-default-right-width 2)
(setq window-divider-default-places 'right-only)
(window-divider-mode)

;; Highlight the current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#333344")
(set-face-attribute 'hl-line nil :inherit nil)

;; Modern line numbers display
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
;; Disable line numbers in certain modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                vterm-mode-hook
                eshell-mode-hook
                treemacs-mode-hook
                dashboard-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; All-the-icons is used by doom-themes and modern UI packages
(use-package all-the-icons
  :straight t
  :if (display-graphic-p)
  :init
  ;; Make sure icons load even when we defer
  (setq inhibit-compacting-font-caches t)
  :config
  ;; Only try to install fonts if we've never done it before
  (unless (file-exists-p "~/.config/emacs/.all-the-icons-installed")
    (all-the-icons-install-fonts t)
    (with-temp-file "~/.config/emacs/.all-the-icons-installed"
      (insert "Icons were installed")))

  ;; Ensure it loads properly
  (require 'all-the-icons))

;; Icons for dired mode
(use-package all-the-icons-dired
  :straight t
  :if (display-graphic-p)
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

;; Additional icons for completion and more
(use-package all-the-icons-completion
  :straight t
  :if (display-graphic-p)
  :after (all-the-icons marginalia)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; Modern, minimal and customizable modeline
(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-window-width-limit fill-column)
  (doom-modeline-project-detection 'project)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-persp-name t)
  (doom-modeline-display-default-persp-name nil)
  (doom-modeline-persp-icon t)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-env-version t)
  (doom-modeline-env-enable-python t)
  (doom-modeline-env-enable-ruby t)
  (doom-modeline-env-enable-go t)
  (doom-modeline-env-enable-perl t)
  (doom-modeline-env-enable-rust t)
  (doom-modeline-env-python-executable "python")
  (doom-modeline-env-ruby-executable "ruby")
  (doom-modeline-env-go-executable "go")
  (doom-modeline-env-perl-executable "perl")
  (doom-modeline-env-rust-executable "rustc"))

;; Display available keybindings in popup
(use-package which-key
  :straight t
  :diminish
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+")
  :config
  (which-key-mode))

;; Colorful delimiters based on nesting level
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Dashboard for a nicer startup experience
(use-package dashboard
  :straight t
  :config
  (setq dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-show-shortcuts t
        dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5))
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-init-info t)
  (dashboard-setup-startup-hook))


(use-package emojify
  :hook (org-mode . emojify-mode)
  :commands emojify-mode)

(set-face-attribute 'default nil :height 160)
