;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Iosevka Nerd Font" :size 12 :weight 'regular)
     doom-variable-pitch-font (font-spec :family "Iosevka Nerd Font Mono" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; dark theme
;; (setq doom-theme 'doom-oceanic-next)
;; light theme
(setq doom-theme 'doom-earl-grey)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the load-path when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;; This is so buffers auto-save
(setq auto-save-visited-interval 15)
(auto-save-visited-mode +1)


(map! :after org
      :map org-mode-map
;; use command k to create a new link
      "s-k" 'org-insert-link
      "C-c C-l" 'org-insert-link
      "s-[" 'org-mark-ring-goto
      "s-K" 'org-roam-node-insert
      "s-r" 'org-roam-db-sync
      "s-i" 'org-clock-in
      "s-o" 'org-clock-out
      "s-j" 'org-clock-goto
      )

;; general shortcuts
(map!
  "M-TAB" 'other-window
  "M-o" 'other-window
 )
;; Shortcuts for storing links, viewing the agenda, and starting a capture
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-ca" 'org-agenda)


;; custom TODO states
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(i@/!)" "VERIFYING(v!)" "BLOCKED(b@)" "IDEA(d)" "|" "DONE(d!)" "WONT-DO(w@/!)")
        )
      org-todo-keyword-faces
      '(
        ("TODO" . (:foreground :weight bold, (doom-color 'blue)))
        ("IN-PROGRESS" . (:foreground "Cyan" :weight bold))
        ("VERIFYING" . (:foreground "DarkOrange" :weight bold))
        ("BLOCKED" . (:foreground "Red" :weight bold))
        ("IDEA" . (:foreground "DeepPink" :weight bold))
        ("DONE" . (:foreground :weight bold, (doom-color 'green)))
        ("WONT-DO" . (:foreground :weight bold, (doom-color 'red)))
        )
      org-id-link-to-org-use-id t
      org-startup-with-inline-images t
      org-fold-core-style 'overlays
      ;; default width for all images
      ;; see https://stackoverflow.com/questions/11670654/how-to-resize-images-in-org-mode
      org-image-max-width 300

      ;; Hide the markers so you just see bold text as BOLD-TEXT and not *BOLD-TEXT*
      org-hide-emphasis-markers t
      )

(use-package! org-roam
  :defer t
  :custom
  (org-roam-directory (file-truename "~/org"))
  :config
  (org-roam-db-autosync-enable)
)

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t
        org-roam-mode-sections
        (list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              #'org-roam-unlinked-references-section)))


(unless (equal "Battery status not available"
               (battery))
  (display-battery-mode 1))                           ; On laptops it's nice to know how much power you have
(display-time-mode 1)

;;(if (eq initial-window-system ;; 'x)                 ; if started by emacs command or desktop file
  ;;   (toggle-frame-maximized)
  ;; (toggle-frame-fullscreen))

;; Allows you to jump in and out of latex fragments without using `C-c C-x C-l` all the time, beautiful.
;; (use-package! org-fragtog
;;   :after org
;;   :hook (org-mode . org-fragtog-mode)
;;   )

;; drag and drop images to org-mode files
(use-package!  org-download)
;; (after! org-download
;; (setq org-download-method 'directory)
;; (setq org-download-image-dir (concat (file-name-sans-extension (buffer-file-name)) "-img"))
;; (setq org-download-image-org-width 600)
;; (setq org-download-link-format "[[file:%s]]\n"        org-download-abbreviate-filename-function #'file-relative-name)
;; (setq org-download-link-format-function #'org-download-link-format-function-default))

(add-hook 'dired-mode-hook 'org-download-enable)


;; mermaid charts in babel, from https://github.com/arnm/ob-mermaid
(org-babel-do-load-languages
    'org-babel-load-languages
    '((mermaid . t))
    )

;;
;; clean up hyperbole keymap
;; https://emacs.stackexchange.com/questions/75055/hyperbole-mode-overrides-org-mode-c-c-binding
(defun my/hyperbole-disable-key-bindings-after-init-hook ()
  (define-key hyperbole-mode-map (kbd "C-c /") nil)
  (define-key hyperbole-mode-map (kbd "M-o") nil)
  )
;;
;;
;; custom links for JIRA issues
;; as seen on https://www.beorgapp.com/blog/hyperbole-implicit-buttons/;;
(use-package! hyperbole
  :ensure t
  :init
  (add-hook 'after-init-hook #'hyperbole-mode)
  :config
  (add-hook 'after-init-hook #'my/hyperbole-disable-key-bindings-after-init-hook t)

  ;; --- Config ---
  (defvar my/jira-project-prefixes '("TPS")
    "List of JIRA project keys to match, e.g. \'(\"TPS\" \"DEV\").")

  (defvar my/jira-cs-browse-url "https://triply.atlassian.net/browse/"
    "Base URL for JIRA tickets.")

  ;; Build regexes once
  (defun my/jira--regex ()
    "Plain (no capture) regex for any configured JIRA key."
    (concat "\\<" (regexp-opt my/jira-project-prefixes t) "-[0-9]+\\>"))

  (defun my/jira--regex-grouped ()
    "Regex with a single capturing group around the whole ticket."
    (concat "\\(" (my/jira--regex) "\\)"))

  ;; --- Action: open ticket ---
  (defun my/jira-cs-reference (jira-id)
    (browse-url (concat my/jira-cs-browse-url jira-id)))

  ;; --- Hyperbole implicit button ---
  (defib my/jira-cs ()
    "Make things like TPS-1234 clickable via Hyperbole."
    (let* ((case-fold-search t)
           (re (my/jira--regex-grouped)))
      (when (or (looking-at re)
                (save-excursion
                  (skip-chars-backward "[:alnum:]-")
                  (looking-at re)))
        (let ((id (match-string-no-properties 1)))
          (ibut:label-set id (match-beginning 1) (match-end 1))
          (hact 'my/jira-cs-reference id)))))

  ;; --- Highlighting ---
  (defface my/jira-issue-face
    '((t :background "light yellow" :underline t :weight bold))
    "Face for JIRA issue references.")

  (defun my/jira-add-highlighting ()
    (font-lock-add-keywords
     nil `((,(my/jira--regex) 0 'my/jira-issue-face prepend)) 'append))

  (dolist (hook '(org-mode-hook text-mode-hook prog-mode-hook))
    (add-hook hook #'my/jira-add-highlighting))
  )


;; custom colors for different links
(defconst my/org-link-colors
  '(("http"  . blue)
    ("https" . blue)
    ("file"  . orange)
    ("pdf"   . orange)
    ("id"    . violet))
  "Alist of Org link prefixes and their Doom theme colours.")

(defun my/org-apply-link-colors ()
  "Register custom faces for Org link schemes based on `my/org-link-colors'."
  (dolist (entry my/org-link-colors)
    (let ((scheme (car entry))
          (color  (cdr entry)))
      (org-link-set-parameters
       scheme
       :face       `(:foreground ,(doom-color color))
       :mouse-face `(:foreground ,(doom-color 'bg)
                     :background ,(doom-color color))))))

(after! org (my/org-apply-link-colors))
