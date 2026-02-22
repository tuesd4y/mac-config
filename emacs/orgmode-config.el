;;   -*- lexical-binding: t; -*-
(setq org-directory "~/org/")

;; agenda config
;; Core performance optimization for large org-agenda installations
(defun org-agenda-files-cached-setup ()
  "Set up org-agenda with high-performance caching for many files."
  (interactive)

  ;; Major GC optimization during agenda generation
  (defun my/org-agenda-gc-optimize (&rest _)
    "Temporarily increase GC threshold for agenda operations."
    (setq gc-cons-threshold (* 50 1024 1024)))  ; 50MB

  (defun my/org-agenda-gc-restore (&rest _)
    "Restore GC threshold after agenda operations."
    (run-with-idle-timer 1 nil (lambda () (setq gc-cons-threshold (* 16 1024 1024)))))  ; 16MB

  ;; Apply GC optimizations to agenda commands
  (advice-add 'org-agenda :before #'my/org-agenda-gc-optimize)
  (advice-add 'org-agenda :after #'my/org-agenda-gc-restore)

  ;; Create the agenda file cache
  (defvar org-agenda-files-cache nil "Cache for org agenda files.")
  (defvar org-agenda-files-cache-time 0 "Last time the agenda files cache was updated.")

  ;; Use a filtered cache to limit files processed
  (defun org-agenda-files-use-cache ()
    "Use cached and filtered agenda files list for better performance."
    (if (and org-agenda-files-cache
             (< (- (float-time) org-agenda-files-cache-time) 300))  ; 5 minutes cache
        org-agenda-files-cache
      (progn
        (setq org-agenda-files-cache-time (float-time))
        (let* ((all-files (append
                           ;; Regular org files
                           (cl-remove-if
                            (lambda (x)
                              (or (string-match (regexp-quote ".#") x)
                                  (string-match "\\.#" x)
                                  (string-match "~$" x)))
                            (directory-files-recursively org-directory "\\.org$"))
                           ;; Project files from org-roam if available
                           (when (fboundp 'vulpea-project-files)
                             (vulpea-project-files))))
               ;; Keep only recently modified files (improves performance dramatically)
               (sorted-files (seq-sort-by
                              (lambda (f)
                                (or (file-attribute-modification-time (file-attributes f)) '(0 0 0 0)))
                              #'time-less-p all-files))
               ;; Take the 200 most recently modified files
               (recent-files (if (> (length sorted-files) 200)
                                 (seq-take (reverse sorted-files) 200)
                               sorted-files)))
          (setq org-agenda-files-cache recent-files)
          org-agenda-files-cache))))

  ;; Use our cached version
  (setq org-agenda-files #'org-agenda-files-use-cache)

  ;; Function to force refresh
  (defun org-agenda-files-force-refresh ()
    "Force refresh the org agenda files cache."
    (interactive)
    (setq org-agenda-files-cache nil)
    (setq org-agenda-files-cache-time 0)
    (message "Org agenda files cache cleared. Will rebuild on next agenda view."))

  ;; Additional performance settings for agenda
  (setq
   ;; Skip org agenda files that are locked or being edited elsewhere
   org-agenda-skip-unavailable-files t

   ;; Prevent auto-save file name transform during agenda generation
   org-agenda-inhibit-startup t

   ;; Fontification slows down agenda generation dramatically
   org-agenda-fontify-priorities t
   org-agenda-fontify-todo-keywords nil

   ;; Limit scope
   org-agenda-span 'day

   ;; Skip archiving processing (big performance win)
   org-agenda-skip-archived-trees t

   ;; Skip comment subtrees (performance)
   org-agenda-skip-comment-trees t

   ;; Cache properties during agenda generation
   org-agenda-cache-properties-for-time t

   ;; Only process current level, not deep subtrees
   org-agenda-todo-list-sublevels nil

   ;; Don't process items with a specific tag (add more as needed)
   org-agenda-tag-filter-preset '("-ARCHIVE")

   ;; Don't inherit tags when building agenda (massive performance boost)
   org-agenda-use-tag-inheritance nil

   ;; Only short deadlines
   org-deadline-warning-days 7

   ;; Use sticky agenda for caching
   org-agenda-sticky t

   ;; Speed up first agenda creation by processing fewer entries at once
   org-agenda-batch-size 20)

  ;; Lazy loading of org files
  (defvar org-agenda--files-cache nil)
  (advice-add 'org-agenda-prepare-buffers :around
              (lambda (orig-fn files)
                "Load org files incrementally for better performance."
                (let ((files-to-process files))
                  ;; Only load files we haven't loaded yet
                  (setq files-to-process
                        (seq-filter (lambda (f)
                                      (not (get-file-buffer f)))
                                    files-to-process))
                  ;; Process in smaller batches
                  (if (> (length files-to-process) 10)
                      (let ((batches (seq-partition files-to-process 10)))
                        (dolist (batch batches)
                          (funcall orig-fn batch)
                          (redisplay t)))
                    (funcall orig-fn files-to-process))))))

;; Initialize our optimization setup
(org-agenda-files-cached-setup)


;; org mode itself

(defun abrahms/org-mode-setup ()
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (diminish org-indent-mode))

(use-package org
  :bind (("\C-ca" . org-agenda)
         :map org-mode-map
         ("C-c l" . org-store-link)
         ;;("C-c s" . ja/windows-screenshot)
         )

  :hook ((org-mode . abrahms/org-mode-setup))

  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-image-actual-width '(550)
        org-src-fontify-natively t
        org-enforce-todo-dependencies t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-babel-default-header-args '((:session . "none")
                                        (:results . "replace")
                                        (:exports . "code")
                                        (:cache . "no")
                                        (:noweb . "no")
                                        (:hlines . "no")
                                        (:tangle . "no")
                                        (:comments . "link") ; add link to original source
                                        )
        org-confirm-babel-evaluate nil
org-todo-keywords
              '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "BLOCKED(b)" "DONE(d)"))
        org-src-preserve-indentation t
        org-startup-folded 'content
        org-cycle-separator-lines 2
        org-modules nil
        org-agenda-files (directory-files-recursively org-directory "org$")
        org-id-link-to-org-use-id t
        org-hide-emphasis-markers t
         org-refile-targets '((nil :maxlevel . 9)
                                        (org-agenda-files :maxlevel . 9))
         org-outline-path-complete-in-steps nil         ; Refile in a single go
         org-refile-use-outline-path t                  ; Show full paths for refiling
         org-log-done 'note

        )

  (set-face-attribute 'org-document-title nil :font "Helvetica Neue" :weight 'bold :height 1.5)
  (dolist (face '((org-level-1 . 1.4)
                  (org-level-2 . 1.35)
                  (org-level-3 . 1.35)
                  (org-level-4 . 1.3)
                  (org-level-5 . 1.2)
                  (org-level-6 . 1.2)
                  (org-level-7 . 1.2)
                (org-level-8 . 1.2)))
    (set-face-attribute (car face) nil :font "Helvetica Neue" :weight 'medium :height (cdr face)))



;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
;; Fix for face warnings - use 'unspecified instead of nil for color attributes
(defun safe-face-attr-set (face attr value)
  "Safely set FACE's ATTR to VALUE, using 'unspecified instead of nil for colors."
  (set-face-attribute face nil attr (if (and (memq attr '(:foreground :background)) (null value))
                                        'unspecified
                                      value)))

;; Set fixed-pitch faces for code elements
(set-face-attribute 'org-block nil :inherit 'fixed-pitch)
(safe-face-attr-set 'org-block :foreground nil)
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; Fix background settings for column views
(safe-face-attr-set 'org-column :background nil)
(safe-face-attr-set 'org-column-title :background nil)
;; maybe https://github.com/alphapapa/magit-todos ?
) ; close opening from org block.


(setq org-agenda-custom-commands
      '(("d" "Daily agenda and all TODOs"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (alltodo ""
                   ((org-agenda-skip-function '(or (abrahms-org-skip-subtree-if-priority ?A)
                                                   (org-agenda-skip-entry-if 'todo '("WAITING"))
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:")))
          (agenda "")
          (alltodo ""
                   ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("TODO")))
                    (org-agenda-overriding-header "Things I'm waiting on:"))))
         ((org-agenda-compact-blocks nil)))))

(defun abrahms-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))


;; custom org settings

(setq org-default-notes-file (concat org-directory "/captured.org"))
(global-set-key (kbd "C-c c") 'org-capture)


(setq org-capture-templates
      ;; Explaination of values here:
      ;; https://orgmode.org/manual/Template-elements.html#Template-elements
      `(("t" "Todo" entry (file ,(concat org-directory "captured.org")) "**** TODO %?\n%a")
        ("m" "Meeting" entry (file ,(concat org-directory "captured.org")) "**** %?\n%t" )
        ("i" "Item" entry (file ,(concat org-directory "captured.org")) "**** %?\n%a" )
        ("a" "Action item" entry (file ,(concat org-directory "captured.org")) "* WAITING %?\n:PROPERTIES:\n:WAITING_ON: %^{Who owns this?}\n:END:\n %i %U %a")
        ("p" "Perf Note" entry (file ,(concat org-directory "captured.org")) "* %? :perf:\n\n %i %U" )
	("c" "org-capture selected" entry (file ,(concat org-directory "captured.org"))
         "* [[%:link][%:description]]\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
	("C" "org-capture unselected" entry (file ,(concat org-directory "captured.org"))
         "* %? [[%:link][%:description]] \nCaptured On: %U")
        ))


(defvar my/org-last-refile-marker nil)
(defvar my/org-last-refile-link nil)

(advice-add 'org-refile
            :before
            (lambda (&rest _)
              (save-excursion
                (org-back-to-heading)
                (setq my/org-last-refile-marker (point-marker))))
            '((name . "my/org-set-refile-marker")))

(defun my/org-set-last-refile-link ()
  (setq my/org-last-refile-link (org-store-link nil)))

(add-hook 'org-after-refile-insert-hook #'my/org-set-last-refile-link)

(advice-add 'org-refile
            :after
            (lambda (&rest _)
              (when (and my/org-last-refile-marker
                         my/org-last-refile-link)
                (let ((buf (marker-buffer my/org-last-refile-marker)))
                  (when (buffer-live-p buf)
                    (with-current-buffer buf
                      (save-excursion
                        (goto-char my/org-last-refile-marker)
                        (insert (concat my/org-last-refile-link "\n"))))))
                (setq my/org-last-refile-marker nil)
                (setq my/org-last-refile-link nil)))
            '((name . "my/org-insert-refile-marker")))


(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode))


(use-package olivetti
  :disabled
  :hook (org-mode . olivetti-mode))


(use-package org-variable-pitch
  :init (org-variable-pitch-setup)
  :hook (org-mode . org-variable-pitch-minor-mode))

(use-package org-modern)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t)))


;; via http://mbork.pl/2022-01-17_Making_code_snippets_in_Org-mode_easier_to_type
(defun org-insert-tilde ()
  "Insert a tilde using `org-self-insert-command'."
  (interactive)
  (if (string= (buffer-substring-no-properties (- (point) 3) (point))
	       "\n~~")
      (progn (delete-char -2)
	     (insert (format "#+begin_src %s\n#+end_src"
                             (read-string "Which language? " nil nil "text")))
	     (forward-line -1)
             (org-edit-special))
    (setq last-command-event ?~)
    (call-interactively #'org-self-insert-command)))


(defun org-insert-backtick ()
  "Insert a backtick using `org-self-insert-command'."
  (interactive)
  (setq last-command-event ?`)
  (call-interactively #'org-self-insert-command))

(define-key org-mode-map (kbd "`") #'org-insert-tilde)
(define-key org-mode-map (kbd "~") #'org-insert-backtick)

(use-package org-roam
  :straight t
  :demand t
  :after org
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
	 ("C-c n s" . org-roam-db-sync)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
	 ("C-c n t" . org-roam-dailies-goto-today))
  :init
  (setq org-roam-v2-ack t
        org-roam-directory "~/org")

  :config
  (org-roam-db-autosync-mode)

  (setq org-roam-capture-templates
	'(
	  ("d" "default" plain "%?"
           :if-new (file+head "roam/%<%Y%m%d>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
           :unnarrowed t)
	  ("p" "project" plain (file "~/org/templates/project.org")
           :if-new (file+head "roam/%<%Y%m%d>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
           :unnarrowed t)
	  ("s" "source / paper" plain (file "~/org/templates/paper.org")
           :if-new (file+head "roam/%<%Y%m%d>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
           :unnarrowed t)
	  ("c" "Contact" entry "* %{title}\n:PROPERTIES:\n:EMAIL: %^{Email}\n:END:\n- %?"
           ;; target the heading with specific ID
           :target (file+olp-id "triply-general.org" "89B06A99-9FAE-4518-91ED-1FE6F477CB2C")
	   :unnarrowed t
           :empty-lines 1)
   ))

)


;; From org-roam-ui: https://github.com/org-roam/org-roam-ui#manually b/c they don't have a melpa release
(use-package websocket)
(use-package simple-httpd)

(use-package org-roam-ui
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t
        org-roam-mode-sections
        (list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              #'org-roam-unlinked-references-section)))

(use-package markdown-mode
  :commands markdown-mode
  :init
  (add-hook 'markdown-mode-hook #'visual-line-mode)
  (add-hook 'markdown-mode-hook #'variable-pitch-mode)
  (add-hook 'markdown-mode-hook #'flyspell-mode)
  :config
  (setq flymd-markdown-regex (mapconcat 'identity '("\\.md\\'" "\\.markdown\\'" "markdown") "\\|"))

  ;; The default command for markdown (~markdown~), doesn't support tables
  ;; (e.g. GitHub flavored markdown). Pandoc does, so let's use that.
  (setq markdown-command "pandoc --from markdown --to html")
  (setq markdown-command-needs-filename t))

(use-package flymd
  :hook markdown-mode
  :commands flymd-flyit
  :requires markdown-mode)

;; configure how links are opened -> in same frame
;; https://emacs.stackexchange.com/questions/69706/how-to-open-a-file-from-an-org-link-within-current-window
       (defun mda/org-open-current-window ()                                              
         "Opens file in current window."                                                  
         (interactive)                                                                    
         (let ((org-link-frame-setup (cons (cons 'file 'find-file) org-link-frame-setup)))
           (org-open-at-point)))                                                          
       (define-key global-map (kbd "C-o") #'mda/org-open-current-window) (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

;; custom configuration for org roam + zotero bibtex
					; see more details here: https://daryl.wakatara.com/zotero-and-org-roam-academic-research-workflow/

(use-package org-ref
  :straight t
  :config
  (setq bibtex-completion-bibliography '("~/org/refs/zotero.bib")
        bibtex-completion-notes-path "~/org/refs/notes"
        bibtex-completion-pdf-field "file"
        bibtex-completion-pdf-opn-function
        (lambda (fpath)
          (call-process "open" nil 0 nil fpath))))

(use-package ivy-bibtex
  :straight t
  :after org-ref)

(use-package org-roam-bibtex
  :straight t
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (require 'org-ref)
  
  )

;; jira org config
;; (use-package org-jira
;;   :ensure t
;;   :after org
;;   :config
;;   (setq jiralib-url "https://triply.atlassian.net")
;;   (setq org-jira-working-dir "~/org/jira")
;;   (setq auth-sources '("~/.authinfo"))
;;   )


;; automatically configure org export location
(defun my/org-export-output-file-name (orig-fun extension &optional subtreep pub-dir)
  (let ((pub-dir (expand-file-name "~/docs/org")))
    (unless (file-directory-p pub-dir)
      (make-directory pub-dir t))
    (funcall orig-fun extension subtreep pub-dir)))

(advice-add 'org-export-output-file-name :around #'my/org-export-output-file-name)

(setq org-export-with-broken-links 'mark)


;; org modern config
(use-package org-modern
	:straight t
 :after org
 )
(with-eval-after-load 'org (global-org-modern-mode))

;; Add frame borders and window dividers
(modify-all-frames-parameters
 '((right-divider-width . 40)
   (internal-border-width . 40)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))
