(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(ignore-errors (load custom-file))

(defvar user-cache-directory (expand-file-name ".cache" user-emacs-directory))
(defvar user-backup-directory (expand-file-name "backup" user-emacs-directory))
(defvar user-autosave-directory (expand-file-name "autosave" user-emacs-directory))
(dolist (dir (list user-cache-directory user-backup-directory user-autosave-directory))
  (when (not (file-directory-p dir))
    (make-directory dir t)))
(setq backup-directory-alist `(("." . ,user-backup-directory))
      auto-save-filename-transforms `(("." ,user-autosave-directory t))
      auto-save-list-file-prefix (concat user-autosave-directory ".saves-")
      tramp-backup-directory-alist `((".*" . ,user-backup-directory))
      tramp-auto-save-directory user-autosave-directory)

(prefer-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")
(setq load-prefer-newer t
      inhibit-startup-message t)
(global-display-line-numbers-mode 1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (fboundp 'electric-pair-mode)
  (electric-pair-mode t))

(use-package emacs
  :config
  (defun open-init-file-config ()
    "Open the init.el Emacs configuration file."
    (interactive)
    (find-file "~/Documents/org/roam/emacs_config.org"))
  :bind ("C-x c" . open-init-file-config))

;;(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;;(package-initialize)
;;(package-refresh-contents)

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install-noverify)
  :bind
  (:map pdf-view-mode-map ("q" . #'kill-current-buffer)))
;; Desativar numeração de linha em modos específicos
(dolist (mode '(pdf-view-mode-hook writeroom-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package org-modern
  :after org
  :config
  (global-org-modern-mode))

(use-package org-agenda
  :custom
  (org-agenda-files '("tasks.org"))
  (org-agenda-start-with-log-mode t))

(use-package org
  :ensure t
  :custom
  (org-directory (file-truename "~/Documents/org/"))
  (org-todo-keywords '((sequence "TODO(t)" "ONGOING(o)" "WAIT(w@)" "|" "DONE(d!)" "CANCELED(c@)")
		       (sequence "[ ](T)" "[-](O)" "[?](W)" "|" "[X](D)")
		       (sequence "POST(p)" "|" "POSTED(P!)")
		       (sequence "TOREAD(r)" "|" "READ(R!)")
		       (sequence "TOLEARN(l)" "|" "LEARNED(L!)")))
  (org-hide-emphasis-markers t)

 ;; indentation
  (org-startup-truncated t)
  (org-startup-indented t)
 ;; src block indentation
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
;  ; logging
  (org-log-done 'time)
  (org-log-into-drawer t)
;;; Template
  (org-capture-templates
   '(("t" "Tasks")
      ("th" "Home tasks" entry (file+olp "~/Documents/org/tasks.org" "Home")
       "* TODO %? :home:\nSCHEDULED: %^t\n%i" :empty-lines-after 1)
      ("tf" "Finance's task" entry (file+olp "~/Documents/org/tasks.org" "Finance")
       "* TODO %? :finance:\nSCHEDULED: %^t\n%i" :empty-lines-after 1)
      ("tc" "Cyberz's task" entry (file+olp "~/Documents/org/tasks.org" "CyberZ")
       "* TODO %? :cyberz:\nSCHEDULED: %^t\n%i" :empty-lines-after 1)
      ("tr" "Read tasks" entry (file+olp "~/Documents/org/tasks.org" "Read")
       "* TOREAD %? :read:\nSCHEDULED: %^t\n%i" :empty-lines-after 1)
      ("tg" "General Tasks" entry (file+olp "~/Documents/org/tasks.org" "General")
       "* TODO %? :general:\nSCHEDULED: %^t\n%i" :empty-lines-after 1)))
  :bind
  ("C-c a" . org-agenda)
  ("C-c l" . org-store-link)
  ("C-c c" . org-capture))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Documents/org/roam"))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-enable)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  ;;org-roam templates
  (setq org-roam-capture-templates
   '(("d" "default" plain "#+filetags: :%?:"
      :if-new (file+head "${slug}.org" "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)
     ("c" "documentation" plain "* index of\n- %?"
      :if-new (file+head "${slug}.org" "#+title: ${title}\n#+date: %U\n")
      :unarrowed t)
     ("n" "nirvax notes" plain "- tags ::\n- source ::\n\n%?"
      :target (file+head "nirvax/${slug}.org" "#+title: Nirvax: ${title}\n#+filetags: :nirvax:\n#+author: %n\n#+date: %U\n\n")
      :unarrowed t)
     ("r" "reading notes" plain "%?"
      :target (file+head "${citar-citekey}.org" "#+title: ${note-title}\n#+created: %U\n")
      :unarrowed t)))
  :bind
  ; org-roam bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-node-capture)
   ("C-c n u" . org-roam-ui-mode)
))

(use-package org-roam-bibtex
  :ensure t
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :custom
  (org-roam-bibtex-preformat-keywords
   '("=key=" "title" "file" "author" "keywords"))
  (orb-process-file-keyword t)
  (orb-process-file-field t)
  (orb-attached-file-extensions '("pdf")))

(use-package org-roam-ui
  :ensure t
  :after (org-roam)
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

(use-package projectile
  :ensure t
  :init
  (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package citar
  :ensure t
  :custom
  (citar-bibliography global/bibliography-list)
  (citar-notes-paths '("~/Documents/org/roam/"))
  (citar-open-note-function 'orb-citar-edit-note)
  (citar-at-point-function 'embark-act)
  ; templates
  (citar-templates
   '((main . "${author editor:30%sn}     ${date year issued:4}     ${title:48}")
     (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
     (preview . "${author editor:%etal} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
     (note . "Notes on ${author editor:%etal}, ${title}")))
  ; advices
  (advice-add 'org-cite-insert :after #'(lambda (args)
					              (save-excursion (left-char) (citar-org-update-prefix-suffix))))
  :bind
    (("C-c b b" . citar-insert-citation)
     ("C-c b r" . citar-insert-reference)
     ("C-c b o" . citar-open)))

(use-package citar-embark
  :after (citar embark)
  :config
  (citar-embark-mode))
(setq global/bibliography-list '("~/.emacs.d/file.bib"))

(use-package oc
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (org-cite-global-bibliography global/bibliography-list)
  (org-cite-export-processors '((latex biblatex)
				(t csl)))
  (org-cite-csl-styles-dir "~/Documents/org/csl/"))

(use-package oc-biblatex
  :after oc)
(use-package oc-csl
  :after oc)
(use-package oc-natbib
  :after oc)

(use-package citar-org-roam
  :ensure t
  :after (citar org-roam)
  :config
  (citar-org-roam-mode)
  (setq citar-org-roam-note-title-template "${author} - ${title}")
  (setq citar-org-roam-capture-template-key "r"))

;; Embark
(use-package embark
  :ensure t
    :hook (eldoc-documentation-functions . embark-eldoc-first-target)
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none))))
  :bind
  ("C-." . embark-act)
  ("C-;" . embark-dwim)
  ("C-h B" . embark-bindings))

(use-package company
  :ensure t
  :custom
  (company-minimum-prefix-length 2)
  (company-tooltip-limit 14)
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  (company-auto-commit nil)
  (company-dabbrev-other-buffers nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil))

(use-package company-box
  :ensure t
  :after company
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-show-single-candidate t)
  (company-box-backends-colors nil)
  (company-box-tooltip-limit 50))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(require 'ob-C)
(use-package ob
  :custom
  (org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
							   (rust . t)
							   (C . t)
							   (mermaid . t))))
(use-package ob-rust
  :ensure t)
(use-package ob-async
  :ensure t)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  ;; treemacs theme
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode)
  :config
  (which-key-setup-side-window-bottom))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  :bind
  (:map vertico-map
	("C-j" . vertico-next)
	("C-k" . vertico-previous)
	("C-f" . vertico-exit)
	:map minibuffer-local-map
	("M-h" . backward-kill-word)))

(use-package savehist
  :ensure t
  :init
  (savehist-mode))
(use-package marginalia
  :ensure t
  :after (vertico)
  :init
  (marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("C-x t o"   . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))
(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)
(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)
(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)
(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)
(use-package all-the-icons
  :ensure t)
(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))
(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))
