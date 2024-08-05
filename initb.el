;;; package --- sumary
;;; Commentary:
;;; Code:

(setq user-full-name "zDragonSK"
      user-email-address "zdragonsk@pm.me")

;; ---------------------------------------------- Configurações Emacs

;; Carregar custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(ignore-errors (load custom-file))

;; Diretórios de Cache, Backup e Autosave
(defvar user-cache-directory (expand-file-name ".cache" user-emacs-directory))
(defvar user-backup-directory (expand-file-name "backup" user-emacs-directory))
(defvar user-autosave-directory (expand-file-name "autosave" user-emacs-directory))
(dolist (dir (list user-cache-directory user-backup-directory user-autosave-directory))
  (unless (file-directory-p dir)
    (make-directory dir t)))
(setq backup-directory-alist `(("." . ,user-backup-directory))
      auto-save-file-name-transforms `((".*" ,user-autosave-directory t))
      auto-save-list-file-prefix (concat user-autosave-directory ".saves-")
      tramp-backup-directory-alist `((".*" . ,user-backup-directory))
      tramp-auto-save-directory user-autosave-directory)

;; Configurações básicas
(prefer-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")
(setq load-prefer-newer t
      inhibit-startup-message t)

;; Interface
(global-display-line-numbers-mode 1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (fboundp 'electric-pair-mode)
  (electric-pair-mode 1))

;; Desativar numeração de linha em modos específicos
(dolist (mode '(pdf-view-mode-hook writeroom-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; ---------------------------------------------- Pacotes

;; Configuração de use-package
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Org-mode modern
(use-package org-modern
  :after org
  :config
  (global-org-modern-mode))

;; All-the-icons para o Treemacs
(use-package all-the-icons)

;; Configurando evil & carregando evil-collection
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Configuração Org
(use-package org
  :custom
  (org-directory (file-truename "~/Documents/org/"))
  (org-todo-keywords '((sequence "TODO(t)" "ONGOING(o)" "WAIT(w@)" "|" "DONE(d!)" "CANCELED(c@)")
                       (sequence "[ ](T)" "[-](O)" "[?](W)" "|" "[X](D)")
                       (sequence "POST(p)" "|" "POSTED(P!)")
                       (sequence "TOREAD(r)" "|" "READ(R!)")
                       (sequence "TOLEARN(l)" "|" "LEARNED(L!)")))
  (org-startup-truncated t)
  (org-startup-indented t)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  (org-log-done 'time)
  (org-log-into-drawer t)
  :bind
  (("C-c a" . org-agenda)
   ("C-c l" . org-store-link)
   ("C-c c" . org-capture)))

(use-package org-agenda
  :ensure nil
  :custom
  (org-agenda-files '("habits.org" "tasks.org" "learn.org" "home.org"))
  (org-agenda-start-with-log-mode t))

(use-package org-super-agenda
  :hook (org-agenda-mode . org-super-agenda-mode))

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Documents/org/roam/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

;; Configurações do Treemacs
(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (setq treemacs-collapse-dirs (if treemacs-python-executable 3 0)
        treemacs-deferred-git-apply-delay 0.5
        treemacs-directory-name-transformer #'identity
        treemacs-display-in-side-window t
        treemacs-eldoc-display 'simple
        treemacs-file-event-delay 2000
        treemacs-file-extension-regex treemacs-last-period-regex-value
        treemacs-file-follow-delay 0.2
        treemacs-file-name-transformer #'identity
        treemacs-follow-after-init t
        treemacs-expand-after-init t
        treemacs-find-workspace-method 'find-for-file-or-pick-first
        treemacs-git-command-pipe ""
        treemacs-goto-tag-strategy 'refetch-index
        treemacs-header-scroll-indicators '(nil . "^^^^^^")
        treemacs-hide-dot-git-directory t
        treemacs-indentation 2
        treemacs-indentation-string " "
        treemacs-is-never-other-window nil
        treemacs-max-git-entries 5000
        treemacs-missing-project-action 'ask
        treemacs-move-files-by-mouse-dragging t
        treemacs-move-forward-on-expand nil
        treemacs-no-png-images nil
        treemacs-no-delete-other-windows t
        treemacs-project-follow-cleanup nil
        treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-position 'left
        treemacs-read-string-input 'from-child-frame
        treemacs-recenter-distance 0.1
        treemacs-recenter-after-file-follow nil
        treemacs-recenter-after-tag-follow nil
        treemacs-recenter-after-project-jump 'always
        treemacs-recenter-after-project-expand 'on-distance
        treemacs-litter-directories '("/node_modules" "/.venv" "/.cask")
        treemacs-project-follow-into-home nil
        treemacs-show-cursor nil
        treemacs-show-hidden-files t
        treemacs-silent-filewatch nil
        treemacs-silent-refresh nil
        treemacs-sorting 'alphabetic-asc
        treemacs-select-when-already-in-treemacs 'move-back
        treemacs-space-between-root-nodes t
        treemacs-tag-follow-cleanup t
        treemacs-tag-follow-delay 1.5
        treemacs-text-scale nil
        treemacs-user-mode-line-format nil
        treemacs-user-header-line-format nil
        treemacs-width 35)
  (treemacs-resize-icons 22)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))
  (treemacs-hide-gitignored-files-mode nil))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

;; Apenas instale treemacs-all-the-icons se disponível
(use-package treemacs-all-the-icons
  :after treemacs
  :if (package-installed-p 'treemacs-all-the-icons)
  :config (treemacs-load-theme "all-the-icons"))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-persp
  :after (treemacs persp-mode)
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

;; Treemacs e Magit
(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; Flycheck
(use-package flycheck
  :init (global-flycheck-mode))

;; Yasnippet
(use-package yasnippet
  :init (yas-global-mode 1))

;; Auto Complete
(use-package company
  :init (global-company-mode))

;; Consult e Embark
(use-package consult)
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)))

;; Which Key
(use-package which-key
  :init (which-key-mode))

;; Doom themes
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-palenight t)
  (doom-themes-org-config))

;; Doom modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Vertico e Orderless
(use-package vertico
  :init (vertico-mode))

(use-package orderless
  :custom (completion-styles '(orderless)))

;; Magit
(use-package magit)

;; Consult Projectile
;;(use-package consult-projectile
  ;;:config (consult-projectile-mode))

;; Copilot
;;(use-package copilot
  ;;:hook (prog-mode . copilot-mode))

;; Ligatures
(use-package ligature
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures 'prog-mode '("!=" "!=="))
  (global-ligature-mode t))

(provide 'init)
;;; init.el ends here

