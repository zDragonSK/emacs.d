;;; package --- sumary
;;; Commentary:
;;; Code:
(setq user-full-name "zDragonSK"
      user-email-address "zdragonsk@pm.me")

;; ---------------------------------------------- Configurações Emacs
;; Carregar custom.el
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

;; Configurações básicas
(prefer-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")
(setq load-prefer-newer t
      inhibit-startup-message t)
;; Ligar numeração de linha
(global-display-line-numbers-mode 1)
;; Desligar scroll-bar
(scroll-bar-mode -1)
;; Desligar barra de menu
(menu-bar-mode -1)
;; Desligar barra de ferramentas
(tool-bar-mode -1)
;; Carregar parenteses
(when (fboundp 'electric-pair-mode)
  (electric-pair-mode t))

;; PDF
(dolist (mode '(pdf-view-mode-hook writeroom-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Binds
(use-package emacs
  :config
  (defun open-init-file-config ()
    "Open the init.el Emacs configuration file."
    (interactive)
    (find-file "~/.emacs.d/init.el"))
  :bind ("C-x c" . open-init-file-config))

;; ---------------------------------------------- Pacotes
;; Set up package.el to work with MELPA
;;(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;;(package-initialize)
;;(package-refresh-contents)

;; All-the-icons para o Treemacs
(use-package all-the-icons
  :ensure t)

;; Configurando evil & carregando evil-collection
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

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Documents/org/roam/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

;; Configurações do Treemacs
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
  ;; Shortcurts
  ("C-x t o"   . treemacs-select-window)
  ("C-x t 1"   . treemacs-delete-other-windows)
  ("C-x t t"   . treemacs)
  ("C-x t d"   . treemacs-select-directory)
  ("C-x t B"   . treemacs-bookmark)
  ("C-x t C-t" . treemacs-find-file)
  ("C-x t M-t" . treemacs-find-tag))

;; Treemacs f-evil
(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

;; Tema do DOOM
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

;; Modeline DOOM
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; Exibir atalhos
(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode)
  :config
  (which-key-setup-side-window-bottom))

;; Exibir snips
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :custom
  (company-minimum-prefix-length 2)
  (company-tooltip-limit 14)
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  (company-auto-commit nil)
  (company-dabbrev-other-buffers nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil))
;; Exibir pop-up
(use-package company-box
  :ensure t
  :after company
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-show-single-candidate t)
  (company-box-backends-colors nil)
  (company-box-tooltip-limit 50))

;; Completer do M-X | Vertical Completion
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
;; Dependências do Vertico
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

;; Babel
(use-package ob
  :custom
  (org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
							   (rust . t)
							   (mermaid . t))))
;; Dependencias do Babel
(use-package ob-rust
  :ensure t)
(use-package ob-async
  :ensure t)

;; Ler pds (PDFTOOLS)
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install-noverify)
  :bind
  (:map pdf-view-mode-map ("q" . #'kill-current-buffer)))

;; Editorconfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Projetos
(use-package projectile
  :ensure t
  :init
  (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; EGLOT (LSP)
(use-package eglot
  :init
  (setq eglot-sync-connect 1
	eglot-autoshutdown t
	eglot-auto-display-help-buffer nil)
  :config
  (setq eglot-stay-out-of '(flymake)))

;; VTERM
(use-package vterm
  :ensure t
  :init
  (defun run-vterm-custom ()
    "This function will run vterm inside the project root or in the current directory."
    (interactive)
    (if (projectile-project-p) (projectile-run-vterm) (vterm default-directory)))

  (defun run-vterm-other-window-custom ()
    "This function will run vterm in other window inside the project root or in the current directory."
    (interactive)
    (if (projectile-project-p) (projectile-run-vterm-other-window) (vterm-other-window default-directory)))
  
  :bind (("C-c t" . run-vterm-custom)
	 ("C-c C-t" . run-vterm-other-window-custom)))

;; flycheck | warnings && errors checker
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))
(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode))
(use-package flycheck-popup-tip
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-popup-tip-mode))
(use-package flycheck-eglot
  :ensure t
  :after (eglot flycheck)
  :hook (eglot-managed-mode . flycheck-eglot-mode))

;; ordeless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  :config
  (defun just-one-face (fn &rest args)
    (let ((orderless-match-faces [completions-common-part]))
      (apply fn args)))

  (advice-add 'company-capf--candidates :around #'just-one-face)
  (setq orderless-component-separator "[ &]"))

;; ---------------------------------------------- Linguagens
(setq treesit-language-source-alist
      '((rust "https://github.com/tree-sitter/tree-sitter-rust")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(make "https://github.com/alemuller/tree-sitter-make")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")
	(c "https://github.com/tree-sitter/tree-sitter-c")
	(cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	(cmake "https://github.com/uyha/tree-sitter-cmake")))

(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :hook (rust-ts-mode . eglot-ensure)
  :init
  (add-to-list 'org-src-lang-modes '("rust" . rust-ts)))

(use-package js-ts-mode
  :mode "\\.js\\'"
  :hook (js-ts-mode . eglot-ensure)
  :init
  (add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode))
  (add-to-list 'org-src-lang-modes '("javascript" . js-ts)))

(use-package typescript-ts-mode
  :mode "\\.ts\\'"
  :hook (typescript-ts-mode . eglot-ensure)
  :init
  (add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))
  (add-to-list 'org-src-lang-modes '("typescript" . typescript-ts)))

(use-package c-ts-mode
  :mode "\\.c\\'"
  :mode "\\.h\\'"
  :hook (c-ts-mode . eglot-ensure)
  :init
  ; (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'org-src-lang-modes '("c" . c-ts)))
;;; init.el ends here
