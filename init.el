;;; package --- sumary
;;; Treemacs    Tree-explorer for files
;;; Commentary:
;;; Code:
;; -------------------------------------------------------- Configurações
; Carregar custom.el
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
(global-display-line-numbers-mode 1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (fboundp 'electric-pair-mode)
  (electric-pair-mode t))

;; Binds
(use-package emacs
  :config
  (defun open-init-file-config ()
    "Open the init.el Emacs configuration file."
    (interactive)
    (find-file "~/.emacs.d/init.el"))
  :bind ("C-x c" . open-init-file-config))

; Set up package.el to work with MELPA
;;(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;;(package-initialize)
;;(package-refresh-contents)
;; -------------------------------------------------------- Packages
;; PDFTOOLS
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

;; Babel
(require 'ob-C)
(use-package ob
  :custom
  (org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
							   (rust . t)
							   (C . t)
							   (mermaid . t))))
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
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

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

;; Exibir atalhos
(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode)
  :config
  (which-key-setup-side-window-bottom))

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

;; Treemacs
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
;; -------------------------------------------------------- Languages
;; dependências Babel
(use-package ob-rust
  :ensure t)
(use-package ob-async
  :ensure t)
;;; init.el ends here
