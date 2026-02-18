;;; init.el --- Emacs configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;; My Emacs configuration.
;;; Code:

;; Any Customize-based settings should live in custom.el, not here.
;; Without this emacs will dump generated custom settings in this file.
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file 'noerror)

;; Font -- use set-face-attribute for unambiguous family + size.
(set-face-attribute 'default nil :family "Iosevka Nerd Font Mono" :height 160)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(column-number-mode 1)
(show-paren-mode 1)

(require 'package)

;; Nice macro for updating lists in place.
(defmacro append-to-list (target suffix)
  "Append SUFFIX to TARGET in place."
  `(setq ,target (append ,target ,suffix)))

;; Set up emacs package archives with 'package.
(append-to-list package-archives
                '(("melpa" . "https://melpa.org/packages/")
                  ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)

(setq
 use-package-always-ensure t ;; Makes sure to download new packages if they aren't already downloaded
 use-package-verbose t) ;; Package install logging. Packages break, it's nice to know why.

(use-package evil
  :init
  (setq
   evil-want-integration t
   evil-want-keybinding nil
   evil-want-C-u-scroll t
   evil-want-Y-yank-to-eol t)
  :config
  (evil-mode))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Best completion style ?!
(use-package hotfuzz
  :config
  (setq completion-styles '(hotfuzz)))

;; Slurp environment variables from the shell.
;; a.k.a. The Most Asked Question On r/emacs
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; TODO: Use package general.el

(use-package posframe)

(use-package which-key
  :config
  (which-key-mode))

(use-package which-key-posframe
  :after which-key
  :config
  (which-key-posframe-mode))

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t))

(use-package vertico-posframe
  :after vertico
  :config
  (vertico-posframe-mode))

;; Marginalia -- annotations for completion.
(use-package marginalia
  :init
  (marginalia-mode))

;; Flycheck -- on-the-fly syntax checking.
(use-package flycheck
  :init
  (global-flycheck-mode))

;; Package for interacting with language servers.
(use-package lsp-mode
  :commands lsp
  :config
  (setq lsp-diagnostics-provider :flycheck
        lsp-headerline-breadcrumb-enable nil)) ;; Disable breadcrumb header a-la-vscode.
(use-package lsp-ui :commands lsp-ui-mode)

(use-package consult-lsp
  :after lsp-mode)

(use-package magit)
(use-package eat)
(use-package direnv
  :config
  (direnv-mode))

;; Markdown preview with plantuml diagram support.
(use-package markdown-mode)
(use-package plantuml-mode
  :config
  (setq plantuml-executable-path (executable-find "plantuml")
        plantuml-default-exec-mode 'executable))
(use-package markdown-preview-mode
  :config
  (setq markdown-preview-mode-plantuml-enabled t)) ;; Renders ```plantuml blocks in preview

(use-package gruber-darker-theme
  :config
  (load-theme 'gruber-darker t))

;;; init.el ends here
