;;; package --- Emacs configuration
;;; Commentary:
;;; My Emacs configuration

;; (setq package-archives nil)

;; Any Customize-based settings should live in custom.el, not here.
(setq custom-file "~/.config/emacs/custom.el") ;; Without this emacs will dump generated custom settings in this file. No bueno.
(load custom-file 'noerror)

(require 'package)

;; Nice macro for updating lists in place.
(defmacro append-to-list (target suffix)
  "Append SUFFIX to TARGET in place."
  `(setq ,target (append ,target ,suffix)))

;; Set up emacs package archives with 'package
(append-to-list package-archives
                '(("melpa" . "http://melpa.org/packages/") ;; Main package archive
                  ("melpa-stable" . "http://stable.melpa.org/packages/") ;; Some packages might only do stable releases?
                  ("org-elpa" . "https://orgmode.org/elpa/"))) ;; Org packages, I don't use org but seems like a harmless default

(package-initialize)

;; Ensure use-package is present. From here on out, all packages are loaded
;; with use-package, a macro for importing and installing packages. Also, refresh the package archive on load so we can pull the latest packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq
 use-package-always-ensure t ;; Makes sure to download new packages if they aren't already downloaded
 use-package-verbose t) ;; Package install logging. Packages break, it's nice to know why.

(use-package evil
  :init
  (setq
   evil-want-integration t ;; This is optional since it's already set to t by default.
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

(use-package which-key)

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

;; Marginalia - annotations for completion
(use-package marginalia
  :init
  (marginalia-mode))

;; Flycheck is the newer version of flymake and is needed to make lsp-mode not freak out.
(use-package flycheck
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode) ;; always lint my code
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; Package for interacting with language servers
(use-package lsp-mode
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil ;; Flymake is outdated
        lsp-headerline-breadcrumb-mode nil)) ;; I don't like the symbols on the header a-la-vscode, remove this if you like them.
(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package magit)
(use-package eat)
(use-package direnv
  :config
  (direnv-mode))

;; Start of config
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)

;; (set-frame-font nil t)
;; (set-face-attribute 'line-number nil :inherit 'default)
(add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font 16" ))
(global-display-line-numbers-mode)
;; (setq display-line-numbers 'relative)

(use-package gruber-darker-theme
  :init
  (load-theme 'gruber-darker))

