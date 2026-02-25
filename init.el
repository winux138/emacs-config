;;; init.el --- Emacs configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;; My Emacs configuration.
;;; Code:

;; Any Customize-based settings should live in custom.el, not here.
;; Without this emacs will dump generated custom settings in this file.
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file 'noerror)

;; Font -- use set-face-attribute for unambiguous family + size.
(set-face-attribute 'default nil :family "Iosevka Nerd Font Mono" :height 120)
(set-face-attribute 'fixed-pitch nil :family "Hack Nerd Font Mono" :height 120)

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

;; Refresh package archive contents if not cached yet.
;; Without this, use-package :ensure silently fails for new packages.
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package-ensure) ;; Not autoloaded in Emacs 29+; without this :ensure is silently ignored.
(setq
  use-package-always-ensure t ;; Makes sure to download new packages if they aren't already downloaded
  use-package-always-defer nil
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

;; Jump to visible text with avy, bound to 's' in normal state.
(use-package avy
  :after evil
  :config
  (evil-define-key 'normal 'global "s" 'avy-goto-char-2))

;; ;; Best completion style ?!
;; (use-package hotfuzz
;;   :config
;;   (setq completion-styles '(hotfuzz)
;;         completion-ignore-case t
;;         read-buffer-completion-ignore-case t
;;         read-file-name-completion-ignore-case t))

;; Slurp environment variables from the shell.
;; a.k.a. The Most Asked Question On r/emacs
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Leader key setup with general.el -- Spacemacs-like SPC bindings.
(use-package general
  :after evil
  :config
  (general-evil-setup)

  ;; Create a definer for SPC as leader in normal/visual states.
  (general-create-definer leader-def
    :states '(normal visual)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC") ;; fallback for insert/emacs states

  (leader-def
    ""    '(nil :wk "leader")

    ;; Buffers
    "b"   '(:ignore t :wk "buffer")
    "bb"  '(switch-to-buffer :wk "switch buffer")
    "bd"  '(kill-current-buffer :wk "kill buffer")
    "bn"  '(next-buffer :wk "next buffer")
    "bp"  '(previous-buffer :wk "prev buffer")
    "bi"  '(ibuffer :wk "ibuffer")
    "bs"  '(save-buffer :wk "save buffer")

    ;; Files
    "f"   '(:ignore t :wk "file")
    "ff"  '(find-file :wk "find file")
    "fs"  '(save-buffer :wk "save file")
    "fr"  '(recentf :wk "recent files")

    ;; Windows
    "w"   '(:ignore t :wk "window")
    "wv"  '(evil-window-vsplit :wk "vertical split")
    "ws"  '(evil-window-split :wk "horizontal split")
    "wd"  '(evil-window-delete :wk "delete window")
    "wh"  '(evil-window-left :wk "window left")
    "wj"  '(evil-window-down :wk "window down")
    "wk"  '(evil-window-up :wk "window up")
    "wl"  '(evil-window-right :wk "window right")
    "w="  '(balance-windows :wk "balance windows")

    ;; Git (magit)
    "g"   '(:ignore t :wk "git")
    "gs"  '(magit-status :wk "status")
    "gb"  '(magit-blame :wk "blame")
    "gl"  '(magit-log-current :wk "log")
    "gd"  '(magit-diff :wk "diff")

    ;; LSP
    "l"   '(:ignore t :wk "lsp")
    "ll"  '(lsp :wk "start lsp")
    "lr"  '(lsp-rename :wk "rename")
    "la"  '(lsp-execute-code-action :wk "code action")
    "ld"  '(lsp-find-definition :wk "find definition")
    "lD"  '(lsp-find-declaration :wk "find declaration")
    "li"  '(lsp-find-implementation :wk "find implementation")
    "lR"  '(lsp-find-references :wk "find references")
    "ls"  '(consult-lsp-symbols :wk "workspace symbols")
    "le"  '(consult-lsp-diagnostics :wk "diagnostics")
    "lf"  '(lsp-format-buffer :wk "format buffer")
    "lq"  '(lsp-workspace-restart :wk "restart lsp")
    "lQ"  '(lsp-workspace-shutdown :wk "shutdown lsp")

    ;; Quit / help
    "h"   '(:ignore t :wk "help")
    "hk"  '(describe-key :wk "describe key")
    "hf"  '(describe-function :wk "describe function")
    "hv"  '(describe-variable :wk "describe variable")
    "hm"  '(describe-mode :wk "describe mode")

    ;; Toggle
    "t"   '(:ignore t :wk "toggle")
    "tn"  '(display-line-numbers-mode :wk "line numbers")
    "tw"  '(whitespace-mode :wk "whitespace")
    "tt"  '(load-theme :wk "load theme")

    ;; Search
    "s"   '(:ignore t :wk "search")
    "ss"  '(consult-line :wk "search buffer")
    "sp"  '(consult-ripgrep :wk "search project")

    ;; Open
    "o"   '(:ignore t :wk "open")
    "od"  '(dired :wk "dired")
    "ot"  '(eat :wk "terminal")

    ;; Quit
    "q"   '(:ignore t :wk "quit")
    "qq"  '(save-buffers-kill-emacs :wk "quit emacs")))

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

;; Consult -- enhanced search and navigation commands.
(use-package consult
  :config
  (setq consult-line-start-from-top t)) ;; Search from top of buffer.

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match 'insert) ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :init

  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

  ;; Enable optional extension modes:
  ;; (corfu-history-mode)
  ;; (corfu-popupinfo-mode)
  )

;; Orderless -- hopefully a fzf-like finder/matcher
(use-package orderless
  :custom
  (completion-styles '(orderless flex))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

;; Flycheck -- on-the-fly syntax checking.
(use-package flycheck
  :init
  (global-flycheck-mode))

(use-package consult-flycheck
  :after (consult flycheck))

;; Package for interacting with language servers.
(use-package lsp-mode
  :commands lsp
  :after flycheck
  :init
  (setq lsp-keymap-prefix nil) ;; Disable default lsp-mode keybindings; we use SPC l via general.el.
  :config
  (setq lsp-diagnostics-provider :flycheck
        lsp-headerline-breadcrumb-enable nil)) ;; Disable breadcrumb header a-la-vscode.

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package consult-lsp
  :after lsp-mode)

(use-package rustic
  :custom
  (rustic-lsp-client 'lsp-mode))

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

;; (use-package gruber-darker-theme
;;   :config
;;   (load-theme 'gruber-darker t))

(use-package doric-themes
  :config
  (doric-themes-select 'doric-wind))

;;; init.el ends here
