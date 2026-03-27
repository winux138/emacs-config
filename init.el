;;; init.el --- Emacs configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;; My Emacs configuration.
;;; Code:

;; Font -- use set-face-attribute for unambiguous family + size.
(set-face-attribute 'default nil :family "Iosevka Nerd Font Mono" :height 120 :weight 'regular)
(set-face-attribute 'fixed-pitch nil :family "Iosevka Nerd Font Mono" :height 120 :weight 'light)

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

(use-package emacs
  :ensure nil
  :custom                                         ;; Set custom variables to configure Emacs behavior.
  (auto-save-default nil)                         ;; Disable automatic saving of buffers.
  (column-number-mode t)                          ;; Display the column number in the mode line.
  (create-lockfiles nil)                          ;; Prevent the creation of lock files when editing.
  (delete-by-moving-to-trash t)                   ;; Move deleted files to the trash instead of permanently deleting them.
  (delete-selection-mode 1)                       ;; Enable replacing selected text with typed text.
  (display-line-numbers-type 'relative)           ;; Use relative line numbering in programming modes.
  (global-auto-revert-non-file-buffers t)         ;; Automatically refresh non-file buffers.
  (history-length 25)                             ;; Set the length of the command history.
  (indent-tabs-mode nil)                          ;; Disable the use of tabs for indentation (use spaces instead).
  (initial-scratch-message "")                    ;; Clear the initial message in the *scratch* buffer.
  (ispell-dictionary "en_US")                     ;; Set the default dictionary for spell checking.
  (make-backup-files nil)                         ;; Disable creation of backup files.
  (pixel-scroll-precision-mode t)                 ;; Enable precise pixel scrolling.
  (pixel-scroll-precision-use-momentum nil)       ;; Disable momentum scrolling for pixel precision.
  (ring-bell-function 'ignore)                    ;; Disable the audible bell.
  (split-width-threshold 300)                     ;; Prevent automatic window splitting if the window width exceeds 300 pixels.
  (switch-to-buffer-obey-display-actions t)       ;; Make buffer switching respect display actions.
  (tab-width 4)                                   ;; Set the tab width to 4 spaces.
  (treesit-font-lock-level 4)                     ;; Use advanced font locking for Treesit mode.
  (use-dialog-box nil)                            ;; Disable dialog boxes in favor of minibuffer prompts.
  (use-short-answers t)                           ;; Use short answers in prompts for quicker responses (y instead of yes)
  (scroll-off 5)

  :config
  ;; By default emacs gives you access to a lot of *special* buffers, while navigating with [b and ]b,
  ;; this might be confusing for newcomers. This settings make sure ]b and [b will always load a
  ;; file buffer. To see all buffers use <leader> SPC, <leader> b l, or <leader> b i.
  (defun skip-these-buffers (_window buffer _bury-or-kill)
    "Function for `switch-to-prev-buffer-skip'."
    (string-match "\\*[^*]+\\*" (buffer-name buffer)))
  (setq switch-to-prev-buffer-skip 'skip-these-buffers)

  ;; Save manual customizations to a separate file instead of cluttering `init.el'.
  ;; You can M-x customize, M-x customize-group, or M-x customize-themes, etc.
  ;; The saves you do manually using the Emacs interface would overwrite this file.
  ;; The following makes sure those customizations are in a separate file.
  (setq custom-file (locate-user-emacs-file "custom-vars.el")) ;; Specify the custom file path.
  (load custom-file 'noerror 'nomessage)                       ;; Load the custom file quietly, ignoring errors.

  ;; Makes Emacs vertical divisor the symbol │ instead of |.
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

  :init                        ;; Initialization settings that apply before the package is loaded.
  (global-hl-line-mode -1)     ;; Disable highlight of the current line
  (global-auto-revert-mode 1)  ;; Enable global auto-revert mode to keep buffers up to date with their corresponding files.
  (recentf-mode 1)             ;; Enable tracking of recently opened files.
  (savehist-mode 1)            ;; Enable saving of command history.
  (save-place-mode 1)          ;; Enable saving the place in files for easier return.
  (winner-mode 1)              ;; Enable winner mode to easily undo window configuration changes.
  (xterm-mouse-mode 1)         ;; Enable mouse support in terminal mode.
  (file-name-shadow-mode 1)    ;; Enable shadowing of filenames for clarity.
  (global-display-line-numbers-mode 1)

  ;; Set the default coding system for files to UTF-8.
  (modify-coding-system-alist 'file "" 'utf-8))

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
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-Y-yank-to-eol t
        evil-want-fine-undo t)
  :config
  (evil-set-undo-system 'undo-tree)
  (evil-mode))

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-setup-minibuffer t) ;; Evil bindings in minibuffer (insert mode by default)
  :config
  (evil-collection-init))

(use-package evil-numbers
  :after evil
  :config
  ;; C-a to increment (like Vim), C-S-a to decrement (GUI only).
  ;; Keeps C-x free as Emacs prefix key.
  (evil-define-key '(normal visual) 'global
    (kbd "C-a") 'evil-numbers/inc-at-pt
    (kbd "C-S-a") 'evil-numbers/dec-at-pt)
  ;; Doom-style g= / g- as secondary bindings (also work in terminal).
  (evil-define-key 'normal 'global
    (kbd "g=") 'evil-numbers/inc-at-pt
    (kbd "g-") 'evil-numbers/dec-at-pt)
  (evil-define-key 'visual 'global
    (kbd "g=") 'evil-numbers/inc-at-pt-incremental
    (kbd "g-") 'evil-numbers/dec-at-pt-incremental
    (kbd "g+") 'evil-numbers/inc-at-pt))
(use-package evil-surround
             :config
             (global-evil-surround-mode 1))

;; Jump to visible text with avy, bound to 's' in normal state.
(use-package avy
  :after evil
  :config
  (evil-define-key 'normal 'global "s" 'avy-goto-char-timer))

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

    ;; Everything else

    "u"   '(undo-tree-visualize :wk "undo tree")

    ;; Buffers
    "b"   '(:ignore t :wk "buffer")
    "bb"  '(consult-buffer :wk "switch buffer")
    "bd"  '(kill-current-buffer :wk "kill buffer")
    "bn"  '(next-buffer :wk "next buffer")
    "bp"  '(previous-buffer :wk "prev buffer")
    "bi"  '(ibuffer :wk "ibuffer")
    "bs"  '(save-buffer :wk "save buffer")

    ;; Files
    "f"   '(:ignore t :wk "file")
    "ff"  '(affe-find :wk "find file")
    "fg"  '(affe-grep :wk "grep files")
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

;; Affe -- async fuzzy file finder and grep
(use-package affe
  :config
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    "Compile INPUT to regexps via orderless for affe's async pre-filtering."
    (setq input (cdr (orderless-compile input '(orderless-flex))))
    (cons input (apply-partially #'orderless--highlight input t)))
  (consult-customize affe-grep :preview-key "M-."))

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

;; Hotfuzz -- fzf-like scoring and ranking for completion candidates
(use-package hotfuzz)

;; Orderless -- multi-component matching (space-separated patterns)
(use-package orderless
  :custom
  (completion-styles '(hotfuzz orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (orderless-smart-case t)
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-category-defaults nil)
  (completion-pcm-leading-wildcard t))

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
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-show-with-cursor nil   ;; Don't auto-show doc on cursor hover
        lsp-ui-doc-show-with-mouse nil     ;; Don't auto-show doc on mouse hover
        lsp-ui-doc-position 'at-point)     ;; Show doc near cursor, not top-right

  ;; Neovim-style K: first press shows doc, second press focuses it for scrolling/copying.
  (defun lsp-ui-doc-toggle-focus ()
    "Show LSP hover doc, or focus it if already visible."
    (interactive)
    (if (lsp-ui-doc--frame-visible-p)
        (lsp-ui-doc-focus-frame)
      (lsp-ui-doc-show)))

  (evil-define-key 'normal lsp-ui-mode-map "K" #'lsp-ui-doc-toggle-focus)

  ;; Press q to close and return from the focused doc frame.
  (define-key lsp-ui-doc-frame-mode-map [?q] #'lsp-ui-doc-unfocus-frame))

(use-package consult-lsp
  :after lsp-mode)

(use-package rustic
  :custom
  (rustic-lsp-client 'lsp-mode))

;; UNDO TREE
;; The `undo-tree' package provides an advanced and visual way to
;; manage undo history. It allows you to navigate and visualize your
;; undo history as a tree structure, making it easier to manage
;; changes in your buffers.
(use-package undo-tree
  :defer t
  :hook
  (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        ;; Increase undo limits to avoid losing history due to Emacs' garbage collection.
        ;; These values can be adjusted based on your needs.
        ;; 10X bump of the undo limits to avoid issues with premature
        ;; Emacs GC which truncates the undo history very aggressively.
        undo-limit 800000                     ;; Limit for undo entries.
        undo-strong-limit 12000000            ;; Strong limit for undo entries.
        undo-outer-limit 120000000)           ;; Outer limit for undo entries.
  :config
  ;; Set the directory where `undo-tree' will save its history files.
  ;; This keeps undo history across sessions, stored in a cache directory.
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/.cache/undo"))))

(use-package evil-string-inflection)
(use-package magit)
(use-package eat)
(use-package direnv
  :config
  (direnv-mode))

;;; XCLIP
;; `xclip' is an Emacs package that integrates the X Window System clipboard
;; with Emacs. It allows seamless copying and pasting between Emacs and other
;; applications using the clipboard. When `xclip' is enabled, any text copied
;; in Emacs can be pasted in other applications, and vice versa, providing a
;; smooth workflow when working across multiple environments.
(use-package xclip
  :hook
  (after-init . xclip-mode))     ;; Enable xclip mode after initialization.

(use-package git-gutter
  :config
  (global-git-gutter-mode 1))

;; Markdown preview with plantuml diagram support.
(use-package markdown-mode)
(use-package plantuml-mode
  :config
  (setq plantuml-executable-path (executable-find "plantuml")
        plantuml-default-exec-mode 'executable))

;; (use-package gruber-darker-theme
;;   :config
;;   (load-theme 'gruber-darker t))

;; (use-package nano-theme
;;   :config
;;   (load-theme 'nano-light t))

(use-package doric-themes
  :config
  (doric-themes-select 'doric-jade))

;;; init.el ends here
