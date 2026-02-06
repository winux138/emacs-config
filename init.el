;; -*- lexical-binding: t; -*-
(use-package emacs
  :ensure nil
  :init

  (cond
   ((eq system-type 'darwin)
    (defconst IS-MAC t)
    (defconst IS-LINUX nil)
    (defconst IS-WINDOWS nil))
   ((eq system-type 'gnu/linux)
    (defconst IS-MAC nil)
    (defconst IS-LINUX t)
    (defconst IS-WINDOWS nil))
   ((memq system-type '(cygwin windows-nt ms-dos))
    (defconst IS-MAC nil)
    (defconst IS-LINUX nil)
    (defconst IS-WINDOWS t)))

  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; When having windows with repeated filenames, uniquify them
  ;; by the folder they are in rather those annoying <2>,<3>,.. etc
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        ;; don't screw special buffers
        uniquify-ignore-buffers-re "^\\*")
  ;;; info+
  (setq Info-fontify-angle-bracketed-flag nil)

  ;;; smooth scrolling
  (setq hscroll-margin 2
        hscroll-step 1
        ;; Emacs spends too much effort recentering the screen if you scroll the
        ;; cursor more than N lines past window edges (where N is the settings of
        ;; `scroll-conservatively'). This is especially slow in larger files
        ;; during large-scale scrolling commands. If kept over 100, the window is
        ;; never automatically recentered.
        scroll-conservatively 101
        scroll-margin 0
        scroll-preserve-screen-position t
        ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
        ;; for tall lines.
        auto-window-vscroll nil
        ;; mouse
        mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 2)

  ;; no beep pleeeeeease ! (and no visual blinking too please)
  (setq ring-bell-function 'ignore
        visible-bell nil)

  ;; Keep focus while navigating help buffers
  (setq help-window-select 't)

  ;; ---------------------------------------------------------------------------
  ;; Clipboard
  ;; ---------------------------------------------------------------------------

  ;; Cull duplicates in the kill ring to reduce bloat and make the kill ring
  ;; easier to peruse (with `counsel-yank-pop' or `helm-show-kill-ring'.
  (setq kill-do-not-save-duplicates t)

  ;; ---------------------------------------------------------------------------
  ;; Edit
  ;; ---------------------------------------------------------------------------

  ;; use only spaces and no tabs
  (setq-default indent-tabs-mode nil
                tab-width 4)

  ;; Text
  (setq longlines-show-hard-newlines t)

  ;; fill-paragraph length
  ;; auto fill breaks line beyond buffer's fill-column
  (setq-default fill-column 110)

  ;; Save clipboard contents into kill-ring before replace them
  (setq save-interprogram-paste-before-kill t)

  ;; Single space between sentences is more widespread than double
  (setq-default sentence-end-double-space nil)

  ;; Automatically replace selection if active
  (delete-selection-mode 1)

  ;; Continue wrapped words at whitespace, rather than in the middle of a word.
  (setq-default word-wrap t)
  ;; ...but don't do any wrapping by default. It's expensive. Enable
  ;; `visual-line-mode' if you want soft line-wrapping. `auto-fill-mode' for hard
  ;; line-wrapping.

  (setq-default truncate-lines t)
  ;; If enabled (and `truncate-lines' was disabled), soft wrapping no longer
  ;; occurs when that window is less than `truncate-partial-width-windows'
  ;; characters wide. We don't need this, and it's extra work for Emacs otherwise,
  ;; so off it goes.
  (setq truncate-partial-width-windows nil)

  ;; The POSIX standard defines a line is "a sequence of zero or more non-newline
  ;; characters followed by a terminating newline", so files should end in a
  ;; newline. Windows doesn't respect this (because it's Windows), but we should,
  ;; since programmers' tools tend to be POSIX compliant (and no big deal if not).
  (setq require-final-newline t)

  ;; the default warning threshold in emacs 26 is working better
  (setq large-file-warning-threshold (* 1000 1024 1024))

  ;; ---------------------------------------------------------------------------
  ;; Encoding
  ;; ---------------------------------------------------------------------------
  ;; UTF-8 please
  (set-charset-priority 'unicode)
  (setq locale-coding-system   'utf-8)   ; pretty
  (set-terminal-coding-system  'utf-8)   ; pretty
  (set-keyboard-coding-system  'utf-8)   ; pretty
  (set-selection-coding-system 'utf-8)   ; please
  (prefer-coding-system        'utf-8)   ; with sugar on top
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  ;; Fix svn "warning: cannot set LC_CTYPE locale"
  (setenv "LC_ALL" "C")

  ;; ---------------------------------------------------------------------------
  ;; Session
  ;; ---------------------------------------------------------------------------
  ;; don't create backup~ files
  (setq backup-inhibited t
        make-backup-files nil
        auto-save-default nil
        auto-save-list-file-name nil
        create-lockfiles nil)

  ;; remove annoying ellipsis when printing sexp in message buffer
  (setq eval-expression-print-length nil
        eval-expression-print-level nil)

  ;; seems pointless to warn. There's always undo.
  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)
  (put 'scroll-left 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil)

  ;; ---------------------------------------------------------------------------
  ;; Other, Tweaks, Optimizations
  ;; ---------------------------------------------------------------------------

  ;; Emacs "updates" its ui more often than it needs to, so we slow it down
  ;; slightly, from 0.5s:
  (setq idle-update-delay 1)

;;; Disable some stuff -> emacs is quite slow sometimes on windows
  ;; slows down very much opening files
  (remove-hook 'kill-buffer-hook 'browse-url-delete-temp-file)

  (setq inhibit-compacting-font-caches t)

  ;; HACK `tty-run-terminal-initialization' is *tremendously* slow for some
  ;;      reason. Disabling it completely could have many side-effects, so we
  ;;      defer it until later.
  (unless (display-graphic-p)
    (advice-add #'tty-run-terminal-initialization :override #'ignore)
    (add-hook 'window-setup-hook
              (defun init-tty-h ()
                (advice-remove #'tty-run-terminal-initialization #'ignore)
                (tty-run-terminal-initialization (selected-frame) nil t))))


  ;; Disable warnings from legacy advice system. They aren't useful, and we can't
  ;; often do anything about them besides changing packages upstream
  (setq ad-redefinition-action 'accept)

  ;; Make apropos omnipotent. It's more useful this way.
  (setq apropos-do-all t)

  ;; Don't make a second case-insensitive pass over `auto-mode-alist'. If it has
  ;; to, it's our (the user's) failure. One case for all!
  (setq auto-mode-case-fold nil)


  ;; Disable bidirectional text rendering for a modest performance boost. Of
  ;; course, this renders Emacs unable to detect/display right-to-left languages
  ;; (sorry!), but for us left-to-right language speakers/writers, it's a boon.
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right)

  ;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
  ;; in non-focused windows.
  (setq-default cursor-in-non-selected-windows nil)
  (setq highlight-nonselected-windows nil)

  ;; More performant rapid scrolling over unfontified regions. May cause brief
  ;; spells of inaccurate fontification immediately after scrolling.
  (setq fast-but-imprecise-scrolling t)

  ;; Resizing the Emacs frame can be a terribly expensive part of changing the
  ;; font. By inhibiting this, we halve startup times, particularly when we use
  ;; fonts that are larger than the system default (which would resize the frame).
  (setq frame-inhibit-implied-resize t)

  ;; Remove command line options that aren't relevant to our current OS; means
  ;; slightly less to process at startup.
  (unless IS-MAC   (setq command-line-ns-option-alist nil))
  (unless IS-LINUX (setq command-line-x-option-alist nil))


  ;; ---------------------------------------------------------------------------
  ;; UI -- keep at the end after other performance tweaks have been applied
  ;; ---------------------------------------------------------------------------

  ;; Don't resize the frames in steps; it looks weird, especially in tiling window
  ;; managers, where it can leave unseemly gaps.
  (setq frame-resize-pixelwise t)

  ;; But resize windows pixelwise only on WSL, this can cause crashes in some
  ;; cases when resizing too many windows at once or rapidly.
  (setq window-resize-pixelwise t)

  ;; GUIs are inconsistent across systems and themes (and will rarely match our
  ;; active Emacs theme). They impose inconsistent shortcut key paradigms too.
  ;; It's best to avoid them altogether and have Emacs handle the prompting.
  (setq use-dialog-box nil)
  (when (bound-and-true-p tooltip-mode)
    (tooltip-mode -1))
  (when IS-LINUX
    (setq x-gtk-use-system-tooltips nil))

  ;; Favor vertical splits over horizontal ones. Monitors are trending toward
  ;; wide, rather than tall.
  (setq split-width-threshold 160
        split-height-threshold nil)

  ;; The native border "consumes" a pixel of the fringe on righter-most splits,
  ;; `window-divider' does not. Available since Emacs 25.1.
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1)

  (when IS-MAC
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . dark)))


  ;; important for golden-ratio to better work
  (setq window-combination-resize t)

  ;; When emacs asks for "yes" or "no", let "y" or "n" suffice
  (if (boundp use-short-answers) ;; since EMACS28+
      (setq use-short-answers t)
    (fset 'yes-or-no-p 'y-or-n-p))


  ;; draw underline lower
  (setq x-underline-at-descent-line t)

  ;; don't let the cursor go into minibuffer prompt
  ;; Tip taken from Xah Lee: http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
  (setq minibuffer-prompt-properties
        '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setq ns-use-native-fullscreen t)

  ;; delete duplicates from commands history (ie. helm displays duplicates)
  (setq history-delete-duplicates t)

  ;; Disable tool, menu, and scrollbars. Doom is designed to be keyboard-centric,
  ;; so these are just clutter (the scrollbar also impacts performance). Whats
  ;; more, the menu bar exposes functionality that Doom doesn't endorse.
  ;;
  ;; I am intentionally not calling `menu-bar-mode', `tool-bar-mode', and
  ;; `scroll-bar-mode' because they do extra and unnecessary work that can be more
  ;; concisely and efficiently expressed with these six lines:
  (setq default-frame-alist
        '((menu-bar-lines . 0)
          (tool-bar-lines . 0)
          (horizontal-scroll-bars)
          (vertical-scroll-bars)))


  ;; start emacs maximized
  ;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
  ;; no blink
  (blink-cursor-mode 0)

  (if (fboundp 'fringe-mode) (fringe-mode '3))
  (window-divider-mode))

;; Package manager
(setq package-archives '(("nongnu"    . "https://elpa.nongnu.org/nongnu/") ;; org-contrib not resides here
                         ("gnu"       . "https://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")))

(package-initialize)
;; (package-refresh-contents)

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-expand-minimally t
      use-package-verbose init-file-debug
      ;; inject use-package hooks for easy customization of stock package
      ;; configuration
      use-package-inject-hooks t
      use-package-compute-statistics nil)

;; ---------------------------------------------------------------------------
;; evil - vim emulation
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)
  (evil-mode t)

  :config
  (with-eval-after-load 'magit
    (general-define-key
     :states 'normal
     :keymaps 'magit-mode-map
     "<escape>" nil))
  (progn
    ;;; Cursor customizations
    (defvar my:__evil-cursors '(("emacs" "SkyBlue2" box)
                                ("iedit" "firebrick1" box)
                                ("iedit-insert" "firebrick1" (bar . 2))
                                ("insert" "chartreuse3" (bar . 2))
                                ("motion" "plum3" box)
                                ("normal" "DarkGoldenrod2" box)
                                ("replace" "chocolate" (hbar . 2))
                                ("visual" "gray" (hollow . 2))))
    (cl-loop for (state color shape) in my:__evil-cursors
             do (set (intern (format "evil-%s-state-cursor" state))
                     (list color shape)))

    (evil-define-operator my:evil-join (beg end)
      "Join the selected lines.

Implementation taken from doom-emacs.
This advice improves on `evil-join' by removing comment delimiters when joining
commented lines, by using `fill-region-as-paragraph'.

From https://github.com/emacs-evil/evil/issues/606"
      :motion evil-line
      (let* ((count (count-lines beg end))
             (count (if (> count 1) (1- count) count))
             (fixup-mark (make-marker)))
        (dotimes (var count)
          (if (and (bolp) (eolp))
              (join-line 1)
            (let* ((end (line-beginning-position 3))
                   (fill-column (1+ (- end beg))))
              (set-marker fixup-mark (line-end-position))
              (fill-region-as-paragraph beg end nil t)
              (goto-char fixup-mark)
              (fixup-whitespace))))
        (set-marker fixup-mark nil)))

    ;; evil-want-Y-yank-to-eol must be set via customize to have an effect
    (customize-set-variable 'evil-want-Y-yank-to-eol t)
    (customize-set-variable 'evil-disable-insert-state-bindings t)
    ;; it's local buffer variable
    (customize-set-variable 'evil-symbol-word-search t)
    (setq-default evil-want-C-u-scroll t)

    (setq ;; customizations
     evil-visual-char 'inclusive
     evil-ex-search-vim-style-regexp t
     evil-ex-visual-char-range t
     evil-want-fine-undo nil
     evil-v$-excludes-newline nil
     evil-mode-line-format 'nil
     ;; more vim-like behavior
     evil-symbol-word-search t
     ;; Only do highlighting in selected window so that Emacs has less work
     ;; to do highlighting them all.
     evil-ex-interactive-search-highlight 'selected-window
     ;; It's infuriating that innocuous "beginning of line" or "end of line"
     ;; errors will abort macros, so suppress them:
     evil-kbd-macro-suppress-motion-error t)

    (setq evil-goto-definition-functions
          '(evil-goto-definition-xref
            evil-goto-definition-imenu
            evil-goto-definition-semantic
            evil-goto-definition-search))

    (evil-set-undo-system 'undo-redo)

    ;; define text-objects
    (defmacro my:define-text-object (key name start end)
      "Define a text object and a surround pair.
START and END are strings (not regular expressions) that define
the boundaries of the text object."
      `(progn
         (my:define-text-object-regexp ,key ,name
                                       ,(regexp-quote start)
                                       ,(regexp-quote end))
         (with-eval-after-load 'evil-surround
           (add-to-list 'evil-surround-pairs-alist
                        (cons (string-to-char ,key)
                              (if ,end
                                  (cons ,start ,end)
                                ,start))))))

    (defmacro my:define-text-object-regexp (key name start-regexp end-regexp)
      "Define a text object.
START-REGEXP and END-REGEXP are the boundaries of the text object."
      (let ((inner-name (make-symbol (concat "evil-inner-" name)))
            (outer-name (make-symbol (concat "evil-outer-" name))))
        `(progn
           (evil-define-text-object ,inner-name (count &optional beg end type)
             (evil-select-paren ,start-regexp ,end-regexp beg end type count nil))
           (evil-define-text-object ,outer-name (count &optional beg end type)
             (evil-select-paren ,start-regexp ,end-regexp beg end type count t))
           (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
           (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

    (my:define-text-object "$" "dollar" "$" "$")
    (my:define-text-object "*" "star" "*" "*")
    (my:define-text-object "8" "block-star" "/*" "*/")
    (my:define-text-object "|" "bar" "|" "|")
    (my:define-text-object "%" "percent" "%" "%")
    (my:define-text-object "/" "slash" "/" "/")
    (my:define-text-object "_" "underscore" "_" "_")
    (my:define-text-object "-" "hyphen" "-" "-")
    (my:define-text-object "~" "tilde" "~" "~")
    (my:define-text-object "=" "equal" "=" "=")
    (my:define-text-object "«" "double-angle-bracket" "«" "»")
    (my:define-text-object "｢" "corner-bracket" "｢" "｣")
    (my:define-text-object "‘" "single-quotation-mark" "‘" "’")
    (my:define-text-object "“" "double-quotation-mark" "“" "”")
    (evil-define-text-object evil-pasted (count &rest args)
      (list (save-excursion (evil-goto-mark ?\[) (point))
            (save-excursion (evil-goto-mark ?\]) (1+ (point)))))
    (define-key evil-inner-text-objects-map "P" 'evil-pasted)
    ;; define text-object for entire buffer
    (evil-define-text-object evil-inner-buffer (count &optional beg end type)
      (list (point-min) (point-max)))
    (define-key evil-inner-text-objects-map "g" 'evil-inner-buffer)

    (evil-define-key 'insert 'global [remap evil-complete-previous] 'hippie-expand)

    ;; advices
    ;; HACK '=' moves the cursor to the beginning of selection. Disable this,
    ;;      since it's more disruptive than helpful.
    (advice-add #'evil-indent :around
                (defun my:--evil-indent(orig-fn &rest args)
                  (save-excursion (apply orig-fn args))))

    ;; list of modes which have custom keybindings (other than evil-collection)
    (setq evil-normal-state-modes
          (append evil-normal-state-modes
                  '(occur-mode)))

    (setq evil-insert-state-modes
          (append evil-insert-state-modes
                  '(occur-edit-mode))))

  :bind (
         :map evil-motion-state-map
         ;; bind evil-jump-forward for GUI only.
         ("C-i" . 'evil-jump-forward)
         ("*" . 'evil-search-unbounded-word-forward)

         :map evil-normal-state-map
         ;; Make the current definition and/or comment visible.
         ("zf" . 'reposition-window)

         :map evil-window-map
         ;; make cursor keys work
         ("<left>" . 'evil-window-left)
         ("<right>" . 'evil-window-right)
         ("<up>" . 'evil-window-up)
         ("<down>" . 'evil-window-down)))

;; by default it's too intrusive and evilifies most of the known modes, I prefer to have control
(use-package evil-collection
  :after evil
  :init
  (evil-collection-init
   '(arc-mode
     bookmark
     calc
     cmake-mode
     compile
     consult
     diff-mode
     dired
     dired-sidebar
     doc-view
     docker
     embark
     ediff
     help
     helpful
     ibuffer
     log-view
     image
     image-dired
     image+
     imenu
     imenu-list
     log-edit
     log-view
     macrostep
     man
     (magit magit-repos magit-submodule)
     magit-section
     magit-todos
     org
     replace
     (package-menu package)
     (pdf pdf-view)
     proced
     (process-menu simple)
     profiler
     restclient
     (term term ansi-term multi-term)
     tar-mode
     vc-annotate
     vc-dir
     vc-git
     vdiff
     view
     vterm
     vundo
     xref
     (ztree ztree-diff ztree-dir))))

;; ---------------------------------------------------------------------------
;; Utils to setup key-bindings (compatible with evil mode)
(use-package general :ensure t
  :defer nil
  :config
  ;; !! ATENTION

  ;; !! Some packages (like evil-lion-mode) are using evil to define keybindings
  ;; !! like "gl", "gr" for their minor modes, which will have a higher
  ;; !! precedence over our defined keys ("g" by example in some modes like
  ;; !! process-list-mode, where "g is bound to 'revert-buffer). So, in case
  ;; !! keybindings are not working as expected, first check which minor modes
  ;; !! are active and search in their corresponding package source for
  ;; !! "evil-define-minor-mode-key".

  ;; (general-override-mode)
  (general-create-definer my:leader-def
                          :states '(normal visual insert emacs motion)
                          :keymaps 'override
                          :prefix "SPC"
                          :non-normal-prefix "M-SPC")
  (defmacro my:major-mode-leader-def (keymaps &rest body)
    `(general-define-key
      :states '(normal visual insert emacs)
      :keymaps ,keymaps
      :prefix ","
      :non-normal-prefix "M-m"
      ,@body)))


;; ---------------------------------------------------------------------------
;; posframe is used by packages like whichkey-posframe or vertico-posframe and needs some fine tunning
(use-package posframe
  :if (display-graphic-p)
  :config
  (defun my:posframe-poshandler-p0.5p1-to-f0.5f1--10 (info)
    "Redefine posframe-poshandler-frame-bottom-center alias to
reposition frames slightly above mode-line."
    (let ((offset 10))
      (cons (/ (- (plist-get info :parent-frame-width)
                  (plist-get info :posframe-width))
               2)
            (- (plist-get info :parent-frame-height)
               (plist-get info :posframe-height)
               (plist-get info :mode-line-height)
               (plist-get info :minibuffer-height)
               offset))))
  (defalias 'posframe-poshandler-frame-bottom-center #'my:posframe-poshandler-p0.5p1-to-f0.5f1--10))

;; ---------------------------------------------------------------------------
;; Which key
(use-package which-key
  :ensure nil
  :diminish which-key-mode
  :init
  ;; (which-key-mode)
  :config
  (which-key-setup-side-window-bottom)
  (setq which-key-special-keys nil
        which-key-use-C-h-for-paging t
        which-key-prevent-C-h-from-cycling t
        which-key-echo-keystrokes 0.02
        which-key-max-description-length 32
        which-key-sort-order 'which-key-key-order-alpha
        which-key-idle-delay echo-keystrokes
        which-key-idle-secondary-delay 0.01
        which-key-allow-evil-operators t)
  (push '((nil . "my:\\(.+\\)") .
          (nil . "\\1"))
        which-key-replacement-alist)
  (push '((nil . "evilem--*motion-\\(.+\\)") .
          (nil . "\\1"))
        which-key-replacement-alist))


;; helper function
(defun my:buffer-canonical-chars-size (&optional buffer start end)
  (with-current-buffer (or buffer (current-buffer))
    (let ((.start (or start (point-min)))
          (.end (or end (point-max)))
          (.width 0)
          (.height 0))
      (save-excursion
        (save-restriction
          (narrow-to-region .start .end)

          ;; compute width
          (goto-char (point-min))
          (setq .width (- (line-end-position) (line-beginning-position)))
          (while (= 0 (forward-line))
            (setq .width (max .width (- (line-end-position) (line-beginning-position)))))

          ;; compute height
          (setq .height (count-lines (point-min) (point-max)))))
      (cons .width .height))))

(defface my:common-posframe-border-color
  '((t (:inherit default :background "gray50")))
  "Face used for posframe controls border, like helm,
which-key, hydra posframes."
  :group 'common-posframe-customizations)

;; this for UI - displays the window in a separate frame
(use-package which-key-posframe
  :if (display-graphic-p)
  :after which-key
  :init
  (which-key-posframe-mode)

  :config
  (progn
    (defun my:which-key-posframe--show-buffer (act-popup-dim)
      "Show which-key buffer when popup type is posframe.
Argument ACT-POPUP-DIM includes the dimension, (height . width)
of the buffer text to be displayed in the popup"
      (when (posframe-workable-p)
        (let* ((.size (my:buffer-canonical-chars-size which-key--buffer))
               (.min-width (car .size))
               (.min-height (cdr .size))
               (frame (posframe-show which-key--buffer
                         :font which-key-posframe-font
                         :position (point)
                         :poshandler which-key-posframe-poshandler
                         :background-color (face-attribute 'which-key-posframe :background nil t)
                         :foreground-color (face-attribute 'which-key-posframe :foreground nil t)
                         :height (car act-popup-dim)
                         :min-height .min-height
                         :width (cdr act-popup-dim)
                         :min-width (+ 1 .min-width)
                         ;; :x-pixel-offset 30
                         ;; :y-pixel-offset 30
                         :left-fringe (frame-char-width)
                         :internal-border-width 1
                         :internal-border-color (face-attribute 'my:common-posframe-border-color :background)
                         :override-parameters which-key-posframe-parameters)))

          (set-face-background 'fringe nil frame))))


    (advice-add #'which-key-posframe--show-buffer :override #'my:which-key-posframe--show-buffer)

    (setq
     which-key-posframe-poshandler 'posframe-poshandler-frame-bottom-center
     which-key-posframe-border-width 1)))

;; ---------------------------------------------------------------------------
;; Completion
(use-package vertico
  :hook (after-init . vertico-mode)
  :init
  (defun my:completing-read-multiple-ad(args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add 'completing-read-multiple :filter-args
              #'my:completing-read-multiple-ad)
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t)

  (vertico-multiform-mode 1)

  (defun my:vertico--setup-minibuffer()
    (setq vertico-count
          (truncate (* 0.3 (frame-height))))
    (vertico-repeat-save))
  (add-hook 'minibuffer-setup-hook #'my:vertico--setup-minibuffer)

  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  (general-define-key
   :keymaps 'vertico-map
   "DEL" #'my:vertico-directory-delete-char
   [(shift backspace)] #'(lambda () (interactive) (delete-char (- 1)))
   [(shift delete)] #'(lambda () (interactive) (delete-char (- 1))))

  ;; These commands are problematic and automatically show the *Completions* buffer
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)

  ;; cloned from `vertico-directory-up', but instead it does re-search-backward for tramp
  ;; hop separators
  (defun my:vertico-directory-up (&optional n)
    "Delete N names before point."
    (interactive "p")
    (when (and (> (point) (minibuffer-prompt-end))
               (eq 'file (vertico--metadata-get 'category)))
      (let ((path (buffer-substring (minibuffer-prompt-end) (point))) found)
        (when (string-match-p "\\`~[^/]*/\\'" path)
          (delete-minibuffer-contents)
          (insert (expand-file-name path)))
        (dotimes (_ (or n 1) found)
          (save-excursion
            (let ((end (point)))
              (goto-char (1- end))
              (when (re-search-backward "[/\\:|]" (minibuffer-prompt-end) t)
                (delete-region (1+ (point)) end)
                (setq found t))))))))

  ;; cloned from `vertico-directory-delete-char' but looks at `?:' to call
  ;; `my:vertico-directory-up'
  (defun my:vertico-directory-delete-char (&optional n)
    "Delete N directories or chars before point."
    (interactive "p")
    (unless (and (or (eq (char-before) ?/)
                     (eq (char-before) ?:))
                 (my:vertico-directory-up n))
      (delete-char (- n))))

  (defun my:ffap-menu-ask-ad-suppress-completion-help(fn &rest args)
    (cl-letf ((#'minibuffer-completion-help #'ignore))
      (apply fn args)))
  (advice-add #'ffap-menu-ask :around
              #'my:ffap-menu-ask-ad-suppress-completion-help)

  (defun my:vertico-highlight-directory (file)
    "Highlight FILE if it ends with a slash."
    (if (string-suffix-p "/" file)
        (propertize file 'face 'marginalia-file-priv-dir)
      (file-name-nondirectory file)))

  (defvar my:completion-category-hl-func-overrides
    `((file . ,#'my:vertico-highlight-directory))
    "Alist mapping category to highlight functions.")

  (defun my:completion-category-hl-candidate (args)
    (when-let* ((fun (alist-get (vertico--metadata-get 'category)
                                my:completion-category-hl-func-overrides)))
      (setcar args (funcall fun (car args))))
    args)

  (advice-add #'vertico--format-candidate
              :filter-args #'my:completion-category-hl-candidate))

(use-package vertico-posframe
  :if (display-graphic-p)
  :after (vertico)
  :hook (vertico-mode . vertico-posframe-mode)

  :config
  (setq vertico-posframe-parameters '((left-fringe . 2)
                                      (right-fringe . 2)
                                      (border-width . 1)))

  ;; rebind the "M-p" to previous history element
  (general-define-key
   :keymaps 'vertico-multiform-map
   "M-p" #'previous-history-element)

  (defcustom my:vertico-posframe-top-offset 80
    "Top offset of vertico-posframe, relative to parent frame in chars.")

  (defvar my:vertico-posframe--current-number-of-lines 0)

  (defun my:vertico-posframe-poshandler-frame-default (info)
    (cons (/ (- (plist-get info :parent-frame-width)
                (plist-get info :posframe-width))
             2)
          my:vertico-posframe-top-offset))

  (defun my:vertico-posframe-sizehandler-frame-default (_buffer)
    "The default functon used by `vertico-posframe-size-function'."
    (let ((width (truncate (* 0.7 (frame-width))))
          (height (+ 1 (max 2 my:vertico-posframe--current-number-of-lines))))
      (list :height height :width width :min-height height :min-width width)))

  (defun my:vertico-posframe-poshandler-frame-search (info)
    (cons 0
          (- (plist-get info :parent-frame-height)
             (plist-get info :posframe-height)
             (plist-get info :minibuffer-height))))

  (defun my:vertico-posframe-sizehandler-frame-search (_buffer)
    "The default functon used by `vertico-posframe-size-function'."
    (let ((width (frame-width))
          (height vertico-count))
      (list :height height :width width :min-height height :min-width width)))

  (defvar my:vertico-posframe-pos/sizehandler-default
    '(my:vertico-posframe-poshandler-frame-default
      my:vertico-posframe-sizehandler-frame-default))

  (defvar my:vertico-posframe-pos/sizehandler-search
    (list 'my:vertico-posframe-poshandler-frame-search
          'my:vertico-posframe-sizehandler-frame-search))

  (defvar my:posframe-poshandler--handlers-mapping
    ;; evaluate value directly in assoc list otherwise `sumbol-value' should be used which
    ;; adds overhead
    `((consult-grep . ,my:vertico-posframe-pos/sizehandler-search)
      (consult-location . ,my:vertico-posframe-pos/sizehandler-search)))

  (defun my:posframe-poshandler--get-handlers()
    (let ((category (vertico--metadata-get 'category)))
      (or (when (and (< 1 (minibuffer-depth))
                     (not category))
            ;; When showing history from `eval-expression' it opens another (recursive)
            ;; minibuffer and vertico category is null. I think the same scenario happens
            ;; for commands for which I would like to see the frame at the bottom. It's
            ;; more like a hacky variant, but I will leave like that for now and I will
            ;; look for another solution in case something will not work as expected.
            my:vertico-posframe-pos/sizehandler-search)
          (cdr (assq category my:posframe-poshandler--handlers-mapping))
          my:vertico-posframe-pos/sizehandler-default)))

  (defun my:vertico-posframe--display-candidates-ad(lines)
    (let ((handlers (my:posframe-poshandler--get-handlers)))
      (setq my:vertico-posframe--current-number-of-lines (length lines))
      (setq vertico-posframe-poshandler (car handlers))
      (setq vertico-posframe-size-function (cadr handlers))))
  (advice-add #'vertico--display-candidates :before #'my:vertico-posframe--display-candidates-ad)
  (defun my:vertico--resize-inhibit())
  (advice-add #'vertico--resize :override #'my:vertico--resize-inhibit))

(use-package orderless
  :init
  (defun my:vertico-orderless-dispatch (pattern _index _total)
    (cond
     ;; Ensure $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ;; Character folding
     ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
     ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
     ;; Initialism matching
     ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))

;;;###autoload
  (defun my:vertico-basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))

;;;###autoload
  (defun my:vertico-basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))

  (add-to-list
   'completion-styles-alist
   '(my:vertico-basic-remote my:vertico-basic-remote-try-completion
     my:vertico-basic-remote-all-completions
     "Use basic completion on remote files only"))

  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        ;; note that despite override in the name orderless can still be used in
        ;; find-file etc.
        completion-category-overrides '((file (styles my:vertico-basic-remote
                                                      orderless partial-completion)))
        orderless-style-dispatchers '(my:vertico-orderless-dispatch)
        orderless-component-separator "[ &]")
  ;; ...otherwise find-file gets different highlighting than other commands
  (set-face-attribute 'completions-first-difference nil :inherit nil)

  :config
  ;; https://github.com/minad/corfu#auto-completion
  (defun my:orderless-fast-dispatch (word index total)
    (and (= index 0) (= total 1) (length< word 4)
         `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(my:orderless-fast-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp)))
  (setq completion-styles '(orderless partial-completion basic)))


(use-package consult
  :commands (consult--directory-prompt
             consult--read)
  :init
  (general-define-key
   [remap apropos]                       #'consult-apropos
   [remap bookmark-jump]                 #'consult-bookmark
   [remap evil-show-marks]               #'consult-mark
   [remap evil-show-registers]           #'consult-register
   [remap goto-line]                     #'consult-goto-line
   [remap imenu]                         #'consult-imenu
   [remap locate]                        #'consult-locate
   [remap load-theme]                    #'consult-theme
   [remap man]                           #'consult-man
   [remap recentf-open-files]            #'consult-recent-file
   [remap switch-to-buffer]              #'consult-buffer
   [remap switch-to-buffer-other-window] #'consult-buffer-other-window
   [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
   [remap yank-pop]                      #'consult-yank-pop)
  (advice-add #'multi-occur :override #'consult-multi-occur)

  (general-define-key
   :keymaps 'minibuffer-local-map
   "C-c C-h" #'consult-history
   "C-M-<up>" #'(lambda () (interactive) (scroll-other-window-down 1))
   "C-M-<down>" #'(lambda () (interactive) (scroll-other-window 1))
   "C-M-<prior>" #'scroll-other-window-down
   "C-M-<next>" #'scroll-other-window)

  (general-define-key
   :keymaps 'vertico-map
   "<next>" #'vertico-scroll-up
   "<prior>" #'vertico-scroll-down)

  :config
  (defun my:consult-recentf-activate-recentf-mode(&rest _args)
    "`consult-recent-file' needs to have `recentf-mode' on to work correctly"
    (recentf-mode +1))
  (advice-add #'consult-recent-file :before
              #'my:consult-recentf-activate-recentf-mode)

  (defun my:consult--regexp-compiler (input type ignore-case)
    "Compile the INPUT string to a list of regular expressions.
The function should return a pair, the list of regular expressions and a
highlight function.  The highlight function should take a single
argument, the string to highlight given the INPUT.  TYPE is the desired
type of regular expression, which can be `basic', `extended', `emacs' or
`pcre'.  If IGNORE-CASE is non-nil return a highlight function which
matches case insensitively."
    (setq input (consult--split-escaped input))
    (cons (mapcar (lambda (x) (my:consult--convert-pcre-regexp x type)) input)
          (apply-partially #'consult--highlight-regexps
                           (mapcar (lambda (x) (my:consult--convert-pcre-regexp x 'emacs)) input)
                           ignore-case)))

  (setq consult--regexp-compiler #'my:consult--regexp-compiler
        consult-async-input-debounce 0.1
        consult-async-input-throttle 0.2
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-line-numbers-widen t
        consult-narrow-key "<"
        consult-preview-key 'any
        consult-project-root-function #'projectile-project-root)

  ;; display xrefs functions with consult (and vertico)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (consult-customize
   consult-source-project-recent-file
   consult-source-bookmark
   consult-source-recent-file
   consult-bookmark
   consult-git-grep
   consult-grep
   consult-recent-file
   consult-ripgrep
   consult-line
   :preview-key "C-SPC")

  (consult-customize
   consult-theme
   :preview-key (list "C-SPC" :debounce 0.5 'any))

  (defvar my:--vertico--consult-org-source
    (list :name     "Org Buffer"
          :category 'buffer
          :narrow   ?o
          :hidden   t
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :new
          (lambda (name)
            (with-current-buffer (get-buffer-create name)
              (insert "#+title: " name "\n\n")
              (org-mode)
              (consult--buffer-action (current-buffer))))
          :items
          (lambda ()
            (mapcar #'buffer-name
                    (if (featurep 'org)
                        (org-buffer-list)
                      (seq-filter
                       (lambda (x)
                         (eq (buffer-local-value 'major-mode x) 'org-mode))
                       (buffer-list))))))))

(use-package consult-dir
  :bind (([remap list-directory] . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))

  :config
  (defun my:vertico--consult-dir-docker-hosts ()
    "Get a list of hosts from docker."
    (cl-loop for line in
             (ignore-errors
               (apply #'process-lines
                      tramp-docker-program (list "ps" "--format" "{{.Names}}")))
             collect (concat "/docker:" line ":/")))

  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-local t))


(use-package consult-flycheck
  :after (consult flycheck))


(use-package consult-projectile
  :after (consult projectile)
  :config
  (require 'i-consult-projectile-ext))

(use-package embark
  :defer t
  :after (vertico)
  :init
;;;###autoload
  (defun my:embark-export-write ()
    "Export the current vertico results to a writable buffer if possible.

Supports exporting consult-grep to wgrep, file to wdeired, and consult-location to occur-edit"
    (interactive)
    (require 'embark)
    (require 'wgrep)
    (let* ((edit-command
            (pcase-let ((`(,type . ,candidates)
                         (run-hook-with-args-until-success 'embark-candidate-collectors)))
              (pcase type
                ('consult-grep #'wgrep-change-to-wgrep-mode)
                ('file #'wdired-change-to-wdired-mode)
                ('consult-location #'occur-edit-mode)
                (x (user-error "embark category %S doesn't support writable export" x)))))
           (embark-after-export-hook `(,@embark-after-export-hook ,edit-command)))
      (embark-export)))

  (setq which-key-use-C-h-commands nil
        prefix-help-command #'embark-prefix-help-command)

  (general-define-key
   :keymaps 'minibuffer-local-map
   "C-;"     #'embark-act
   "C-s"     #'embark-select
   "C-c C-;" #'embark-export
   "C-c C-l" #'embark-collect
   "C-c C-e" #'my:embark-export-write
   ;; (:leader
   ;;  :desc "Actions" "a" #'embark-act)
   )

  :config
  (require 'consult)

  ;; (set-popup-rule! "^\\*Embark Export:" :size 0.35 :ttl 0 :quit nil)

  ;; consult-projectile-find-file use multi-category to display sources with project file
  ;; candidates, there we use custom items to display folder after file name, like in
  ;; VSCode. In that case we need also to transform, by example, the candidate to its full
  ;; path before invoking embark-act (see value if `embark-transformer-alist'). With the
  ;; original `embark--refine-multi-category' the type and target is not fully resolved.
  (defun my:embark--refine-multi-category (_type target)
    "Compared to original function, this recursively descends to
refine resulted category from multi-category"
    (if-let* ((type:target (get-text-property 0 'multi-category target)))
        (if-let* ((transform (alist-get (car type:target) embark-transformer-alist)))
            (let ((trans (funcall transform (car type:target) target)))
              (cons (car trans) (cdr trans)))
          type:target)
      (cons 'general target)))
  (advice-add #'embark--refine-multi-category :override #'my:embark--refine-multi-category)

  (defun my:embark-consult-export-grep-ad(origFun lines)
    (funcall origFun (append '("" "") lines)))
  (advice-add #'embark-consult-export-grep :around #'my:embark-consult-export-grep-ad)

  (defun my:embark-which-key-prompt-a(fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))
  (advice-add #'embark-completing-read-prompter :around #'my:embark-which-key-prompt-a)

  (with-eval-after-load 'dirvish
    (defun my:embark-consult-export-dired-ad(origFun &rest files)
      ;; HACK: Prevent reusing existing dirvish buffer (check also original implementation).
      (let ((dirvish--sessions (make-hash-table)))
        (apply origFun files)
        ;; the icons are not displayed by `dirvish-override-dired-mode' (?)
        (nerd-icons-dired-mode))))
  (advice-add #'embark-export-dired :around #'my:embark-consult-export-dired-ad)

;;;###autoload
  (defun my:embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (cl-nsubstitute #'my:embark-which-key-indicator #'embark-mixed-indicator embark-indicators)

;;;###autoload
  (defun my:embark-magit-status (file)
    "Run `magit-status` on repo containing the embark target."
    (interactive "GFile: ")
    (magit-status (locate-dominating-file file ".git")))

  (general-define-key
   :keymaps 'embark-file-map
   "s" '("Open with sudo" . my:sudo-edit-file)))

(use-package nerd-icons-completion
  :after (vertico)
  :config
  (defconst my:nerd-icons-completion--icon-height 0.85)
  (defconst my:nerd-icons-completion--icon-v-adjust 0.075)

  (defun my:nerd-icons-completion-get-file-icon (cand)
    "Return the icon for the candidate CAND of completion category file."
    (cond ((eq t (compare-strings "/" 0 1 cand (1- (length cand)) (length cand))) ;; check if it ends with '/'
           (concat
            (nerd-icons-icon-for-dir
             cand
             :face 'nerd-icons-completion-dir-face
             :height my:nerd-icons-completion--icon-height
             :v-adjust my:nerd-icons-completion--icon-v-adjust)
            " "))
          (t (concat (nerd-icons-icon-for-file
                      cand
                      :height my:nerd-icons-completion--icon-height
                      :v-adjust my:nerd-icons-completion--icon-v-adjust)
                     " "))))

  (defun my:nerd-icons-completion-get-buffer-icon (cand)
    "Return the icon for the candidate CAND of completion category buffer."
    (let* ((mode (buffer-local-value 'major-mode (get-buffer cand)))
           (icon (nerd-icons-icon-for-mode
                  mode
                  :height my:nerd-icons-completion--icon-height
                  :v-adjust my:nerd-icons-completion--icon-v-adjust))
           (parent-icon (nerd-icons-icon-for-mode
                         (get mode 'derived-mode-parent)
                         :height my:nerd-icons-completion--icon-height
                         :v-adjust my:nerd-icons-completion--icon-v-adjust)))
      (concat
       (if (symbolp icon)
           (if (symbolp parent-icon)
               (nerd-icons-codicon "nf-cod-symbol_file")
             parent-icon)
         icon)
       " ")))

  (defun my:nerd-icons-completion-get-icon-ad (candidate category)
    "Return the icon for the candidate of completion category."
    (cl-case category
      (consult-projectile-project (concat (nerd-icons-octicon
                                           "nf-oct-file_directory"
                                           :face 'nerd-icons-completion-dir-face
                                           :height my:nerd-icons-completion--icon-height
                                           :v-adjust my:nerd-icons-completion--icon-v-adjust)
                                          " "))
      (consult-projectile-file (concat (nerd-icons-icon-for-file
                                        (get-text-property 0 :path candidate)
                                        :height my:nerd-icons-completion--icon-height
                                        :v-adjust my:nerd-icons-completion--icon-v-adjust)
                                       " "))
      (file (my:nerd-icons-completion-get-file-icon candidate))
      (recent-file (my:nerd-icons-completion-get-file-icon candidate))
      (project-file (my:nerd-icons-completion-get-file-icon candidate))
      (buffer (my:nerd-icons-completion-get-buffer-icon candidate))))

  (advice-add #'nerd-icons-completion-get-icon
              :before-until #'my:nerd-icons-completion-get-icon-ad))

(use-package marginalia
  :hook (vertico-mode . marginalia-mode)
  :init
  ;; (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)

  :config
  (advice-add #'marginalia--project-root :override #'projectile-project-root)

  (let ((categories '(;; '(doom/find-file-in-other-project . project-file)
                      ;; '(doom/describe-active-minor-mode . minor-mode)
                      '(flycheck-error-list-set-filter . builtin)
                      '(projectile-find-file . project-file)
                      '(projectile-recentf . project-file)
                      '(projectile-switch-to-buffer . buffer)
                      '(projectile-switch-project . project-file))))
    (dolist (result categories)
      (cl-pushnew result marginalia-command-categories :test #'equal)))

  (general-define-key
   :keymaps 'minibuffer-local-map
   "M-A" #'marginalia-cycle))

(use-package embark-consult
  :after (embark consult)
  :init
  (require 'embark-consult)
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))

;; ---------------------------------------------------------------------------
;;; key-bindings

;; instantly display current keystrokes in mini buffer
(setq echo-keystrokes 0.7)

(general-define-key
 :keymaps '(minibuffer-local-map
            minibuffer-local-ns-map
            minibuffer-local-completion-map
            minibuffer-local-must-match-map
            minibuffer-local-isearch-map)
 "<escape>" 'keyboard-escape-quit)

(my:leader-def
 "SPC" #'execute-extended-command)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; TODO move to a dedicated file
;; ---------------------------------------------------------------------------
;; LSP config
(use-package bind-map)
(use-package lsp-mode
  :commands (lsp lsp-deffered)
  :config
  (progn
    ;; try to disbale jerkines caused by lsp-mode
    ;; (setq lsp-ui-doc-enable nil ;; default is t, but causes freezes on jump to definition, very annoying
    ;;       lsp-lens-enable nil   ;; lens makes the editing and navigation slow
    ;;       lsp-use-plists t)

    ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
    (setq
     lsp-idle-delay 0.500
     ;; doc
     lsp-ui-doc-enable t ;; on hover documentation
     lsp-signature-doc-lines 5 ;; limit number of doc lines
     ;; auto completion
     lsp-completion-enable t
     lsp-completion-show-detail t
     lsp-completion-show-kind t
     lsp-completion-default-behaviour :insert
     lsp-enable-indentation nil
     ;; lens
     lsp-lens-enable t
     ;; headline
     lsp-headerline-breadcrumb-enable t
     ;; sideline
     lsp-ui-sideline-enable t     ;; enable sideline info
     lsp-ui-sideline-show-hover t ;;   on hover only
     lsp-ui-sideline-show-code-actions t
     lsp-ui-sideline-show-diagnostics t ;; showing diagnostics on sideline are better than in the status bar (it's disturbing)
     lsp-ui-sideline-show-symbol nil
     ;; modeline
     lsp-modeline-code-actions-enable t ;; show code actions if available in modeline
     lsp-modeline-diagnostics-enable t
     lsp-eldoc-enable-hover t
     lsp-signature-auto-activate t)

    (setq lsp-clients-clangd-args '("--enable-config"
                                    "--compile-commands-dir=./build/"
                                    "--background-index"
                                    "--clang-tidy"
                                    "--completion-style=detailed"
                                    "--header-insertion=never"
                                    "--log=error"
                                    "--pch-storage=memory"
                                    "--malloc-trim"))

    (setq read-process-output-max (* 1024 1024)) ;; 1mb

    ;; Disable features that have great potential to be slow.
    (setq lsp-enable-folding nil
          lsp-enable-text-document-color nil)
    ;; Reduce unexpected modifications to code
    (setq lsp-enable-on-type-formatting nil)
    ;; try to fix error: 'lsp--on-idle': (error "The connected server(s) does not support method textDocument/documentHighlight.
    (setq lsp-enable-links nil)

    (defun my:lsp-setup-side-line-show-auto-toggle()
      (defvar-local my:lsp-ui-sideline-show-diagnostics-previous nil)
      (defun my:lsp-sideline-toggle-show-diagnostics(enable)
        (if enable
            (progn
              (setq-local lsp-ui-sideline-show-diagnostics my:lsp-ui-sideline-show-diagnostics-previous)
              (setq-local my:lsp-ui-sideline-show-diagnostics-previous nil)
              (lsp-ui-sideline--diagnostics-changed))
          ;; disable
          (setq-local my:lsp-ui-sideline-show-diagnostics-previous lsp-ui-sideline-show-diagnostics)
          (setq-local lsp-ui-sideline-show-diagnostics nil)
          (lsp-ui-sideline--delete-ov)))

      ;; inhibit diagnostic messages in insert mode
      (add-hook 'evil-insert-state-entry-hook
                (lambda() (my:lsp-sideline-toggle-show-diagnostics nil)) nil t)
      (add-hook 'evil-insert-state-exit-hook
                (lambda() (my:lsp-sideline-toggle-show-diagnostics t)) nil t)

      ;; and disable flycheck display error function all together, otherwise it will be used
      ;; to show diagnostic messages in the status line, which might be disturbing because
      ;; of its sporadic height changes
      (setq-local flycheck-display-errors-function nil))

    (defun lsp-booster--advice-json-parse (old-fn &rest args)
      "Try to parse bytecode instead of json."
      (or
       (when (equal (following-char) ?#)
         (let ((bytecode (read (current-buffer))))
           (when (byte-code-function-p bytecode)
             (funcall bytecode))))
       (apply old-fn args)))
    (advice-add (if (progn (require 'json)
                           (fboundp 'json-parse-buffer))
                    'json-parse-buffer
                  'json-read)
                :around
                #'lsp-booster--advice-json-parse)

    (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
      "Prepend emacs-lsp-booster command to lsp CMD."
      (let ((orig-result (funcall old-fn cmd test?)))
        (if (and (not test?)                             ;; for check lsp-server-present?
                 (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
                 lsp-use-plists
                 (not (functionp 'json-rpc-connection))  ;; native json-rpc
                 (executable-find "emacs-lsp-booster"))
            (progn
              (when-let* ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
                (setcar orig-result command-from-exec-path))
              (message "Using emacs-lsp-booster for %s!" orig-result)
              (cons "emacs-lsp-booster" orig-result))
          orig-result)))
    (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

    ;; bind lsp keys to major mode leader
    (bind-map lsp-command-map
              :minor-modes (lsp-mode)
              :keys ("M-m m")
              :evil-keys ("SPC m" ",")
              :evil-states (normal motion visual evilified))

    ;; update lsp key rebindings to which key
    (dolist (it '(("=" . "format")
                  ("F" . "folder")
                  ("T" . "toggle")
                  ("g" . "goto")
                  ("h" . "help")
                  ("r" . "refactor")
                  ("w" . "workspace")
                  ("a" . "actions")
                  ("G" . "peek")))
      (which-key-add-keymap-based-replacements lsp-command-map (car it) (cdr it)))))


(use-package lsp-ui
  :commands (lsp-ui-mode
             lsp-ui-peek-find-implementation
             lsp-ui-peek-find-definitions
             lsp-ui-peek-find-references
             lsp-ui-peek-find-workspace-symbol
             lsp-ui-peek-jump-backward
             lsp-ui-peek-jump-forward)

  :config
  (require 'lsp-ui-doc)
  (progn
    ;; (require 'xref)
    ;; (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    ;; (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
    )

  (setq lsp-ui-sideline-show-hover nil ;; disable inline docu
        lsp-ui-doc-enable nil ;; disable ui doc by default, can be enabled with ",Td"
        lsp-ui-doc-position 'top
        lsp-ui-doc-alignment 'window)

  (general-def
    :keymaps 'lsp-ui-peek-mode-map
    "h" #'lsp-ui-peek--select-prev-file
    "j" #'lsp-ui-peek--select-next
    "k" #'lsp-ui-peek--select-prev
    "l" #'lsp-ui-peek--select-next-file
    "<tab>" #'lsp-ui-peek--toggle-file))

(use-package lsp-treemacs)

;; ---------------------------------------------------------------------------
;; Rust
(use-package rust-mode
  :init
  ;;(setq rust-mode-treesitter-derive t)

  :config
  (setq rust-indent-method-chain t))

(use-package rustic
  :init
  ;; HACK Certainly, `rustic-babel' does this, but the package (and many other
  ;;   rustic packages) must be loaded in order for them to take effect. To lazy
  ;;   load it all, we must do it early:
  (with-eval-after-load 'org-src
    (defalias 'org-babel-execute:rust #'org-babel-execute:rustic)
    (add-to-list 'org-src-lang-modes '("rust" . rustic)))

  :config
  (require 'lsp-mode)
  (cl-callf2 rassq-delete-all 'rust-mode auto-mode-alist)
  (cl-callf2 rassq-delete-all 'rust-ts-mode-maybe auto-mode-alist)
  ;; (cl-callf2 rassq-delete-all 'rust-ts-mode treesit-major-mode-remap-alist)

  (with-eval-after-load 'rustic-lsp
    (remove-hook 'rustic-mode-hook 'rustic-setup-lsp))
  (with-eval-after-load 'rustic-flycheck
    (remove-hook 'rustic-mode-hook #'flycheck-mode)
    (remove-hook 'rustic-mode-hook #'flymake-mode-off)
    (remove-hook 'flycheck-mode-hook #'rustic-flycheck-setup))

  (setq rustic-lsp-client 'lsp-mode)
  ;; (setq rustic-babel-format-src-block nil
  ;;       rustic-format-trigger nil)

  (defun my:rustic-mode-hook()
    (rustic-setup-lsp)
    (flycheck-mode 1)
    (flymake-mode-off)
    (rustic-flycheck-setup)
    (rainbow-delimiters-mode t))
  (add-hook 'rustic-mode-hook #'my:rustic-mode-hook)

  (with-eval-after-load 'lsp
    ;; hack: fix for signatures on hover on LSP mode (see emacs-lsp/lsp-mode#1740)
    (defun my:rust--dont-cache-results-from-ra-a(&rest _)
      (when (derived-mode-p 'rust-mode 'rust-ts-mode)
        (setq lsp--hover-saved-bounds nil)))
    (advice-add #'lsp-eldoc-function :after #'my:rust--dont-cache-results-from-ra-a)

    ;; temporary hack: Extract and show short signature for rust-analyzer.
    (cl-defmethod lsp-clients-extract-signature-on-hover (contents (_server-id (eql rust-analyzer)))
          (let* ((value (if lsp-use-plists (plist-get contents :value) (gethash "value" contents)))
                 (groups (--partition-by (s-blank? it) (s-lines (s-trim value))))
                 (mod-group (cond ((s-equals? "```rust" (car (-fifth-item groups))) (-third-item groups))
                                  ((s-equals? "```rust" (car (-third-item groups))) (-first-item groups))
                                  (t nil)))
                 (cmt (if (null mod-group) "" (concat " // " (cadr mod-group))))
                 (sig-group (cond ((s-equals? "```rust" (car (-fifth-item groups))) (-fifth-item groups))
                                  ((s-equals? "```rust" (car (-third-item groups))) (-third-item groups))
                                  (t (-first-item groups))))
                 (sig (->> sig-group
                           (--drop-while (s-equals? "```rust" it))
                           (--take-while (not (s-equals? "```" it)))
                           (--map (s-replace-regexp "//.*" "" it))
                           (--map (s-trim it))
                           (s-join " "))))
            (lsp--render-element (concat "```rust\n" sig cmt "\n```")))))

  ;; cannot override the keymap with general, other than replacing the keybinding in lsp-command-map
  (bind-key "==" #'rustic-format-dwim lsp-command-map)
  (bind-key "=r" #'rustic-format-region lsp-command-map)
  (my:major-mode-leader-def '(rustic-mode-map)
                            "c" '(:ignore t :which-key "cargo")
                            "ca" '((lambda() (interactive) (rustic-run-cargo-command "cargo audit")) :which-key "cargo audit")
                            "cb" '(rustic-cargo-build :which-key "cargo build")
                            "cB" '(rustic-cargo-bench :which-key "cargo bench")
                            "cc" '(rustic-cargo-check :which-key "cargo check")
                            "cC" '(rustic-cargo-clippy :which-key "cargo clippy")
                            "cd" '(rustic-cargo-build-doc :which-key "cargo doc")
                            "cD" '(rustic-cargo-doc :which-key "cargo doc --open")
                            "cf" '(rustic-cargo-clippy-fix :which-key "cargo fmt")
                            "cn" '(rustic-cargo-new :which-key "cargo new")
                            "co" '(rustic-cargo-outdated :which-key "cargo outdated")
                            "cr" '(rustic-cargo-run :which-key "cargo run")

                            "t" '(:ignore t :which-key "cargo test")
                            "ta" '(rustic-cargo-test :which-key "all tests")
                            "tt" '(rustic-cargo-current-test :which-key "current test")))
