;;; early-init.el --- Early initialization  -*- lexical-binding: t; -*-
;;; Commentary:
;; Runs before init.el. Used to suppress UI elements before the first
;; frame is drawn and to speed up startup by deferring garbage collection.
;;; Code:

;; Raise GC threshold during startup to avoid collection pauses.
;; Reset to a reasonable value after init completes.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 128 1024 1024)))) ;; 128 MB

;; Suppress UI chrome before the first frame is created -- avoids flash.
(setq default-frame-alist
      '((tool-bar-lines . 0)
        (menu-bar-lines . 0)
        (vertical-scroll-bars . nil)))

;;; early-init.el ends here
