;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum

      ;; Premature redisplays can substantially affect startup times and produce
      ;; ugly flashes of unstyled Emacs.
      ;; inhibit-redisplay t
      ;; inhibit-message t
      ;; Display the bare minimum at startup. We don't need all that noise. The
      ;; dashboard/empty scratch buffer is good enough.
      ;; inhibit-startup-message t
      ;; inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      ;; inhibit-startup-screen t

      ;; Prioritize old byte-compiled source files over newer sources. It saves us a
      ;; little IO time to skip all the mtime checks on each lookup.
      load-prefer-newer nil


      package-enable-at-startup nil ; tells emacs not to load any packages before starting up
      package-native-compile t      ; native compile packages at installation

      default-frame-alist '((menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            (horizontal-scroll-bars)
                            (vertical-scroll-bars))

      ;; And set these to nil so users don't have to toggle the modes twice to
      ;; reactivate them.
      menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil

      ;; https://hub.docker.com/r/andreacorallo/emacs-nativecomp
      ;; defer package native compilation
      native-comp-async-jobs-number (num-processors)
      native-comp-speed 3
      native-comp-compiler-options "-O3 -fgraphite -floop-interchange -floop-nest-optimize -pipe -fipa-pta -fallow-store-data-races -fno-semantic-interposition"
      native-comp-async-query-on-exit t
      native-comp-async-report-warnings-errors 'silent
      native-comp-deferred-compilation-deny-list '("\\(?:[^z-a]*-autoloads\\.el$\\)"))

(setq file-name-handler-alist-backup (copy-alist file-name-handler-alist))
(setq file-name-handler-alist nil)

(add-hook
 'emacs-startup-hook
 (defun startup-hook ()
   (setq-default inhibit-redisplay nil
                 inhibit-message nil
                 load-prefer-newer t)
   (message "Emacs ready in %s with %.2f seconds spent for %d garbage collections."
            (format "%.2f seconds"
                    (float-time
                     (time-subtract after-init-time before-init-time)))
            gc-elapsed
            gcs-done)

   (setq gc-cons-threshold 134217728) ;; 128MB

   (setq file-name-handler-alist
         ;; Merge instead of overwrite because there may have bene changes to
         ;; `file-name-handler-alist' since startup we want to preserve.
         (delete-dups (append file-name-handler-alist
                              file-name-handler-alist-backup))))
 100)
