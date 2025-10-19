;; HACK Work around native compilation on macOS failing with 'ld: library not
;; found for -lemutls_w'.
;; https://github.com/d12frosted/homebrew-emacs-plus/issues/554 and https://github.com/d12frosted/homebrew-emacs-plus/issues/323
;;(setenv "LIBRARY_PATH" "/opt/homebrew/opt/gcc/lib/gcc/14:/opt/homebrew/opt/libgccjit/lib/gcc/current:/opt/homebrew/opt/gcc/lib/gcc/14/gcc/aarch64-apple-darwin24/14")

;;; early-init.el --- Early Emacs initialization -*- lexical-binding: t -*-
(defvar my/emacs-load-start-time (current-time))
(message "Early init loaded!")

;; due to issues with native comp not finding libgccjit
(setenv "PATH" (concat "/opt/homebrew/opt/gcc/bin:" (getenv "PATH")))
(setq exec-path (cons "/opt/homebrew/opt/gcc/bin" exec-path))

(setenv "LIBRARY_PATH" 
  (string-join 
    '("/opt/homebrew/opt/gcc/lib/gcc/15"
      "/opt/homebrew/opt/libgccjit/lib/gcc/15"
      "/opt/homebrew/opt/gcc/lib/gcc/current")
    ":"))

;; Setting ‘native-comp’ speed and flags. They are important for getting the performance.
;; https://github.com/D4lj337/Emacs-performance
(setq native-comp-speed 3)
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-deferred-compilation t)
(native-compile-async "/opt/homebrew/Cellar/emacs-plus@30/30.2/Emacs.app/Contents/native-lisp/" 'recursively)

;; set this early for lsp to use plists instead of hash tables
;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
(setenv "LSP_USE_PLISTS" "true")

(setq package-enable-at-startup nil)

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize 'force
      frame-title-format '("%b")
      ring-bell-function 'ignore
      use-dialog-box t ; only for mouse events, which I seldom use
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t)

;; Increase GC threshold during startup for faster loading
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Restore GC threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216  ; 16MB 
                  gc-cons-percentage 0.1)))

;; Improve performance with language servers.
(setq read-process-output-max (* 1024 1024)) ;; 1 MB

(load "server")
(unless (server-running-p) (server-start))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
            (message "Emacs loaded in %.2f seconds with %d garbage collections"
                     (float-time (time-subtract after-init-time 
                                                my/emacs-load-start-time))
                     gcs-done)
            (message "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")))
