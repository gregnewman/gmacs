;; HACK Work around native compilation on macOS failing with 'ld: library not
;; found for -lemutls_w'.
;; https://github.com/d12frosted/homebrew-emacs-plus/issues/554 and https://github.com/d12frosted/homebrew-emacs-plus/issues/323
;;(setenv "LIBRARY_PATH" "/opt/homebrew/opt/gcc/lib/gcc/14:/opt/homebrew/opt/libgccjit/lib/gcc/current:/opt/homebrew/opt/gcc/lib/gcc/14/gcc/aarch64-apple-darwin24/14")

;;; early-init.el --- Early Emacs initialization -*- lexical-binding: t -*-

(message "Early init loaded!")

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
            (setq gc-cons-threshold 16777216  ; 16mb
                  gc-cons-percentage 0.1)))

(load "server")
(unless (server-running-p) (server-start))
