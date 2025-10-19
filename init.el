;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename:      $HOME/.emacs.d/init.el
;; Time-stamp:    <2020-01-31 23:21:56 vk>
;; Source:        https://github.com/gregnewman/gmacs
;; Purpose:       configuration file for Emacs
;; Authors:       Greg Newman
;; License:       This file is licensed under the GPL v2.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ================================================================================================
;; For a brief description of my configuration, please take a look at gmacs.org in this directory!
;; ================================================================================================

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(defvar my-init-el-start-time (current-time) "Time when init.el was started")
(setq my-user-emacs-directory "~/.emacs.d/")

;; Boostrap Straight for package management
(setq straight-repository-branch "develop") ;; temporary work around for errors compiling https://github.com/radian-software/straight.el/pull/1054
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'org)

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Install use-package
(straight-use-package 'use-package)

;; =======================================================================================
;; The init.el file looks for "gmacs.org" and tangles its elisp blocks (matching
;; the criteria described below) to "gmacs.el" which is loaded as Emacs configuration.
;; Inspired and copied from: https://github.com/novoid/dot-emacs which in turn was
;; Inspired and copied from: http://www.holgerschurig.de/en/emacs-init-tangle/
;; =======================================================================================

;; from: http://stackoverflow.com/questions/251908/how-can-i-insert-current-date-and-time-into-a-file-using-emacs
(defvar current-date-time-format "%a %b %d %Y-%m-%dT%H:%M:%S "
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

;; from: http://stackoverflow.com/questions/251908/how-can-i-insert-current-date-and-time-into-a-file-using-emacs
(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun my-tangle-gmacs.org ()
  "This function will write all source blocks from =gmacs.org= into =gmacs.el= that are ...

- not marked as =tangle: no=
- doesn't have the TODO state =DISABLED=
- have a source-code of =emacs-lisp="
  (require 'org)
  (let* ((body-list ())
         (output-file (concat my-user-emacs-directory "gmacs.el"))
         (org-babel-default-header-args (org-babel-merge-params org-babel-default-header-args
                                                                (list (cons :tangle output-file)))))
    (message "—————• Re-generating %s …" output-file)
    (save-restriction
      (save-excursion
        (org-babel-map-src-blocks (concat my-user-emacs-directory "gmacs.org")
	  (let* (
		 (org_block_info (org-babel-get-src-block-info 'light))
		 ;;(block_name (nth 4 org_block_info))
		 (tfile (cdr (assq :tangle (nth 2 org_block_info))))
		 (match_for_TODO_keyword)
		 )
	    (save-excursion
	      (catch 'exit
		;;(when (string= "" block_name)
		;;  (message "Going to write block name: " block_name)
		;;  (add-to-list 'body-list (concat "message(\"" block_name "\")"));; adding a debug statement for named blocks
		;;  )
		(org-back-to-heading t)
		(when (looking-at org-outline-regexp)
		  (goto-char (1- (match-end 0))))
		(when (looking-at (concat " +" org-todo-regexp "\\( +\\|[ \t]*$\\)"))
		  (setq match_for_TODO_keyword (match-string 1)))))
	    (unless (or (string= "no" tfile)
	                (string= "DISABLED" match_for_TODO_keyword)
	                (not (string= "emacs-lisp" lang)))
	      (add-to-list 'body-list (concat "\n\n;; #####################################################################################\n"
		                              "(message \"config • " (org-get-heading) " …\")\n\n")
		           )
	      (add-to-list 'body-list body)
	      ))))
      (with-temp-file output-file
        (insert ";; ============================================================\n")
        (insert ";; Don't edit this file, edit gmacs.org' instead ...\n")
        (insert ";; Auto-generated at " (format-time-string current-date-time-format (current-time)) " on host " system-name "\n")
        (insert ";; ============================================================\n\n")
        (insert (apply 'concat (reverse body-list))))
      (message "—————• Wrote %s" output-file))))


;; following lines are executed only when my-tangle-gmacs.org-hook-func()
;; was not invoked when saving gmacs.org which is the normal case:
(let ((orgfile (concat my-user-emacs-directory "gmacs.org"))
      (elfile (concat my-user-emacs-directory "gmacs.el"))
      (gc-cons-threshold most-positive-fixnum))
  (when (or (not (file-exists-p elfile))
            (file-newer-than-file-p orgfile elfile))
    (my-tangle-gmacs.org)
    ;;(save-buffers-kill-emacs);; TEST: kill Emacs when config has been re-generated due to many issues when loading newly generated gmacs.el
    )
  (load-file elfile))

;; when gmacs.org is saved, re-generate gmacs.el:
(defun my-tangle-gmacs-org-hook-func ()
  (when (string= "gmacs.org" (buffer-name))
	(let ((orgfile (concat my-user-emacs-directory "gmacs.org"))
		  (elfile (concat my-user-emacs-directory "gmacs.el")))
	  (my-tangle-gmacs.org))))
(add-hook 'after-save-hook 'my-tangle-gmacs-org-hook-func)

(message "→★ loading init.el in %.2fs" (float-time (time-subtract (current-time) my-init-el-start-time)))
