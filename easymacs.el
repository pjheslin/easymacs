;;; Easymacs
;;
;; easymacs.el is a configuration for Emacs which is designed to be a learning environment for the digital humanities.  It provides an easy-to-install, cross-platform, comprehensive tool with key-bindings for basic editing tasks that should be familiar to non-technical users (some of these overwrite Emacs defaults).   It provides, for example, a schema-aware validating XML editor (nxml), a cross-platform command-line (eshell), integration with Git (magit) and a rich devopment environment for teaching programming in a variety of languages.
;; 
;; Author: Peter Heslin <p.j.heslin@dur.ac.uk>
;; Maintainer: Peter Heslin <p.j.heslin@dur.ac.uk>
;; 
;; Copyright (C) 2003-18 Peter Heslin
;; 
;; This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License along with GNU Emacs; see the file COPYING.  If not, write to the Free Software Foundation, 675 Massachusettes Ave, Cambridge, MA 02139, USA.

(defvar easymacs-version "3.1")
(unless (string-match "^24.[456789]\\|^2[56789]\\|^[3456789]" emacs-version)
  (error "This version of Emacs is too old to run Easymacs; aborting."))
(defvar easymacs-dir (file-name-directory
		      (or load-file-name
			  buffer-file-name)))
(add-to-list 'load-path easymacs-dir)


;;; Set-up for packages
(require 'package)
(setq package-enable-at-startup nil)
;; No TLS library for https on Windows
(add-to-list 'package-archives
	     (if (eq system-type 'windows-nt)
		 '("melpa" . "http://melpa.org/packages/")
	       '("melpa" . "https://melpa.org/packages/"))
	     t)
(package-initialize)
;; Set up use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
;; Needed by use-package
(use-package diminish :ensure t)
(use-package bind-key :ensure t)
;; Easymacs packages to load
(require 'fold-dwim)
(require 'tei-html-docs-p5)
;; Internal Emacs packages to load
(require 'misc)
(require 'hideshow)
(require 'outline)
(require 'thingatpt)

(require 'info)
(info-initialize)
(add-to-list 'Info-directory-list easymacs-dir)

;;; General Settings

(setq message-log-max 1000)
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(kill-buffer (get-buffer "*scratch*"))
(set-language-environment "UTF-8")

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)
;; Use dialog boxes, if available
(setq use-dialog-box t)
;; Put current line number and column in the mode line
(line-number-mode 1)
(setq column-number-mode t)
; For visual-line-mode
(setq line-number-display-limit-width 2000000)
;; Use menu-bar
(menu-bar-mode 1)
;; Paste at cursor, rather than pointer
(setq mouse-yank-at-point t)
;; save command history
(savehist-mode 1)
;; Save our session
(require 'saveplace)
(setq-default save-place t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq frame-title-format 
      '(buffer-file-name "%f" (dired-directory dired-directory "%b")))

;; Make very frequent autosaves
(setq auto-save-interval 5)
;; Make all backups in a single directory
(when (boundp 'backup-directory-alist)
  (let ((dir (expand-file-name "~/.emacs-backups")))
    (or (file-directory-p dir) (make-directory dir))
    (setq backup-directory-alist `(("." . ,dir)))))

;; Word wrapping
(use-package adaptive-wrap :ensure t)
(add-hook 'visual-line-mode-hook
	  (lambda ()
	    (adaptive-wrap-prefix-mode t)
	    (diminish 'visual-line-mode)))
(global-visual-line-mode)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))
;; Tabs are evil and break adaptive-wrap
(setq-default indent-tabs-mode nil)

;; CUA-mode
(cua-mode t)
(setq cua-enable-cursor-indications t)
(setq cua-normal-cursor-color '(bar . "black")
      cua-overwrite-cursor-color '(box . "blue")
      cua-read-only-cursor-color '(box . "red"))
;; Keep selection active after copy
(setq cua-keep-region-after-copy t)
(setq org-support-shift-select 'always)

;; The excellent old-fashioned-undo.el gives a simple, linear undo/redo facility; it uses the obsolete flet macro, so we need to load a workaround 
(load "subr-compat")
(load "dflet.el")
(require 'dflet)
(load "old-fashioned-undo.el")
(require 'old-fashioned-undo)
(old-fashioned-undo-mode t)
(diminish 'old-fashioned-undo-mode)
(global-set-key [(control z)] 'undo)
(global-set-key [(control Z)] 'redo)

;; oed-org.el gives access to the OED API.  I have edited it to add one of my own keys, as the free usage limits are generous. 
(use-package request :ensure t)
(load "oed-org-easymacs.el")
(require 'oed-org)

;; Enable recently-opened files menu
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode 1)
(setq recentf-max-menu-items 30)
(setq recentf-max-saved-items 500)
(setq recentf-exclude '("[.]recentf" "[.]bm-repository$"
			   "[.]bmk$" "[.]abbrev_defs"
			   "[.]elc$" "ido.last" "autoloads.el"
			   "easymacs-help.txt"))
;; Save list when used, in case of crashes
(defadvice recentf-open-files (after easymacs-recentf-advice activate)
  (recentf-save-list))

;; Enable font-lock (syntax highlighting) in modes which support it
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; show matching and mismatching brackets etc
(setq show-paren-delay 0)
(show-paren-mode t)

;; Completion
(setq dabbrev-check-all-buffers t)
(use-package company
  :ensure t
  :diminish company-mode
  :config (progn
	    (global-company-mode)
	    (setq company-idle-delay nil))
  :bind* (("<f3>" . company-complete)
	  :map company-active-map
	  ("<escape>" . company-abort)))

;; Ido and ibuffer for buffer switching
(ido-mode 'buffer)
(setq ido-use-virtual-buffers t)
;; Ignore non-user files in ido
(defun easymacs-ido-ignore (name)
  "Ignore all non-user (a.k.a. *starred*) buffers."
  (string-match "^\*" name))
(setq ido-ignore-buffers '("\\` " easymacs-ido-ignore))

(require 'ibuffer)
(require 'ibuf-ext)
;; Simplified ibuffer display
(setq ibuffer-formats
      '((mark modified read-only " "
	      (name 0 -1 :left :elide))))
;; Make ibuffer refresh after every command
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))

;; imenu
(defun imenu-or-not ()
  "Try to add an imenu when we visit a file, catch and nil if
the mode doesn't support imenu."
  (condition-case nil
      (imenu-add-menubar-index)
    (error nil)))
(add-to-list 'find-file-hooks 'imenu-or-not)
(setq imenu-max-items 50
      imenu-scanning-message nil
      imenu-auto-rescan t)

(use-package drag-stuff
  :ensure t
  :diminish 'drag-stuff-mode
  :config (progn (drag-stuff-global-mode 1)
                 (drag-stuff-define-keys)))

;; Programming tools
(add-hook 'prog-mode-hook 'linum-mode)


(use-package magit
  :ensure t
  :bind* ("<f6>" . magit-status)
  :config (setq magit-diff-refine-hunk 'all))
;; To finish magit sub-editor
(eval-after-load "with-editor"
    '(define-key with-editor-mode-map (kbd "<f12>") 'with-editor-finish))
(use-package git-gutter-fringe
  :ensure t
  :diminish 'git-gutter-mode
  :bind* (("<M-f6>" . git-gutter:next-hunk)
	  ("<S-M-f6>" . git-gutter:previous-hunk)))
;; Not sure why this doesn't work with autoloads
(require 'git-gutter-fringe)
(global-git-gutter-mode 1)
(setq git-gutter:update-interval 0)

(defun easymacs-git-wdiff ()
  (interactive)
  (let ((inhibit-read-only t)
        (coding-system-for-read 'utf-8-unix)
        (coding-system-for-write 'utf-8-unix)
        (git-command (read-string "Git command: "
                                  "git diff --color-words HEAD")))
    (shell-command git-command "*git-wdiff*")
    (switch-to-buffer "*git-wdiff*")
    (delete-other-windows)
    (ansi-color-apply-on-region (point-min) (point-max))
    (visual-line-mode 1)
    (diff-mode)
    (read-only-mode)))
(bind-key* (kbd "<C-S-f6>") 'easymacs-git-wdiff)


;; Visible bookmarks

(use-package bm
         :ensure t
         :demand t
         :init
         (setq bm-restore-repository-on-load t)
         :config
         (setq bm-highlight-style 'bm-highlight-only-fringe)
         (setq bm-cycle-all-buffers t)
         (setq bm-repository-file (expand-file-name "~/.emacs.d/bm-repository"))
         (setq-default bm-buffer-persistence t)
         (add-hook' after-init-hook 'bm-repository-load)
         (add-hook 'find-file-hooks 'bm-buffer-restore)
         (add-hook 'kill-buffer-hook #'bm-buffer-save)
         (add-hook 'kill-emacs-hook #'(lambda nil
                                          (bm-buffer-save-all)
                                          (bm-repository-save)))
         (add-hook 'after-save-hook #'bm-buffer-save)
         (add-hook 'find-file-hooks   #'bm-buffer-restore)
         (add-hook 'after-revert-hook #'bm-buffer-restore)
         (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
         (global-set-key (kbd "<left-fringe> <mouse-5>") 'bm-next-mouse)
         (global-set-key (kbd "<left-fringe> <mouse-4>") 'bm-previous-mouse)
         (global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)
         :bind (("<C-f5>" . bm-next)
                ("<C-S-f5>" . bm-previous)
                ("<M-f5>" . bm-toggle)))


;; Folding for fold-dwim
(setq hs-isearch-open t)
(setq hs-hide-comments-when-hiding-all t)
(setq hs-allow-nesting t)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'prog-mode-hook #'outline-minor-mode)
(add-hook 'text-mode-hook #'outline-minor-mode)
(diminish 'hs-minor-mode)
(diminish 'outline-minor-mode)

(use-package browse-kill-ring
  :ensure t
  :bind* ("<C-S-v>" . browse-kill-ring))

;; Improve Scrolling Behaviour

;; smooth-scrolling.el keeps several lines of context visible when the cursor nears the top or bottom of the screen, and when the cursor hits that limit it scrolls by a single line rather than jumping by a page.

 (use-package smooth-scrolling
   :ensure t
   :diminish 'smooth-scrolling-mode
   :config (progn (smooth-scrolling-mode 1)
                  (setq smooth-scroll-margin 5)))

;; smooth-scroll.el has a very different purpose: it implements a scrolling motion when paging up and down.  But this does not work well with the reversible paging below, and it is really just a visual effect; so it is not enabled here.  But the functions to scroll without moving the cursor are useful.

(use-package smooth-scroll
  :ensure t
  :diminish 'smooth-scroll-mode
  :config (smooth-scroll-mode -1)
  :bind* (("<C-down>"  . scroll-up-1)
          ("<C-up>"    . scroll-down-1)
          ("<C-left>"  . scroll-right-1)
          ("<C-right>" . scroll-left-1)))

;; Code adapted from Emacswiki/Stack Overflow to implement reversible paging.  Paging down and then back up puts you back in the same spot where you started.

(defun sfp-page-down (&optional arg)
  (interactive "^P")
  (setq this-command 'next-line)
  (let ((smooth-scrolling-mode nil))
    (next-line
     (- (window-text-height)
        next-screen-context-lines))))
(put 'sfp-page-down 'isearch-scroll t)
(put 'sfp-page-down 'CUA 'move)

(defun sfp-page-up (&optional arg)
  (interactive "^P")
  (setq this-command 'previous-line)
  (let ((smooth-scrolling-mode nil))
    (previous-line
     (- (window-text-height)
        next-screen-context-lines))))

(put 'sfp-page-up 'isearch-scroll t)
(put 'sfp-page-up 'CUA 'move)

(global-set-key [next] 'sfp-page-down)
(global-set-key [prior] 'sfp-page-up)

;; Nice looking tabs from tabbar-ruler

;; Used by tabar-ruler; focus is buggy, so switch it off for now
(use-package powerline
  :ensure t)

(defun tabbar-buffer-groups-by-dir ()
        "Put all files in the same directory into the same tab bar"
        (with-current-buffer (current-buffer)
          (let ((dir (expand-file-name default-directory)))
            (cond ;; assign group name until one clause succeeds, so the order is important
             ((eq major-mode 'dired-mode)
              (list "Dired"))
             ((memq major-mode
                    '(help-mode apropos-mode Info-mode Man-mode))
              (list "Help"))
             ((string-match-p "\*.*\*" (buffer-name))
              (list "Misc"))
             (t (list dir))))))

(defun tabbar-switch-grouping-method (&optional arg)
  "Changes grouping method of tabbar to grouping by dir.
With a prefix arg, changes to grouping by major mode."
  (interactive "P")
  (ignore-errors
    (if arg
      (setq tabbar-buffer-groups-function 'tabbar-buffer-groups) ;; the default setting
        (setq tabbar-buffer-groups-function 'tabbar-buffer-groups-by-dir))))

(use-package tabbar-ruler
  :config
  (setq tabbar-cycle-scope 'tabs)
  (setq tabbar-ruler-global-tabbar t)
  (setq tabbar-ruler-popup-menu nil)
  (setq tabbar-ruler-popup-toolbar nil)
  (setq tabbar-ruler-popup-scrollbar nil)
  (setq tabbar-ruler-fancy-tab-separator 'bar)
  (setq tabbar-ruler-fancy-current-tab-separator 'wave)
  (setq tabbar-ruler-tab-padding nil)
  ;;(setq tabbar-ruler-style 'firefox)
  (tabbar-switch-grouping-method)

  :bind* (("<C-tab>" . tabbar-forward)
          ("<C-S-tab>" . tabbar-backward)
          ("<M-tab>" . tabbar-backward-group)
          ("<M-S-tab>" . tabbar-forward-group)
          ("<S-tab>" . tabbar-ruler-move)))

(require 'tabbar-ruler)

;; Remove interfering mappings (there are others, such as magit)
(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map (kbd "<M-tab>") nil)))
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "<C-tab>") nil)))


;;; Utility functions

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))
(bind-key* (kbd "M-Q") 'unfill-paragraph)

;; Like vi's % command (from Emacswiki)
(defun forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))
(bind-key* (kbd "C-%") 'forward-or-backward-sexp)

(defun easymacs-make-dist ()
  (interactive)
  (save-some-buffers)
  (cd easymacs-dir)
  (cd "..")
  (start-process "zip" "*zip*"
		 "zip" "-r" "easymacs.zip" "easymacs" "-x" "easymacs/.git/*"))

(defun easymacs-comment-line-or-region (arg)
  (interactive)
  (let ((start (line-beginning-position))
	(end (line-end-position)))
    (when (or (not transient-mark-mode) (region-active-p))
      (setq start (save-excursion
		    (goto-char (region-beginning))
		    (beginning-of-line)
		    (point))
	    end (save-excursion
		  (goto-char (region-end))
		  (end-of-line)
		  (point))))
    (comment-region start end arg)))

(defun easymacs-kill-buffer ()
    "Kill buffer and delete window if split without prompting"
    (interactive)
    (let ((buffer (current-buffer)))
      (ignore-errors (delete-window (selected-window)))
      (kill-buffer buffer)))

(defun easymacs-kill-some-buffers ()
  "Kill most unmodified buffers, except for a few."
  (interactive)
  (when (yes-or-no-p "Close unmodified files? ")
    (let ((list (buffer-list)))
      (while list
	(let* ((buffer (car list))
	       (name (buffer-name buffer)))
	  (when (if (string-match "^\\*.*\\*$" name)
		    (and (not (string-equal name "*Messages*"))
			 (not (string-equal name "*eshell*")))
		  (not (buffer-modified-p buffer)))
	    (kill-buffer buffer)))
	(setq list (cdr list))))
    (delete-other-windows)))

(defun easymacs-select-line ()
  "Select current line"
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(defun easymacs-last-buffer ()
  (interactive)
  (switch-to-buffer
   (other-buffer (current-buffer) 1)))

;;; Word count
;; Pinched from http://www.dr-qubit.org/emacs.php

(setq mode-line-position (assq-delete-all 'wc-mode mode-line-position))
(setq mode-line-position
      (append
       mode-line-position
       '((wc-mode
	  (6 (:eval (if (use-region-p)
			(format " %d,%d,%d"
				(abs (- (point) (mark)))
				(count-words-region (point) (mark))
				(abs (- (line-number-at-pos (point))
					(line-number-at-pos (mark)))))
		      (format " %d,%d,%d"
			      (- (point-max) (point-min))
			      (count-words-region (point-min) (point-max))
			      (line-number-at-pos (point-max))))))
	  nil))))
(define-minor-mode wc-mode
  "Toggle word-count mode.
With no argument, this command toggles the mode.
A non-null prefix argument turns the mode on.
A null prefix argument turns it off.

When enabled, the total number of characters, words, and lines is
displayed in the mode-line.")

;;; Mac stuff
(when (memq window-system '(mac ns))
  (setq ns-pop-up-frames nil
	mac-command-modifier 'control
	x-select-enable-clipboard t))
(use-package exec-path-from-shell
  :ensure t
  :config (when (memq window-system '(mac ns))
	    (exec-path-from-shell-initialize)))

;;; Spell-checking
;; Get hunspell dictionaries like so:
;; svn co https://src.chromium.org/chrome/trunk/deps/third_party/hunspell_dictionaries/
;; make sure that one dictionary is soft-linked to default.dic and default.aff
;(setenv "DICTIONARY" "en_GB")
;(setenv "LANG" "en_GB")
;(setq ispell-local-dictionary "en_GB")
;(setq ispell-local-dictionary-alist
;        '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))
(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))
(add-hook 'text-mode-hook '(lambda ()
			     (flyspell-mode 1)))
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;;; Eshell
;; Always save eshell history without asking
(setq eshell-save-history-on-exit 't)
(setq eshell-ask-to-save-history 'always)
(setq eshell-prefer-to-shell t)
;; Don't auto-complete ambiguities
(setq eshell-cmpl-cycle-completions nil)

(defun easymacs-eshell ()
  "Eshell with switch to directory of current buffer"
  (interactive)
  (let ((dir default-directory))
    (eshell)
    ;; Make sure it's at the front of the buffer-list
    (switch-to-buffer "*eshell*")
    (eshell-kill-input)
    (unless (string= (expand-file-name dir) (expand-file-name default-directory))
      (eshell/cd dir)
      (eshell-send-input))))

;;; Dired

  ;; Make dired less weird -- it always opens new files or directories
  ;; in the current buffer, rather than endless spawning of new buffers
  (defun easymacs-dired-mouse-find-file-same-window (event)
    ;; Never open a new buffer from dired, even when clicking with the mouse
    ;; Modified from dired.el
    "In Dired, visit the file or directory name you click on."
    (interactive "e")
    (let (window pos file)
      (save-excursion
	(setq window (posn-window (event-end event))
	      pos (posn-point (event-end event)))
	(if (not (windowp window))
	    (error "No file chosen"))
	(set-buffer (window-buffer window))
	(goto-char pos)
	(setq file (dired-get-file-for-visit)))
      (select-window window)
      (find-alternate-file (file-name-sans-versions file t))))

  (eval-after-load "dired"
    '(progn
       ;; Never open a new buffer from dired, neither for files nor directories.
       (defadvice dired-find-file (around dired-subst-directory activate)
	 "Replace current buffer if file is a directory."
	 (interactive)
	 (let ((orig (current-buffer))
	       (filename (dired-get-filename nil t)))
	   ad-do-it
	   (kill-buffer orig)))
       (define-key dired-mode-map [mouse-2]
	 'easymacs-dired-mouse-find-file-same-window)
       (define-key dired-mode-map "^" (function
				       (lambda nil (interactive)
					 (find-alternate-file ".."))))))

;;; Isearch

(bind-key* (kbd "C-f") 'isearch-forward)
(bind-key* (kbd "C-S-f") 'isearch-backward)
(bind-key* (kbd "C-r") 'query-replace)
(bind-key* (kbd "C-S-r") 'replace-string)
(bind-key* (kbd "M-r") 'query-replace-regexp)
(bind-key* (kbd "M-S-r") 'replace-regexp)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-S-f") 'isearch-repeat-backward)
(define-key isearch-mode-map [escape] 'isearch-cancel)
(define-key isearch-mode-map (kbd "<C-up>") 'isearch-ring-retreat)
(define-key isearch-mode-map (kbd "<C-down>") 'isearch-ring-advance)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<f1>") 'ido-switch-buffer)

(bind-key* (kbd "C-d") '(lambda () (interactive)
			  (beginning-of-thing 'symbol)
			  (push-mark)
			  (activate-mark)
			  (end-of-thing 'symbol)))
(bind-key* (kbd "S-C-d") 'easymacs-select-line)

;; Modifies isearch to search for selected text, if there is a selection
(defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))

;; Important for long lines
(require 'grep)
(grep-apply-setting 'grep-highlight-matches 'always)
(grep-apply-setting 'grep-command
                    "pcregrep -MHn --color=always '' *.*")


;;; Regexps: re-builder and pcre2el
(use-package pcre2el
  :ensure t
  :config (pcre-mode t)
  :diminish pcre-mode)

(use-package re-builder
  :ensure t
  :config (setq reb-re-syntax 'pcre))
;; use-package bind does not work here
(add-hook 'reb-mode-hook (lambda ()
                           (local-set-key (kbd "<f5>")
                                          'reb-next-match)
                           (local-set-key (kbd "<S-f5>")
                                          'reb-prev-match)
                           (local-set-key (kbd "C-b")
                                          'reb-change-target-buffer)))


;;; Elisp
(defun easymacs-elisp-help ()
  (interactive)
  (let ((sym (intern-soft (thing-at-point 'symbol))))
    (cond
     ((and sym
	   (fboundp sym)
	   (not (boundp sym)))
      (describe-function sym))
     ((and sym
	   (not (fboundp sym))
	   (boundp sym))
      (describe-variable sym))
     ((and sym
	   (fboundp sym)
	   (boundp sym))
      (if (yes-or-no-p "Both value and function are bound; describe function? ")
	  (describe-function sym)
	(describe-variable sym)))
     (t
      (call-interactively 'describe-function)))))
(define-key emacs-lisp-mode-map (kbd "<f9>") 'easymacs-elisp-help)
(define-key emacs-lisp-mode-map (kbd "<f10>")  'completion-at-point)
(define-key emacs-lisp-mode-map (kbd "<f11>") 'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "<f12>") 'eval-defun)

(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(add-hook 'emacs-lisp-mode-hook '(lambda ()
				   (turn-on-eldoc-mode)
				   (diminish 'eldoc-mode)))
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;; XML

(setq auto-mode-alist
      (cons '("\\.x?html\\'" . nxml-mode) auto-mode-alist))
(setq magic-mode-alist
          (cons '("<\\?xml\\s " . nxml-mode) magic-mode-alist))

(defun easymacs-xhtml-outline-level ()
  (let ((tag (buffer-substring (match-beginning 1) (match-end 1))))
    (if (eq (length tag) 2)
	(- (aref tag 1) ?0)
      0)))
(defun easymacs-xhtml-extras ()
  (make-local-variable 'outline-regexp)
  (setq outline-regexp "\\s *<\\([h][1-6]\\|html\\|body\\|head\\)\\b")
  (make-local-variable 'outline-level)
  (setq outline-level 'easymacs-xhtml-outline-level)
  (outline-minor-mode 1)
  (hs-minor-mode 1))


;; For folding elements 
(add-to-list 'hs-special-modes-alist
	     '(nxml-mode
	       "<!--\\|<[^/>]>\\|<[^/][^>]*[^/]>"
	       ""
	       "<!--" ;; won't work on its own; uses syntax table
	       (lambda (arg) (easymacs-nxml-forward-element))
	       nil
	       ))
(defun easymacs-nxml-forward-element ()
  (let ((nxml-sexp-element-flag))
    (setq nxml-sexp-element-flag (not (looking-at "<!--")))
    (unless (looking-at outline-regexp)
      (condition-case nil
	  (nxml-forward-balanced-item 1)
	(error nil)))))
(add-hook 'nxml-mode-hook #'hs-minor-mode)

(defun easymacs-insert-tag (tag-name beg end)
  (interactive "sTag name: \nr")
  (if mark-active
      (save-excursion
	(goto-char beg)
	(insert "<" tag-name ">")
	(goto-char (+ end 2 (length tag-name)))
	(insert "</" tag-name ">"))
    (progn
      (insert "<" tag-name ">")
      (save-excursion
	(insert "</" tag-name ">")))))

;; from emacswiki
(defun nxml-where ()
      "Display the hierarchy of XML elements the point is on as a path."
      (interactive)
      (let ((path nil))
        (save-excursion
          (save-restriction
            (widen)
            (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                        (condition-case nil
                            (progn
                              (nxml-backward-up-element) ; always returns nil
                              t)
                          (error nil)))
              (setq path (cons (xmltok-start-tag-local-name) path)))
            (if (called-interactively-p t)
                (message "/%s" (mapconcat 'identity path "/"))
              (format "/%s" (mapconcat 'identity path "/")))))))

(defun easymacs-nxml-mode-hook ()
  (bind-key (kbd "<f5>") 'rng-next-error nxml-mode-map)
  (bind-key (kbd "<S-f5>") 'rng-next-error nxml-mode-map)
  (bind-key (kbd "<f9>") 'tei-html-docs-p5-element-at-point nxml-mode-map)
  (bind-key (kbd "<f10>") 'browse-url-of-buffer nxml-mode-map)
  (bind-key (kbd "<S-f10>") 'nxml-where nxml-mode-map)
  (bind-key (kbd "<f11>") 'easymacs-insert-tag nxml-mode-map)
  (bind-key (kbd "<C-f11>") 'nxml-split-element nxml-mode-map)
  (bind-key (kbd "<f12>") 'nxml-complete nxml-mode-map)
  (bind-key (kbd "<S-f12>") 'nxml-finish-element nxml-mode-map)
  (bind-key (kbd "<M-f12>") 'nxml-dynamic-markup-word nxml-mode-map)
  (bind-key (kbd "<C-f12>") 'nxml-balanced-close-start-tag-block
	    nxml-mode-map)
  (bind-key (kbd "<C-S-f12>") 'nxml-balanced-close-start-tag-inline
	    nxml-mode-map) 

  (when (and (buffer-file-name)
	     (string-match "\\.xhtml$"
			   (file-name-sans-versions (buffer-file-name))))
    (easymacs-xhtml-extras)))
(add-hook 'nxml-mode-hook 'easymacs-nxml-mode-hook)

;;; Markdown
(use-package markdown-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  :bind (:map markdown-mode-map
	      ("<f10>" . markdown-other-window)
	      ("<f11>" . markdown-preview)
	      ("<f12>" . markdown-live-preview-mode)))

;;; Global key-bindings
(bind-key* [escape] 'keyboard-escape-quit)
(bind-key* (kbd "<S-escape>") 'delete-other-windows)
(bind-key* (kbd "C-`") 'other-frame)
(bind-key* (kbd "C-§") 'other-frame)
(bind-key* (kbd "C-a") 'mark-whole-buffer)
(bind-key* (kbd "C-s") 'save-buffer)
(bind-key* (kbd "C-S-s") 'save-some-buffers)
(bind-key* (kbd "C-n") '(lambda () (interactive)
				 (let ((last-nonmenu-event nil))
				   (call-interactively 'find-file))))
(bind-key* (kbd "C-S-n") '(lambda () (interactive)
			    (find-file-other-frame
			     (concat easymacs-dir "easymacs-help.txt"))))
(bind-key* (kbd "C-o") '(lambda () (interactive)
				 (let ((last-nonmenu-event nil))
				   (call-interactively 'find-file-existing))))
(bind-key* (kbd "C-q") 'save-buffers-kill-emacs)
(bind-key* (kbd "C-w") 'easymacs-kill-buffer)
(bind-key* (kbd "S-C-w") 'delete-frame)
(bind-key* (kbd "M-w") 'easymacs-kill-some-buffers)

(bind-key* (kbd "<end>") 'end-of-visual-line)
(bind-key* (kbd "<home>") 'beginning-of-visual-line)

;;; Function keys

;; F1
(bind-key* (kbd "<f1>") 'ido-switch-buffer)
(bind-key* (kbd "<S-f1>") 'ibuffer)
(bind-key* (kbd "<C-f1>") 'find-file)
(bind-key* (kbd "<S-C-f1>") '(lambda () (interactive) (find-file "")))
(bind-key* (kbd "<M-f1>") 'recentf-open-files)
(bind-key* (kbd "<S-M-f1>") 'ffap)

;; F2
(bind-key* (kbd "<f2>") 'flyspell-auto-correct-previous-word)
(bind-key* (kbd "<S-f2>") 'ispell-complete-word)
(bind-key* (kbd "<C-f2>") 'insert-char)
(bind-key* (kbd "<M-f2>") 'oed-quickword)
(bind-key* (kbd "<S-M-f2>")
	   '(lambda () (interactive)
	      (eww (concat "http://moby-thesaurus.org/search?q="
				  (substring-no-properties
				    (thing-at-point 'word))))))

;; F3 is company-complete (defined above)
(bind-key* (kbd "<S-f3>") '(lambda () (interactive)
			     (copy-from-above-command 1)))
(bind-key* (kbd "<C-f3>") '(lambda () (interactive)
			     (copy-from-above-command)))
(bind-key* (kbd "<M-f3>") '(lambda () (interactive)
			     (easymacs-comment-line-or-region 1)))
(bind-key* (kbd "<M-S-f3>") '(lambda () (interactive)
			     (easymacs-comment-line-or-region -1)))
;; F4
(bind-key* (kbd "<f4>") 'delete-other-windows)
(bind-key* (kbd "<S-f4>") 'other-window)
(bind-key* (kbd "<C-f4>") 'kmacro-end-or-call-macro)
(bind-key* (kbd "<C-S-f4>") 'kmacro-start-macro-or-insert-counter) 
(bind-key* (kbd "<M-f4>") 'save-buffers-kill-emacs)

;; F5
(bind-key (kbd "<f5>") 'next-error)
(bind-key (kbd "<S-f5>") 'previous-error)
(bind-key (kbd "<M-S-f5>")
          '(lambda () (interactive)
             (call-interactively 'grep)))


;; F6
;; F6 is magit-status, defined above
;; (S-)M-F6 is git-gutter:next-hunk and previous-hunk
(bind-key* (kbd "<C-f6>")
	   '(lambda () (interactive)
	      (if (string= (buffer-name) "*eshell*")
		  (switch-to-buffer (other-buffer (current-buffer)))
		(easymacs-eshell))))

;; F7
(bind-key* (kbd "<f7>") 'fold-dwim-toggle)
(bind-key* (kbd "<M-f7>") 'fold-dwim-hide-all)
(bind-key* (kbd "<S-M-f7>") 'fold-dwim-show-all)
(bind-key* (kbd "<C-f7>") 'outline-next-visible-heading)
(bind-key* (kbd "<S-C-f7>") 'outline-previous-visible-heading)

;;; Show help screen at startup

;; Workaround for a frame-related mac bug
(find-file-other-frame
 (concat easymacs-dir "easymacs-help.txt"))
(delete-other-frames)
(goto-char (point-min))
(cd (expand-file-name "~/"))
(defun kill-unkillable-buffer ()
  (message "%s" "Error: you cannot close this file.")
  nil)
(make-local-variable 'kill-buffer-query-functions)
(add-hook 'kill-buffer-query-functions 'kill-unkillable-buffer)
(read-only-mode)
(view-mode)
