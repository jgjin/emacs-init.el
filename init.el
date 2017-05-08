;;; package --- Summary

;;; Commentary:
;;; POOP

;;; Code:

;; Set initial packages
(setq package-list '(spacemacs-theme use-package))


;; Write functions to parenthesize to beginning and end of line

;; Initialize package archives
(add-to-list 'load-path "~/.emacs.d/lisp/")
;; Activate benchmark
(require 'benchmark-init)
(benchmark-init/activate)
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Turn on/off non-package modes
(tool-bar-mode -1)
(menu-bar-mode -1)
(cua-selection-mode 1)

;; Set tabbing options
(setq c-default-style "linux"
      tab-width 8
      indent-tabs-mode t)

;; Set non-package variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(confirm-kill-emacs (quote y-or-n-p))
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(inhibit-startup-screen t))

;; Set non-package faces (colors)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'spacemacs-dark)

(require 'use-package)

(use-package avy
  :ensure t
  :bind
  (("M-k" . avy-goto-char-timer)
   ("M-l" . avy-goto-line)))

(use-package bm
  :ensure t
  :init
  ;; Restore on load
  (setq bm-restore-repository-on-load t)

  :config
  ;; Allow bm-next and bm-previous to travel between buffers.
  (setq bm-cycle-all-buffers t)
  ;; Set where to store persistant files
  (setq bm-repository-file "~/.emacs.d/bm-repository")
  ;; Save bookmarks
  (setq-default bm-buffer-persistence t)
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook #'(lambda nil
				 (bm-buffer-save-all)
				 (bm-repository-save)))
  (add-hook 'after-save-hook #'bm-buffer-save)
  ;; Load bookmarks from file when on start up.
  (add-hook' after-init-hook 'bm-repository-load)
  ;; Restore bookmarks
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)

  ;; Set package-related faces (colors)
  (set-face-foreground bm-fringe-persistent-face "dark gray")
  (set-face-background bm-fringe-persistent-face "RoyalBlue3")
  (set-face-foreground bm-persistent-face "dark gray")
  (set-face-background bm-persistent-face "RoyalBlue3")
  
  :bind
  (("C-b" . bm-toggle)
   ("C-M-b" . bm-next)
   ("M-b" . bm-previous)))

(use-package company
  :ensure t
  :config
  ;; Enable global-company-mode
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-c-headers
  :ensure t
  :config
  ;; Add C(++) headers to company
  (add-hook 'c-mode-hook (lambda()
	    (add-to-list 'company-backends 'company-c-headers)))
  (add-hook 'c++-mode-hook (lambda()
	    (add-to-list 'company-backends 'company-c-headers))))

(use-package company-irony
  :ensure t
  :config
  ;; Add C(++) to company
  (add-hook 'c-mode-hook (lambda()
	    (add-to-list 'company-backends 'company-irony)))
  (add-hook 'c++-mode-hook (lambda()
	    (add-to-list 'company-backends 'company-irony))))

(use-package company-jedi
  :ensure t
  :config
  ;; Add Python to company
  (add-hook 'python-mode-hook (lambda()
	    (add-to-list 'company-backends 'company-jedi)))
  (setq python-shell-interpreter "/usr/bin/python")
  (setq python-shell-interpreter-args "-O -3"))

(use-package company-quickhelp
  :ensure t
  :config
  ;; Enable popup for company
  (company-quickhelp-mode 1))

;; (use-package company-tern
;;   :ensure t
;;   :config
;;   ;; Add JavaScript to company
;;   (add-hook 'js2-mode-hook (lambda()
;; 	    (add-to-list 'company-backends 'company-tern))))

(use-package dumb-jump
  :ensure t
  :bind
  (("C-c C-r" . dumb-jump-back)
   ("C-j" . dumb-jump-go)))

(use-package fill-column-indicator
  :ensure t
  :config
  ;; Set fci color to red
  (setq fci-rule-color "red")
  ;; Set fci to 80th column
  (setq fci-rule-column 80)
  ;; Set fci width to 1
  (setq fci-rule-width 1))

(use-package flycheck
  :ensure t
  :init
  (require 'semantic/bovine/gcc)

  :config
  ;; Turn on global-flycheck-mode
  (add-hook 'after-init-hook #'global-flycheck-mode)
  ;; Try to set c++ standard to c++11
  (add-hook 'c++-mode-hook
	    (lambda ()
	      (setq flycheck-gcc-language-standard "c++11"))))

(use-package helm
  :ensure t
  :config
  ;; ???
  (setq helm-ff-file-name-history-use-recentf t)
  ;; ???
  (setq helm-move-to-line-cycle-in-source t)
  ;; Set helm buffer to split current window
  (setq helm-split-window-in-side-p t)
  ;; Turn on helm-mode
  (helm-mode 1)
  ;; Allow helm buffer to auto-resize
  (helm-autoresize-mode 1)

  :bind
  (("M-x" . helm-M-x)
   ("C-p" . helm-locate)
   ("C-x b" . helm-buffers-list)
   ("C-x C-f" . helm-find-files)
   ("C-c b" . helm-show-kill-ring)
   :map helm-find-files-map
   ("M-w" . helm-previous-line)
   ("M-a" . helm-find-files-up-one-level)
   ("<C-tab>" . helm-find-files-up-one-level)
   ("M-s" . helm-next-line)
   ("M-d" . helm-execute-persistent-action)
   ("<tab>" . helm-execute-persistent-action)
   :map helm-map
   ("M-w" . helm-previous-line)
   ("M-a" . helm-find-files-up-one-level)
   ("<C-tab>" . helm-find-files-up-one-level)
   ("M-s" . helm-next-line)
   ("M-d" . helm-execute-persistent-action)
   ("<tab>" . helm-execute-persistent-action)))

(use-package helm-swoop
  :ensure t
  :config
  ;; Set first helm-swoop input as empty
  (setq helm-swoop-pre-input-function
	(lambda nil
	  (if
	      (boundp
	       (quote helm-swoop-pattern))
	      helm-swoop-pattern "")))
  ;; Set helm-swoop buffer to split window vertically
  (setq helm-swoop-split-direction (quote split-window-vertically))
  ;; Split current window when multiple windows are open
  (setq helm-swoop-split-with-multiple-windows t)

  :bind
  (("C-f" . helm-swoop)
   ("C-f" . helm-multi-swoop-current-mode)
   :map helm-swoop-map
   ("C-f" . helm-multi-swoop-current-mode-from-helm-swoop)
   ("M-w" . helm-previous-line)
   ("M-s" . helm-next-line)
   :map helm-multi-swoop-map
   ("M-w" . helm-previous-line)
   ("M-s" . helm-next-line)))

(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

;; (use-package js2-mode
;;   :ensure t
;;   :config
;;   ;; Enable js2-mode as major mode for JavaScript
;;   (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package json-mode
  :ensure t
  :bind
  (:map json-mode-map
   ("M-f" . jsons-print-path)))

(use-package magit
  :ensure t
  :bind
  ("M-g" . magit-status))

(use-package powerline
  :ensure t
  :config
  ;; Set powerline to default theme
  (powerline-default-theme))

(use-package rainbow-delimiters
  :init
  (require 'rainbow-delimiters)
  :config
  ;; Turn on rainbow-delimiters mode for programming modes
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

  ;; Set package-related faces (colors)
  ;; (set-face-foreground 'rainbow-delimiters-depth-1-face  "red")
  ;; (set-face-foreground 'rainbow-delimiters-depth-2-face  "orange")
  ;; (set-face-foreground 'rainbow-delimiters-depth-3-face  "yellow")
  ;; (set-face-foreground 'rainbow-delimiters-depth-4-face  "spring green")
  ;; (set-face-foreground 'rainbow-delimiters-depth-5-face  "forest green")
  ;; (set-face-foreground 'rainbow-delimiters-depth-6-face  "deep sky blue")
  ;; (set-face-foreground 'rainbow-delimiters-depth-7-face  "blue")
  ;; (set-face-foreground 'rainbow-delimiters-depth-8-face  "purple")
  ;; (set-face-foreground 'rainbow-delimiters-depth-9-face  "purple4")
  ;; (set-face-foreground 'rainbow-delimiters-unmatched-face  "DeepPink3"))

(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  
  :config
  ;; Turn on smartparens-global-mode
  (smartparens-global-mode 1)
  
  :bind
  (("C-a" . sp-backward-symbol)
   ("C-d" . sp-forward-symbol)
   ("M-q" . sp-beginning-of-sexp)
   ("M-e" . sp-end-of-sexp)))

(use-package zzz-to-char
  :ensure t
  :bind
  (("M-z" . zzz-to-char)))

(defun yank-pop-reverse ()
  "Invoke 'yank-pop' with -1."
  (interactive)
  (yank-pop -1))

;; https://www.emacswiki.org/emacs/DuplicateStartOfLineOrRegion
(defun duplicate-line-or-region ()
  "Duplicate region if region is marked, otherwise duplicate line."
  (interactive)
  (if mark-active
      (duplicate-region)
    (duplicate-line)))

(defun duplicate-line ()
  "Duplicate line."
  (let ((text (buffer-substring (line-beginning-position)
                                (line-end-position))))
    (forward-line)
    (push-mark)
    (insert text)
    (open-line 1)))

(defun duplicate-region ()
  "Duplicate region if region is marked."
  (let* ((end (region-end))
         (text (buffer-substring (region-beginning)
                                 end)))
    (goto-char end)
    (insert text)
    (push-mark end)
    (setq deactivate-mark nil)
    (exchange-point-and-mark)))

;; https://www.emacswiki.org/emacs/AutoInsertHeaderGuards
;; Create header guards with f12
(global-set-key [f12]
		'(lambda ()
		   (interactive)
		   (if (buffer-file-name)
		       (let*
			   ((fName (upcase (file-name-nondirectory
					    (file-name-sans-extension buffer-file-name))))
			    (ifDef (concat "#ifndef " fName "_H" "\n#define " fName "_H" "\n"))
			    (begin (point-marker)))
			 (progn
			   
			   ;; If less then 5 characters are in the buffer,
			   ;; insert the class definition
			   (if (< (- (point-max) (point-min)) 5 )
			       (progn
				 (insert "\nclass "
					 (capitalize fName) "{\npublic:\n\nprivate:\n\n};\n")
				 (goto-char (point-min))
				 (next-line-nomark 3)
				 (setq begin (point-marker))))
			   
			   ;; Insert the Header Guard
			   (goto-char (point-min))
			   (insert ifDef)
			   (goto-char (point-max))
			   (insert "\n#endif" " //" fName "_H")
			   (goto-char begin)))
		     ;; else
		     (message (concat "Buffer " (buffer-name) " must have a filename")))))

(defun indent-whole-buffer ()
  "Mark whole buffer then indent region."
  (interactive)
  (push-mark)
  (mark-whole-buffer)
  (indent-region (region-beginning) (region-end))
  (pop-mark)
  (exchange-point-and-mark))


;; https://www.emacswiki.org/emacs/AutoIndentation
(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

(defun left-char-message ()
  "Execute 'left-char' with reminder not to use the left arrow key."
  (interactive)
  (left-char)
  (message "%s" "Please use M-a instead of the left arrow key."))

(defun right-char-message ()
  "Execute 'right-char' with reminder not to use the right arrow key."
  (interactive)
  (right-char)
  (message "%s" "Please use M-d instead of the right arrow key."))

(defun previous-line-message ()
  "Execute 'previous-line' with reminder not to use the up arrow key."
  (interactive)
  (previous-line)
  (message "%s" "Please use M-w instead of the up arrow key."))

(defun next-line-message ()
  "Execute 'left-char' with reminder not to use the down arrow key."
  (interactive)
  (next-line)
  (message "%s" "Please use M-s instead of the down arrow key."))

;; Set overriding non-package personal keybindings
(bind-keys*
 ((kbd "C-M-a") . beginning-of-line)
 ((kbd "C-M-d") . end-of-line)
 ((kbd "C-w") . beginning-of-buffer)
 ((kbd "C-s") . end-of-buffer)
 ((kbd "C-q") . scroll-down-command)
 ((kbd "C-M-q") . beginning-of-defun)
 ((kbd "C-e") . scroll-up-command)
 ((kbd "C-M-e") . end-of-defun)
 ((kbd "C-c C-x") . kill-region)
 ((kbd "C-c C-c") . kill-ring-save)
 ((kbd "C-c C-v") . yank-and-indent)
 ((kbd "M-v") . yank-pop)
 ((kbd "C-M-v") . yank-pop-reverse)
 ((kbd "C-S-d") . kill-whole-line)
 ((kbd "C-M-f") . isearch-forward-regexp)
 ((kbd "C-l") . goto-line)
 ((kbd "C-z") . undo)
 ((kbd "M-c") . comment-dwim)
 ((kbd "C-x C-r") . repeat-complex-command)
 ((kbd "C-t") . term)
 ((kbd "C-x C-z") . split-window-right)
 ((kbd "C-x z") . other-window)
 ((kbd "C-x a") . delete-other-windows)
 ((kbd "M-r") . query-replace)
 ((kbd "C-M-r") . query-replace-regexp)
 ((kbd "C-x <tab>") . indent-whole-buffer)
 ((kbd "C-x C-d") . duplicate-line-or-region)
 ((kbd "C-;") . iedit-mode))

;; Set non-overriding personal keybindings
(global-set-key (kbd "M-s") 'next-line)
(global-set-key (kbd "M-w") 'previous-line)
(global-set-key (kbd "M-d") 'right-char)
(global-set-key (kbd "M-a") 'left-char)
(global-set-key (kbd "<left>") 'left-char-message)
(global-set-key (kbd "<right>") 'right-char-message)
(global-set-key (kbd "<up>") 'previous-line-message)
(global-set-key (kbd "<down>") 'next-line-message)

;; ;; Enable emacsclient
;; (server-start)

;; Report benchmark
(benchmark-init/show-durations-tabulated)

(provide 'init)
;;; init.el ends here
