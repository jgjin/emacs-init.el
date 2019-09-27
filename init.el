;;; package --- Summary

;;; Commentary:
;; (define-key polymode-mode-map (kbd "M-n M-e") ess-eval-line-and-step)
;; (define-key polymode-mode-map (kbd "M-n C-e") rmd-send-buffer)
;;; Check color to LSP
;;; Add C-S-c tex-compile to LaTeX

;;; Code:

;; ;; Activate benchmark
;; (add-to-list 'load-path "~/.emacs.d/elpa/benchmark-init/")
;; (require 'benchmark-init)
;; (benchmark-init/activate)

;; Add load paths
(add-to-list 'load-path "~/.emacs.d/elpa/")

;; Initialize package archives
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Fetch list of available packages
(package-refresh-contents)
;; (unless package-archive-contents
;;   )

;; Turn on/off non-package modes
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;; (cua-selection-mode 1)

;; Set tabbing options
(setq c-default-style "linux"
      tab-width 4
      indent-tabs-mode t)

;; Truncate lines to not go beyond screen
(set-default 'truncate-lines t)

;; Create temp directory for auto-save and backup files
;; Set non-package variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/temp/" t))))
 '(backup-by-copying t)
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/temp/"))))
 '(confirm-kill-emacs (quote y-or-n-p))
 '(custom-safe-themes
   (quote
    ("5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(delete-old-versions t)
 '(fci-rule-column 120)
 '(inhibit-startup-screen t)
 '(kept-new-versions 6)
 '(kept-old-versions 2)
 '(package-selected-packages
   (quote
    (protobuf-mode origami web-mode js2-mode zzz-to-char yasnippet-snippets vlf use-package spacemacs-theme smartparens rainbow-delimiters projectile powerline pdf-tools org-bullets multiple-cursors magit lsp-ui json-mode iedit hydra hungry-delete hide-lines haskell-mode goto-chg flycheck-rust fill-column-indicator expand-region emojify diminish dashboard csv-mode counsel company-tabnine company-quickhelp company-lsp color-identifiers-mode cask cargo bm apropospriate-theme ag))))

;; Set non-package faces (colors)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Set font size
(set-face-attribute 'default nil :height 240)
;; Font type set in .Xresources, could be set here
;; (set-face-attribute 'default t :font FONT)

;; Set transparency
;; (set-frame-parameter (selected-frame) 'alpha '(85 . 50))
;; (add-to-list 'default-frame-alist '(alpha . (85 . 50)))

;; Enable use-package
;; See https://github.com/jwiegley/use-package for explanation
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Diminish non-package minor modes
(diminish 'abbrev-mode)

;; Set package configurations
(use-package ag
  :ensure t)

(use-package avy
  :ensure t
  :config
  ;; Set package-related faces (colors)
  (set-face-attribute 'avy-lead-face nil
		      :foreground "#86dc2f" :background "#444444")
  (set-face-attribute 'avy-lead-face-0 nil
		      :foreground "#86dc2f" :background "#444444")
  (set-face-attribute 'avy-lead-face-1 nil
		      :foreground "#86dc2f" :background "#444444")
  (set-face-attribute 'avy-lead-face-2 nil
		      :foreground "#86dc2f" :background "#444444")

  :bind
  (("M-k" . avy-goto-char-timer)
   ("M-l" . avy-goto-line)))

;; ;; Rename benchmark-init.x folder to benchmark-init
;; (use-package benchmark-init
;;   :ensure t)

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
  (("C-S-SPC" . bm-toggle)
   ("M-S-SPC" . bm-next)
   ("C-M-SPC" . bm-previous)))

;; Needed for installing pdf-tools initially
(use-package cask
  :ensure t)

(use-package cargo
  :ensure t
  :config
  (defhydra hydra-cargo
    (:hint nil
	   :idle 3.00)
    "
^Commands^
^^^^^^-------------------------
_b_ build _n_ new    _t_ test
_c_ clean _r_ run _u_ update
_i_ init  _s_ search
    "
    ("b" cargo-process-build)
    ("c" cargo-process-clean :exit t)
    ("i" cargo-process-init :exit t)
    ("n" cargo-process-new :exit t)
    ("r" cargo-process-run :exit t)
    ("s" cargo-process-search :exit t)
    ("t" cargo-process-test :exit t)
    ("u" cargo-process-update :exit t)))

;; (use-package color-identifiers-mode
;;   :ensure t
;;   :diminish color-identifiers-mode
;;   :config
;;   (global-color-identifiers-mode 1))

(use-package company
  :ensure t
  :diminish company-mode
  :config
  ;; Enable global-company-mode
  (add-hook 'after-init-hook 'global-company-mode)
  ;; Set minimal delay
  (setq company-idle-delay 0)
  ;; Wait for only 1 character to start completions
  (setq company-minimum-prefix-length 1)
  (setq company-show-numbers t))

(use-package company-lsp
  :ensure t
  :config
  ;; Add LSP to company backends
  (push 'company-lsp company-backends))

(use-package company-quickhelp
  :ensure t
  :config
  ;; Enable quickhelp popup
  (company-quickhelp-mode)

  :bind
  (:map company-active-map
        ("RET" . nil)))

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :config
  (counsel-mode 1)
  :bind
  (("M-x" . counsel-M-x)
   ("C-h a" . counsel-apropos)
   ("C-h b" . counsel-descbinds)
   ("C-p" . counsel-ag)))

(use-package csv-mode
  :ensure t)

(use-package dashboard
  :ensure t
  :init
  ;; Start dashboard when emacs starts
  (dashboard-setup-startup-hook)

  :config
  ;; Set greeting message
  (setq dashboard-banner-logo-title "Welcome to Jason's BananaMacs!")
  ;; ;; Set dashboard image
  (setq dashboard-startup-banner "~/.emacs.d/banana/banana.png")
  ;; Set dashboard items
  (setq dashboard-items '((recents . 10)
                          (bookmarks . 0)
                          (projects . 10)
                          (agenda . 10)))

  :bind
  (:map dashboard-mode-map
        ("M-w" . widget-backward)
        ("M-s" . widget-forward)))

(use-package diminish
  :ensure t)

(use-package dired
  :bind
  ;; Overrides M-s as a prefix key
  (:map dired-mode-map
	("M-s" . next-line)))

;; On first installation, restart emacs --daemon and respond yes to downloading emojis
;; Emojis will be put in .emacs.d/emojis/
(use-package emojify
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-emojify-mode))

;; (use-package ess
;;   :ensure t)

;; ;; https://emacs.stackexchange.com/questions/44880/use-package-bind-not-working-as-expected
;; (use-package ess-site
;;   :ensure ess)

;; ;; https://emacs.stackexchange.com/questions/44880/use-package-bind-not-working-as-expected
;; (use-package ess-inf
;;   :bind
;;   (:map inferior-ess-r-mode-map
;;   	("<up>" . comint-previous-input)
;;   	("<down>" . comint-next-input)
;;   	("C-a" . sp-backward-sexp)
;;   	("C-d" . sp-forward-sexp)
;; 	("TAB" . ess-insert-assign)))

(use-package expand-region
  :ensure t
  :config
  (defhydra hydra-mark
    (:hint nil
	   :idle 3.00)
    "
Move    ^Small^      ^Containers^  ^Miscellaneous^
^^^^^^-----------------------------------------------
Normal  _p_ Point    _q_ In ''     _u_       Url
        _w_ Word     _Q_ Out ''    _c_       Comment
        _s_ Symbol   _i_ In Pair   _t_       In Tag
        _S_ Pre+sym  _o_ Out Pair  _T_       Out Tag
        _c_ Call     _b_ Block     _C-SPC_   Expand
        _a_ Accessor _f_ Function  _C-M-SPC_ Contract
    "
    ("M-w"     previous-line)
    ("M-s"     next-line)
    ("M-a"     left-char)
    ("M-d"     right-char)
    ("<up>"    previous-line-message)
    ("<down>"  next-line-message)
    ("<left>"  left-char-message)
    ("<right>" right-char-message)
    ("C-w"     beginning-of-buffer)
    ("C-s"     end-of-buffer)
    ("C-a"     sp-backward-sexp)
    ("C-d"     sp-forward-sexp)
    ("M-q"     sp-backward-delete-word)
    ("C-q"     (lambda() (interactive) (custom-mc-select-previous) (hydra-mc/body)) :exit t)
    ("C-M-q"   sp-beginning-of-sexp)
    ("M-e"     sp-delete-word)
    ("C-e"     (lambda() (interactive) (custom-mc-select-next) (hydra-mc/body)) :exit t)
    ("C-M-e"   sp-end-of-sexp)
    ("p"       (lambda() (interactive) (deactivate-mark) (push-mark-command (point))))
    ("w"       er/mark-word)
    ("s"       er/mark-symbol)
    ("S"       er/mark-symbol-with-prefix)
    ("c"       er/mark-method-call)
    ("a"       er/mark-next-accessor)
    ("q"       er/mark-inside-quotes)
    ("Q"       er/mark-outside-quotes)
    ("i"       er/mark-inside-pairs)
    ("o"       er/mark-inside-pairs)
    ("b"       mark-sexp)
    ("d"       er/mark-defun)
    ("f"       er/mark-defun)
    ("u"       er/mark-url)
    ("c"       er/mark-comment)
    ("t"       er/mark-inner-tag)
    ("T"       er/mark-outer-tag)
    ("k"       avy-goto-char-timer)
    ("l"       avy-goto-line)
    ("C-f"     swiper)
    ("e"       exchange-point-and-mark)
    ("C-SPC"   (er/expand-region 1))
    ("C-M-SPC" (er/contract-region 1))
    ("C-z"     undo)
    ("r"       (lambda() (interactive) (deactivate-mark) (rectangle-mark-mode) (hydra-rectangle-mark/body)) :exit t)
    ("C-c"     (lambda() (interactive) (kill-ring-save (region-beginning) (region-end)) (deactivate-mark)) :exit t)
    ("C-x"     (lambda() (interactive) (kill-region (region-beginning) (region-end))) :exit t)
    ("C-l"     copy-lines :exit t)
    ("C-M-d"   duplicate-line-or-region :exit t))

  (defhydra hydra-rectangle-mark
    (:hint nil
	   :idle 3.00)
    "
Move    ^Commands^
^^^^-------------------------
Normal  _s_ String _d_ Delete
        _c_ Copy   _x_ Cut
        _v_ Paste  _o_ Open
        _C_ Close
    "
    ("M-w"     previous-line)
    ("M-s"     next-line)
    ("M-a"     left-char)
    ("M-d"     right-char)
    ("<up>"    previous-line-message)
    ("<down>"  next-line-message)
    ("<left>"  left-char-message)
    ("<right>" right-char-message)
    ("C-w"     beginning-of-buffer)
    ("C-s"     end-of-buffer)
    ("C-a"     sp-backward-sexp)
    ("C-d"     sp-forward-sexp)
    ("M-q"     sp-backward-delete-word)
    ("C-q"     scroll-down)
    ("M-e"     sp-delete-word)
    ("C-e"     scroll-up)
    ("s"       string-rectangle :exit t)
    ("c"       copy-rectangle-as-kill :exit t)
    ("v"       yank-rectangle :exit t)
    ("C"       close-rectangle)
    ("d"       delete-rectangle :exit t)
    ("x"       kill-rectangle :exit t)
    ("o"       open-rectangle)
    ("k"       avy-goto-char-timer)
    ("e"       exchange-point-and-mark)
    ("C-z"     undo)
    ("r"       (lambda() (interactive) (deactivate-mark) (push-mark-command (point)) (hydra-mark/body)) :exit t)))

(use-package fill-column-indicator
  :ensure t
  :config
  ;; Set fci color to red
  (setq fci-rule-color "red")
  ;; Set fci to 80th column
  (setq fci-rule-column 80)
  ;; Set fci width to 1
  (setq fci-rule-width 1)

  :bind
  (("C-c i" . fci-mode)))

;; Joke package!
;; (use-package fireplace
;;   :ensure t)

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init (global-flycheck-mode)
  :config
  ;; ;; codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html#install_eslint_and_babel
  ;; (setq-default flycheck-disabled-checkers
  ;; 		(append flycheck-disabled-checkers
  ;; 			'(javascript-jshint)))

  ;; (flycheck-add-mode 'javascript-eslint 'js2-mode)
  ;; (flycheck-add-mode 'javascript-eslint 'js2-jsx-mode)

  ;; ;; This may not be necessary
  ;; (setq-default flycheck-disabled-checkers
  ;; 		(append flycheck-disabled-checkers
  ;; 			'(json-jsonlist)))
  
  (setq-default flycheck-temp-prefix ".flycheck"))

(use-package flycheck-rust
  :ensure t
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

;; ;; Try to set c++ standard to c++11
;; (add-hook 'c++-mode-hook
;;           (lambda()
;;             (setq flycheck-gcc-language-standard "c++11"))))

(use-package goto-chg
  :ensure t
  :bind
  ("C-<" . goto-last-change)
  ("C->" . goto-last-change-reverse))

;; (use-package haskell-mode
;;   :ensure t)

;; (use-package helm
;;   :ensure t
;;   :diminish helm-mode
;;   :config
;;   ;; ???
;;   (setq helm-ff-file-name-history-use-recentf t)
;;   ;; ???
;;   (setq helm-move-to-line-cycle-in-source t)
;;   ;; Set helm buffer to split current window
;;   (setq helm-split-window-in-side-p t)
;;   ;; Turn on helm-mode
;;   (helm-mode 1)
;;   ;; Allow helm buffer to auto-resize
;;   (helm-autoresize-mode 1)
;;   ;; Set helm-grep to default to ack-grep
;;   (when (executable-find "ag")
;;     (setq helm-grep-ag-command "ag --line-numbers -S --hidden --color --color-match '31;43' --nogroup %s %s %s")
;;     (setq helm-grep-ag-pipe-cmd-switches '("--color-match '31;43'")))
;;   ;; Add man pages to sources for helm-man-woman
;;   (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
;;   ;; Add sources for helm-mini
;;   (setq helm-mini-default-sources '(helm-source-buffers-list
;;                                     helm-source-recentf
;;                                     helm-source-bookmarks
;;                                     helm-source-buffer-not-found))

;;   :bind
;;   (("M-x" . helm-M-x)
;;    ("C-p" . helm-do-ag)
;;    ("C-h a" . helm-apropos)
;;    ("C-h f" . helm-man-woman)
;;    ("C-h s" . helm-surfraw)
;;    :map helm-find-files-map
;;    ("M-w" . helm-previous-line)
;;    ("M-a" . helm-find-files-up-one-level)
;;    ("<C-tab>" . helm-find-files-up-one-level)
;;    ("M-s" . helm-next-line)
;;    ("M-d" . helm-execute-persistent-action)
;;    ("<tab>" . helm-execute-persistent-action)
;;    ("<C-return>" . helm-select-action)
;;    :map helm-buffer-map
;;    ("C-S-k" . helm-buffer-run-kill-buffers)
;;    :map helm-map
;;    ("M-w" . helm-previous-line)
;;    ("M-a" . helm-find-files-up-one-level)
;;    ("<C-tab>" . helm-find-files-up-one-level)
;;    ("M-s" . helm-next-line)
;;    ("M-d" . helm-execute-persistent-action)
;;    ("<tab>" . helm-execute-persistent-action)
;;    ("<C-return>" . helm-select-action)))

;; (use-package helm-ag
;;   :ensure t)

;; (use-package helm-company
;;   :ensure t)

;; (use-package helm-descbinds
;;   :ensure t
;;   :config
;;   ;; Enable helm-descbinds-mode
;;   (helm-descbinds-mode)

;;   :bind
;;   (("C-h b" . helm-descbinds)))

;; (use-package helm-flx
;;   :ensure t
;;   :config
;;   (setq helm-flx-for-helm-find-files t)
;;   (setq helm-flx-for-helm-locate t)
;;   (helm-flx-mode 1))

;; (use-package helm-mt
;;   :ensure t
;;   :bind
;;   (("C-t" . helm-mt)
;;    :map term-raw-map
;;    ("C-t" . helm-mt)))

;; (use-package helm-projectile
;;   :ensure t
;;   :config
;;   ;; Enable projectile with helm
;;   (helm-projectile-on))

;; Check if works again
;; (use-package helm-swoop
;;   :ensure t
;;   :config
;;   ;; ;; Disable pre-input
;;   ;; (setq helm-swoop-pre-input-function
;;   ;; 	(lambda () ""))
;;   ;; Set helm-swoop buffer to split window vertically
;;   (setq helm-swoop-split-direction (quote split-window-vertically))
;;   ;; Split current window when multiple windows are open
;;   (setq helm-swoop-split-with-multiple-windows t)

;;   :bind
;;   (("C-f" . helm-swoop)
;;    ("C-S-f" . helm-multi-swoop-current-mode)
;;    :map helm-swoop-map
;;    ("C-f" . helm-multi-swoop-current-mode-from-helm-swoop)
;;    ("C-S-f" . helm-multi-swoop-all-from-helm-swoop)
;;    ("M-w" . helm-previous-line)
;;    ("M-s" . helm-next-line)
;;    ("C-e" . helm-swoop-edit)
;;    :map helm-multi-swoop-map
;;    ("C-f" . helm-multi-swoop-all)
;;    ("M-w" . helm-previous-line)
;;    ("M-s" . helm-next-line)
;;    ("C-e" . helm-multi-swoop-edit)))

;; (use-package helm-xref
;;   :ensure t
;;   :config
;;   (setq xref-show-xrefs-function 'helm-xref-show-xrefs))

(use-package hide-lines
  :ensure t)

(use-package hl-line
  :config
  (global-hl-line-mode)
  (set-face-background 'hl-line "#c4cedd"))

(use-package hungry-delete
  :ensure t)

(use-package hydra
  :ensure t)

(use-package iedit
  :ensure t
  :bind
  (("C-;" . iedit-mode)))

;; (use-package intero
;;   :ensure t
;;   :config
;;   (intero-global-mode 1))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (setq ivy-wrap t)
  (setq ivy-action-wrap t)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (ivy-mode 1)
  :bind
  (("C-c C-M-r" . ivy-resume)
   :map ivy-switch-buffer-map
   ("C-S-k" . ivy-switch-buffer-kill)
   :map ivy-minibuffer-map
   ("C-r" . ivy-reverse-i-search)
   ("C-M-s" . ivy-next-line-and-call)
   ("M-d" . ivy-dispatching-done)
   ("C-M-d" . ivy-dispatching-call)
   ("M-l" . ivy-avy)
   ("TAB" . ivy-alt-done)
   ("<C-tab>" . ivy-backward-kill-word)))

;; (use-package js2-mode
;;   :ensure t
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;;   (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
;;   ;; lsp not activating rn for js2-jsx-mode for some reason
;;   (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-mode))

;;   (setq js2-basic-offset 2)

;;   (setq js2-mode-hook
;; 	'(lambda () (progn
;; 		      (set-variable 'indent-tabs-mode nil)))))

(use-package json-mode
  :ensure t
  :bind
  (:map json-mode-map
        ("M-f" . jsons-print-path)))

;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :hook (prog-mode . lsp)
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil)

  (set-face-attribute 'lsp-face-highlight-read nil
		      :background "#555599")
  (set-face-attribute 'lsp-face-highlight-textual nil
		      :background "#555599")
  (set-face-attribute 'lsp-face-highlight-write nil
		      :background "#008787")
  
  (use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode
    :config
    ;; Enable doc
    (setq lsp-ui-doc-enable t)
    ;; Enable peek
    (setq lsp-ui-peek-enable t)
    ;; Enable sideline
    (setq lsp-ui-sideline-enable t)
    
    :bind
    (:map lsp-ui-mode-map
          ("C-M-S-d" . #'lsp-ui-peek-find-definitions)
          ("C-M-r" . #'lsp-ui-peek-find-references)))

  (use-package company-lsp
    :commands company-lsp))

(use-package magit
  :ensure t
  :bind
  ("M-g" . magit-status)
  (:map magit-status-mode-map
	("M-w" . previous-line)))

;; ;; https://www.emacswiki.org/emacs/MultiTerm
;; ;; http://rawsyntax.com/blog/learn-emacs-zsh-and-multi-term
;; (use-package multi-term
;;   :ensure t
;;   :config
;;   ;; Set zsh as default shell
;;   (setq multi-term-program "/usr/bin/zsh")
;;   ;; Set term buffer max size to 8192 lines
;;   (add-hook 'term-mode-hook
;;             (lambda()
;;               (setq term-buffer-maximum-size 8192)))

;;   ;; :bind term-raw-map/term-mode-map does not work
;;   ;; These custom variables must be set instead
;;   (setq term-unbind-key-list
;; 	'("C-c" "C-x" "C-h" "C-p"))

;;   (setq term-bind-key-alist
;; 	'(("C-M-c" . term-kill-subjob)
;; 	  ("M-w" . previous-line)
;; 	  ("M-s" . next-line)
;; 	  ("C-f" . helm-occur)
;; 	  ;; ("C-m" . term-send-raw)
;; 	  ;; ("M-f" . term-send-forward-word)
;; 	  ;; ("M-b" . term-send-backward-word)
;; 	  ;; ("M-o" . term-send-backspace)
;; 	  ;; ("M-p" . term-send-up)
;; 	  ;; ("M-n" . term-send-down)
;; 	  ;; ("M-M" . term-send-forward-kill-word)
;; 	  ;; ("M-N" . term-send-backward-kill-word)
;; 	  ;; ("M-," . term-send-input)
;; 	  ;; ("M-." . comint-dynamic-complete))
;; 	  ("C-c <left>" . multi-term-prev)
;; 	  ("C-c <right>" . multi-term-next)
;; 	  ;; ("C-c C-c" . term-copy-ish?)
;; 	  ;; ("C-c C-x" . term-cut-ish?)
;; 	  ("C-c C-v" . term-paste)
;; 	  ("M-x" . helm-M-x)
;; 	  ("<escape> <escape>" . term-send-esc)
;; 	  ("C-r" . term-send-reverse-search-history))))

(use-package multiple-cursors
  :ensure t
  :config
  (add-to-list 'mc/cmds-to-run-for-all 'custom-delete)
  (add-to-list 'mc/cmds-to-run-once 'hydra-mark/lambda-C-q-and-exit)
  (add-to-list 'mc/cmds-to-run-once 'hydra-mark/lambda-C-e-and-exit)
  
  (defhydra hydra-mc
    (:hint nil
	   :idle 3.00)
    "
( ͡° ͜ʖ ͡°)
    "
    ("<up>"    mc/mmlte--up)
    ("<down>"  mc/mmlte--down)
    ("<left>"  mc/mmlte--left)
    ("<right>" mc/mmlte--right)
    ("M-q"     mc/cycle-backward)
    ("C-q"     custom-mc-select-previous)
    ("C-M-q"   mc/unmark-previous-like-this)
    ("M-e"     mc/cycle-forward)
    ("C-e"     custom-mc-select-next)
    ("C-M-e"   mc/unmark-next-like-this))

  (add-to-list 'mc/cmds-to-run-once 'hydra-mc/body)
  (add-to-list 'mc/cmds-to-run-once 'hydra-mc/mc/mmlte--up)
  (add-to-list 'mc/cmds-to-run-once 'hydra-mc/mc/mmlte--down)
  (add-to-list 'mc/cmds-to-run-once 'hydra-mc/mc/mmlte--left)
  (add-to-list 'mc/cmds-to-run-once 'hydra-mc/mc/mmlte--right)
  (add-to-list 'mc/cmds-to-run-once 'hydra-mc/mc/cycle-backward)
  (add-to-list 'mc/cmds-to-run-once 'hydra-mc/custom-mc-select-previous)
  (add-to-list 'mc/cmds-to-run-once 'hydra-mc/mc/unmark-previous-like-this)
  (add-to-list 'mc/cmds-to-run-once 'hydra-mc/mc/cycle-forward)
  (add-to-list 'mc/cmds-to-run-once 'hydra-mc/custom-mc-select-next)
  (add-to-list 'mc/cmds-to-run-once 'hydra-mc/mc/unmark-next-like-this))

(use-package org
  :ensure t
  :config
  (setq org-export-preserve-breaks t)
  (setq org-catch-invisible-edits 1)
  (setq org-startup-folded 1)
  (defhydra hydra-org
    (:hint nil
	   :idle 3.00)
    "
^Kill^      ^Narrow^
^^^^-------------------
_C-x_ Cut   _b_ Block
_C-c_ Copy  _e_ Element
_C-v_ Paste _s_ Subtree
_C-r_ Ring  _w_ Widen
    "
    ("M-w"          previous-line)
    ("M-s"          next-line)
    ("M-a"          left-char)
    ("M-d"          right-char)
    ("<up>"         previous-line-message)
    ("<down>"       next-line-message)
    ("<left>"       left-char-message)
    ("<right>"      right-char-message)
    ("C-w"          beginning-of-buffer)
    ("C-s"          end-of-buffer)
    ("C-a"          sp-backward-sexp)
    ("C-d"          sp-forward-sexp)
    ("M-q"          sp-backward-delete-word)
    ("C-q"          scroll-down)
    ("M-e"          sp-delete-word)
    ("C-e"          scroll-up)
    ("<M-return>"   org-insert-heading)
    ("<C-return>"   org-insert-heading-respect-content)
    ("<M-S-return>" org-insert-todo-heading)
    ("<C-S-return>" org-insert-todo-heading-respect-content)
    ("<tab>"        org-cycle)
    ("<M-left>"     org-do-promote)
    ("<M-right>"    org-do-demote)
    ("<C-left>"     org-promote-subtree)
    ("<C-right>"    org-demote-subtree)
    ("<C-up>"       org-move-subtree-up)
    ("<C-down>"     org-move-subtree-down)
    ("C-z"          undo)
    ("C-x"          org-cut-subtree)
    ("C-c"          org-copy-subtree)
    ("C-v"          org-yank)
    ("b"            org-narrow-block)
    ("e"            org-narrow-element)
    ("s"            org-narrow-subtree)
    ("w"            widen)
    ;; ("l" org-agenda-list)
    ;; ("a" org-agenda-file-to-front)
    ;; ("r" org-remove-file)
    ;; ("v" hydra-org-agenda/body :exit t)
    ("C-r"          counsel-yank-pop :exit t))

  ;;   (defhydra hydra-org-agenda
  ;;     (:hint nil
  ;;      :idle 3.00)
  ;;     "
  ;; ^Actions^    ^Period^
  ;; ^^^^------------------
  ;; _T_ Todo     _d_ Day
  ;; _t_ Tags     _w_ Week
  ;; _l_ Timeline _m_ Month
  ;; _s_ Search   _y_ Year
  ;;     "
  ;;     ("T" (lambda() (interactive) (org-todo-list) (hydra-org/body)) :exit t)
  ;;     ("t" (lambda() (interactive) (org-tags-view) (hydra-org/body)) :exit t)
  ;;     ("l" (lambda() (interactive) (org-timeline) (hydra-org/body)) :exit t)
  ;;     ("s" (lambda() (interactive) (org-search-view) (hydra-org/body)) :exit t)
  ;;     ;; ("d" org-ag :exit t)
  ;;     ;; ("w"  :exit t)
  ;;     ;; ("m"  :exit t)
  ;;     ;; ("y"  :exit t)
  ;;     )
  
  :bind
  (:map org-mode-map
	("C-M-w" . org-custom-close)
	("C-a" . sp-backward-sexp)
	("M-a" . left-char)
	("<M-return>" . org-insert-heading)
	("<C-return>" . org-insert-heading-respect-content)
	("<M-S-return>" . org-insert-todo-heading)
	("<C-S-return>" . org-insert-todo-heading-respect-content)
	("<tab>" . org-cycle)
	("<M-left>" . org-do-promote)
	("<M-right>" . org-do-demote)
	("<C-left>" . org-promote-subtree)
	("<C-right>" . org-demote-subtree)
	("<C-up>" . org-move-subtree-up)
	("<C-down>" . org-move-subtree-down)
	("M-SPC" . org-mark-element)
	("C-M-SPC" . org-mark-subtree)))

;; https://thraxys.wordpress.com/2016/01/14/pimp-up-your-org-agenda/
(use-package org-bullets
  :ensure t
  :init
  (setq org-bullets-bullet-list '("🥑" "🍌" "🍒" "🍇" "🥝" "🍋" "🍑" "🍓" "🍅" "🍉"))
  (setq org-todo-keywords
	'((sequence "👋 TODO"
		    "👌 DOING"
		    "👍 DONE"
		    "⏱ WAITING"
		    "❌ CANCELLED")))
  
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package origami
  :ensure t
  :bind
  (:map origami-mode-map
	("<C-tab>" . origami-recursively-toggle-node)
	("<C-S-iso-lefttab>" . origami-open-all-nodes)
	("<C-M-tab>" . origami-close-all-nodes)))

;; (use-package poly-markdown
;;   :ensure t
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.Rmd\\'" . poly-markdown-mode))
  
;;   ;; :bind
;;   ;; not working?
;;   ;; (:map poly-markdown-mode-map
;;   ;; 	("M-n M-e" . ess-eval-line-and-step)
;;   ;; 	("M-n C-e" . rmd-send-buffer)
;;   ;; 	)
;;   )

(use-package powerline
  :ensure t
  :config
  ;; Set powerline to default theme
  (powerline-default-theme))

(use-package projectile
  :ensure t
  :config
  ;; Enable projectile-mode
  (projectile-mode)
  ;; Set ivy as projectile completion system
  (setq projectile-completion-system 'ivy)

  (defhydra hydra-projectile-other-window
    (:hint nil
	   :idle 3.00)
    "
^Commands^
^^---------
_ff_ File
_fd_ Dwim
_d_  Dir
_b_  Buffer
    "
    ("ff"  projectile-find-file-other-window)
    ("fd"  projectile-find-file-dwim-other-window)
    ("d"   projectile-find-dir-other-window)
    ("b"   projectile-switch-to-buffer-other-window))

  (defhydra hydra-projectile
    (:hint nil
	   :idle 3.00)
    "
Project: %(projectile-project-root)

^File^      ^Search^  ^Buffer^   ^Cache^    ^Project^
^^^^^^^^^^------------------------------------------------
_ff_ Find   _a_ Ag    _b_ Switch _c_ Clear  _C-p_ Switch
_fd_ Dwim   _g_ Gtags _k_ Kill   _A_ Add    _C-c_ Compile
_fc_ Cwd    _o_ Occur          ^^_R_ Remove _C-r_ Run
_r_  Recent                  ^^^^_X_ Clean  _p_   Prev err
_d_  Dir                              ^^^^^^_n_   Next err
    "
    ("ff"  projectile-find-file)
    ("fd"  projectile-find-file-dwim)
    ("fc"  projectile-find-file-in-directory)
    ("r"   projectile-recentf)
    ("d"   projectile-find-dir)
    ("a"   projectile-ag)
    ("g"   ggtags-update-tags)
    ("o"   projectile-multi-occur)
    ("b"   projectile-switch-to-buffer)
    ("k"   projectile-kill-buffers)
    ("c"   projectile-invalidate-cache)
    ("A"   projectile-add-known-project)
    ("R"   projectile-remove-known-project)
    ("X"   projectile-cleanup-known-projects)
    ("C-p" projectile-switch-project :exit t)
    ("C-c" projectile-compile-project)
    ("C-r" projectile-run-project)
    ("p"   previous-error)
    ("n"   next-error)
    ("C-o" hydra-projectile-other-window/body :exit t)))

(use-package protobuf-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :init
  ;; ???
  (require 'rainbow-delimiters)

  :config
  ;; Turn on rainbow-delimiters mode for programming modes
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rust-mode
  :ensure t
  :interpreter
  ("rs" . rust-mode)
  :config
  (projectile-register-project-type
   'rust-cargo '("Cargo.toml")
   :compilation-dir "."
   :compile "cargo build"
   :run "cargo run"
   :test "cargo test"))

(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :init
  ;; Configure smartparens
  (require 'smartparens-config)

  :config
  ;; Turn on smartparens globally
  (smartparens-global-mode 1)
  ;; Highlight matching parentheses
  (show-smartparens-global-mode 1)
  ;; Turn on "strict mode" globally
  (smartparens-global-strict-mode 1)
  
  :bind
  (("C-a" . sp-backward-sexp)
   ("C-d" . sp-forward-sexp)
   ("M-q" . sp-backward-delete-word)
   ("M-e" . sp-delete-word)))

(use-package spacemacs-theme
  :ensure t
  :defer t
  :init
  (defun load-spacemacs-light-for-new-frames (&optional frame)
    "Load spacemacs-light theme for new frames."
    (interactive)
    (with-selected-frame (or frame (selected-frame))
      (load-theme 'spacemacs-light t)))

  (defun load-spacemacs-dark-for-new-frames (&optional frame)
    "Load spacemacs-dark theme for new frames."
    (interactive)
    (with-selected-frame (or frame (selected-frame))
      (load-theme 'spacemacs-dark t)))
  
  (if (daemonp)
      (add-hook 'after-make-frame-functions
		'load-spacemacs-dark-for-new-frames))
  (load-theme 'spacemacs-dark t)
  (set-face-attribute 'default nil :background "#fff4ee")
  (set-face-background 'font-lock-comment-face "#f5eae5"))

(use-package swiper
  :ensure t
  :config
  ;; Does this work? I want swiper-mc to then call hydra-mc/body
  (delete 'swiper-mc mc/cmds-to-run-for-all)
  (add-to-list 'mc/cmds-to-run-once 'swiper-mc)
  (add-to-list 'mc/cmds-to-run-once 'swiper-mc-custom)
  :bind
  (("C-f" . swiper)
   ("C-S-f" . swiper-all)
   :map swiper-map
   ("M-l" . swiper-avy)
   ("C-;" . swiper-mc-custom)
   :map swiper-all-map
   ("M-l" . swiper-avy)
   ("C-;" . swiper-mc-custom)))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1)

  (setq undo-tree-auto-save-history t)
  (setq undo-tree-enable-undo-in-region t)
  (setq undo-tree-visualizer-diff t)

  (setq undo-tree-history-dir
	(let ((dir (concat user-emacs-directory
                           "undo-tree-history/")))
          (make-directory dir :parents) dir))
  (setq undo-tree-history-directory-alist `(("." . , undo-tree-history-dir)))

  (add-hook 'write-file-functions #'undo-tree-save-history-hook)
  (add-hook 'find-file-hook #'undo-tree-load-history-hook))

(use-package vlf
  :ensure t
  :config
  (require 'vlf-setup)
  (eval-after-load "vlf"
    '(define-key vlf-prefix-map (kbd "C-x C-v") vlf-mode-map)))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode)))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  :bind
  (("<C-tab>" . yas-expand)
   ("<M-tab>" . yas-next-field)
   ("<C-M-tab>" . company-yasnippet)
   :map yas-keymap
   ("TAB" . nil)
   ("<tab>" . nil)
   :map yas-minor-mode-map
   ("TAB" . nil)
   ("<tab>" . nil)))

(use-package yasnippet-snippets
  :ensure t)

(use-package zzz-to-char
  :ensure t
  :config
  (setq zzz-to-char-reach 800)
  :bind
  (("M-z" . zzz-to-char)))

;; Set custom functions
(defun yank-pop-reverse ()
  "Invoke 'yank-pop' with -1 argument."
  (interactive)
  (yank-pop -1))

;; https://www.emacswiki.org/emacs/DuplicateStartOfLineOrRegion
(defun duplicate-line-or-region ()
  "Duplicate region if region is marked, otherwise duplicate line."
  (interactive)
  (if mark-active
      (duplicate-region)
    (if (string-match-p "^[[:space:]]*$" (buffer-substring (line-beginning-position) (line-end-position)))
	(copy-from-above-command)
      (duplicate-line))))

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
(global-set-key
 [f11]
 '(lambda()
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

(defun indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (indent-buffer)
        (message "Indented buffer.")))))

;; https://www.emacswiki.org/emacs/AutoIndentation
(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

(defun left-char-message ()
  "Execute 'left-char' with reminder not to use the left arrow key."
  (interactive)
  ;; (left-char)
  (message "%s" "Please use M-a instead of the left arrow key."))

(defun right-char-message ()
  "Execute 'right-char' with reminder not to use the right arrow key."
  (interactive)
  ;; (right-char)
  (message "%s" "Please use M-d instead of the right arrow key."))

(defun previous-line-message ()
  "Execute 'previous-line' with reminder not to use the up arrow key."
  (interactive)
  ;; (previous-line)
  (message "%s" "Please use M-w instead of the up arrow key."))

(defun next-line-message ()
  "Execute 'next-line' with reminder not to use the down arrow key."
  (interactive)
  ;; (next-line)
  (message "%s" "Please use M-s instead of the down arrow key."))

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun parenthesize-to-beginning-of-line ()
  "Select current position to beginning of line as region and insert parentheses."
  (interactive)
  (insert ")")
  (set-mark (point))
  (beginning-of-line)
  (insert "(")
  (exchange-point-and-mark))

(defun parenthesize-to-end-of-line ()
  "Select current position to end of line as region and insert parentheses."
  (interactive)
  (insert "(")
  (set-mark (point))
  (end-of-line)
  (insert ")")
  (exchange-point-and-mark))

(defun parenthesize-end-of-line ()
  "Insert parentheses at end of line."
  (interactive)
  (set-mark (point))
  (end-of-line)
  (insert ")")
  (exchange-point-and-mark))

(defun double-quotes-to-end-of-line ()
  "Select current position to end of line as region and insert double quotes."
  (interactive)
  (insert "\"")
  (set-mark (point))
  (end-of-line)
  (insert "\"")
  (exchange-point-and-mark))

(defun single-quotes-to-end-of-line ()
  "Select current position to end of line as region and insert single quotes."
  (interactive)
  (insert "'")
  (set-mark (point))
  (end-of-line)
  (insert "'")
  (exchange-point-and-mark))

(defun square-brackets-to-end-of-line ()
  "Select current position to end of line as region and insert square brackets."
  (interactive)
  (insert "[")
  (set-mark (point))
  (end-of-line)
  (insert "]")
  (exchange-point-and-mark))

(defun angular-brackets-to-end-of-line ()
  "Select current position to end of line as region and insert angular brackets."
  (interactive)
  (insert "<")
  (set-mark (point))
  (end-of-line)
  (insert ">")
  (exchange-point-and-mark))

(defun custom-delete ()
  "Delete with custom (more complex) behavior.  See function body for more information."
  (interactive)
  (if mark-active
      ;; If region is active,
      ;; then delete region
      (if (eq (line-number-at-pos (region-beginning)) (line-number-at-pos (region-end)))
	  ;; If region contained in single line,
	  ;; then sp-delete-region
	  (sp-delete-region (region-beginning) (region-end))
	;; If region contained in multiple lines,
	;; then regular delete-region
	(delete-region (region-beginning) (region-end)))
    (if (or
	 (sp--looking-back (sp--get-opening-regexp (sp--get-pair-list-context 'navigate)))
	 (sp--looking-back (sp--get-closing-regexp (sp--get-pair-list-context 'navigate))))
	;; If point is after opening or closing pair,
	;; then delete opening and closing pair.
	(sp-backward-unwrap-sexp)
      (if (sp-point-in-blank-line)
	  ;; If current line is empty or only whitespace characters,
	  ;; then delete consecutive empty surrounding lines if they exist,
	  ;; otherwise delete current line.
	  (delete-blank-lines)
	(if (string-match-p "[^[:space:]]+" (buffer-substring (line-beginning-position) (point)))
	    ;; If line up to point contains at least one non-whitespace character,
	    ;; then delete non-whitespace character previous to point if it exists,
	    ;; otherwise hungrily delete whitespace characters backward
	    (hungry-delete-backward 1)
	  (let ((original-line (string-to-number (format-mode-line "%l")))
		(original-column (string-to-number (format-mode-line "%c"))))
	    (forward-line -1)
	    (if (string-match-p "^[[:space:]]*$" (buffer-substring (line-beginning-position) (line-end-position)))
		;; If line up to point is empty or only whitespace characters and previous line, Line P, is empty,
		;; then delete consecutive empty lines previous to Line P if they exist,
		;; otherwise delete Line P
		(progn
		  (goto-char (point-max))
		  (let ((original-number-of-lines (string-to-number (format-mode-line "%l"))))
		    (goto-char (point-min))
		    (forward-line (- original-line 2))
		    (delete-blank-lines)
		    (goto-char (point-max))
		    (let ((new-number-of-lines (string-to-number (format-mode-line "%l"))))
		      (goto-char (point-min))
		      (forward-line (- original-line (- original-number-of-lines new-number-of-lines) 1))
		      (right-char original-column))))
	      ;; If line up to point is empty or only whitespace characters and previous line is non-empty,
	      ;; then hungrily delete whitespace characters backward
	      (goto-char (point-min))
	      (forward-line (- original-line 1))
	      (right-char original-column)
	      (hungry-delete-backward 1))))))))

(defun call-hydra-mark ()
  "Call 'push-mark-command' then 'hydra-mark/body'."
  (interactive)
  (push-mark-command (point))
  (hydra-mark/body))

;; https://www.emacswiki.org/emacs/CopyingWholeLines
(defun copy-lines (arg)
  "Copy whole lines.  If region is active, copy its whole lines (even if whole line is not in region).  Otherwise, copy ARG lines.  If last command was copy-lines, append lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
	(end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
	  (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
	(setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-lines)
	(kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(defun custom-mc-select-next ()
  "Multiple cursors with custom behavior."
  (interactive)
  (when mark-active
    (mc/mark-next-like-this 1)
    (mc/cycle-forward)))

(defun custom-mc-select-previous ()
  "Multiple cursors with custom behavior."
  (interactive)
  (when mark-active
    (mc/mark-previous-like-this 1)
    (mc/cycle-backward)))

(defun org-custom-close ()
  "Close current headings upward.  If last call was org-custom-close, close parent heading."
  (interactive)
  (if (eq last-command 'org-custom-close)
      (progn
	(outline-up-heading 1)
	(org-cycle))
    (outline-hide-leaves)))

(defun swiper-input ()
  "Either word at point or empty string if no symbol at point."
  (interactive)
  (if (symbol-at-point)
      (format "%s" (sexp-at-point))
    ""))

(defun swiper-mc-custom ()
  "Call swiper-mc then hydra-mc/body."
  (interactive)
  (hydra-mc/body)
  (swiper-mc))

(defun toggle-transparency ()
  "Toggle transparency on and off."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))

;; http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; https://stackoverflow.com/questions/40894202/execute-all-r-chunks-at-once-from-an-rmd-document
(defun rmd-send-buffer (arg)
  "Send all R code blocks in buffer to ess process.
With prefix ARG send regions above point."
  (interactive "P")
  (save-restriction
    (widen)
    (save-excursion
      (pm-map-over-spans
       'ess-eval-region
       (point-min)
       (if arg (point) (point-max))
       nil
       nil
       'R))))

;; Set advice (fix???)
;; http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
;; (defadvice find-file (after find-file-sudo activate)
;;   "Find file as root if necessary."
;;   (unless (and buffer-file-name
;;                (file-writable-p buffer-file-name))
;;     (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; https://unix.stackexchange.com/questions/48289/emacs-m-x-query-replace-wrap-around-the-document
(defadvice query-replace
    (around replace-wrap
	    (FROM-STRING TO-STRING &optional DELIMITED START END))
  "Execute 'query-replace', wrapping to the top of the buffer after you reach the bottom."
  (save-excursion
    (let ((start (point)))
      ad-do-it
      (beginning-of-buffer)
      (ad-set-args 4 (list (point-min) start))
      ad-do-it)))

;; Set non-package keybindings
;; Set overriding non-package keybindings
(bind-keys*
 ((kbd "C-q") . scroll-down-command)
 ((kbd "C-S-q") . switch-to-prev-buffer)
 ;; Check why behavior is "reversed" in some contexts
 ((kbd "C-M-q") . sp-beginning-of-sexp)
 ((kbd "C-e") . scroll-up-command)
 ((kbd "C-S-e") . switch-to-next-buffer)
 ((kbd "C-M-e") . sp-end-of-sexp)
 ((kbd "M-v") . yank-pop)
 ((kbd "C-M-v") . yank-pop-reverse)
 ((kbd "M-f") . goto-line)
 ((kbd "C-z") . undo)
 ((kbd "C-S-z") . undo-tree-redo)
 ((kbd "M-c") . comment-dwim)
 ((kbd "M-r") . query-replace-regexp)
 ((kbd "C-x <tab>") . indent-region-or-buffer)
 ;; ((kbd "C-;") . (lambda() (interactive) (if (not mark-active) (er/mark-word)) (mc/mark-all-like-this) (hydra-mc/body)))
 ((kbd "C-(") . parenthesize-to-beginning-of-line)
 ((kbd "C-)") . parenthesize-to-end-of-line)
 ((kbd "M-)") . parenthesize-end-of-line)
 ((kbd "C-\"") . double-quotes-to-end-of-line)
 ((kbd "C-'") . single-quotes-to-end-of-line)
 ((kbd "C-]") . square-brackets-to-end-of-line)
 ((kbd "C->") . angular-brackets-to-end-of-line)
 ((kbd "C-k") . kill-line)
 ((kbd "<C-S-escape>") . server-force-delete)
 ;; ((kbd "C-x C-f") . helm-find-files)
 ((kbd "C-x C-f") . counsel-find-file)
 ((kbd "C-x b") . ivy-switch-buffer)
 ((kbd "C-x r") . rename-buffer)
 ((kbd "C-x C-r") . rename-file-and-buffer)
 ((kbd "C-x d") . kill-this-buffer)
 ((kbd "C-x C-M-d") . delete-file-and-buffer)
 ((kbd "C-x C-S-w") . split-window-vertically)
 ((kbd "C-x C-S-s") . split-window-below)
 ((kbd "C-x C-S-a") . split-window-horizontally)
 ((kbd "C-x C-S-d") . split-window-right)
 ((kbd "C-x C-S-f") . delete-other-windows)
 ((kbd "C-c C-v") . yank-and-indent)
 ((kbd "C-c C-d") . duplicate-line-or-region)
 ((kbd "C-c C-r") . counsel-yank-pop)
 ((kbd "C-c C-p") . hydra-projectile/body)
 ((kbd "C-c C-c") . hydra-cargo/body))

;; Set non-overriding non-package keybindings
(global-set-key (kbd "M-w") 'previous-line)
(global-set-key (kbd "M-s") 'next-line)
(global-set-key (kbd "M-a") 'left-char)
(global-set-key (kbd "M-d") 'right-char)
(global-set-key (kbd "<up>") 'previous-line-message)
(global-set-key (kbd "<down>") 'next-line-message)
(global-set-key (kbd "<left>") 'left-char-message)
(global-set-key (kbd "<right>") 'right-char-message)
(global-set-key (kbd "C-w") 'beginning-of-buffer)
(global-set-key (kbd "C-s") 'end-of-buffer)
(global-set-key (kbd "C-S-w") (lambda() (interactive) (windmove-up)))
(global-set-key (kbd "C-S-s") (lambda() (interactive) (windmove-down)))
(global-set-key (kbd "C-S-a") (lambda() (interactive) (windmove-left)))
(global-set-key (kbd "C-S-d") (lambda() (interactive) (windmove-right)))
(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "C-r") 'query-replace)
(global-set-key (kbd "C-SPC") 'call-hydra-mark)
(global-set-key (kbd "<C-backspace>") 'hungry-delete-backward)
(global-set-key (kbd "<backspace>") 'custom-delete)
(global-set-key (kbd "C-f") (lambda () (interactive) (swiper (swiper-input))))
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "C-o") 'hydra-org/body)
(global-set-key (kbd "C-M-a") 'beginning-of-line)
(global-set-key (kbd "C-M-d") 'end-of-line)
(global-set-key (kbd "C-M-r") 'repeat-complex-command)
(global-set-key (kbd "C-l") 'copy-lines)
(global-set-key (kbd "<C-up>") 'upcase-dwim)
(global-set-key (kbd "<C-down>") 'downcase-dwim)
(global-set-key (kbd "M-DEL") 'backward-delete-char)
(global-set-key (kbd "C-M-S-e") 'eval-region)
(global-set-key (kbd "C-M-S-t") 'toggle-truncate-lines)

;; Set non-overriding non-package mode-specific bindings
;; (add-hook 'latex-mode-hook
;; 	  (define-key latex-mode-map (kbd "C-S-c") 'tex-compile))

(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; Set non-package major modes
;; e.g. (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

;; Set non-package hooks
(defun pdf-view-hook ()
  "Execute 'pdf-tools-install' when opening pdf files."
  (when (string= (file-name-extension buffer-file-name) "pdf")
    (pdf-tools-install)))
(add-hook 'find-file-hook 'pdf-view-hook)

(defun minibuffer-setup-avoid-gc-hook ()
  "Extend garbage collection threshold when loading buffer."
  (setq gc-cons-threshold most-positive-fixnum))

(defun minibuffer-exit-restore-gc-hook ()
  "Restore garbage collection threshold once done loading buffer."
  (setq gc-cons-threshold 800000))
(add-hook 'minibuffer-setup-hook #'minibuffer-setup-avoid-gc-hook)
(add-hook 'minibuffer-exit-hook #'minibuffer-exit-restore-gc-hook)

;; Enable emacsclient
(server-start)

;; ;; Report benchmark
;; (benchmark-init/show-durations-tabulated)

;; (evil mode 1)

(provide 'init)
;;; init.el ends here
