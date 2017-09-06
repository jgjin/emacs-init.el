;;; package --- Summary

;;; Commentary:
;;; I don't know what to put here, man.

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
(unless package-archive-contents
  (package-refresh-contents))

;; Turn on/off non-package modes
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(cua-selection-mode 1)

;; Set tabbing options
(setq c-default-style "linux"
      tab-width 8
      indent-tabs-mode t)

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
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(delete-old-versions t)
 '(inhibit-startup-screen t)
 '(kept-new-versions 6)
 '(kept-old-versions 2)
 '(org-agenda-files (quote ("~/.emacs.d/calender/test.org")))
 '(package-selected-packages
   (quote
    (origami flycheck-rust rust-mode iedit zzz-to-char yasnippet use-package spacemacs-theme smartparens scala-mode rainbow-delimiters powerline magit json-mode hungry-delete helm-swoop helm-projectile helm-mt helm-descbinds flycheck fill-column-indicator expand-region dumb-jump dashboard csv-mode company-quickhelp company-jedi company-irony-c-headers company-irony company-c-headers bm benchmark-init)))
 '(version-control t))

;; Set non-package faces (colors)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Set font size
(set-face-attribute 'default nil :height 240)

;; Enable use-package
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package avy
  :ensure t
  :config
  ;; Set package-related faces (colors)
  (set-face-foreground 'avy-lead-face "#4f97d7")
  (set-face-foreground 'avy-lead-face-0 "#2d9574")
  (set-face-foreground 'avy-lead-face-1 "#4f97d7")
  (set-face-foreground 'avy-lead-face-2 "#2d9574")

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
  (("C-b" . bm-toggle)
   ("C-M-b" . bm-next)
   ("M-b" . bm-previous)))

(use-package company
  :ensure t
  :diminish company-mode
  :config
  ;; Enable global-company-mode
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-irony
  :ensure t
  :config
  ;; Add C(++) to company
  (add-hook 'c-mode-hook (lambda()
                           (add-to-list 'company-backends 'company-irony)))
  (add-hook 'c++-mode-hook (lambda()
                             (add-to-list 'company-backends 'company-irony))))

(use-package company-irony-c-headers
  :ensure t
  :config
  ;; Add C(++) headers to company
  (add-hook 'c-mode-hook (lambda()
                           (add-to-list 'company-backends 'company-c-headers)))
  (add-hook 'c++-mode-hook (lambda()
                             (add-to-list 'company-backends
                                          'company-c-headers))))

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
;;                              (add-to-list 'company-backends 'company-tern))))

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
  ;; (setq dashboard-startup-banner "~/.emacs.d/banana.png")
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
  (("M-s" . next-line)))

;; (use-package dumb-jump
;;   :ensure t
;;   :config
;;   ;; Enable dumb-jump mode
;;   (dumb-jump-mode)
;;   ;; Enable dumb-jump with helm
;;   (setq dumb-jump-selector 'helm)
;;   ;; Force dumb-jump to use ag
;;   (setq dumb-jump-force-searcher 'ag)

;;   :bind
;;   (("C-c C-r" . dumb-jump-back)
;;    ("C-j" . dumb-jump-go-prefer-external)))

(use-package expand-region
  :ensure t
  :config
  (defhydra hydra-mark
    (:foreign-keys nil
     :hint nil)
    "
^Move^    ^Small^      ^Containers^  ^Miscellaneous^
^^^^^^-----------------------------------------------
Normal  _p_ Point    _q_ In ''     _u_       Url
        _w_ Word     _Q_ Out ''    _c_       Comment
        _s_ Symbol   _i_ In Pair   _t_       In Tag
        _S_ Pre+sym  _o_ Out Pair  _T_       Out Tag
        _c_ Call     _b_ Block     _C-SPC_   Expand
        _a_ Accessor _f_ Function  _C-M-SPC_ Contract
                   _r_ Rectangle
    "
    ("M-w" previous-line)
    ("M-a" left-char)
    ("M-s" next-line)
    ("M-d" right-char)
    ("<up>" previous-line-message)
    ("<left>" left-char-message)
    ("<down>" next-line-message)
    ("<right>" right-char-message)
    ("M-q" sp-beginning-of-sexp)
    ("M-e" sp-end-of-sexp)
    ("C-w" beginning-of-buffer)
    ("C-a" sp-backward-symbol)
    ("C-s" end-of-buffer)
    ("C-d" sp-forward-symbol)
    ("C-q" cua-scroll-down)
    ("C-e" cua-scroll-up)
    ("p" (lambda() (interactive) (deactivate-mark) (cua-set-mark)))
    ("w" er/mark-word)
    ("s" er/mark-symbol)
    ("S" er/mark-symbol-with-prefix)
    ("c" er/mark-method-call)
    ("a" er/mark-next-accessor)
    ("q" er/mark-inside-quotes)
    ("Q" er/mark-outside-quotes)
    ("i" er/mark-inside-pairs)
    ("o" er/mark-inside-pairs)
    ("b" mark-sexp)
    ("d" er/mark-defun)
    ("f" er/mark-defun)
    ("r" (lambda() (interactive) (deactivate-mark) (rectangle-mark-mode)))
    ("u" er/mark-url)
    ("c" er/mark-comment)
    ("t" er/mark-inner-tag)
    ("T" er/mark-outer-tag)
    ("C-SPC" (er/expand-region 1))
    ("C-M-SPC" (er/contract-region 1))
    ("C-z" undo)
    ("C-c" (lambda () (interactive) (kill-ring-save (region-beginning) (region-end)) (deactivate-mark) (hydra-kill-ring/body)) :exit t)
    ("C-x" (lambda () (interactive) (kill-region (region-beginning) (region-end)) (hydra-kill-ring/body)) :exit t)
    ("C-M-d" duplicate-line-or-region :exit t)
    ("O" hydra-org/body :exit t)))

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
  :init
  (require 'semantic/bovine/gcc)

  :config
  ;; Turn on global-flycheck-mode
  (add-hook 'after-init-hook #'global-flycheck-mode)
  ;; Try to set c++ standard to c++11
  (add-hook 'c++-mode-hook
            (lambda ()
              (setq flycheck-gcc-language-standard "c++11"))))

(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package helm
  :ensure t
  :diminish helm-mode
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
  ;; Set helm-grep to default to ack-grep
  (when (executable-find "ag")
    (setq helm-grep-ag-command "ag --line-numbers -S --hidden --color --color-match '31;43' --nogroup %s %s %s")
    (setq helm-grep-ag-pipe-cmd-switches '("--color-match '31;43'")))
  ;; Add man pages to sources for helm-man-woman
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  ;; Add sources for helm-mini
  (setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-bookmarks
                                    helm-source-buffer-not-found))
  
  :bind
  (("M-x" . helm-M-x)
   ("C-p" . helm-locate)
   ("C-h a" . helm-apropos)
   ("C-h f" . helm-man-woman)
   ("C-h s" . helm-surfraw)
   :map helm-find-files-map
   ("M-w" . helm-previous-line)
   ("M-a" . helm-find-files-up-one-level)
   ("<C-tab>" . helm-find-files-up-one-level)
   ("M-s" . helm-next-line)
   ("M-d" . helm-execute-persistent-action)
   ("<tab>" . helm-execute-persistent-action)
   ("<C-return>" . helm-select-action)
   ("C-f" . helm-ff-run-grep-ag)
   :map helm-buffer-map
   ("C-S-d" . helm-buffer-run-kill-buffers)
   :map helm-map
   ("M-w" . helm-previous-line)
   ("M-a" . helm-find-files-up-one-level)
   ("<C-tab>" . helm-find-files-up-one-level)
   ("M-s" . helm-next-line)
   ("M-d" . helm-execute-persistent-action)
   ("<tab>" . helm-execute-persistent-action)
   ("<C-return>" . helm-select-action)))

(use-package helm-descbinds
  :ensure t
  :config
  ;; Enable helm-descbinds-mode
  (helm-descbinds-mode)

  :bind
  (("C-h b" . helm-descbinds)))

(use-package helm-mt
  :ensure t
  :bind
  (("C-t" . helm-mt)
   :map term-raw-map
   ("C-t" . helm-mt)))

(use-package helm-projectile
  :ensure t
  :config
  ;; Enable projectile with helm
  (helm-projectile-on))

(use-package helm-swoop
  :ensure t
  :config
  ;; Set first helm-swoop input as empty, subsequent searches use previous query
  (setq helm-swoop-pre-input-function
        (lambda nil
          (if (boundp (quote helm-swoop-pattern))
              helm-swoop-pattern "")))
  ;; Set helm-swoop buffer to split window vertically
  (setq helm-swoop-split-direction (quote split-window-vertically))
  ;; Split current window when multiple windows are open
  (setq helm-swoop-split-with-multiple-windows t)

  :bind
  (("C-f" . helm-swoop)
   ("C-S-f" . helm-multi-swoop-current-mode)
   :map helm-swoop-map
   ("C-f" . helm-multi-swoop-current-mode-from-helm-swoop)
   ("C-S-f" . helm-multi-swoop-all-from-helm-swoop)
   ("M-w" . helm-previous-line)
   ("M-s" . helm-next-line)
   ("C-e" . helm-swoop-edit)
   :map helm-multi-swoop-map
   ("C-f" . helm-multi-swoop-all)
   ("M-w" . helm-previous-line)
   ("M-s" . helm-next-line)
   ("C-e" . helm-multi-swoop-edit)))

(use-package hl-line
  :config
  (global-hl-line-mode))

(use-package hungry-delete
  :ensure t)

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-buffer-and-window
    (:foreign-keys nil
     :hint nil)
     "
^Navigate^     ^Window^      ^Split^          ^Buffer^
^^^^^^^^---------------------------------------------------
_M-w_ Scroll ↑ _C-w_ Move ↑  _C-M-w_ Split ↑  _b_   Buffers
_M-a_ Prev Buf _C-a_ Move ← _C-M-a_ Split ← _t_   Terminals
_M-s_ Scroll ↓ _C-s_ Move ↓  _C-M-s_ Split ↓  _C-f_ Files
_M-d_ Next Buf _C-d_ Move → _C-M-d_ Split → _RET_ Select
_M-f_ Find     _d_   Del
             _f_   Focus
    "
    ("M-w" scroll-down)
    ("M-a" previous-buffer)
    ("M-s" scroll-up)
    ("M-d" next-buffer)
    ("M-f" helm-swoop)
    ("C-w" (other-window -1))
    ("C-a" (other-window -1))
    ("C-s" (other-window 1))
    ("C-d" (other-window 1))
    ("d" delete-window)
    ("f" delete-other-windows)
    ("C-M-w" (lambda() (interactive) (split-window-below) (other-window -1)))
    ("C-M-a" (lambda() (interactive) (split-window-right) (other-window 1)))
    ("C-M-s" split-window-below)
    ("C-M-d" split-window-right)
    ("b" helm-mini)
    ("t" helm-mt)
    ("C-f" helm-find-files)
    ("RET" helm-maybe-exit-minibuffer)
    ("C-x" hydra-control-x-prefix/body :exit t))

  (defhydra hydra-control-x-prefix
    (:foreign-keys nil
     :hint nil
     :idle 0.25)
    "
^Buffer-Change^ ^Buffer-Modify^ ^Buffers^    ^Files^
^^^^^^^^-------------------------------------------------
_s_   Save      _r_   Ren Buf   _b_   List   _C-f_ Find
_M-f_ Search    _d_   Del Buf   _f_   Search _w_   Write
_C-=_ Zoom In   _C-r_ Ren F+B   _RET_ Select _i_   Insert
_C--_ Zoom Out  _C-d_ Del F+B   _h_   Helm
_C-a_ Mark All
    "
    ("C-s" save-buffer :exit t)
    ("C-c" save-buffers-kill-terminal :exit t)
    ("s" save-buffer)
    ("M-f" helm-swoop)
    ("C-=" text-scale-increase)
    ("C--" text-scale-decrease)
    ("C-a" mark-whole-buffer)
    ("r" rename-buffer)
    ("d" kill-this-buffer)
    ("C-r" rename-file-and-buffer)
    ("C-d" delete-file-and-buffer)
    ("b" helm-mini)
    ("f" helm-multi-swoop-all)
    ("RET" helm-maybe-exit-minibuffer)
    ("h" helm-resume)
    ("C-f" helm-find-files)
    ("w" write-file)
    ("i" insert-file)
    ("C-v" yank-and-indent :exit t)
    ("C-w" hydra-buffer-and-window/body :exit t)
    ("O" hydra-org/body :exit t))
  
  (defhydra hydra-kill-ring
    (:foreign-keys nil
     :hint nil
     :idle 0.25)
    "
^Actions^
^^---------
_C-v_ Paste
_C-d_ Duplicate
_C-r_ Ring
    "
    ("M-w" previous-line)
    ("M-a" left-char)
    ("M-s" next-line)
    ("M-d" right-char)
    ("<up>" previous-line-message)
    ("<left>" left-char-message)
    ("<down>" next-line-message)
    ("<right>" right-char-message)
    ("M-q" sp-beginning-of-sexp)
    ("M-e" sp-end-of-sexp)
    ("C-w" beginning-of-buffer)
    ("C-a" sp-backward-symbol)
    ("C-s" end-of-buffer)
    ("C-d" sp-forward-symbol)
    ("C-q" cua-scroll-down)
    ("C-e" cua-scroll-up)
    ("C-z" undo)
    ("<backspace>" custom-delete)
    ("RET" newline)
    ("C-v" yank-and-indent :exit t)
    ("C-d" duplicate-line-or-region :exit t)
    ("C-r" helm-show-kill-ring :exit t)
    ("O" hydra-org/body :exit t)))

(use-package iedit
  :ensure t
  :bind
  (("C-;" . iedit-mode)))

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

;; https://www.emacswiki.org/emacs/MultiTerm
;; http://rawsyntax.com/blog/learn-emacs-zsh-and-multi-term
(use-package multi-term
  :ensure t
  :config
  ;; Set zsh as default shell
  (setq multi-term-program "/usr/bin/zsh")
  ;; Set term buffer max size to 8192 lines
  (add-hook 'term-mode-hook
            (lambda ()
              (setq term-buffer-maximum-size 8192)))

  ;; :bind term-raw-map/term-mode-map does not work
  ;; These custom variables must be set instead
  (setq term-unbind-key-list
	'("C-c" "C-x" "C-h" "C-p"))

  (setq term-bind-key-alist
	'(("C-M-c" . term-kill-subjob)
	  ("M-w" . previous-line)
	  ("M-s" . next-line)
	  ("C-f" . helm-swoop)
	  ;; ("C-m" . term-send-raw)
	  ;; ("M-f" . term-send-forward-word)
	  ;; ("M-b" . term-send-backward-word)
	  ;; ("M-o" . term-send-backspace)
	  ;; ("M-p" . term-send-up)
	  ;; ("M-n" . term-send-down)
	  ;; ("M-M" . term-send-forward-kill-word)
	  ;; ("M-N" . term-send-backward-kill-word)
	  ;; ("M-," . term-send-input)
	  ;; ("M-." . comint-dynamic-complete))
	  ("C-c <left>" . multi-term-prev)
	  ("C-c <right>" . multi-term-next)
	  ;; ("C-c C-c" . term-copy-ish?)
	  ;; ("C-c C-x" . term-cut-ish?)
	  ("C-c C-v" . term-paste)
	  ("M-x" . helm-M-x)
	  ("<escape> <escape>" . term-send-esc)
	  ("C-r" . term-send-reverse-search-history))))

(use-package org
  :ensure t
  :config
  (defhydra hydra-org
    (:foreign-keys nil
     :hint nil
     :idle 0.25)
     "
^Kill^      ^Narrow^    ^Agenda^
^^^^^^---------------------------
_C-x_ Cut   _b_ Block   _v_ Views
_C-c_ Copy  _e_ Element _l_ List
_C-v_ Paste _s_ Subtree _a_ Add
_C-r_ Ring  _w_ Widen   _r_ Rem
    "
    ("M-w" previous-line)
    ("M-a" left-char)
    ("M-s" next-line)
    ("M-d" right-char)
    ("<up>" previous-line-message)
    ("<left>" left-char-message)
    ("<down>" next-line-message)
    ("<right>" right-char-message)
    ("M-q" sp-beginning-of-sexp)
    ("M-e" sp-end-of-sexp)
    ("C-w" beginning-of-buffer)
    ("C-a" sp-backward-symbol)
    ("C-s" end-of-buffer)
    ("C-d" sp-forward-symbol)
    ("C-q" cua-scroll-down)
    ("C-e" cua-scroll-up)
    ("<M-return>" org-insert-heading)
    ("<C-return>" org-insert-heading-respect-content)
    ("<M-S-return>" org-insert-todo-heading)
    ("<C-S-return>" org-insert-todo-heading-respect-content)
    ("<tab>" org-cycle)
    ("<M-left>" org-do-promote)
    ("<M-right>" org-do-demote)
    ("<C-left>" org-promote-subtree)
    ("<C-right>" org-demote-subtree)
    ("<C-up>" org-move-subtree-up)
    ("<C-down>" org-move-subtree-down)
    ("C-z" undo)
    ("C-x" org-cut-subtree)
    ("C-c" org-copy-subtree)
    ("C-v" org-yank)
    ("b" org-narrow-block)
    ("e" org-narrow-element)
    ("s" org-narrow-subtree)
    ("w" widen)
    ("l" org-agenda-list)
    ("a" org-agenda-file-to-front)
    ("r" org-remove-file)
    ("C-r" helm-show-kill-ring :exit t)
    ("C-x" hydra-control-x-prefix/body :exit t)
    ("v" hydra-org-agenda/body :exit t))

  (defhydra hydra-org-agenda
    (:foreign-keys nil
     :hint nil
     :idle 0.25)
    "
^Actions^
^^----------
_T_ Todo
_t_ Tags
_l_ Timeline
_s_ Search
    "
    ("T" (lambda () (interactive) (org-todo-list) (hydra-org/body)) :exit t)
    ("t" (lambda () (interactive) (org-tags-view) (hydra-org/body)) :exit t)
    ("l" (lambda () (interactive) (org-timeline) (hydra-org/body)) :exit t)
    ("s" (lambda () (interactive) (org-search-view) (hydra-org/body)) :exit t))
    
  :bind
  (:map org-mode-map
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

(use-package origami
  :ensure t
  :bind
  (:map origami-mode-map
	("<C-tab>" . origami-recursively-toggle-node)
	("<C-S-iso-lefttab>" . origami-open-all-nodes)
	("<C-M-tab>" . origami-close-all-nodes)))

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
  ;; Set helm as projectile completion system
  (setq projectile-completion-system 'helm))

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
  ("rs" . rust-mode))

(use-package scala-mode
  :interpreter
  ;; Set scala-mode to interpret .scala files
  ("scala" . scala-mode))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :init
  ;; Configure smartparens
  (require 'smartparens-config)

  :config
  ;; Turn on smartparens-global-mode
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  ;; Turn off "strict mode"
  (setq smartparens-strict-mode nil)
  ;; ;; ???
  ;; (setq sp-autoskip-closing-pair 'always)
  ;; Do not insert parenthesis pair if point is before word
  (sp-pair "(" nil :unless '(sp-point-before-word-p))
  ;; Do not insert square brackets pair if point is before word
  (sp-pair "[" nil :unless '(sp-point-before-word-p))
  ;; Do not insert curly brackets pair if point is before word
  (sp-pair "{" nil :unless '(sp-point-before-word-p))

  :bind
  (("C-a" . sp-backward-symbol)
   ("C-d" . sp-forward-symbol)
   ("M-q" . sp-beginning-of-sexp)
   ("M-e" . sp-end-of-sexp)))

(use-package spacemacs-theme
  :ensure t
  :defer t
  :init
  ;; Load spacemacs-dark theme
  (load-theme 'spacemacs-dark t))

(use-package zzz-to-char
  :ensure t
  :bind
  (("M-z" . zzz-to-char)))

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
(global-set-key
 [f12]
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

(defun custom-delete ()
  "Delete with custom (more complex) behavior.  See function body for more information."
  (interactive)
  (if mark-active
      ;; If region is active,
      ;; then delete region
      (delete-region (region-beginning) (region-end))
    (if (string-match-p "^[[:space:]]*$" (buffer-substring (line-beginning-position) (line-end-position)))
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
	            ;; (goto-char original-point-position))
	    ;; If line up to point is empty or only whitespace characters and previous line is non-empty,
	    ;; then hungrily delete whitespace characters backward
	    (goto-char (point-min))
	    (forward-line (- original-line 1))
	    (right-char original-column)
	    ;; (goto-char original-point-position)
	    (hungry-delete-backward 1)))))))

(defun call-hydra-mark ()
  "Call 'cua-set-mark' then 'hydra-mark/body'."
  (interactive)
  (cua-set-mark)
  (hydra-mark/body))

;; (defun call-hydra-kill-ring-with-kill-line ()
;;   "Call 'kill-line' then 'hydra-kill-ring/body'."
;;   (interactive)
;;   (kill-line)
;;   (hydra-kill-ring/body))

;; Set overriding non-package personal keybindings
(bind-keys*
 ((kbd "C-M-a") . beginning-of-line)
 ((kbd "C-M-d") . end-of-line)
 ((kbd "C-q") . scroll-down-command)
 ((kbd "C-M-q") . beginning-of-defun)
 ((kbd "C-e") . scroll-up-command)
 ((kbd "C-M-e") . end-of-defun)
 ((kbd "M-v") . yank-pop)
 ((kbd "C-M-v") . yank-pop-reverse)
 ((kbd "M-f") . goto-line)
 ((kbd "C-z") . undo)
 ((kbd "M-c") . comment-dwim)
 ((kbd "C-M-r") . repeat-complex-command)
 ((kbd "M-r") . query-replace-regexp)
 ((kbd "C-x <tab>") . indent-region-or-buffer)
 ((kbd "C-;") . iedit-mode)
 ((kbd "C-(") . parenthesize-to-beginning-of-line)
 ((kbd "C-)") . parenthesize-to-end-of-line)
 ((kbd "C-x") . hydra-control-x-prefix/body)
 ((kbd "C-c") . hydra-kill-ring/body)
 ((kbd "C-k") . kill-line))
 

;; Set non-overriding personal keybindings
(global-set-key (kbd "M-s") 'next-line)
(global-set-key (kbd "M-w") 'previous-line)
(global-set-key (kbd "M-d") 'right-char)
(global-set-key (kbd "M-a") 'left-char)
(global-set-key (kbd "<left>") 'left-char-message)
(global-set-key (kbd "<right>") 'right-char-message)
(global-set-key (kbd "<up>") 'previous-line-message)
(global-set-key (kbd "<down>") 'next-line-message)
(global-set-key (kbd "C-s") 'end-of-buffer)
(global-set-key (kbd "C-w") 'beginning-of-buffer)
(global-set-key (kbd "C-S-d") 'kill-whole-line)
(global-set-key (kbd "C-r") 'query-replace)
(global-set-key (kbd "C-SPC") 'call-hydra-mark)
(global-set-key (kbd "<C-backspace>") 'delete-backward-char)
(global-set-key (kbd "<backspace>") 'custom-delete)

;; Enable emacsclient
(server-start)

;; ;; Report benchmark
;; (benchmark-init/show-durations-tabulated)

(provide 'init)
;;; init.el ends here
