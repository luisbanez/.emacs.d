;; connect to package repositories
(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(use-package doom-themes
  :ensure t)

(use-package kaolin-themes
  :ensure t)

(use-package treemacs
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/yasnippets"))
  (yas-global-mode 1))

;; change backup directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

;; remove clutter
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(global-linum-mode 1)

;; treemacs
(setq treemacs-text-scale -1)
(setq treemacs-width 25)
(setq treemacs-python-executable "C:/Program Files/Python310/python.exe")

;; remove annoying bell soundevl
(setq ring-bell-function 'ignore)

;; dont blink cursor
(blink-cursor-mode -1)

;; maximize
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; load theme
(dolist (theme custom-enabled-themes)
  (disable-theme theme))
  (load-theme 'kaolin-galaxy t)
;; (set-cursor-color "#ebdbb2")

;; set font
(set-face-attribute 'default nil
                    :family "Hasklig"
                    :height 100
                    :weight 'normal
                    :width 'normal)

;; disable text wrapping
(setq-default truncate-lines t)

;; text editing
(defconst my-cc-style
  '("awk"
    (c-offsets-alist . ((case-label +)
                        (statement-cont -)
                        ))))
(c-add-style "my-cc-style" my-cc-style)
  
(setq-default c-default-style "my-cc-style")
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

(electric-pair-mode 1)
(put 'downcase-region 'disabled nil)

;; clarity
(global-hl-line-mode 1)

;; enable windmove
(windmove-default-keybindings)
  
