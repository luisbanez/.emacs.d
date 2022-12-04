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

(use-package almost-mono-themes
  :ensure t)

(use-package klere-theme
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

(use-package hasklig-mode
  :ensure t)

(use-package disable-mouse
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package csharp-mode
  :ensure t)

(use-package sql-indent
  :ensure t)

;;sql-indent mode
(eval-after-load "sql"
  '(load-library "sql-indent"))

;; disable mouse
(disable-mouse-global-mode)

;; always refresh buffers when files have changed on disk
(global-auto-revert-mode t)

;; initial buffer
(setq initial-buffer-choice "~/.emacs.d/init.el")

;; change backup directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

;; remove clutter
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; line numbers
(global-linum-mode 1)

;; treemacs
(setq treemacs-text-scale -1)
(setq treemacs-width 25)
(setq treemacs-python-executable "C:/Users/ACER/AppData/Local/Programs/Python/Python311/python.exe")
(add-hook 'after-init-hook 'treemacs)

;; remove annoying bell soundevl
(setq ring-bell-function 'ignore)

;; dont blink cursor
(blink-cursor-mode -1)

;; maximize
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; load theme
(dolist (theme custom-enabled-themes)
  (disable-theme theme))
  (load-theme 'doom-gruvbox t)
;; (set-cursor-color "#ebdbb2")

;; set font
(set-face-attribute 'default nil
                    :family "Hasklig Medium"
                    :height 90
                    :weight 'normal
                    :width 'normal)

;; disable text wrapping
(setq-default truncate-lines t)

;; text editing
(defconst my-cc-style
  '("awk"
    (c-offsets-alist . ((case-label +)
                        (statement-cont -)
                        (arglist-close 0)
                        (inline-open 0)
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
  
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9cd57dd6d61cdf4f6aef3102c4cc2cfc04f5884d4f40b2c90a866c9b6267f2b3" "4fda8201465755b403a33e385cf0f75eeec31ca8893199266a6aeccb4adedfa4" "251ed7ecd97af314cd77b07359a09da12dcd97be35e3ab761d4a92d8d8cf9a71" "f053f92735d6d238461da8512b9c071a5ce3b9d972501f7a5e6682a90bf29725" "9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68" "11cc65061e0a5410d6489af42f1d0f0478dbd181a9660f81a692ddc5f948bf34" "adaf421037f4ae6725aa9f5654a2ed49e2cd2765f71e19a7d26a454491b486eb" "2853dd90f0d49439ebd582a8cbb82b9b3c2a02593483341b257f88add195ad76" "e266d44fa3b75406394b979a3addc9b7f202348099cfde69e74ee6432f781336" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "d9a28a009cda74d1d53b1fbd050f31af7a1a105aa2d53738e9aa2515908cac4c" "249e100de137f516d56bcf2e98c1e3f9e1e8a6dce50726c974fa6838fbfcec6b" "5a00018936fa1df1cd9d54bee02c8a64eafac941453ab48394e2ec2c498b834a" "2ce76d65a813fae8cfee5c207f46f2a256bac69dacbb096051a7a8651aa252b0" "e8567ee21a39c68dbf20e40d29a0f6c1c05681935a41e206f142ab83126153ca" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" default))
 '(package-selected-packages
   '(disable-mouse hasklig-mode yasnippet use-package treemacs rust-mode kaolin-themes doom-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work 
