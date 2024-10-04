;;; Commentary: Emacs config
;;; package --- init

;; ----------------------------------
;; Package Management & Repositories
;; ----------------------------------

;;; Code:
(eval-when-compile
  (require 'use-package))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq package-check-signature nil)
(setq custom-file "~/.emacs.d/custom.el")

;; Fix for find, grep commands used by consult
(when (eq system-type 'windows-nt)
  (add-to-list 'exec-path "C:/msys64/usr/bin")
  (add-to-list 'exec-path "C:/msys64/mingw/usr/bin"))

(load-file "~/.emacs.d/powershell.el")

;; ----------------------------------
;; Basic Settings
;; ----------------------------------
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(delete-selection-mode 1)
(global-display-line-numbers-mode t)
(setq ring-bell-function 'ignore
      scroll-conservatively 10000
      make-backup-files nil
      create-lockfiles nil
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't
      scroll-margin 5
      scroll-step 1
      scroll-conservatively 10000
      scroll-step 1
      indent-tabs-mode 't
      make-backup-files nil
      backup-inhibited nil
      create-lockfiles nil
      inhibit-startup-screen 't)

(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode 't)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; Настроить кодировку для shell-mode
(add-hook 'shell-mode-hook
          (lambda ()
            (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))


;; Настроить кодировку для eshell
(add-hook 'eshell-mode-hook
          (lambda ()
            (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))


;; Настроить кодировку для powershell
(add-hook 'powershell-mode-hook
          (lambda ()
            (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))


;; ----------------------------------
;; UI Packages
;; ----------------------------------
(use-package fontaine
  :ensure t
  :config
  (setq fontaine-presets
        '((default
            :default-family "Cascadia Code"
            :default-weight normal
            :default-height 120
            :fixed-pitch-family "Cascadia Code"
            :fixed-pitch-weight normal
            :fixed-pitch-height 120
            :variable-pitch-family "Sans Serif"
            :variable-pitch-weight normal
            :variable-pitch-height 120)
          (jet-brains-mono
            :default-family "JetBrains Mono"
            :default-weight normal
            :default-height 120
            :fixed-pitch-family "JetBrains Mono"
            :fixed-pitch-weight normal
            :fixed-pitch-height 120
            :variable-pitch-family "JetBrains Mono"
            :variable-pitch-weight normal
            :variable-pitch-height 120)
          (courier-new
            :default-family "Courier New"
            :default-weight normal
            :default-height 120
            :fixed-pitch-family "Courier New"
            :fixed-pitch-weight normal
            :fixed-pitch-height 120
            :variable-pitch-family "Sans Serif"
            :variable-pitch-weight normal
            :variable-pitch-height 120)
	  (iosevka
            :default-family "IosevkaTerm NF"
            :default-weight normal
            :default-height 120
            :fixed-pitch-family "IosevkaTerm NF"
            :fixed-pitch-weight normal
            :fixed-pitch-height 120
            :variable-pitch-family "IosevkaTerm NF"
            :variable-pitch-weight normal
            :variable-pitch-height 120)))
  (fontaine-set-preset 'jet-brains-mono))

;; Solarized theme
;; (load-theme 'solarized-light t)
(setq catppuccin-flavor 'latte)
(load-theme 'catppuccin :no-confirm)


;; Icons for dashboard and general UI
(use-package all-the-icons
  :if (display-graphic-p))
;; ----------------------------------
;; Mode Line
;; ----------------------------------


;; ----------------------------------
;; Completion, Minibuffer, and Navigation
;; ----------------------------------
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure t
  :bind (("M-s M-g" . consult-grep)
         ("M-s M-f" . consult-find)
         ("M-s M-o" . consult-outline)
         ("M-s M-l" . consult-line)
         ("M-s M-b" . consult-buffer)))

(use-package marginalia
  :ensure t
  :config (marginalia-mode 1))

;; Enable history in minibuffer
(use-package savehist
  :ensure t
  :init (savehist-mode))

;; ----------------------------------
;; Programming Language and LSP Support
;; ----------------------------------
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c-mode c++-mode lua-mode python-mode) . lsp)
  :commands lsp
  :config
  (setq read-process-output-max (* 1024 1024)
        gc-cons-threshold (* 100 1024 1024)
        lsp-idle-delay 0.100)
  (define-key lsp-mode-map [S-f12] #'lsp-find-references)
  (define-key lsp-mode-map [f12] #'lsp-find-definition)
  (define-key lsp-mode-map [C-f12] #'lsp-find-declaration)
  (global-set-key (kbd "M-<return>") #'lsp-execute-code-action))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-sideline-enable nil))

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(use-package company
  :ensure t
  :config
  (setq company-minimum-prefix-length 1)
  (global-company-mode))

(use-package glsl-mode
  :ensure t)

(use-package cmake-mode
  :ensure t
  :init (setq-default cmake-tab-width 4))

(use-package clang-format
  :ensure t)

;; ----------------------------------
;; Miscellaneous Packages
;; ----------------------------------
(use-package magit
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

;; ----------------------------------
;; User Functions
;; ----------------------------------

(defun move-line-up()
  "Move up current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun duplicate-line()
  "Duplicate line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))

(defun open-emacs-config-file()
  "Open Emacs config."
  (interactive)
  (find-file "C:\\Users\\golds\\AppData\\Roaming\\.emacs.d\\init.el"))

;; ----------------------------------
;; Keybindings
;; ----------------------------------

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "\C-c i") 'open-emacs-config-file)
(global-set-key (kbd "\C-c t") 'open-test-project)
(global-set-key (kbd "C-<tab>") 'insert-tab-char)
(global-set-key (kbd "<backtab>") 'my-backtab-action)
(global-set-key "\C-c\C-d" 'duplicate-line)

(provide 'init)
;;; init.el ends here
