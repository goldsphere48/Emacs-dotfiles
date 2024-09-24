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

;; Fix for find, grep commands used by consult
(when (eq system-type 'windows-nt)
  (let ((my-path "C:/msys64/usr/bin"))
    (setenv "PATH" (concat my-path ";" (getenv "PATH")))
    (setq exec-path (append (list my-path) exec-path))))

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
      tab-width 4
      c-basic-offset 4
      make-backup-files nil
      backup-inhibited nil
      create-lockfiles nil)

;; ----------------------------------
;; UI Packages
;; ----------------------------------

;; Use spacious-padding for UI spacing
(use-package spacious-padding
  :ensure t
  :config
  (setq spacious-padding-widths
        '(:internal-border-width 10
          :header-line-width 4
          :mode-line-width 6
          :tab-width 4
          :right-divider-width 10
          :scroll-bar-width 8
          :fringe-width 8))
  (spacious-padding-mode 1))

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
            :variable-pitch-height 120)))
  (fontaine-set-preset 'jet-brains-mono))

;; Set themes
(use-package ef-themes
  :ensure t
  :init
  (add-hook 'after-init-hook
            (lambda ()
              (setq ef-themes-to-toggle '(ef-maris-dark ef-frost))
              (mapc #'disable-theme custom-enabled-themes)
              (load-theme 'ef-maris-dark :no-confirm))))

;; Icons for dashboard and general UI
(use-package all-the-icons
  :if (display-graphic-p))

;; Dashboard setup
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t))

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
  :ensure t)

(use-package clang-format
  :ensure t)

;; ----------------------------------
;; Debugging & DAP Mode
;; ----------------------------------
(use-package dap-mode
  :defer
  :custom
  (dap-auto-configure-mode t)
  :config
  (require 'dap-lldb)
  (setq dap-lldb-debug-program '("C:/Program Files/LLVM/bin/lldb-vscode")
        dap-external-terminal t)
  (global-set-key [f5] #'dap-debug)
  (global-set-key (kbd "C-'") #'dap-breakpoint-toggle)
  (dap-register-debug-template "C++ LLDB dap"
                               (list :type "lldb-vscode"
                                     :cwd nil :args nil
                                     :request "launch" :program nil)))

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
(global-set-key (kbd "\C-c e") 'flycheck-list-errors-toggle)
(global-set-key (kbd "\C-c t") 'open-test-project)
(global-set-key (kbd "C-<tab>") 'insert-tab-char)
(global-set-key (kbd "<backtab>") 'my-backtab-action)
(global-set-key "\C-c\C-d" 'duplicate-line)

;; ----------------------------------
;; Emacs generated
;; ----------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(all-the-icons clang-format cmake-mode glsl-mode magit company flycheck-posframe flycheck lsp-mode which-key marginalia consult orderless vertico dashboard ef-themes fontaine spacious-padding)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t :background "SystemWindow")))
 '(header-line ((t :box (:line-width 4 :color "grey90" :style nil))))
 '(header-line-highlight ((t :box (:color "SystemWindowText"))))
 '(keycast-key ((t)))
 '(line-number ((t :background "SystemWindow")))
 '(mode-line ((t :box (:line-width 6 :color "grey75" :style nil))))
 '(mode-line-active ((t :box (:line-width 6 :color "grey75" :style nil))))
 '(mode-line-highlight ((t :box (:color "SystemWindowText"))))
 '(mode-line-inactive ((t :box (:line-width 6 :color "grey90" :style nil))))
 '(tab-bar-tab ((t :box (:line-width 4 :color "systembuttonface" :style nil))))
 '(tab-bar-tab-inactive ((t :box (:line-width 4 :color "grey75" :style nil))))
 '(tab-line-tab ((t)))
 '(tab-line-tab-active ((t)))
 '(tab-line-tab-inactive ((t)))
 '(vertical-border ((t :background "SystemWindow" :foreground "SystemWindow")))
 '(window-divider ((t (:background "SystemWindow" :foreground "SystemWindow"))))
 '(window-divider-first-pixel ((t (:background "SystemWindow" :foreground "SystemWindow"))))
 '(window-divider-last-pixel ((t (:background "SystemWindow" :foreground "SystemWindow")))))
