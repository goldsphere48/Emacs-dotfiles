(eval-when-compile
  (require 'use-package))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Fix for find, grep commands used by consult
(when (eq system-type 'windows-nt)
  (let ((my-path "C:/msys64/usr/bin"))
    (setenv "PATH" (concat my-path ";" (getenv "PATH")))
    (setq exec-path (append (list my-path) exec-path))))

(setq package-check-signature nil)

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

(use-package consult
  :ensure t
  :bind (;; A recursive grep
         ("M-s M-g" . consult-grep)
         ;; Search for files names recursively
         ("M-s M-f" . consult-find)
         ;; Search through the outline (headings) of the file
         ("M-s M-o" . consult-outline)
         ;; Search the current buffer
         ("M-s M-l" . consult-line)
         ;; Switch to another buffer, or bookmarked file, or recently
         ;; opened file.
         ("M-s M-b" . consult-buffer)))

(use-package savehist
  :ensure t
  :init
  (savehist-mode))

(use-package emacs
  :init
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)
  (setq read-extended-command-predicate #'command-completion-default-include-p))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package lsp-mode
  :ensure t
  :init
  (defun setup-lsp-diagnostics-for-scons ()
    (when (member (file-name-nondirectory buffer-file-name)
                  '("SConstruct" "SConscript"))
      ;; Установка диагностики в 'none' только для этих файлов
      (setq-local lsp-diagnostics-provider :none)))
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (c-mode . lsp)
  (c++-mode . lsp)
  (lua-mode . lsp)
  (python-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-mode . setup-lsp-diagnostics-for-scons)
  :commands lsp
  :config
  (setq read-process-output-max (* 1024 1024))
  (setq gc-cons-threshold (* 100 1024 1024))
  (setq lsp-idle-delay 0.100)
  (define-key lsp-mode-map [S-f12] #'lsp-find-references)
  (define-key lsp-mode-map [f12] #'lsp-find-definition)
  (define-key lsp-mode-map [C-f12] #'lsp-find-declaration)
  (define-key lsp-mode-map (kbd "\C-r\C-r") #'lsp-rename)
  (global-set-key (kbd "M-<return>") #'lsp-execute-code-action))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-ui-flycheck-list-position 'bottom)
  (setq lsp-ui-flycheck-live-reporting t)
  (setq lsp-ui-sideline-show-flycheck nil))

(add-hook 'lsp-after-apply-edits-hook
          (lambda (operation)
            (when (eq operation 'rename)
              (projectile-save-project-buffers))))

(defun disable-flymake-for-scons ()
  (when (member (file-name-nondirectory buffer-file-name)
                '("SConstruct" "SConscript"))
    (flymake-mode -1)))

(defun disable-flycheck-for-scons ()
  (when (member (file-name-nondirectory buffer-file-name)
                '("SConstruct" "SConscript"))
    (flycheck-mode -1)))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'find-file-hook 'disable-flycheck-for-scons)
  (add-hook 'find-file-hook 'disable-flymake-for-scons)
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package company
  :ensure t
  :config
  (setq company-minimum-prefix-length 1))

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-cmake))

(add-hook 'after-init-hook 'global-company-mode)

(use-package magit
  :ensure t)

(use-package glsl-mode
  :ensure t)

(add-to-list 'auto-mode-alist '("SConstruct\\'" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript\\'" . python-mode))

(use-package cmake-mode
  :ensure t)

(use-package clang-format
  :ensure t)

(use-package marginalia
  :ensure t
  :config (marginalia-mode 1))

(use-package projectile
  :ensure t
  :config
  (projectile-mode))

(use-package dap-mode
  :defer
    :custom
	(dap-auto-configure-mode t "Automatically configure dap.")
	(dap-auto-configure-features
	 '(locals breakpoints expressions tooltip)  "Remove the button panel in the top.")
  :hook (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
  :config
  ;;; dap for c++
  (require 'dap-lldb)

  ;;; set the debugger executable (c++)
  (setq dap-lldb-debug-program '("C:/Program Files/LLVM/bin/lldb-vscode"))
  (setq dap-external-terminal t)
  ;;; ask user for executable to debug if not specified explicitly (c++)
  (setq dap-lldb-debugged-program-function (lambda () (read-file-name "Select file to debug.")))
  (global-set-key [f5] #'dap-debug)
  (global-set-key (kbd "C-'") #'dap-breakpoint-toggle)
  ;;; default debug template for (c++)
  (dap-register-debug-template
   "C++ LLDB dap"
   (list :type "lldb-vscode"
         :cwd nil
         :args nil
         :request "launch"
         :program nil))
  
  (defun dap-debug-create-or-edit-json-template ()
    "Edit the C++ debugging configuration or create + edit if none exists yet."
    (interactive)
    (let ((filename (concat (projectile-project-root) "launch.json"))
		  (default "~/.emacs.d/default-launch.json"))
      (unless (file-exists-p filename)
		(copy-file default filename))
      (find-file-existing filename))))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package all-the-icons
  :if (display-graphic-p))

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(use-package spacious-padding
  :ensure t
  :config
  (setq spacious-padding-widths
		'( :internal-border-width 10
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 10
           :scroll-bar-width 8
           :fringe-width 8))
  (spacious-padding-mode 1))

(use-package ef-themes
  :ensure t)

(add-hook 'after-init-hook
          (lambda ()
			(setq ef-themes-to-toggle '(ef-maris-dark ef-frost))
            (mapc #'disable-theme custom-enabled-themes)
            (load-theme 'ef-maris-dark :no-confirm)))

(use-package fontaine
  :ensure t
  :config
  (setq fontaine-presets
        '((default
            :default-family "Cascadia Code"
            :default-weight normal
            :default-height 120 ; 14pt
            :fixed-pitch-family "Cascadia Code"
            :fixed-pitch-weight normal
            :fixed-pitch-height 120
            :variable-pitch-family "Sans Serif"
            :variable-pitch-weight normal
            :variable-pitch-height 120)
		  (jet-brains-mono
            :default-family "JetBrains Mono"
            :default-weight normal
            :default-height 120 ; 14pt
            :fixed-pitch-family "JetBrains Mono"
            :fixed-pitch-weight normal
            :fixed-pitch-height 120
            :variable-pitch-family "JetBrains Mono"
            :variable-pitch-weight normal
            :variable-pitch-height 120)
          (courier-new
            :default-family "Courier New"
            :default-weight normal
            :default-height 120 ; 12pt
            :fixed-pitch-family "Courier New"
            :fixed-pitch-weight normal
            :fixed-pitch-height 120
            :variable-pitch-family "Sans Serif"
            :variable-pitch-weight normal
            :variable-pitch-height 120)))
  (fontaine-set-preset 'jet-brains-mono))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   '("f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a" "90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" default))
 '(global-display-line-numbers-mode t)
 '(indent-tabs-mode t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(python-black zig-mode elpy lsp-pyright flycheck-posframe clang-format magit dap-mode multiple-cursors dashboard lua-mode spacious-padding ef-themes cmake-mode projectile consult marginalia company glsl-mode material-theme flycheck lsp-ui lsp-mode which-key orderless vertico use-package))
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
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

(set-face-attribute 'default nil :height 120)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq
  scroll-margin 5
  scroll-step 1
  scroll-conservatively 10000)

(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq-default indent-tabs-mode 't)
(setq-default tab-width 4)
(setq c-basic-offset 4)
(setq make-backup-files nil)
(setq backup-inhibited nil)
(setq create-lockfiles nil)

(delete-selection-mode 1)

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

(defun open-emacs-config-file()
  "Open Emacs config."
  (interactive)
  (find-file "C:\\Users\\golds\\AppData\\Roaming\\.emacs.d\\init.el"))

(defun open-test-project()
  "Open Emacs config."
  (interactive)
  (dired "D:\\Code\\Test\\"))

(defun duplicate-line()
  "Duplicate line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))

(defun flycheck-list-errors-toggle ()
  "Toggle the error list for the current buffer, ensuring it opens at the bottom."
  (interactive)
  (if (not (get-buffer flycheck-error-list-buffer))
      (call-interactively 'flycheck-list-errors))
  (let ((flycheck-errors-window (get-buffer-window flycheck-error-list-buffer)))
    (if (window-live-p flycheck-errors-window)
        (delete-window flycheck-errors-window)
      (display-buffer-at-bottom
       (get-buffer flycheck-error-list-buffer)
       '((window-height . 0.3))))))

(defun insert-tab-char ()
  "Insert a tab character."
  (interactive)
  (insert "\t"))

(defun my-backtab-action ()
  "A custom action for backtab (Shift+Tab) to decrease indentation."
  (interactive)
  (if (use-region-p)
      (indent-rigidly (region-beginning) (region-end) (- tab-width))
    (indent-rigidly (line-beginning-position) (line-end-position) (- tab-width))))

;;; Indentation for python

;; Ignoring electric indentation
(defun electric-indent-ignore-python (char)
  "Ignore electric indentation for python-mode"
  (if (equal major-mode 'python-mode)
      'no-indent
    nil))
(add-hook 'electric-indent-functions 'electric-indent-ignore-python)

;; Enter key executes newline-and-indent
(defun set-newline-and-indent ()
  "Map the return key with `newline-and-indent'"
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'python-mode-hook 'set-newline-and-indent)

(windmove-default-keybindings)
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "\C-c i") 'open-emacs-config-file)
(global-set-key (kbd "\C-c e") 'flycheck-list-errors-toggle)
(global-set-key (kbd "\C-c t") 'open-test-project)
(global-set-key (kbd "C-<tab>") 'insert-tab-char)
(global-set-key (kbd "<backtab>") 'my-backtab-action)
(global-set-key "\C-c\C-d" 'duplicate-line)
