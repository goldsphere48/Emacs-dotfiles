(eval-when-compile
  (require 'use-package))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

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
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (c-mode . lsp)
  (c++-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp
  :config
  (setq lsp-idle-delay 0.100))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package company
  :ensure t
  :config
  (setq company-minimum-prefix-length 1
		company-idle-delay 0.01))

(add-hook 'after-init-hook 'global-company-mode)

(use-package glsl-mode
  :ensure t)

(use-package marginalia
  :ensure t
  :config (marginalia-mode 1))

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
   '(consult marginalia company glsl-mode material-theme flycheck lsp-ui lsp-mode which-key orderless vertico use-package))
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

(defun powershell (&optional buffer)
  "Launches a PowerShell in BUFFER *powershell* and switch to it."
  (interactive)
  (let ((buffer (or buffer "*powershell*"))
        (powershell-prog "c:\\windows\\system32\\WindowsPowerShell\\v1.0\\powershell.exe")
        (init-command "chcp 65001\n"))
    (make-comint-in-buffer "powershell" buffer powershell-prog nil "-NoExit" "-Command" init-command)
    (switch-to-buffer buffer)))

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

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "\C-c i") 'open-emacs-config-file)
(global-set-key (kbd "\C-c t") 'open-test-project)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key "\C-c\C-d" 'duplicate-line)
