;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(add-to-list 'exec-path "/home/sigma/.local/share/nvim/mason/bin/bin")

(setq compilation-ask-about-save nil)

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s"
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           ))


(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents)

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; (use-package auto-package-update
;; :custom
;; (auto-package-update-interval 7)
;; (auto-package-update-prompt-before-update t)
;; (auto-package-update-hide-results t)
;; :config
;; (auto-package-update-maybe)
;; (auto-package-update-at-time "09:00"))

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 1)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; line numbers
(setq whitespace-display-mappings
      '((space-mark)
	(newline-mark)
	(tab-mark )))
(setq visible-bell nil)
(blink-cursor-mode 0 )
(setq truncate-lines t)
(column-number-mode)
;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; install straight.el 
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package general
  :after evil)

(use-package avy
  :after evil-mc
  :ensure t
  :config
  (evil-global-set-key 'normal "s" 'avy-goto-char-2)
  (evil-global-set-key 'normal "S" 'avy-goto-char-timer)
  (setq avy-timeout-seconds 0.1))

(use-package yasnippet-snippets)

(use-package hl-todo
  :after evil
  :config
  (evil-define-key 'normal 'hl-todo-mode-map "g{" #'hl-todo-previous)
  (evil-define-key 'normal 'hl-todo-mode-map "g}" #'hl-todo-next)
  )

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-insert-state-cursor 'box)
  (setq default-tab-width 8)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  :config
  (evil-mode 1)
  (evil-set-leader nil (kbd ";"))
  (evil-global-set-key 'normal  "H" 'evil-beginning-of-line)
  (evil-global-set-key 'visual  "H" 'evil-beginning-of-line)
  (evil-global-set-key 'normal  "L" 'evil-end-of-line)
  (evil-global-set-key 'visual  "L" 'evil-end-of-line)
  (evil-define-key '(visual) 'global (kbd "C-c C-c C-v") 'clipboard-yank)
  (evil-define-key '(insert) 'global (kbd "C-c C-c C-c") 'clipboard-kill-region)

  (evil-define-key 'normal 'global (kbd "<leader>fs") 'save-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>to") 'tab-next)
  (evil-define-key 'normal 'global (kbd "<leader>t2") 'tab-new)
  (evil-define-key 'normal 'global (kbd "<leader>t0") 'tab-close)
  (evil-define-key 'normal 'global (kbd "<leader>xx") 'execute-extended-command)
  (evil-define-key 'normal 'global (kbd "<leader>hk") 'describe-key)
  (evil-define-key 'normal 'global (kbd "<leader>xo") 'other-window)
  (evil-define-key 'normal 'global (kbd "<leader>x0") 'delete-window)
  (evil-define-key 'normal 'global (kbd "<leader>wl") 'evil-window-right)
  (evil-define-key 'normal 'global (kbd "<leader>wh") 'evil-window-left)
  (evil-define-key 'normal 'global (kbd "<leader>wk") 'evil-window-up)
  (evil-define-key 'normal 'global (kbd "<leader>wj") 'evil-window-down)
  (evil-define-key 'normal 'global (kbd "<leader>ww") 'evil-window-next)
  (evil-define-key 'normal 'global (kbd "<leader>wp") 'evil-window-mru)
  (evil-define-key 'normal 'global (kbd "<leader>wc") 'evil-window-delete)
  (evil-define-key 'visual 'global (kbd "gcc") 'comment-region)
  (evil-define-key 'visual 'global (kbd "gcu") 'uncomment-region)
  (evil-define-key 'visual 'global (kbd "<leader>ee") 'eval-region)
  (evil-define-key 'normal 'global (kbd "<leader>ee") 'eval-last-sexp) 
  (evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)
  (evil-define-key 'normal 'global (kbd "<leader>fr") 'recentf)
  (evil-define-key 'normal 'global (kbd "<leader>fb") 'ido-switch-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>fk") 'ido-kill-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-this-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>rb") 'bookmark-jump)
  (evil-define-key 'normal 'global (kbd "<leader>rm") 'bookmark-set)
  (evil-define-key 'normal 'global (kbd "<leader>cc") 'recompile)
  (evil-define-key 'normal 'global (kbd "<leader>cC") 'compile)
  (evil-define-key 'normal 'global (kbd "g]") 'lsp-bridge-diagnostic-jump-next)
  (evil-define-key 'normal 'global (kbd "g[") 'lsp-bridge-diagnostic-jump-prev)
  (evil-define-key 'normal 'global (kbd "C-x C-m") 'chess/select-music)
  (evil-define-key 'normal 'global (kbd "M-[") 'lsp-bridge-find-def)
  (evil-define-key 'normal 'global (kbd "M-]") 'lsp-bridge-find-impl)
  (evil-define-key 'normal 'global (kbd "g[") 'lsp-bridge-diagnostic-jump-next)
  (evil-define-key 'normal 'global (kbd "g@") 'lsp-bridge-rename)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key global-map (kbd "C-x C-a C-r") 'gud-run)
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'es-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-mc
  :config
  (evil-define-key '(normal visual) 'global (kbd "<leader>mm") 'evil-mc-make-and-goto-next-match)
  (evil-define-key '(normal visual) 'global (kbd "<leader>mr") 'evil-mc-undo-all-cursors)
  (evil-define-key '(normal visual) 'global (kbd "<leader>mp") 'evil-mc-make-and-goto-prev-match)
  (evil-define-key '(normal visual) 'global (kbd "C->") 'evil-mc-skip-and-goto-next-match)
  (evil-define-key '(normal visual) 'global (kbd "C-<") 'evil-mc-skip-and-goto-prev-match)
  )

(global-evil-mc-mode 1)
(use-package expand-region
  :config
  (evil-define-key 'normal 'global (kbd "<leader>es") 'er/expand-region)
  (keymap-global-set "M-'" 'er/expand-region))

(use-package evil-snipe
  :after evil
  :config
  (evil-snipe-override-mode 1)
  (when evil-snipe-override-evil-repeat-keys
    (evil-define-key 'motion map
      "f" 'evil-snipe-repeat
      "F" 'evil-snipe-repeat-reverse))
  (evil-define-key '(normal motion) evil-snipe-local-mode-map
    "s" 'nil
    "S" 'nil)
  (evil-snipe-mode 1 )
  )

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  (setq evil-collection-setup-minibuffer nil))

(use-package command-log-mode
  :commands command-log-mode)


(use-package doom-themes
  :init (load-theme 'custom-gruvbox t))

(use-package all-the-icons)

;; devdocs
(use-package devdocs
  :ensure t)

;; typit
(use-package typit
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (require 'yasnippet)
  (yas-global-mode 1)) 

(use-package smartparens
  :ensure smartparens  
  :hook (prog-mode text-mode markdown-mode) 
  :config
  (require 'smartparens-config))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(defun efs/org-mode-setup ()
  (org-indent-mode))


(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-directory "~/projects/notes/org/")
  (setq org-M-RET-may-split-line nil)
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (line-number-mode 1)
  (setq org-agenda-files
	(append '("~/projects/notes/todo.org")
		(directory-files-recursively "~/projects/notes/org/" "\\.org$")))
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)
  (setq org-todo-keywords '((sequence "TODO(t)" "CONFIG(c)" "NEXT(n)" "STEP(s)" "INFO(f)" "PROGRESS(i)" "CLASS(c)" "PROJ(p)" "LOOP(r)" "WAIT(w)" "EVENT(e)" "HOLD(h)" "EMAIL(m)"  "IDEA(i)" "|""DONE(d)" "KILL(k)" "[X](X)")))
  (setq org-time-stamp-custom-formats '( "%H:%M>"))
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "~/projects/notes/todo.org" "Todos")
	   "* TODO %?\n  %i\n  %a")
	  ("r" "refile" entry (file+headline "~/projects/notes/refile.org" "Todos")
	   "* TODO %?\n  %i\n  %a TIL: ")
	  ("r" "refile" entry (file+headline "~/projects/notes/refile.org" "Todos")
	   "* TODO %?\n  %i refile: ")
	  ("l" "today I learnt" entry (file+headline "~/projects/notes/today-i-learnt.org" "Todos")
	   "* TODO %i TIL: %? ")
	  ("c" "Config" entry (file+headline "~/projects/notes/config.org" "Configuration")
	   "* CONFIG %i %? ")
	  ))
  (efs/org-font-setup)
  (add-hook 'org-mode-hook (lambda () (setq-local lsp-bridge-mode -1)))
  (global-set-key  (kbd "C-c C-o")'org-roam-visit-thing)

  (evil-define-key 'normal 'global (kbd "<leader>mci") 'org-clock-in)
  (evil-define-key 'normal 'global (kbd "<leader>mco") 'org-clock-out)
  (evil-define-key 'normal 'global (kbd "<leader>mcl") 'org-clock-in-last)
  (evil-define-key 'normal 'global (kbd "<leader>mt") 'org-todo)
  )

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))
  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

(use-package org-roam
  :config
  (setq org-roam-directory "~/projects/notes")
  (global-set-key (kbd "C-c n r f ") 'org-roam-node-find )
  )

(setq gdb-many-windows nil)
(defun set-gdb-layout(&optional c-buffer)
  (if (not c-buffer)
      (setq c-buffer (window-buffer (selected-window)))) ;; save current buffer
  ;; from http://stackoverflow.com/q/39762833/846686
  (set-window-dedicated-p (selected-window) nil) ;; unset dedicate state if needed
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows) ;; clean all
  (let* (
         (w-source (selected-window)) ;; left top
         (w-gdb (split-window w-source nil 'right)) ;; right bottom
         (w-locals (split-window w-gdb nil 'above)) ;; right middle bottom
         ;; (w-stack (split-window w-locals nil 'above)) ;; right middle top
         ;; (w-breakpoints (split-window w-stack nil 'above)) ;; right top
         (w-io (split-window w-source (floor(* 0.9 (window-body-height)))
                             'below)) ;; left bottom
         )
    (set-window-buffer w-io (gdb-get-buffer-create 'gdb-inferior-io))
    (set-window-dedicated-p w-io t)
    ;; (set-window-buffer w-breakpoints (gdb-get-buffer-create 'gdb-breakpoints-buffer))
    ;; (set-window-dedicated-p w-breakpoints t)
    (set-window-buffer w-locals (gdb-get-buffer-create 'gdb-locals-buffer))
    (set-window-dedicated-p w-locals t)
    (set-window-buffer w-stack (gdb-get-buffer-create 'gdb-stack-buffer))
    (set-window-dedicated-p w-stack t)
    (set-window-buffer w-gdb gud-comint-buffer)
    (select-window w-source)
    (set-window-buffer w-source c-buffer)
    ))
(defadvice gdb (around args activate)
  "Change the way to gdb works."
  (setq global-config-editing (current-window-configuration)) ;; to restore: (set-window-configuration c-editing)
  (let (
        (c-buffer (window-buffer (selected-window))) ;; save current buffer
        )
    ad-do-it
    (set-gdb-layout c-buffer))
  )
(defadvice gdb-reset (around args activate)
  "Change the way to gdb exit."
  ad-do-it
  (set-window-configuration global-config-editing))

(require 'ido)
(ido-mode t)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)
(require 'icomplete)
(icomplete-mode 1)
(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode 1))

(defun my/consult-ripgrep-with-fido-vertical ()
  "Run `consult-ripgrep` with `fido-vertical-mode` enabled temporarily."
  (interactive)
  (let ((was-fido-active (bound-and-true-p fido-vertical-mode)))
    (unless was-fido-active
      (fido-vertical-mode 1))  ;; Enable fido-vertical-mode
    (consult-ripgrep)
    (unless was-fido-active
      (fido-vertical-mode -1))))

(use-package consult
  :config 
  (evil-define-key 'normal 'global (kbd "<leader>fw") 'my/consult-ripgrep-with-fido-vertical ))



(defun chess/check-rich ()
  "Check if rich."
  (interactive)
  (progn
    (message "Checking for money...")
                                        ; check if checking-for-money exists
    (when (get-buffer "*checking-for-money*")
      (kill-buffer "*checking-for-money*"))
    (start-process "checking-for-money" "*checking-for-money*" "git" "-C" (expand-file-name "~/projects/repos/Summer2025-Internships") "pull")
    (set-process-sentinel (get-process "checking-for-money")
                          (lambda (_ event)
                            (when (string= "finished\n" event)
                              (with-current-buffer "*checking-for-money*"
                                (goto-char (point-min))
                                (if (re-search-forward "Already up to date" nil t)
                                    (message "Nothing found, keep working hard")
                                  (progn
                                    (message "You should be rich")
                                    (find-file (expand-file-name "~/projects/repos/Summer2025-Internships/README.md"))
                                    (find-file (expand-file-name "~/projects/notes/applications.org"))))))))))

(defun chess/select-music ()
  "Select song on mpv"
  (interactive)
  (progn
    (let ((song (completing-read "Select song: " (append (directory-files "~/Music/" nil ".*mp3$") '("::play::" "::pause::" "::next::" "::clear::")))))
      (cond 
       ((string= song "::play::")
	(progn
	  (start-process "mpc-play" nil "mpc" "play")))
       ((string= song "::pause::")
	(start-process "mpc-pause" nil "mpc" "pause"))
       ((string= song "::clear::")
	(start-process "mpc-clear" nil "mpc" "clear"))
       ((string= song "::next::")
	(start-process "mpc-next" nil "mpc" "next"))
       (t
	(progn
	(start-process "mpc-add" nil "mpc" "add" (format "%s" song))
	(start-process "mpc-play" nil "mpc" "play")
	(message (format "Playing %s" song))
	))))))

(provide 'config)
(setq compilation-scroll-output nil)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ido))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))


(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


;; lsp-bridge
(add-to-list 'load-path "~/.emacs.d/lsp-bridge")
(require 'lsp-bridge)
(global-lsp-bridge-mode)
(setq lsp-bridge-enable-hover-diagnostic t)
(setq lsp-bridge-multi-lang-server-extension-list
      '(
	(("ts" )   . "typescript_eslint")
	(("tsx" )   . "typescriptreact_eslint_tailwindcss")
	(("jsx" )   . "javascriptreact_eslint_tailwindcss")
	(("html") . "html_tailwindcss")
	))
(setq acm-menu-length 5)
(evil-define-key 'visual 'global (kbd "<leader>lr") 'lsp-bridge-find-references)

(use-package pyvenv
  :ensure t)

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(setq flycheck-disabled-checkers nil)


(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "zsh"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "powershell.exe")
  (setq explicit-powershell.exe-args '()))

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  
  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-normalize-keymaps)
  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single
  :commands (dired dired-jump))

(use-package dired-open
  :commands (dired dired-jump)
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))


(defun chess/search-wiki (query)
  "Search Wikipedia for QUERY."
  (interactive "sSearch: ")
  (eww (format "https://en.wikipedia.org/w/index.php?search=%s" query)))

;; fonts and line spacing
(set-face-attribute 'default nil :font "Iosevka")
(set-face-attribute 'fixed-pitch nil :font "Iosevka")
(set-face-attribute 'variable-pitch nil :font "Iosevka")
(setq-default line-spacing nil)  
(global-display-line-numbers-mode t)

(setq-default display-line-numbers 'relative)
(setq display-line-numbers-type 'relative) ;; this is the thing that actually sets the relative line numbers lmao 
(global-display-line-numbers-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

;; split window
(setq split-height-threshold 100)
(setq split-width-threshold 160)

(defun my-split-window-below-compilation (&optional arg)
  "Split the current window 70/30 rather than 50/50.
A single-digit prefix argument gives the top window arg*10%."
  (interactive "P")
  (let ((proportion (* (or arg 7) 0.1)))
    (split-window-below (round (* proportion (window-height))))))

(defun my-split-window-left-compilation (&optional arg)
  "Split the window 70/30, then open the *compilation* window in it."
  (interactive "P")
  (let ((proportion (* (or arg 12) 0.1)))
    (let ((new-window (split-window-horizontally (- 0 (round (* proportion (window-height)))))))
      (select-window new-window)
      (switch-to-buffer "*compilation*"))))

(global-set-key (kbd "C-x 2") 'my-split-window-below-compilation)
(global-set-key (kbd "C-x c") 'my-split-window-left-compilation)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 4 1000 1000))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "691d671429fa6c6d73098fc6ff05d4a14a323ea0a18787daeb93fde0e48ab18b" "e9d2cfe6cdb1ed56d4f886e01c67ffa88aedb315ce7ea795ccdc34f15e01e09b" "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710" default))
 '(global-display-line-numbers-mode t)
 '(package-selected-packages
   '(typit devdocs copilot smartparens yasnippet ido-completing-read+ lsp-bridge evil-snipe ido-mode dired-hide-dotfiles dired-open all-the-icons-dired dired-single eshell-git-prompt vterm eterm-256color rainbow-delimiters evil-nerd-commenter forge magit projectile company-box company pyvenv python-mode typescript-mode dap-mode lsp-treemacs lsp-ui lsp-mode visual-fill-column org-bullets hydra helpful which-key doom-modeline all-the-icons doom-themes command-log-mode evil-collection evil general no-littering auto-package-update)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
