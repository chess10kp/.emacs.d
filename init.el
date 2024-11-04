;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(add-to-list 'exec-path "/home/sigma/.local/share/nvim/mason/bin" )
(add-to-list 'exec-path "/home/sigma/.ghcup/bin" )

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

(setq native-comp-jit-compilation nil)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

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
  (evil-define-key '(visual insert) 'global (kbd "C-c C-c C-v") 'clipboard-yank)
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
  (evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-this-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>rb") 'bookmark-jump)
  (evil-define-key 'normal 'global (kbd "<leader>rm") 'bookmark-set)
  (evil-define-key 'normal 'global (kbd "<leader>cc") 'recompile)
  (evil-define-key 'normal 'global (kbd "<leader>cC") 'compile)
  (evil-define-key 'normal 'global (kbd "C-x C-m") 'chess/select-music)
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

;; (require 'ido)
;; (ido-mode t)
;; (evil-define-key 'normal 'global (kbd "<leader>fb") 'ido-switch-buffer)
;; (evil-define-key 'normal 'global (kbd "<leader>bb") 'ido-switch-buffer)
;; (evil-define-key 'normal 'global (kbd "<leader>fk") 'ido-kill-buffer)
;; (ido-everywhere 1)
;; (setq ido-enable-flex-matching t)
;; (require 'icomplete)
;; (icomplete-mode 1)
;; (use-package ido-completing-read+
;;   :config
;;   (ido-ubiquitous-mode 1))

;; (defun my/consult-ripgrep-with-fido-vertical ()
;;   "Run `consult-ripgrep` with `fido-vertical-mode` enabled temporarily."
;;   (interactive)
;;   (let ((was-fido-active (bound-and-true-p fido-vertical-mode)))
;;     (unless was-fido-active
;;       (fido-vertical-mode 1))  ;; Enable fido-vertical-mode
;;     (consult-ripgrep)
;;     (unless was-fido-active
;;       (fido-vertical-mode -1))))

;; (use-package consult
;;   :config 
;;   (evil-define-key 'normal 'global (kbd "<leader>fw") 'my/consult-ripgrep-with-fido-vertical ))


;; Enable vertico
(use-package vertico
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))
;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))
(setq completion-styles '(basic substring partial-completion flex))
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
(use-package vertico-quick
  :after vertico
  :ensure nil)
;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))
;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"
  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)
(evil-global-set-key 'normal  (kbd "<leader>ff"  )'find-file)
(evil-global-set-key 'normal  (kbd "<leader>fb"  )'consult-buffer)
(evil-global-set-key 'normal  (kbd "<leader>fw"  )'consult-ripgrep)
(evil-global-set-key 'normal  (kbd "<leader>fd"  )'consult-find)

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))



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
  :custom ((projectile-completion-system 'completing-read))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/projects/")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(evil-define-key 'normal 'global (kbd "<leader>p") 'projectile-command-map)

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


;; lsp-bridge
(add-to-list 'load-path "~/.emacs.d/lsp-bridge")
(require 'lsp-bridge)
(global-lsp-bridge-mode)
(setq lsp-bridge-enable-hover-diagnostic t)
(add-hook 'typescript-mode (lambda () (setq-local lsp-bridge-mode -1)))
(add-hook 'typescript-ts-mode (lambda () (setq-local lsp-bridge-mode -1)))
(add-hook 'rjsx-mode (lambda () (setq-local lsp-bridge-mode -1)))
(add-hook 'javascript-mode (lambda () (setq-local lsp-bridge-mode -1)))
(setq acm-menu-length 5)
(evil-define-key 'visual 'global (kbd "<leader>lr") 'lsp-bridge-find-references)
(evil-define-key 'normal 'global (kbd "g]") 'lsp-bridge-diagnostic-jump-next)
(evil-define-key 'normal 'global (kbd "g[") 'lsp-bridge-diagnostic-jump-prev)
(evil-define-key 'normal 'global (kbd "M-[") 'lsp-bridge-find-def)
(evil-define-key 'normal 'global (kbd "M-]") 'lsp-bridge-find-impl)
(evil-define-key 'normal 'global (kbd "g[") 'lsp-bridge-diagnostic-jump-next)
(evil-define-key 'normal 'global (kbd "g@") 'lsp-bridge-rename)

;; haskell-mode
(setq haskell-interactive-popup-errors nil)
(defun hly/evil-open-below (count)
  "Simulate evil’s o using ‘A RET’.

Evil’s native \\[evil-open-below] is too unreliable in the face of various
major
modes messing with indenting. This bruteforces it by relying on (the more
reliable) RET, instead.

See:

- https://github.com/haskell/haskell-mode/issues/1265
- https://emacs.stackexchange.com/a/2471
"
  (interactive "p")
  (setq unread-command-events (listify-key-sequence (kbd "RET")))
  (evil-append-line count))

(defun hly/evil-open-above (count)
  "Simulate evil’s O using ‘UP A RET’.

Evil’s native \\[evil-open-above] is too unreliable in the face of various major
modes messing with indenting. This bruteforces it by relying on (the more
reliable) RET, instead.

Doesn’t work on the first line of a file.
"
  (interactive "p")
  (forward-line -1)
  (hly/evil-open-below count))

(with-eval-after-load "haskell-mode"
  (evil-define-key 'normal haskell-mode-map "o" 'hly/evil-open-below)
  (evil-define-key 'normal haskell-mode-map "O" 'hly/evil-open-above))



;; ; START tsx 
(use-package treesit
  :ensure nil
  :mode (("\\.tsx\\'" . tsx-ts-mode)
	 ("\\.js\\'"  . typescript-ts-mode)
	 ("\\.mjs\\'" . typescript-ts-mode)
	 ("\\.mts\\'" . typescript-ts-mode)
	 ("\\.cjs\\'" . typescript-ts-mode)
	 ("\\.ts\\'"  . typescript-ts-mode)
	 ("\\.jsx\\'" . tsx-ts-mode)
	 ("\\.json\\'" .  json-ts-mode)
	 ("\\.Dockerfile\\'" . dockerfile-ts-mode)
	 ("\\.prisma\\'" . prisma-ts-mode)
	 ;; More modes defined here...
	 )
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
	     '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
	       (bash "https://github.com/tree-sitter/tree-sitter-bash")
	       (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
	       (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
	       (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
	       (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
	       (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
	       (markdown "https://github.com/ikatyang/tree-sitter-markdown")
	       (make "https://github.com/alemuller/tree-sitter-make")
	       (elisp "https://github.com/Wilfred/tree-sitter-elisp")
	       (cmake "https://github.com/uyha/tree-sitter-cmake")
	       (haskell "https://github.com/tree-sitter/tree-sitter-haskell" "master" "src" nil nil)
	       (c "https://github.com/tree-sitter/tree-sitter-c")
	       (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
	       (elixir "https://github.com/elixir-lang/tree-sitter-elixir" "main" "src" nil nil)
	       (go . ("https://github.com/tree-sitter/tree-sitter-go"))
	       (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
	       (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	       (toml "https://github.com/tree-sitter/tree-sitter-toml")
	       (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
	       (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
	       (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
	       (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
	(treesit-install-language-grammar (car grammar)))))

      ;; Optional, but recommended. Tree-sitter enabled major modes are
      ;; distinct from their ordinary counterparts.
      ;;
      ;; You can remap major modes with `major-mode-remap-alist'. Note
      ;; that this does *not* extend to hooks! Make sure you migrate them
      ;; also
      (dolist (mapping
               '((python-mode . python-ts-mode)
                 (css-mode . css-ts-mode)
                 (typescript-mode . typescript-ts-mode)
                 (js-mode . typescript-ts-mode)
                 (js2-mode . typescript-ts-mode)
                 (c-mode . c-ts-mode)
                 (c++-mode . c++-ts-mode)
                 (c-or-c++-mode . c-or-c++-ts-mode)
                 (bash-mode . bash-ts-mode)
                 (css-mode . css-ts-mode)
                 (json-mode . json-ts-mode)
                 (js-json-mode . json-ts-mode)
                 (sh-mode . bash-ts-mode)
                 (sh-base-mode . bash-ts-mode)))
        (add-to-list 'major-mode-remap-alist mapping))
      :config
      (os/setup-install-grammars))
;;     ;;;; Code Completion
;; (use-package corfu
;;   :ensure t
;;   ;; Optional customizations
;;   :custom
;;   (corfu-cycle t)                 ; Allows cycling through candidates
;;   (corfu-auto t)                  ; Enable auto completion
;;   (corfu-auto-prefix 2)           ; Minimum length of prefix for completion
;;   (corfu-auto-delay 0)            ; No delay for completion
;;   (corfu-popupinfo-delay '(0.5 . 0.2))  ; Automatically update info popup after that numver of seconds
;;   (corfu-preview-current 'insert) ; insert previewed candidate
;;   (corfu-preselect 'prompt)
;;   (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
;;   ;; Optionally use TAB for cycling, default is `corfu-complete'.
;;   :bind (:map corfu-map
;; 	      ("M-SPC"      . corfu-insert-separator)
;; 	      ("TAB"        . corfu-next)
;; 	      ([tab]        . corfu-next)
;; 	      ("S-TAB"      . corfu-previous)
;; 	      ([backtab]    . corfu-previous)
;; 	      ("S-<return>" . corfu-insert)
;; 	      ("RET"        . corfu-insert))

;;   :init
;;   (global-corfu-mode)
;;   (corfu-history-mode)
;;   (corfu-popupinfo-mode) ; Popup completion info
;;   :config
;;   (add-hook 'eshell-mode-hook
;;             (lambda () (setq-local corfu-quit-at-boundary t
;;                                    corfu-quit-no-match t
;;                                    corfu-auto nil)
;;               (corfu-mode))
;;             nil
;;             t))
;; (use-package web-mode)
;; (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)) 
;; (add-to-list 'auto-mode-alist '("\\.tsx?$" . web-mode)) 
;; (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
;; (use-package lsp-mode
;;   :diminish "LSP"
;;   :ensure t
;;   :hook ((lsp-mode . lsp-diagnostics-mode)
;; 	 (lsp-mode . lsp-enable-which-key-integration)
;; 	 ((tsx-ts-mode
;; 	   typescript-mode
;; 	   js-mode
;; 	   typescript-ts-mode
;; 	   js-ts-mode) . lsp-deferred))
;;   :custom
;;   (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
;;   (lsp-completion-provider :none)       ; Using Corfu as the provider
;;   (lsp-diagnostics-provider :flycheck)
;;   (lsp-session-file (locate-user-emacs-file ".lsp-session"))
;;   (lsp-log-io nil)                      ; IMPORTANT! Use only for debugging! Drastically affects performance
;;   (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
;;   (lsp-idle-delay 0.5)                  ; Debounce timer for `after-change-function'
;;   ;; core
;;   (lsp-enable-xref t)                   ; Use xref to find references
;;   (lsp-auto-configure t)                ; Used to decide between current active servers
;;   (lsp-eldoc-enable-hover t)            ; Display signature information in the echo area
;;   (lsp-enable-dap-auto-configure t)     ; Debug support
;;   (lsp-enable-file-watchers nil)
;;   (lsp-enable-folding nil)              ; I disable folding since I use origami
;;   (lsp-enable-imenu t)
;;   (lsp-enable-indentation nil)          ; I use prettier
;;   (lsp-enable-links nil)                ; No need since we have `browse-url'
;;   (lsp-enable-on-type-formatting nil)   ; Prettier handles this
;;   (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
;;   (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
;;   (lsp-enable-text-document-color nil)   ; This is Treesitter's job

;;   (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
;;   (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
;;   ;; completion
;;   (lsp-completion-enable t)
;;   (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
;;   (lsp-enable-snippet t)                         ; Important to provide full JSX completion
;;   (lsp-completion-show-kind t)                   ; Optional
;;   ;; headerline
;;   (lsp-headerline-breadcrumb-enable t)  ; Optional, I like the breadcrumbs
;;   (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
;;   (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
;;   (lsp-headerline-breadcrumb-icons-enable nil)
;;   ;; modeline
;;   (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
;;   (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
;;   (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
;;   (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
;;   (lsp-ui-doc-use-childframe t)              ; Show docs for symbol at point
;;   (lsp-eldoc-render-all nil)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
;;   ;; lens
;;   (lsp-lens-enable nil)                 ; Optional, I don't need it
;;   ;; semantic
;;   (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter

;;   :init
;;   (setq lsp-use-plists t))

;; (add-hook 'emacs-lisp-mode-hook (lambda () (setq-local lsp-mode -1)))
;; (add-hook 'haskell-mode-hook (lambda () (setq-local lsp-mode -1)))

;; (use-package typescript-mode
;;   :ensure t
;;   :mode "\\.tsx\\'"
;;   :hook (typescript-mode . lsp-deferred))



;; (use-package lsp-ui
;;   :ensure t
;;   :commands
;;   (lsp-ui-doc-show
;;    lsp-ui-doc-glance)
;;   :bind (:map lsp-mode-map
;; 	      ("C-c C-d" . 'lsp-ui-doc-glance))
;;   :after (lsp-mode evil)
;;   :config (setq lsp-ui-doc-enable t
;; 		evil-lookup-func #'lsp-ui-doc-glance ; Makes K in evil-mode toggle the doc for symbol at point
;; 		lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
;; 		lsp-ui-doc-include-signature t       ; Show signature
;; 		lsp-ui-doc-position 'at-point)
;;   ;; add hook to disable lsp-mode in emacs-lisp mode and haskell-mode
;;   )

;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode)
;;   :bind (:map flycheck-mode-map
;; 	      ("M-N" . flycheck-next-error) ; optional but recommended error navigation
;; 	      ("M-P" . flycheck-previous-error)))
;; (setq-default flycheck-disabled-checkers
;;               (append flycheck-disabled-checkers
;;                       '(javascript-jshint json-jsonlist)))
;; ;; Enable eslint checker for web-mode
;; (flycheck-add-mode 'javascript-eslint 'web-mode)
;; ;; Enable flycheck globally
;; (add-hook 'after-init-hook #'global-flycheck-mode)
;; (use-package add-node-modules-path)
;; (add-hook 'flycheck-mode-hook 'add-node-modules-path)
;; (use-package prettier-js)
;; (use-package emmet-mode)
;; (add-hook 'web-mode-hook 'emmet-mode)
;; (defun web-mode-init-prettier-hook ()
;;   (add-node-modules-path)
;;   (prettier-js-mode))

;; (add-hook 'web-mode-hook  'web-mode-init-prettier-hook)
;; (use-package lsp-tailwindcss
;;   :straight '(lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss")
;;   :init (setq lsp-tailwindcss-add-on-mode t)
;;   :config
;;   (dolist (tw-major-mode
;; 	   '(css-mode
;; 	     css-ts-mode
;; 	     typescript-mode
;; 	     typescript-ts-mode
;; 	     tsx-ts-mode
;; 	     js2-mode
;; 	     js-ts-mode
;; 	     clojure-mode))
;;     (add-to-list 'lsp-tailwindcss-major-modes tw-major-mode)))
;; (setq read-process-output-max (* 10 1024 1024)) ;; 10mb
;; (setenv "LSP_USE_PLISTS" "true") ;; in early-init.el
;; (setq gc-cons-threshold 200000000)
;; ;; (use-package lsp-typescript)
;; (setq lsp-response-timeout 30)
;; (setq lsp-clients-typescript-tsserver "~/.local/share/nvim/mason/bin/typescript-language-server")
;; ; END tsx 

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

;; disable the warning from copilot about copilot--infer-indendation offset
(setq copilot--infer-indentation nil)

(global-set-key (kbd "C-x 2") 'my-split-window-below-compilation)
(global-set-key (kbd "C-x c") 'my-split-window-left-compilation)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 4 1000 1000))

;; disable lsp mode for all filetypes except js,ts,tsx,jsx
(setq lsp-disabled-clients '(css-ls html-ls json-ls))
(setq lsp-enabled-clients '(ts-ls))
(add-hook 'after-init-hook 'org-agenda-list)

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
