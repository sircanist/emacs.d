;; Setup packages and stuff
(require 'package)

;; Add MELPA
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(unless package--initialized
  (package-initialize))

;; List of packages to install
(setq package-selected-packages
      '(
        evil
        evil-leader
        evil-collection
        projectile
        counsel
        ivy-hydra
        mood-line
        evil-terminal-cursor-changer
        modus-vivendi-theme
        modus-vivendi-theme
        rainbow-delimiters
        org-roam
        htmlize
        org-download
        ))

;; Ensure all the packages are installed
(dolist (package package-selected-packages)
  (unless (package-installed-p package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install package)))



;; Sane defaults
(setq
 confirm-kill-processes nil
 frame-title-format '("%b - Emacs Vanilla")
 create-lockfiles nil
 make-backup-files nil
 custom-file (expand-file-name "custom.el" user-emacs-directory)
 )
(load custom-file)

(setq evil-undo-system 'undo-redo)

(setq-default indent-tabs-mode nil
              tab-width 4)

;; Clear whitespace when saving file
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Setup evil and stuff
(setq evil-want-keybinding nil)
(require 'evil)
(when (require 'evil-collection nil t)
  (evil-collection-init))
(evil-mode 1)

;; Setup counsel and ivy
(require 'counsel)
(counsel-mode)
(setq ivy-initial-inputs-alist nil)
(setq ivy-read-action-function #'ivy-hydra-read-action)
(setq ivy-read-action-format-function #'ivy-read-action-format-columns)

;; setup projectile
(require 'projectile)
;; Recommended keymap prefix on Windows/Linux
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)


;; Collect list of recently opened files
(recentf-mode)

;; Highlight matching parens
(show-paren-mode)

;; Enable rainbow parens
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Misc GUI settings
(setq inhibit-splash-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Enable nice minimal mode-line
(mood-line-mode)

   ;; set fonts
   (progn
      (set-face-attribute 'default nil
                          :height 120
                          ;:family "Monospace"
                          :family "Office Code Pro"
                          )
      ;)

  ;; set theme
  (progn
    (require 'evil-terminal-cursor-changer)
    (evil-terminal-cursor-changer-activate)
    (setq evil-motion-state-cursor 'box)
    (setq evil-visual-state-cursor 'box)
    (setq evil-normal-state-cursor 'box)
    (setq evil-insert-state-cursor 'bar)
    (setq evil-emacs-state-cursor  'hbar)
    (load-theme 'modus-vivendi t)
    (custom-theme-set-faces
     'modus-vivendi
     ;'(default ((t (:background nil))))
     )))


;; Global Key Bindings
(global-set-key (kbd "M-a") 'mark-whole-buffer)
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-f") 'swiper)
(global-set-key (kbd "M-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-j") 'evil-window-down)
(global-set-key (kbd "C-k") 'evil-window-up)
(global-set-key (kbd "C-h") 'evil-window-left)
(global-set-key (kbd "C-l") 'evil-window-right)
(global-set-key (kbd "C-SPC") 'complete-symbol)

;; Evil Leader
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "SPC")

(evil-leader/set-key
  "." 'counsel-find-file
  "," 'counsel-switch-buffer
  "f r" 'counsel-recentf
  "b s" 'save-buffer
  "b d" 'evil-delete-buffer
  "b k" (lambda ()
         (interactive)
         (kill-buffer (current-buffer)))
  "w h" 'evil-window-split
  "w v" 'evil-window-vsplit
  "w c" 'evil-window-delete
  "c s" 'eval-last-sexp
  "q"  'evil-quit
  "h e" 'view-echo-area-messages
  "w m" 'toggle-maximize-buffer
  "p SPC" 'projectile-switch-project
  "p p" 'projectile-switch-project
  "p a" 'projectile-add-known-project
  "h f" 'counsel-describe-function
  "h v" 'counsel-describe-variable
  "h ." 'describe-symbol
  "s b" 'swiper
  "s d" 'counsel-rg
  "r f" 'org-roam-node-find
  "r i" 'org-roam-node-insert
  "r t" 'org-roam-buffer-toggle
  "r j n" 'org-roam-dailies-capture-today
  "r j t" 'org-roam-dailies-capture-tomorrow
  "r j y" 'org-roam-dailies-capture-yesterday
  )

(defun toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

; org-roam
(setq org-roam-directory (file-truename "~/notes"))
(org-roam-db-autosync-mode)

(add-to-list 'org-roam-capture-templates
             '("p" "project" plain (file "~/notes/templates/project.org")
               :target (file+head "project/%<%Y-%m-%dT%H%M%S>-${slug}.org" "#+title: ${title}\n#+created: %<%Y-%m-%dT%H%M%S>\n#+filetags: project")
               :unnarrowed t))

(add-to-list 'org-roam-capture-templates
             '("h" "hacking" plain (file "~/notes/templates/hacking.org")
               :target (file+head "hacking/%<%Y-%m-%dT%H%M%S>-${slug}.org" "#+title: ${title}\n#+created: %<%Y-%m-%dT%H%M%S>\n#+filetags: hacking")
               :unnarrowed t))


;; Drag-and-drop to `dired` org-download
(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)


; use undo-tree for undo system
;(global-undo-tree-mode)
