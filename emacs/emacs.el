(require 'package)
(load "package")
(package-initialize)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(hickey))
 '(custom-safe-themes
   '("e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" default))
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(org-bullets visual-regexp dash-functional typescript-mode spacemacs-theme markdown-mode use-package dashboard smex neotree auto-complete cider paredit htmlize magit clojure-mode rainbow-delimiters dracula-theme monokai-theme github-theme cyberpunk-theme sublime-themes writeroom-mode)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

(dolist (package '(use-package))
   (unless (package-installed-p package)
       (package-install package)))

;; (mapcar (lambda (x) (package-install x))
;;     '(writeroom-mode sublime-themes cyberpunk-theme github-theme monokai-theme dracula-theme rainbow-delimiters clojure-mode magit htmlize paredit cider auto-complete neotree smex dashboard use-package markdown-mode spacemacs-theme typescript-mode))

;; (dolist (package '(org-bullets visual-regexp dash-functional typescript-mode spacemacs-theme markdown-mode use-package dashboard smex neotree auto-complete cider paredit htmlize magit clojure-mode rainbow-delimiters dracula-theme monokai-theme github-theme cyberpunk-theme sublime-themes writeroom-mode))


;; emacs visual configurations 
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(menu-bar-mode   -1)

;; enable line numbers
(setq column-number-mode t)

;; disable temporary files
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; enable selection to X
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; Display empty line
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; disable backup files
(setq make-backup-files nil)

;; Key binding
(global-set-key (kbd "C-'") '(lambda () (interactive) (browse-url "www.google.com.pl")))
(global-set-key (kbd "C-c n") 'display-line-numbers-mode)
;; (global-set-key (kbd "C-\"") '(lambda () (interactive) (eww "www.google.com.pl")))
;; (global-set-key (kbd "C-x g") 'magit-status)
;; (global-set-key (kbd "<f8>") 'neotree-toggle)
;; (global-set-key (kbd "C-c t") 'neotree-toggle)

;; move line down/up
(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))
(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))
(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))
(global-set-key (kbd "M-S-<up>") 'move-text-up)
(global-set-key (kbd "M-S-<down>") 'move-text-down)

;;; magit
(use-package magit
  :ensure t
  :defer 3
  :bind (("C-x g" . magit-status)))

;;; neotree configuration
(use-package neotree 
  :ensure t
  :defer 3
  :bind (("<f8>" . neotree-toggle)
	 ("C-c t" . neotree-toggle)))

;;; smex
(use-package smex
  :after package
  :ensure t
  :init
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))


;;; ido 
(use-package ido
  :init
  (setq ido-enable-flex-matching t
	ido-use-virtual-buffers t)
  (ido-mode t))

;; auto-complete
(use-package auto-complete-config
  :config (ac-config-default))
;; (require 'auto-complete-config)
;; (ac-config-default)


;;; lisp-mode
(defun lisp-mode-hook ()
  "enable some plugins after init mode"
  (paredit-mode)
  (prettify-symbols-mode)
  (rainbow-delimiters-mode))
(use-package elisp-mode
  :config (add-hook 'emacs-lisp-mode-hook 'lisp-mode-hook))
(use-package lisp-mode
  :defer t
  :config (add-hook 'lisp-mode-hook 'lisp-mode-hook))
(use-package clojure-mode
  :defer t
  :config (add-hook 'clojure-mode-hook 'lisp-mode-hook))


;;; recentf package keep paths of your last edited files
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))
(use-package recentf
  :bind (("C-c C-b" . recentf-open-files)
	 ("C-c C-r" . ido-recentf-open))
  :config
  (setq recentf-max-menu-items 30
	recentf-max-saved-items 50)
  (recentf-mode 1))



;;; dashboard configuration
(use-package dashboard
    :demand t
    :ensure t
    ;; :diminish dashboard-mode
    :init
    (setq dashboard-banner-logo-title "* Live Long And Prosper *")
    (setq dashboard-startup-banner "~/Spock.png")
    (setq dashboard-page-separator "\n\n")
    (setq dashboard-set-init-info nil)
    (setq dashboard-center-content t)
    (setq dashboard-show-shortcuts t)
    (setq dashboard-set-footer nil)
    (setq dashboard-items '((recents  . 13)(bookmarks . 10)(agenda)))
    :config
    (dashboard-setup-startup-hook))

;;; whiteroom mode
(use-package writeroom-mode
  :ensure t
  :bind (("C-M-<" . writeroom-decrease-width)
	 ("C-M->" . writeroom-increase-width)
	 ("C-M-=" . writeroom-adjust-width)))

;; dash-functions
(use-package dash
  ;; :no-require t
  :after dash
  :config
  (dash-enable-font-lock))


;; org-mode
(use-package org
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda))
  :init (setq org-agenda-files (list "C:/space/agenda.org")
	      org-log-done t
	      org-src-tab-acts-natively t))


;;; org bullets
(use-package org-bullets
  :after org
  :config
  ;; (setq org-bullets-bullet-list '("α" "β" "γ" "δ" "ε" "ζ" "η" "λ"))
  ;; (setq org-bullets-bullet-list '("●" "◉" "○" "◆" "◇"))
  ;; (setq org-bullets-bullet-list '("◆"))
  (setq org-bullets-bullet-list '("●"))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


;;; repalace regular regexp on visula regexp
(use-package visual-regexp
  :bind (("C-c r" . vr/replace)
	 ("C-c q" . vr/query-replace)))


;; (org-babel-load-file "~/emacsconfig.org")

;;; fonts
;; (set-frame-font "Inconsolata SemiCondensed Extra")
;; (set-frame-font "Inconsolata SemiCondensed Light")
;; (set-frame-font "Inconsolata SemiCondensed")
;; (set-frame-font "Inconsolata SemiCondensed Mediu")
;; (set-frame-font "Inconsolata SemiCondensed SemiB")
;; (set-frame-font "Inconsolata SemiCondensed Black")
;; (set-frame-font "Inconsolata ExtraLight")
;; (set-frame-font "Inconsolata Light")
;; (set-frame-font "Inconsolata")
;; (set-frame-font "Inconsolata Medium")
;; (set-frame-font "Inconsolata SemiBold")
;; (set-frame-font "Inconsolata ExtraBold")
;; (set-frame-font "Inconsolata Black")
;; (set-frame-font "Inconsolata SemiExpanded ExtraL")
;; (set-frame-font "Inconsolata SemiExpanded Light")
;; (set-frame-font "Inconsolata SemiExpanded")
;; (set-frame-font "Inconsolata SemiExpanded Medium")
;; (set-frame-font "Inconsolata SemiExpanded SemiBo")
;; (set-frame-font "Inconsolata SemiExpanded ExtraB")
;; (set-frame-font "Inconsolata SemiExpanded Black")
;; (set-frame-font "PT Mono")
;; (set-frame-font "Noto Mono")
;; (set-frame-font "Source Code Pro")
;; (set-frame-font "Source Code Pro ExtraLight")
;; (set-frame-font "Source Code Pro Light")
;; (set-frame-font "Source Code Pro Medium")
;; (set-frame-font "Source Code Pro Semibold")
;; (set-frame-font "Ubuntu Mono Light")
;; (set-frame-font "JetBrains Mono Thin")
;; (set-frame-font "JetBrains Mono ExtraLight")
;; (set-frame-font "JetBrains Mono Light")
;; (set-frame-font "JetBrains Mono")
;; (set-frame-font "JetBrains Mono Medium")
;; (set-frame-font "JetBrains Mono ExtraBold")
;; (set-frame-font "Courier Prime")
;; (set-frame-font "IBM Plex Mono")
;; (set-frame-font "IBM Plex Mono ExtraLight")
;; (set-frame-font "IBM Plex Mono Light")
;; (set-frame-font "IBM Plex Mono Medium")
;; (set-frame-font "IBM Plex Mono SemiBold")
;; (set-frame-font "IBM Plex Mono Thin")
;; (set-frame-font "Space Mono")
