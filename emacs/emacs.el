(require 'package)

(load "package")
(package-initialize)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(org-bullets visual-regexp dash-functional typescript-mode spacemacs-theme markdown-mode use-package dashboard smex neotree auto-complete cider paredit htmlize magit clojure-mode rainbow-delimiters dracula-theme monokai-theme github-theme cyberpunk-theme sublime-themes writeroom-mode)))
;; (dolist (package '(org-bullets visual-regexp dash-functional typescript-mode spacemacs-theme markdown-mode use-package dashboard smex neotree auto-complete cider paredit htmlize magit clojure-mode rainbow-delimiters dracula-theme monokai-theme github-theme cyberpunk-theme sublime-themes writeroom-mode))
;;  (unless (package-installed-p package)
;;    (package-install package))
;;    (require package))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))



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
(global-set-key (kbd "C-\"") '(lambda () (interactive) (eww "www.google.com.pl")))
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "<f8>") 'neotree-toggle)
(global-set-key (kbd "C-c t") 'neotree-toggle)

;; smex config
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; ido naviage filesystems
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)


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

;;; lisp-mode
(defun lisp-mode-hook ()
  "enable some plugins after init mode"
  (paredit-mode)
  (prettify-symbols-mode)
  (rainbow-delimiters-mode))
(add-hook 'emacs-lisp-mode-hook 'lisp-mode-hook)
(add-hook 'lisp-mode-hook 'lisp-mode-hook)
(add-hook 'clojure-mode-hook 'lisp-mode-hook)

;;; recentf package keep paths of your last edited files
(recentf-mode 1)
(setq recentf-max-menu-items 30)
(setq recentf-max-saved-items 50)
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))
(global-set-key (kbd "C-c C-b") 'recentf-open-files)
(global-set-key (kbd "C-c C-r") 'ido-recentf-open)

;;; dashboard configuration 
(require 'dashboard)
(setq dashboard-banner-logo-title "* Live Long And Prosper *")
(setq dashboard-startup-banner "~/Spock.png")
(setq dashboard-page-separator "\n\n")
(setq dashboard-set-init-info nil)
(setq dashboard-center-content t)
(setq dashboard-show-shortcuts t)
(setq dashboard-set-footer nil)
(setq dashboard-items '((recents  . 13)
			(bookmarks . 10)
			(agenda)))
;; (dashboard-setup-startup-hook)
;; Or if you use use-package
(use-package dashboard
  :ensure t
  :config (dashboard-setup-startup-hook))

;;; whiteroom mode
(with-eval-after-load 'writeroom-mode
  (define-key writeroom-mode-map (kbd "C-M-<") #'writeroom-decrease-width)
  (define-key writeroom-mode-map (kbd "C-M->") #'writeroom-increase-width)
  (define-key writeroom-mode-map (kbd "C-M-=") #'writeroom-adjust-width))

;; dash-functions
(eval-after-load 'dash '(dash-enable-font-lock))

;; org-mode
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(setq org-agenda-files (list "C:/space/agenda.org"))
(setq org-log-done t)

(require 'org-bullets)
;; (setq org-bullets-bullet-list '("α" "β" "γ" "δ" "ε" "ζ" "η" "λ"))
;; (setq org-bullets-bullet-list '("●" "◉" "○" "◆" "◇"))
;; (setq org-bullets-bullet-list '("◆"))
(setq org-bullets-bullet-list '("●"))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;;; repalace regular regexp on visula regexp
(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
