(require 'package)
(load "package")
(package-initialize)

(setq custom-safe-themes t)

(setq repository-list
  '(("gnu" . "https://elpa.gnu.org/packages/")
    ("melpa" . "https://melpa.org/packages/")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3c3836" "#fb4934" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(ansi-term-color-vector
   [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"])
 '(custom-enabled-themes '(dracula))
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#d9d9d9")
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-archives repository-list)
 '(package-selected-packages
   '(twilight-bright-theme twilight-theme zzz-to-char espresso-theme gruvbox-theme mood-line counsel-projectile projectile magit dashboard cider visual-regexp slime-volleyball slime rainbow-delimiters paredit neotree markdown-mode ivy htmlize dracula-theme auto-complete))
 '(pdf-view-midnight-colors '("#282828" . "#f9f5d7")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JetBrains Mono")))))

;;; DISABLE UI COMPONENTS
(scroll-bar-mode -1)
(tool-bar-mode   -1)	
(menu-bar-mode   -1)
(mood-line-mode)

;;; DISABLE MAKING BACKUP FILES
(setq custom-safe-themes t)
(setq make-backup-files nil)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))


;;; MAKE GLOBAL CLIPBOARDS
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;;;;;;;;;;;;;;;;
;;; PACKAGES ;;;
;;;;;;;;;;;;;;;;

;KEYBINDGS
(global-set-key (kbd "C-c n") 'display-line-numbers-mode)
(global-set-key (kbd "C-c r") 'vr/replace)
(global-set-key (kbd "C-c q") 'vr/query-replace)
(global-set-key (kbd "<f8>") 'neotree-toggle)

;; helm
(require 'helm-config)
(require 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(helm-mode 1)

(defun lisp-mode-hook ()
      "enable some plugins after init mode"
      (paredit-mode)
      (rainbow-delimiters-mode))
(add-hook 'clojure-mode-hook 'lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'lisp-mode-hook)
(add-hook 'lisp-mode-hook 'lisp-mode-hook)

(require 'dashboard)
(dashboard-setup-startup-hook)

;; (setq dashboard-banner-logo-title "* Live Long And Prosper *")
;; (setq dashboard-startup-banner "~/Spock.png")
(setq dashboard-page-separator "\n\n")
;; (setq dashboard-set-init-info nil)
(setq dashboard-center-content t)
;; (setq dashboard-show-shortcuts t)
;; (setq dashboard-set-footer nil)
(setq dashboard-items '((recents  . 15) (projects . 5) (bookmarks . 5)))
;;(setq inferior-lisp-program "sbcl")
(counsel-projectile-mode 1)
(global-set-key (kbd "M-x") 'counsel-M-x)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
