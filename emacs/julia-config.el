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
 '(custom-enabled-themes '(dracula))
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-archives repository-list)
 '(package-selected-packages 'nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

(load-theme 'dracula t)

;;; QUICK INSTALL PACKAGES
;; (dolist (package '(visual-regexp markdown-mode neotree auto-complete paredit htmlize rainbow-delimiters dracula-theme slime))
;;    (unless (package-installed-p package)
;;        (package-install package)))


;;; DISABLE UI COMPONENTS
(scroll-bar-mode -1)
(tool-bar-mode   -1)	
(menu-bar-mode   -1)

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

;;; KEYBINDGS
(global-set-key (kbd "C-c n") 'display-line-numbers-mode)
(global-set-key (kbd "<f8>") 'neotree-toggle)

;;; IVY MODE
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;;; LISP MODE CONFIGURATIONS
(defun lisp-mode-hook ()
      "enable some plugins after init mode"
      (paredit-mode)
      (rainbow-delimiters-mode))
(add-hook 'emacs-lisp-mode-hook 'lisp-mode-hook)
(add-hook 'lisp-mode-hook 'lisp-mode-hook)

(setq inferior-lisp-program "sbcl")
