(require 'package)
(load "package")
(package-initialize)

(setq repository-list
  '(("gnu" . "https://elpa.gnu.org/packages/")
    ("melpa" . "https://melpa.org/packages/")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-archives repository-list))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

(dolist (package '(use-package poet-theme tao-theme xah-math-input org-bullets visual-regexp dash-functional typescript-mode spacemacs-theme markdown-mode use-package dashboard smex neotree auto-complete cider paredit htmlize magit clojure-mode rainbow-delimiters dracula-theme monokai-theme github-theme cyberpunk-theme sublime-themes writeroom-mode))
   (unless (package-installed-p package)
       (package-install package)))


(org-babel-load-file "~/emacsconfig.org")


