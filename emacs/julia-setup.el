(require 'package)
(setq repository-list
  '(("gnu" . "https://elpa.gnu.org/packages/")
    ("melpa" . "https://melpa.org/packages/")))

(setq package-archives repository-list)


(setq julia-package-set-helm '(helm helm-ag helm-projectile))
(setq julia-package-set-themes '(sublime-themes cyberpunk-theme dracula-theme twilight-bright-theme spacemacs-theme))
(setq julia-package-set-customize '(rainbow-delimiters neotree dashboard use-package))
(setq julia-package-set-development '(projectile magit htmlize paredit cider auto-complete visual-regexp markdown-mode))
(dolist (package (append julia-package-set-development
			 julia-package-set-customize
			 julia-package-set-themes
			 julia-package-set-helm))
   (unless (package-installed-p package)
       (package-install package)))
