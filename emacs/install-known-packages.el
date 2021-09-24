(setq package-set-helm '(helm helm-ag helm-projectile))
(setq package-set-themes '(sublime-themes cyberpunk-theme dracula-theme twilight-bright-theme spacemacs-theme))
(setq package-set-customize '(rainbow-delimiters neotree dashboard use-package))
(setq package-set-development '(projectile magit htmlize paredit cider auto-complete visual-regexp markdown-mode))
(dolist (package (append package-set-development
			 package-set-customize
			 package-set-themes
			 package-set-helm))
  (unless (package-installed-p package)
    (package-install package)))
