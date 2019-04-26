(custom-set-variables
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (misterioso)))
 '(line-number-mode t)
 '(package-selected-packages
   (quote
    (meghanada
     smex 
     neotree
     auto-complete
     cider 
     paredit
     htmlize
     doom-themes
     haskell-mode
     python-mode
     magit
     clojure-mode
     rainbow-delimiters))))

(custom-set-faces)


;; config package
(load "package")
(package-initialize)

;; emacs visual configurations 
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode 1)

;; enable slelction to X
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
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "<f8>") 'neotree-toggle)


;; smex config
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;; ido naviage filesystems
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)


;; enable line numbers
(setq column-number-mode t)

;; disable temporary files
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; python
(setq python-shell-interpreter "C:\\Users\\riznychuk\\AppData\\Local\\Programs\\Python\\Python37\\python.exe")
