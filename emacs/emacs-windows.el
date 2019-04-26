(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (misterioso)))
 '(line-number-mode t)
 '(package-selected-packages
   (quote
    (meghanada smex neotree auto-complete cider paredit htmlize doom-themes haskell-mode python-mode magit clojure-mode rainbow-delimiters))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
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
(global-set-key (kbd "C-'") '(lambda () (interactive) (browse-url "www.google.com.pl")))
(global-set-key (kbd "C-\"") '(lambda () (interactive) (eww "www.google.com.pl")))
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

;; java config meghanada
(require 'meghanada)
(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (flycheck-mode +1)
            (setq c-basic-offset 2)
            ;; use code format
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
(cond
   ((eq system-type 'windows-nt)
    (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
    (setq meghanada-maven-path "mvn.cmd"))
   (t
    (setq meghanada-java-path "java")
    (setq meghanada-maven-path "mvn")))



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

(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)
