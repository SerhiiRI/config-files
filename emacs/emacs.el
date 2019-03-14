;;; init.el --- Spacemacs Initialization File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

;; Increase gc-cons-threshold, depending on your system you may set it back to a
;; lower value in your dotfile (function `dotspacemacs/user-config')
(setq gc-cons-threshold 100000000)

(defconst spacemacs-version         "0.200.13" "Spacemacs version.")
(defconst spacemacs-emacs-min-version   "24.4" "Minimal version of Emacs.")

(if (not (version<= spacemacs-emacs-min-version emacs-version))
    (error (concat "Your version of Emacs (%s) is too old. "
                   "Spacemacs requires Emacs version %s or above.")
           emacs-version spacemacs-emacs-min-version)
  (load-file (concat (file-name-directory load-file-name)
                     "core/core-load-paths.el"))
  (require 'core-spacemacs)
  (spacemacs/init)
  (configuration-layer/sync)
  (spacemacs-buffer/display-startup-note)
  (spacemacs/setup-startup-hook)
  (require 'server)
  (unless (server-running-p) (server-start)))



(require 'neotree)
(global-set-key [f8] 'neotree-toggle)


(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode)
  (display-line-numbers-mode))


(setq x-select-enable-clipboard t)

(global-unset-key (kbd "C-z"))

(global-set-key (kbd "<f12>") 'tomatinho)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))


(require 'flymd)
(require 'magit)
(require 'slime)
(require 'cider)
(require 'nov)
(defun my-flymd-browser-function (url)
  (let ((browse-url-browser-function 'browse-url-firefox))
    (browse-url url)))
(setq flymd-browser-open-function 'my-flymd-browser-function)

(eval-after-load "org" '(require 'ox-gfm nil t))
(eval-after-load "org" '(global-set-key (kbd "C-C l") 'org-store-link))
