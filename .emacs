

(add-to-list 'load-path "~/.emacs.d/libraries/")

;; MELPA
(require 'package) ;; You might already have this line
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; MELPA slut

(package-initialize)

(global-set-key (kbd "<down>") (kbd "C-u 3 C-v"))
(global-set-key (kbd "<up>") (kbd "C-u 3 M-v"))
(global-set-key (kbd "M-_") 'comment-region)
(setq backup-directory-alist `(("." . "~/.saves")))
(show-paren-mode 1)
(icomplete-mode 99)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(global-linum-mode 1)
(setq linum-format "%d ")

(package-initialize)
(elpy-enable)

(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))

;; (eval-after-load "LaTeX")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elpy-rpc-python-command "python3")
 '(python-shell-interpreter "python3")
 '(tab-stop-list (number-sequence 4 200 4)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
