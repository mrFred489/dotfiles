
(require 'package)
(package-initialize)

(org-babel-load-file "~/Dropbox/dotfiles/emacs/configuration.org")





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes (quote (adwaita)))
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(eldoc-idle-delay 0.2)
 '(elfeed-feeds
   (quote
    ("http://feeds.feedburner.com/TroyHunt?format=xml" "http://nullprogram.com/feed/" "http://planet.emacsen.org/atom.xml")))
 '(elpy-mode-hook
   (quote
    (subword-mode
     (lambda nil
       (highlight-indentation-mode -1))
     flycheck-mode)))
 '(elpy-rpc-python-command "python3")
 '(https://feeds\.feedburner\.com/TroyHunt nil)
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . emacs))))
 '(package-selected-packages
   (quote
    (latex-pretty-symbols pretty-symbols projectile jedi auto-complete elfeed flycheck-pycheckers flycheck ac-ispell sml-mode nlinum magit llvm-mode linum-relative lex elpygen elpy)))
 '(python-shell-interpreter "python3")
 '(tab-stop-list (number-sequence 4 200 4)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
