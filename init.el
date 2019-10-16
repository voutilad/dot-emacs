;; Bootstrap org-mode so we can initialize using org files

;; I think this is to get rid of that annoying crap Package.el writes here?
(setq package--init-file-ensured t)

(defun my/log (msg)
  (message (concat ">>> " msg)))

(if (eq system-type 'windows-nt)
  (progn (set-language-environment "UTF-8")
         (require 'package)
         (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
         (require 'org))
  (progn (defconst orgdir (expand-file-name "vendor/org-mode" user-emacs-directory))
         (my/log (concat "Loading local org babel tangle (ob-tangle) install from " orgdir))
         (add-to-list 'load-path (expand-file-name "lisp" orgdir))))

(require 'ob-tangle)
(my/log (concat "ob-tangle loaded!"))

;; switch to initializing via org files
(org-babel-load-file
 (expand-file-name "emacs-init.org" user-emacs-directory))
