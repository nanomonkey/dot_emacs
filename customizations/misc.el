;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; Lockfiles
;; need for ~ files when editing
(setq create-lockfiles t)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; Org-mode Babel
(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (clojure . t)
   (shell . t)
   (python . t)
   (scheme . t) ))

(defun nolinum ()
  (interactive)
  (message "Deactivated linum mode")
  (global-linum-mode 0)
  (linum-mode 0))

(global-set-key (kbd "<f6>") 'nolinum)

(add-hook 'org-mode-hook 'nolinum)
