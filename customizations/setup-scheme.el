(add-hook 'scheme-mode-hook 'turn-on-paredit)
;; (setq geiser-active-implementations '(racket))
(setq geiser-scheme-implementation "/usr/local/scmutils/mit-scheme")

(add-hook 'scheme-mode-hook
          (lambda ()
            (define-key scheme-mode-map (kbd "C-c l") "lambda")
            (define-key inferior-scheme-mode-map (kbd "C-c l") "lambda")))


;; (when (window-system)
;;   (add-hook 'scheme-mode-hook 'pretty-lambdas-haskell)
;;   (add-hook 'inferior-scheme-mode-hook 'pretty-lambdas-haskell))
;; (require 'quack)
;; ;; (add-hook 'scheme-mode-hook 'idle-highlight)
;; (font-lock-add-keywords 'scheme-mode
;;                         '(("(\\|)" . 'starter-kit-paren-face)))

;; Mechanics
;; scheme mode for scmutils
(defun mechanics ()
  (interactive)
  (run-scheme 
    "/usr/local/scmutils/mit-scheme/bin/scheme --library /usr/local/scmutils/mit-scheme/lib"
  ))

;; (defun mechanics ()
;;   (interactive)
;;   (run-scheme "mech-scheme"))

;; Friendly font-lock from https://gist.github.com/dleslie/1257860
(custom-set-faces
 '(quack-about-face ((t (:inherit font-lock-warning))))
 '(quack-about-title-face ((t (:inherit font-lock-warning :weight bold :height 2.0))))
 '(quack-banner-face ((t (:inherit font-lock-warning-face))))
 '(quack-pltfile-dir-face ((t (:inherit dired-face-directory))))
 '(quack-pltfile-file-face ((t (:inherit dired-face-file ))))
 '(quack-pltfile-prologue-face ((((class color)) (:inherit dired-face-boring))))
 '(quack-pltish-class-defn-face ((((class color) (background dark)) (:inherit font-lock-type-face))))
 '(quack-pltish-colon-keyword-face ((t (:inherit font-lock-keyword-face))))
 '(quack-pltish-comment-face ((((class color) (background dark)) (:inherit font-lock-comment-face))))
 '(quack-pltish-defn-face ((t (:inherit font-lock-function-name-face))))
 '(quack-pltish-keyword-face ((t (:inherit font-lock-keyword-face))))
 '(quack-pltish-module-defn-face ((((class color) (background dark)) (:inherit font-lock-type-face :weight bold))))
 '(quack-pltish-paren-face ((((class color) (background dark)) (:inherit font-lock-operator-face))))
 '(quack-pltish-selfeval-face ((((class color) (background dark)) (:inherit font-lock-string-face))))
 '(quack-threesemi-h1-face ((t (:inherit font-lock-doc-string-face :weight bold :height 1.4))))
 '(quack-threesemi-h2-face ((t (:inherit font-lock-doc-string-face :weight bold :height 1.2))))
 '(quack-threesemi-h3-face ((t (:inherit font-lock-doc-string-face :weight bold))))
 '(quack-threesemi-semi-face ((((class color) (background dark)) (:inherit font-lock-doc-face))))
 '(quack-threesemi-text-face ((((class color) (background dark)) (:inherit font-lock-doc-string-face)))))
