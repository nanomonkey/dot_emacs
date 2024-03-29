;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)
;(add-to-lis` 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
;(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/") t)
;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archives '(("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ido-ubiquitous

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; project navigation
    projectile

    ;; colorful parenthesis matching
    rainbow-delimiters
    
    ;; edit html tags like sexps
    tagedit

    ;; git integration
    magit))


(require 'org-passwords)
   (setq org-passwords-file "~/org/passwords.gpg")
   (setq org-passwords-random-words-dictionary "/etc/dictionaries-common/words")

(eval-after-load "org-passwords"
  '(progn
     (define-key org-passwords-mode-map
       (kbd "C-c u")
       'org-passwords-copy-username)
     (define-key org-passwords-mode-map
       (kbd "C-c p")
       'org-passwords-copy-password)
     (define-key org-passwords-mode-map
       (kbd "C-c o")
       '(lambda ()
          (interactive)
          (org-passwords-copy-password)
          (org-passwords-open-url)))))

(global-set-key (kbd "C-c q") 'org-passwords)


(require 'quelpa-use-package)

;; whisper.el

(use-package whisper
  :load-path "~/.emacs.d/vendor/whisper.el/"
  :bind ("C-H-r" . whisper-run)
  :config
  (setq whisper-install-directory "~/git/"
        whisper-model "base"
        whisper-language "en"
        whisper-translate nil))

;; Authentication
(setq auth-sources '("~/.netrc"))

;; Remove bytecomp warning messages at start-up by defining free variables
(defvar predicate nil)
(defvar inherit-input-method nil)
(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")


;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")
(load "setup-scheme.el")

;; Slime and quicklisp
;;(setq inferior-lisp-program "sbcl")
;;(load (expand-file-name "~/quicklisp/slime-helper.el"))


;;Journal
(defun journal-entry ()
  (interactive)
  (find-file "~/journal.txt")
  (goto-char (point-max))
  (insert "\n\n[ " (current-time-string) " ]\n"))


;; Rename buffer C-x C-r
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

(global-set-key (kbd "C-x t") 'treemacs-select-window) 

(global-set-key "\C-x\C-m" 'execute-extended-command)

;;(global-set-key "\C-w" 'backward-kill-word)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'fringe-mode) (fringe-mode 0))
(setq inhibit-startup-message t)

;; initial window
(setq initial-frame-alist
      '(
        (width . 85) ; character
        (height . 41) ; lines
        ))

;; default/sebsequent window
(setq default-frame-alist
      '(
        (width . 100) ; character
        (height . 52) ; lines
        ))



;;ParEdit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)


;; Abbreviations
(global-set-key (kbd "C-<tab>") 'dabbrev-expand) ;;dynamic abbreviations
(define-key minibuffer-local-map (kbd "C-<tab>") 'dabbrev-expand)

(setq abbrev-file-name "~/.emacs.d/abbrev_defs") ;;where to save abbreviations
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

;; MobileOrg Requirements
(setq org-directory "~/org")
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;; Org-mode 
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-ct" 'org-time-stamp)

(setq org-log-done t)
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)

;; Org Agenda
(setq org-agenda-files (list "~/org/work.org"
                             "~/org/House.org"))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("p" "password" entry (file "~/documents/passwords.gpg")
         "* %^{Title}\n  %^{URL}p %^{USERNAME}p %^{PASSWORD}p")))

(setq enable-recursive-minibuffers t)

;; Org babel
(org-babel-do-load-languages
  'org-babel-load-languages
  '((clojure . t)
    (scheme . t)
    (shell . t)
    (clojurescript . t)
    (emacs-lisp . t)
    (python . t)
    (C . t)
    (calc . t)))

(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "babashka")))  ;don't ask for babashka
(setq org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate)

(require 'org-tempo)
(require 'ob-clojurescript)
(require 'ob-clojure) ;; necessary with above?
(setq org-babel-clojure-backend 'cider)


;; (unless (package-installed-p 'quelpa)
;;   (with-temp-buffer
;;     (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
;;     (eval-buffer)
;;     (quelpa-self-upgrade)))

;; (quelpa
;;  '(quelpa-use-package
;;    :fetcher git
;;    :url "https://github.com/quelpa/quelpa-use-package.git"))
;; (require 'quelpa-use-package)


(unbind-key "C-z")
(bind-keys :prefix-map personal-ops-map
           :prefix "C-z"
           :prefix-docstring "Personal key bindings"
           ("v" . emacs-version))

;; embedded programming with gdb
(use-package embed
  :quelpa (embed :fetcher github :repo "sjsch/embed-el")
  :bind (("C-z e o" . embed-openocd-start)
         ("C-z e O" . embed-openocd-stop)
         ("C-z e g" . embed-openocd-gdb)
         ("C-z e f" . embed-openocd-flash)))

;; session.el
;(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; desktop mode for saving registers
;; don't use desktop mode for terminal
;; Automatically save and restore sessions
(setq desktop-dirname             "~/.emacs.d/desktop/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop nil
      desktop-auto-save-timeout   30)
(when (display-graphic-p)
  (desktop-save-mode 1);; is x window
  (desktop-save-mode 0))

;; Add variables to desktop saving
;(add-to-list 'desktop-globals-to-save 'register-alist)

;; Open dired in same buffer
(put 'dired-find-alternate-file 'disabled nil)
;; Sort Dired buffers
(setq dired-listing-switches "-agho --group-directories-first")


(with-eval-after-load 'org
  (mapcar
   (lambda (x) (add-to-list 'org-modules x t))
   '('org-timer 'org-bbdb 'org-bibtex 'org-docview 'org-gnus 'org-info 'org-irc 'org-mhe 'org-rmail 'org-w3m 'org-drill)))

;; M-x eval-buffer
;; to reload this buffer

;; C-h e
;; to see if there was a problem loading


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   '("5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "9e54a6ac0051987b4296e9276eecc5dfb67fdcd620191ee553f40a9b6d943e78" "52588047a0fe3727e3cd8a90e76d7f078c9bd62c0b246324e557dfa5112e0d0c" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))
 '(org-babel-load-languages
   '((clojure . t)
     (scheme . t)
     (shell . t)
     (clojurescript . t)
     (emacs-lisp . t)
     (python . t)
     (C . t)
     (calc . t)))
 '(package-selected-packages
   '(geiser-guile crdt embed f quelpa-use-package quelpa use-package ulisp-repl web-server nov asm-blox geiser-mit guix direnv helm-dash speed-type consult-recoll elfeed async dash parseclj parseedn cider deadgrep elpher emms inf-clojure org-drill vega-view clojurescript-mode org-babel-eval-in-repl calfw-org calfw org-chef sicp cider-decompile json-mode 4clojure ag htmlize luarocks markdown-mode markdown-mode+ lua-mode tagedit sos solarized-theme smex rainbow-delimiters projectile paredit magit ipython ido-ubiquitous exec-path-from-shell clojure-mode-extra-font-locking))
 '(pomodoro-break-time 17)
 '(pomodoro-work-time 57)
 '(safe-local-variable-values
   '((cider-shadow-watched-builds ":server" ":client")
     (cider-shadow-default-options . ":server")
     (cider-preferred-build-tool . shadow-cljs)
     (cider-default-cljs-repl . shadow)))
 '(scad-preview-image-size '(1600 . 1200))
 '(scad-preview-refresh-delay 0.1)
 '(scad-preview-view '("axes" (\, "scales")))
 '(scad-preview-window-size 100)
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#eaeaea" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 200 :width normal :foundry "nil" :family "Fira Code"))))
 '(quack-about-face ((t (:inherit font-lock-warning))))
 '(quack-about-title-face ((t (:inherit font-lock-warning :weight bold :height 2.0))))
 '(quack-banner-face ((t (:inherit font-lock-warning-face))))
 '(quack-pltfile-dir-face ((t (:inherit dired-face-directory))))
 '(quack-pltfile-file-face ((t (:inherit dired-face-file))))
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

;; CEDET
;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;; IMPORTANT: For Emacs >= 23.2, you must place this *before* any
;; CEDET component (including EIEIO) gets activated by another 
;; package (Gnus, auth-source, ...).
;; (load-file "~/.emacs.d/vendor/cedet/cedet-devel-load.el")

;; Add further minor-modes to be enabled by semantic-mode.
;; See doc-string of `semantic-default-submodes' for other things
;; you can use here.
;;(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
;;(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
;(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t)

;; Enable Semantic
;(semantic-mode 1)

;; Enable EDE (Project Management) features
(global-ede-mode 1)

;; Configure arduino OS X dirs.
;(setq ede-arduino-appdir "/Applications/Arduino.app/Contents/Resources/Java")

;;Arduino-mode
(add-to-list 'load-path "~/.emacs.d/vendor/arduino-mode")
(setq auto-mode-alist (cons '("\\.\\(pde\\|ino\\)$" . c++-mode) auto-mode-alist))
(autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t)

;;Calendar Framework
;; from https://github.com/kiwanami/emacs-calfw
(require 'calfw)
(require 'calfw-org) ;;M-x cfw:open-org-calendar
(setq cfw:org-agenda-schedule-args '(:timestamp))

(setq calendar-latitude 37.8)
(setq calendar-longitude -122.27)
(setq calendar-location-name "Oakland, CA")

;; Open Scad preview
(require 'scad-preview)

;;Pomodoro Mode
(require 'pomodoro) 
(pomodoro-add-to-mode-line)

;; DeadGrep
(global-set-key (kbd "<f5>") #'deadgrep)
