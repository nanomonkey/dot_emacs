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
   (scheme . t)
   (sqlite . t)))

;; DOT
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "dot")))  ; don't ask for dot
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)


;; Disable Line Numbers in Org-mode
(defun nolinum ()
  (interactive)
  (message "Deactivated linum mode")
  (global-linum-mode 0)
  (linum-mode 0))

(global-set-key (kbd "<f6>") 'nolinum)

(add-hook 'org-mode-hook 'nolinum)

;; Org-mode tasks
(setq org-todo-keywords 
  '((sequence "TODO" "DOING" "BLOCKED" "REVIEW" "|" "DONE" "ARCHIVED")))

(setq org-todo-keyword-faces
  '(("TODO" . org-warning)
   ("DOING" . "yellow")
   ("BLOCKED" . "red")
   ("REVIEW" . "orange")
   ("DONE" . "green")
   ("ARCHIVED" .  "blue")))

;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(require 'ob)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("babashka" . "clj"))

(defvar org-babel-babashka-command (executable-find "bb")
  "The command to use to compile and run your babashka code.")

;; Org-babel Babashka (clojure)
(defvar org-babel-default-header-args:babashka '())
(defvar org-babel-header-args:babashka '((package . :any)))

(defun ob-babashka-escape-quotes (str-val)
  "Escape quotes for STR-VAL so that Lumo can understand."
  (replace-regexp-in-string "\"" "\\\"" str-val 'FIXEDCASE 'LITERAL))

(defun org-babel-expand-body:babashka (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let* ((vars (org-babel--get-vars params))
         (result-params (cdr (assq :result-params params)))
         (print-level nil) (print-length nil)
         (body (ob-babashka-escape-quotes
                (org-trim
                 (if (null vars)
                     (org-trim body)
                   (concat "(let ["
                           (mapconcat
                            (lambda (var)
                              (format "%S (quote %S)" (car var) (cdr var)))
                            vars "\n      ")
                           "]\n" body ")"))))))
    (if (or (member "code" result-params)
            (member "pp" result-params))
        (format "(print (do %s))" body)
      body)))

(defun org-babel-execute:babashka (body params)
  "Execute a block of babashka code in BODY with Babel using PARAMS."
  (let ((expanded (org-babel-expand-body:babashka body params))
        result)
    (setq result
          (org-babel-trim
           (shell-command-to-string
            (concat org-babel-babashka-command " -e \"" expanded "\""))))
    (org-babel-result-cond (cdr (assoc :result-params params))
      result
      (condition-case nil (org-babel-script-escape result)
        (error result)))))

(define-derived-mode babashka-mode clojure-mode "Babashka"
  "Major mode for editing Babashka code.")

(provide 'ob-babashka)

;; EMMS
(require 'emms-setup)
(emms-all)
(emms-default-players)

(setq emms-source-file-default-directory "~/Music/")

(require 'emms-volume)
(setq emms-volume-change-function 'emms-volume-mpd-change)
(global-set-key (kbd "C-c +") 'emms-volume-mode-plus)
(global-set-key (kbd "C-c -") 'emms-volume-mode-minus)


(require 'emms-info-native)
(add-to-list 'emms-info-functions 'emms-info-native)

;; Choose one of these
;;(setq emms-info-functions '(emms-info-tinytag))  ;; When using Tinytag
;;(setq emms-info-functions '(emms-info-exiftool)) When using Exiftool

;; Load cover images
(setq emms-browser-covers 'emms-browser-cache-thumbnail-async)

;; Keyboard shortcuts
(global-set-key (kbd "<XF86AudioPrev>") 'emms-previous)
(global-set-key (kbd "<XF86AudioNext>") 'emms-next)
(global-set-key (kbd "<XF86AudioPlay>") 'emms-pause)


;; El-Feed
(setq elfeed-feeds
       '("http://nullprogram.com/feed/"
         "https://planet.emacslife.com/atom.xml"
         "https://solar.lowtechmagazine.com/feeds/all-en.atom.xml"))
