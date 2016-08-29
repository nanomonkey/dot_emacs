;; Region lines and then `M-x osx-say' to make OSX speak.

;; Adjust speak speed
(setq osx-say-speed 180)

;; Change voice
;; Kathy, Vicki, Victoria, Alex, Bruce, Fred
(setq osx-say-voice "Alex")
(setq osx-say-buffer "*osx say*")

(defun osx-say-stop ()
  (interactive)
  (when (get-buffer osx-say-buffer)
    (kill-buffer osx-say-buffer)))

(defun osx-say (&optional $word $speed)
  "Utilize `say' command that Mac OSX has."
  (interactive)
  (unless (executable-find "say")
    (error (message "`say' command not found")))
  (osx-say-stop)
  (cond ($word $word)
        (mark-active
         (setq $word (buffer-substring-no-properties
                      (region-beginning) (region-end))))
        ((setq $word (thing-at-point 'word)))
        (t (setq $word (read-string "word: "))))
  (mapc (lambda ($r)
          (setq $word (replace-regexp-in-string (car $r) (cdr $r) $word)))
        (list ;;'("'"   . "\\\\'")
              '("\""  . "\\\\\"")
              '("?"   . "\\\\?")
              '("\n"  . " ")
              '("\("  . "\\\\(")
              '("\)"  . "\\\\)")
              '("\\[" . "\\\\[")
              '("\\]" . "\\\\]")
              '("\;"  . "\\\\;")
              '("\&"  . "\\\\&")
              '("\|"  . "\\\\|")))
  (save-window-excursion
    (start-process "OSX Say" osx-say-buffer
                   "say" "-v" osx-say-voice "-r"
                   (number-to-string (or $speed osx-say-speed)) $word)))

