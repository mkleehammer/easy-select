;;; select-mode.el --- Easily select  -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defface select-mode-origin '((t (:inverse-video t :inherit error)))
  "Faced used to highlight the origin.")

(defface select-mode-number '((t (:inverse-video t :inherit warning)))
  "Faced used to highlight numbers.")


(defvar select-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "x" #'select-mode--sexp-select)
    (keymap-set map "w" #'select-mode--word-select)
    (keymap-set map "l" #'select-mode--line-select)
    (keymap-set map "n" #'select-mode-toggle-numbers)
    (keymap-set map "z" #'select-mode-undo)
    (keymap-set map "+" #'select-mode-expand)
    (keymap-set map "q" #'select-mode-abort)
    (keymap-set map "RET" #'select-mode-exit)

    (dotimes (c 9)
      (keymap-set map (number-to-string (1+ c))
                  (lambda () (interactive) (select-mode--expand-to (+ 1 c)))))

    map))

(defvar select-mode--types
  '((line . (
             :start select-mode--line-start
             :next  select-mode--line-next
             :goto  select-mode--line-goto
             ))
    (word . (
             :start select-mode--word-start
             :next  select-mode--word-next
             ))
    (sexp . (
             :start select-mode--sexp-start
             :next  select-mode--sexp-next
             ))))


(defvar select-mode--origin nil
  "An overlay visually marking the original location of point.

All initial selections of \"things\" start from this point.")

(defvar select-mode--type nil)

(defvar select-mode--dir 'forward)

(defvar select-mode--next nil
  "List of up to next 9 overlays for numbering and expanding.")

(defvar select-mode--undo-list nil
  "The list of previously selected regions used for undo.

Each element is list containing (type beg end).")


(eval-when-compile (require 'cl-lib))

(defun select-mode--setup ()
  "Setup code called when entering select mode."
  (message "setup")

  (setq select-mode--type nil)
  (setq select-mode--dir  'forward)
  (setq select-mode--undo-list nil)
  (setq select-mode--next nil)

  (setq select-mode--origin (make-overlay (point) (1+ (point))))
  (overlay-put select-mode--origin 'face 'select-mode-origin)

  ;; Add the property to our functions so the command hook won't abort.
  (dolist (fun (list #'select-mode-copy-or-mark #'select-mode-exit #'select-mode-abort))
    (put 'select-mode--command fun t))

  (map-keymap (lambda (_type fun)
                (put 'select-mode--command fun t))
              select-mode-map)

  (add-hook 'pre-command-hook #'select-mode--hook-func)
  (select-mode--set-type 'word))


(defun select-mode--undo-push ()
  "Add the current region to the undo list."
  (if (use-region-p)
      (let ((el (list select-mode--type (region-beginning) (region-end))))
        (unless (eq (last 'select-mode--undo-list) el)
          (push el select-mode--undo-list)))))


(defun select-mode-undo ()
  "Return to the previous `select-mode' selection and type."
  (interactive)
  (message "undo!")
  (when-let ((el (pop select-mode--undo-list))
             (type (car el)))
    (message "undo: %s" el)
    (setq select-mode--type (car el))
    (goto-char (nth 1 el))
    (push-mark (point) t t)
    (goto-char (nth 2 el))
    (select-mode--update-overlays (select-mode--setting :next))))


(defun select-mode--cleanup ()
  "Internal common cleanup."

  (message "CLEANUP")
  (remove-hook 'pre-command-hook #'select-mode--hook-func)
  (select-mode--delete-overlays)
  (and (overlayp select-mode--origin)
       (delete-overlay select-mode--origin))
  (setq select-mode--origin nil))


(define-minor-mode select-mode
  "Provides quick commands for selecting and manipulating the region."
  :lighter " sel"
  :keymap select-mode-map

  ;; Reminder: the select-mode variable is toggled before this code is executed.
  ;; If it is true, it was just toggled on.

  ;; If we ran code to cleanup already, we'll deactivate the mark.  If it is
  ;; activated, then the user probably manually turned off the mode which we'll
  ;; treat like exit.  (I don't feel strongly about exit vs abort here.)

  (message "MODE: %s" select-mode)
  (if select-mode
      (select-mode--setup)
    (select-mode--cleanup)))


(defun select-mode-copy-or-mark ()
  "Copy if region exists, otherwise start the select hydra."
  (interactive)
  ;; If there is a selection, copy it.  If the mode wasn't on, then it is a
  ;; normal copy.  If it was on, turn it off and turn off the region.
  (if (use-region-p)
      ;; The region is set, so we're going to copy whether the mode was on or
      ;; not.  If it was on, though, we want to exit and keep the current
      ;; region, but deactivate the mark.  kill-ring-save will deactivate for
      ;; us.
      (progn
        (kill-ring-save (region-beginning) (region-end) t)
        (select-mode-exit))             ; safe to call when not on
    (select-mode)))


(defun select-mode--hook-func ()
  "Added to pre-command hook to exit mode if any unrecognized key is pressed."

  ;; This functions is called before each command, which is each key,
  ;; interactive command, etc.  If the command is not a select-mode command,
  ;; quietly exit.
  ;;
  ;; When the mode is turned on, we add the select-mode--command property to
  ;; each command in the keymap.  If we don't see this property, it isn't one of
  ;; ours so exit.

  (cond ((eq this-command #'keyboard-quit)
         (select-mode-abort))
        ((not (get 'select-mode--command this-command))
         (message "UNKNOWN: %s" this-command)
         (select-mode-exit))
        (t (message "KEEP: %s" this-command))
        ))


(defun select-mode-exit ()
  "Exits select mode, leaving the current selection.

This is safe to call it `select-mode' is not active."
  (interactive)
  (message "exit")
  (if select-mode
      (select-mode 0)
    (select-mode--cleanup)))


(defun select-mode-abort ()
  "Exits select mode and return point to its original position."
  (interactive)
  (message "abort: %s" select-mode)

  (if select-mode--origin
      (goto-char (overlay-start select-mode--origin)))

  (deactivate-mark)

  ;; Careful - the mode itself calls abort
  (if select-mode
      (select-mode 0)
    (select-mode--cleanup)))


(defun select-mode--line-select ()
  "Change type to line and select the line point is on.

If the type is already line, select the next line."
  (interactive)
  (select-mode--set-type 'sexp))

(defun select-mode--sexp-select ()
  "Change type to sexp and select the sexp at point.

If the type is already sexp, select the next expression."
  (interactive)
  (select-mode--set-type 'sexp))

(defun select-mode--word-select ()
  "Change type to word and select the word at point.

If the type is already word, select the next expression."
  (interactive)
  (select-mode--set-type 'word))


(defun select-mode--set-type (type)
  (if (eq select-mode--type type)
      (select-mode-expand)
    (let* ((settings (select-mode--settings type))
           (start (plist-get settings :start))
           (next (plist-get settings :next)))
      (select-mode--undo-push)

      (setq select-mode--type type)

      ;; Return to the origin.
      (goto-char (overlay-start select-mode--origin))

      (funcall start)

      (select-mode--delete-overlays)
      (select-mode--update-overlays next)
      )))


(defun select-mode--word-start ()
  "Select the word around point."
  (if-let ((bounds (bounds-of-thing-at-point 'word)))
      (progn
        (message "bounds: %s" bounds)
        (goto-char (car bounds))
        (push-mark (point) t t)
        (goto-char (cdr bounds)))))

(defun select-mode--word-next (count)
  "Move to and return position of the next word number or nil if at end."

  ;; Point is either on (count = 1, first call of this function) or immediately
  ;; after.
  ;;
  ;; Move to the  of the next expression first.

  (if-let ((current (bounds-of-thing-at-point 'word)))
      (goto-char (cdr current)))

  ;; We want our selections to be lenient.  In the text "end.  Next", the period
  ;; is not a "word" character, so bounds-of-thing-at-point will never include
  ;; it and we would never make to it Next using just thing functions.
  ;; Therefore we need to manually search forward for the next start of a word.
  (re-search-forward "[[:word:]]")

  (when-let ((bounds (bounds-of-thing-at-point 'word)))
    (goto-char (cdr bounds))
    (point)))


(defun select-mode--sexp-start ()
  "Select the sexp around point."
  (if-let ((bounds (bounds-of-thing-at-point 'sexp)))
      (progn
        (message "bounds: %s" bounds)
        (goto-char (car bounds))
        (push-mark (point) t t)
        (goto-char (cdr bounds)))))

(defun select-mode--sexp-next (count)
  "Move to and return position of the next sexp number or nil if at end."

  ;; Point is either on (count = 1, first call of this function) or immediately
  ;; after.
  ;;
  ;; Move to the  of the next expression first.

  (if-let ((current (bounds-of-thing-at-point 'sexp)))
      (goto-char (cdr current)))

  ;; TODO: This needs to find the next expression.
  (re-search-forward "[^[:space:]]")

  (when-let ((bounds (bounds-of-thing-at-point 'sexp)))
    (goto-char (cdr bounds))
    (point)))


(defun select-mode--line-start ()
  "Select the line around point."
  (beginning-of-line)
  (push-mark (point) t t)
  (forward-line 1))


(defun select-mode--line-next (count)
  "Move to and return position of the next line number or nil if at end."

  ;; This should only be called if -start has been called and we are already at
  ;; the beginning of a line.
  (cl-assert (bolp) t)

  ;; We want to display the number at the beginning of the line it selects.
  ;; When we select a line, we include the newline, which means point is
  ;; actually at the beginning of the next line.  Therefore it count is 1, we
  ;; are already at the start of the next line.
  (if (= count 1)
      (point)
    (let ((start (point)))
      (forward-line 1)
      (and (bolp)
           (/= start (point))
           (point)))))


(defun select-mode--line-goto (count)
  "Move to expansion `count'."
  ;; As noted in select-mode--line-next, we display the number at the beginning
  ;; of the line we want to select, which means we want to move to the line
  ;; *after* `count'.  If we have more than `count' overlays, we can just move
  ;; there.
  (select-mode--goto-generic (- count 1)))


(defun select-mode--settings (&optional type)
  "Return the settings plist for `type' or the current type."
  (cdr (assq (or type select-mode--type) select-mode--types)))


(defun select-mode--setting (prop)
  "Return the current setting `prop'."
  (plist-get (select-mode--settings) prop))


(defun select-mode--expand-to (count)
  "Expand selection to the displayed number.

This implements numbers 1-9, which are handled by generated lambdas."
  ;; If the type defines its own :goto function, defer to it.  Otherwise go to
  ;; the position of the given overlay.
  (if (<= count (length select-mode--next))
      (let* ((settings (select-mode--settings))
             (nextfun (plist-get settings :next))
             (gotofun (or (plist-get settings :gotox) #'select-mode--goto-generic)))
        (cl-assert nextfun)
        (select-mode--undo-push)
        (funcall gotofun count)
        (select-mode--update-overlays nextfun))))


(defun select-mode--goto-generic (count)
  "Move to the given overlay position and remove overlays.

This is used when a type does not supply its own :next function."
  (if-let ((o (nth (1- count) select-mode--next)))
    (goto-char (overlay-start o))))


(defun select-mode-expand ()
  "Expand the selection by the current type and direction."
  (interactive)
  (select-mode--expand-to 1))


(defun select-mode--delete-overlays ()
  "Delete overlays and set `select-mode--next' to nil."
      (dolist (o select-mode--next)
        (delete-overlay o))
      (setq select-mode--next nil))


(defun select-mode--update-overlays (nextfun)
  "Update numbered overlays."

  ;; Since we are displaying numbers, we *replace* the character we are
  ;; covering.  If it is a newline, replacing it means there is no line break
  ;; anymore and the line after it is joined to the current line.
  ;;
  ;; We have to add back the newline character after the number.  Same for tab.
  ;;
  ;; However, now we have *another* problem.  If you set the face for "1\n", it
  ;; highlights the whole line.  For our special characters, we'll set the face
  ;; on the text itself, not on the whole overlay.  (This is how meow handles
  ;; it.)

  ;; NOTE: We could keep old items we calculated, but I'll save that
  ;; optimization for later.  For now we'll recalculate all 9 from scratch.
  (select-mode--delete-overlays)

  (save-excursion
    (let ((l (length select-mode--next))
          start
          o next text
          )
      (message "update l=%s" l)

      ;; Call `nextfun' 9 times, or until it returns nil.

      (while (and (< l 9)
                  (progn
                    (setq start (point))
                    (setq next (funcall nextfun (+ 1 l)))
                    (message "next: count=%s pos=%s" (+ 1 l) next)
                    (when (not (null next))
                      (let ((o (make-overlay next (1+ next)))
                            (before-newline (equal 10 (char-after)))
                            (before-tab (equal 9 (char-after)))
                            (n (number-to-string (1+ l))))

                        (setq l (1+ l))

                        (overlay-put o 'priority 999)
                        (cond
                         (before-newline
                          (overlay-put o 'display (concat (propertize n 'face 'select-mode-number) "\n")))
                         (before-tab
                          (overlay-put o 'display (concat (propertize n 'face 'select-mode-number) "\t")))
                         (t
                          (overlay-put o 'display (propertize n 'face 'select-mode-number))))

                        (push o select-mode--next)
                        t ; return true to keep while loop going
                        )))))))
  (setq select-mode--next (nreverse select-mode--next))
  )

(defun select-mode-toggle-numbers ()
  "Toggle display of numbers by hiding the overlays."
  (interactive)

  ;; TODO: None of this works.  We're either going to have to simply delete them
  ;; all and keep track of their boundaries somewhere else.

  ;;  ;; If I remove an overlay, does it remove its position?
  ;;  (let ((o (car select-mode--next)))
  ;;    (message "before: %s" (overlay-start o))
  ;;    (delete-overlay o)
  ;;    (message "after: %s" (overlay-start o))
  ;;    )


  ;;  (if nil ; select-mode--next
  ;;      (progn
  ;;        (message "here: %s" (overlay-get (car select-mode--next) 'invisible))
  ;;      (let* ((oldval (overlay-get (car select-mode--next) 'invisible))
  ;;             (newval (not oldval)))
  ;;        ;;  (message "old" oldval)
  ;;        (dolist (o select-mode--next)
  ;;          (overlay-put o 'invisible newval))))))
)

(provide 'select-mode)

;;; select-mode.el ends here
