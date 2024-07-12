;;; select-mode.el --- Easily select  -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defface select-mode-origin '((t (:inverse-video t :inherit error)))
  "Faced used to highlight the origin.")

(defface select-mode-number '((t (:inverse-video t :inherit warning)))
  "Faced used to highlight the numbers overlays.")

(defvar select-mode-show-delay 1.0
  "If non-nil, time to wait in seconds before showing numbers overlays.
If nil, do not show numbers unless toggled on using \"n\".")

(defvar select-mode-hide-delay 0
  "If non-nil, time to wait in seconds before hiding numbers.
If nil, do not hide numbers unless toggled off using \"n\".")


(defvar select-mode-initial-type 'sexp)

(defvar select-mode--types
  '((line . (
             :key   "l"
             :start select-mode--line-start
             :next  select-mode--line-next
             :goto  select-mode--line-goto
             ))
    (sentence . (
             :key   "s"
             :start select-mode--sentence-start
             :next  select-mode--sentence-next
             ))
    (word . (
             :key   "w"
             :start select-mode--word-start
             :next  select-mode--word-next
             ))
    (sexp . (
             :key   "x"
             :start select-mode--sexp-start
             :next  select-mode--sexp-next
             ))
    (pair . (
             :start select-mode--pair-start
             :next  select-mode--pair-next
             ))))


(defvar select-mode-pairs
  '(("("  . ")")
    ("{"  . "}")
    ("["  . "]")
    ("<"  . ">")
    ("'"  . "'")
    ("`"  . "`")
    ("\"" . "\"")))


(defun select-mode--make-keymap ()

  (let ((map (make-sparse-keymap)))

    ;; Add a key for each type except pair (which doesn't have `key' so if-let).
    (dolist (pair select-mode--types)
      (if-let ((type (car pair))
               (key  (plist-get (cdr pair) :key)))
          (keymap-set map key (lambda ()
                                (interactive)
                                (select-mode--set-type type)))))

    ;; Add a key for each pair, both open and close if different.
    (dolist (pair select-mode-pairs)
      (let* ((left  (car pair))
             (right (cdr pair)))
        (define-key map left
                    (lambda ()
                      (interactive)
                      (select-mode--set-type 'pair left)))
        (unless (string= left right)
          (define-key map right
                    (lambda ()
                      (interactive)
                      (select-mode--set-type 'pair right))))))

    ;; Add keys 1-9 for expanding the selection.
    (dotimes (c 9)
      (keymap-set map (number-to-string (1+ c))
                  (lambda () (interactive) (select-mode--expand-to (+ 1 c)))))

    (keymap-set map "n" #'select-mode-toggle-numbers)
    (keymap-set map "z" #'select-mode-undo)
    (keymap-set map "+" #'select-mode-expand)
    (keymap-set map "q" #'select-mode-abort)

    (keymap-set map "C" #'select-mode-change-pair)
    (keymap-set map "I" #'select-mode-insert-pair)

    (keymap-set map "RET" #'select-mode-exit)

    map))

(defvar select-mode-map (select-mode--make-keymap))


;;
;; "Session" variables that are used for reset when the mode is enabled
;; and some when the type is changed.
;;

(defvar select-mode--origin nil
  "An overlay visually marking the original location of point.

All initial selections of \"things\" start from this point.")

(defvar select-mode--type nil)

(defvar select-mode--key nil
  "The key pressed, as a string, to enable the current type.

For non-pairs, this will match the type's :key property and isn't
very useful.  For pairs, it allows code to determine if an open
or close character was pressed to differentiate between inclusive
and exclusive (outer and inner) selections.")


(defvar select-mode--dir 'forward)

(defvar select-mode--next nil
  "List of up to next 9 positions for numbering and expanding.")

(defvar select-mode--overlays nil
  "Overlays on the `select-mode--next' positions.

There doesn't seem to be a good way to hide overlays since we
replace text, so we delete them when not showing numbers.  This
is an empty list when numbers are not shown.")

(defvar select-mode--undo-list nil
  "The list of previously selected regions used for undo.

Each element is list containing (type beg end).")


(defvar select-mode--show-timer nil "The timer object for showing numbers.")
(defvar select-mode--hide-timer nil "The timer object for hiding numbers.")

(defvar select-mode--show-numbers-override nil
  "Used when numbers are toggled to disable the timers.

When nil, the timers are used.  When numbers are manually toggled
on or off, this is set to the properties always or never.")

(eval-when-compile (require 'cl-lib))

(defun select-mode--setup ()
  "Setup code called when entering select mode."
  (cl-assert (null select-mode--overlays) "Overlays weren't deleted?")

  (setq select-mode--type nil)
  (setq select-mode--key nil)
  (setq select-mode--dir  'forward)
  (setq select-mode--undo-list nil)
  (setq select-mode--next nil)
  (setq select-mode--overlays nil)
  (setq select-mode--show-numbers-override nil)

  (setq select-mode--origin (make-overlay (point) (1+ (point))))
  (overlay-put select-mode--origin 'face 'select-mode-origin)

  ;; Add the property to our functions so the command hook won't abort.
  (dolist (fun (list #'select-mode-copy-or-mark #'select-mode-exit #'select-mode-abort))
    (put 'select-mode--command fun t))

  (map-keymap (lambda (_type fun)
                (put 'select-mode--command fun t))
              select-mode-map)

  (add-hook 'pre-command-hook #'select-mode--hook-func)
  (select-mode--set-type select-mode-initial-type)
  ;;  (select-mode--set-type 'pair ")")
  )


(defun select-mode--cleanup ()
  "Cleanup code called when exiting select mode."
  (remove-hook 'pre-command-hook #'select-mode--hook-func)
  (select-mode--delete-overlays)
  (select-mode--cancel-timers)
  (setq select-mode--show-numbers-override nil)
  (and (overlayp select-mode--origin)
       (delete-overlay select-mode--origin))
  (setq select-mode--origin nil))


(defun select-mode--cancel-timers ()
  "Cancel show / hide timers"
  (if (timerp select-mode--show-timer)
      (cancel-timer select-mode--show-timer))
  (if (timerp select-mode--hide-timer)
      (cancel-timer select-mode--hide-timer))
  (setq select-mode--show-timer nil)
  (setq select-mode--hide-timer nil))


(defun select-mode--undo-push ()
  "Add the current region to the undo list."
  (if (use-region-p)
      (let ((el (list select-mode--type select-mode--key (region-beginning) (region-end))))
        (unless (eq (last 'select-mode--undo-list) el)
          (push el select-mode--undo-list)))))


(defun select-mode-undo ()
  "Return to the previous `select-mode' selection and type."
  (interactive)
  (when-let ((el (pop select-mode--undo-list))
             (type (car el)))
    (setq select-mode--type (car el))
    (setq select-mode--key (nth 1 el))
    (goto-char (nth 2 el))
    (push-mark (point) t t)
    (goto-char (nth 3 el))
    (select-mode--update-numbers (select-mode--setting :next))))


(define-minor-mode select-mode
  "Provides quick commands for selecting and manipulating the region."
  :lighter " sel"
  :keymap select-mode-map
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
         (select-mode-exit))))


(defun select-mode-exit ()
  "Exits select mode, leaving the current selection.

This is safe to call it `select-mode' is not active."
  (interactive)
  (if select-mode
      (select-mode 0)
    (select-mode--cleanup)))


(defun select-mode-abort ()
  "Exits select mode and return point to its original position."
  (interactive)
  (if select-mode--origin
      (goto-char (overlay-start select-mode--origin)))

  (deactivate-mark)

  ;; Careful - the mode itself calls abort
  (if select-mode
      (select-mode 0)
    (select-mode--cleanup)))


(defun select-mode--set-type (type &optional key)
  "Change to TYPE and update display.

This is the common implementation of the keys that represent
things like \"w\" for word.  If the current type is already TYPE
then we expand the selection, which is the same as pressing 1."

    (let* ((settings (select-mode--settings type))
           (start (plist-get settings :start))
           (next (plist-get settings :next))
           (key2 (or key (plist-get settings :key))))


      (if (and (eq select-mode--type type)
               (eq select-mode--key  key2))
          (select-mode-expand)

      (select-mode--undo-push)

      (setq select-mode--type type)
      (setq select-mode--key key2)

      (message "set-type %s key=%s key2=%s" type key key2)

      (cl-assert start "Not a valid type?")
      (cl-assert key2 "No key.  Did you forget to pass it for a pair?")

      ;; Return to the origin.
      (goto-char (overlay-start select-mode--origin))

      (funcall start)

      (select-mode--delete-overlays)
      (select-mode--update-numbers next))))


(defun select-mode--sentence-start ()
  "Select the sentence around point."

  (when-let ((bounds (bounds-of-thing-at-point 'sentence)))
    (goto-char (car bounds))
    (push-mark (point) t t)
    (goto-char (cdr bounds))))


(defun select-mode--sentence-next (num)
  "Return pos for overlay of sentence NUM."
  ;; We should be on a sentence, so move past it to the next one.
  (ignore num)
  (when-let ((current (bounds-of-thing-at-point 'sentence)))
    (goto-char (cdr current))
    (re-search-forward "[^[:space:]]")
    (when-let ((bounds (bounds-of-thing-at-point 'sentence)))
      (goto-char (cdr bounds))
      (point))))


(defun select-mode--word-start ()
  "Select the word around point."

  ;; If we're not on a word character, move to one.  I'm not sure what I want to
  ;; do it we're on punctuation.  For now we'll simply move to a word character
  ;; if possible.

  (if (not (looking-at "[[:word:]]"))
      (re-search-forward "[[:word:]]"))

  (when-let ((bounds (bounds-of-thing-at-point 'word)))
    (goto-char (car bounds))
    (push-mark (point) t t)
    (goto-char (cdr bounds))))


(defun select-mode--word-next (num)
  "Move to and return the position for word overlay NUM."
  (ignore num)
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


(defun select-mode--pair-start ()
  "Select the pair around point."
  (let ((bounds (select-mode--get-pair-bounds select-mode--key 'auto)))
    (message "mark pair: %s" bounds)
    (select-mode--mark-bounds bounds)))

(defun select-mode--pair-next (num)
  ;; There should be a region if select-mode--pair-start found an initial pair.
  ;; If so, move to the beginning of it and go up.
  ;;
  ;; One thing to watch for: if the key is a right char, the region will be
  ;; inside the pair and we need to backup 2 characters.

  (when (use-region-p)
    (let* ((pair (select-mode--make-pair select-mode--key))
           (left (cdr pair)))
      (goto-char (region-beginning))
      (message "char-at 1: %s" (char-to-string (char-after)))
      (if (/= (char-after) (string-to-char left))
          (backward-char))
      (message "char-at 2: %s" (char-to-string (char-after)))
      (backward-char)
      (let ((bounds (select-mode--get-pair-bounds select-mode--key 'auto)))
        (message "pair next: %s %s" num bounds)
        (select-mode--mark-bounds bounds)
        bounds  ;; return bounds
        )))
  )


(defun select-mode--mark-bounds (bounds)
  "Internal function to set the active region to the cons cell BOUNDS."
  (push-mark (point) t)
  (goto-char (car bounds))
  (push-mark (point) t t)
  (goto-char (cdr bounds)))


(defun select-mode--sexp-start ()
  "Select the sexp around point."
  (if-let ((bounds (bounds-of-thing-at-point 'sexp)))
      (progn
        (goto-char (car bounds))
        (push-mark (point) t t)
        (goto-char (cdr bounds)))))


(defun select-mode--sexp-next (num)
  "Move to and return the position for expression overlay NUM."

  (ignore num)
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

  ;; Lines are an exception, visually, because selecting an entire line and its
  ;; newline (which I want) puts the cursor on the *next* line.  However, I want
  ;; the numbers to show on the line we're going to select, not where the cursor
  ;; is going to go.  We provide a line goto function to take this into account.

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


(defun select-mode--line-goto (num)
  "Move to expansion `num' which is the number pressed."
  ;; As noted in select-mode--line-next, we display the number at the beginning
  ;; of the line we want to select, which means we want to move to the line
  ;; *after* `count'.  If we have more than `count' overlays, we can just move
  ;; there.
  (let ((pos (nth (1- num) select-mode--next)))
    (goto-char pos)
    (forward-line 1)))


(defun select-mode--settings (&optional type)
  "Return the settings plist for `TYPE' or the current type."
  (cdr (assq (or type select-mode--type) select-mode--types)))


(defun select-mode--setting (prop)
  "Return the current setting `PROP'."
  (plist-get (select-mode--settings) prop))


(defun select-mode--expand-to (num)
  "Expand selection to the displayed number NUM.

This implements numbers 1-9, which are handled by generated lambdas."
  ;; If the type defines its own :goto function, defer to it.  Otherwise go to
  ;; the position of the given overlay.
  (if (<= num (length select-mode--next))
      (let* ((settings (select-mode--settings))
             (nextfun (plist-get settings :next))
             (gotofun (or (plist-get settings :goto) #'select-mode--goto-generic)))
        (cl-assert nextfun)
        (select-mode--undo-push)
        (funcall gotofun num)
        (select-mode--update-numbers nextfun))))


(defun select-mode--goto-generic (num)
  "Move to the given overlay position and remove overlays.

This is used when a type does not supply its own :next function."
  (if-let ((pos (nth (1- num) select-mode--next)))
      (if (listp pos)
          (select-mode--mark-bounds pos)
        (goto-char pos))))


(defun select-mode-expand ()
  "Expand the selection by the current type and direction."
  (interactive)
  (select-mode--expand-to 1))


(defun select-mode--delete-overlays ()
  "Delete overlays and set `select-mode--overlays' to nil."
      (dolist (o select-mode--overlays)
        (delete-overlay o))
      (setq select-mode--overlays nil))


(defun select-mode--update-numbers (nextfun)
  "Update number positions in select-mode--next and optionally creates overlays."

  (let ((showing-numbers (not (null select-mode--overlays))))

    ;; NOTE: We could keep old items we calculated, but I'll save that
    ;; optimization for later.  For now we'll recalculate all 9 from scratch.
    (select-mode--delete-overlays)
    (select-mode--cancel-timers)
    (setq select-mode--next nil)

    ;; Call `nextfun' 9 times, or until it returns nil.  We also catch errors,
    ;; to allow the :next implementations to be as simple as possible.
    (save-mark-and-excursion
      (ignore-errors
          (let ((count 0))
            (while (and (< count 9)
                        (progn
                          (let ((pos (funcall nextfun (1+ count))))
                            (when (not (null pos))
                              (setq count (1+ count))
                              (push pos select-mode--next)
                              t ; return true to keep while loop going
                              ))))))))

    (setq select-mode--next (nreverse select-mode--next))

    ;; Careful: Numbers could have been turned on and showing-numbers false if
    ;; there were no `type' constructs in the buffer.

    (cond ((eq select-mode--show-numbers-override 'never)
           ;; The user manually turned numbers off with 'n' so do not add any
           ;; back.
           )

          ((or (eq select-mode--show-numbers-override 'always)
               showing-numbers)
            ;; The user manually turned numbers on with 'n' or the show timer
            ;; had expired and we were showing numbers.  Show the updated
            ;; numbers immediately.
           (select-mode--show-numbers))

          ((and (numberp select-mode-show-delay)
                (> select-mode-show-delay 0))
           ;; The user has not manually overridden showing and there is a show
           ;; timer configured.  Start the timer.
            (setq select-mode--show-timer
                  (run-with-timer select-mode-show-delay nil #'select-mode--show-numbers))))))


(defun select-mode--show-numbers ()
  "Displays number overlays and sets the hide timer."
  (setq select-mode--show-timer nil)
  (select-mode--make-overlays)
  (if (and (numberp select-mode-hide-delay)
           (> select-mode-hide-delay 0))
      (setq select-mode--hide-timer
            (run-with-timer select-mode-hide-delay nil #'select-mode--hide-numbers))))


(defun select-mode--hide-numbers ()
  (setq select-mode--hide-timer nil)
  (select-mode--delete-overlays))


(defun select-mode--make-overlays ()
  "Make overlays for the positions in select-mode--next."
  (cl-assert (null select-mode--overlays))

  (save-excursion
  (let ((index 0))
    (setq select-mode--overlays
          (mapcar (lambda (pos)
                    ;; `pos' can be an integer position or for pairs it can be a
                    ;; (beg end) pair of positions.  If it is a pair, we'll put
                    ;; the number at the beginning.
                    (if (listp pos)
                        (setq pos (car pos)))

                    (message "pos=%s" pos)
                    (goto-char pos)

                    (let ((text (propertize (number-to-string (1+ index)) 'face 'select-mode-number))
                          (char (char-to-string (char-after))))

                      (setq index (1+ index))

                      ;; Since we are displaying numbers, we *replace* the
                      ;; character we are covering.  If it is a newline,
                      ;; replacing it means there is no line break anymore and
                      ;; the line after it is joined to the current line.
                      ;; Similarly, replacing a tab char messes up display.  In
                      ;; both cases, add back the character.
                      (if (member char '("\n" "\t"))
                          (setq text (concat text char)))

                      (let ((o (make-overlay pos (1+ pos))))
                        (overlay-put o 'display text)
                        (overlay-put o 'priority 999)
                        o)))
                  select-mode--next)))))


(defun select-mode-toggle-numbers ()
  "Toggle display of numbers."
  (interactive)

  ;; Be careful here.  It's possible that numbers are turned on but there aren't
  ;; any due to the buffer contents.

  (select-mode--cancel-timers)

  (cond ((or (eq select-mode--show-numbers-override 'always)
             select-mode--overlays)
         (setq select-mode--show-numbers-override 'never)
         (select-mode--delete-overlays))

        (t
         (setq select-mode--show-numbers-override 'always)
         (select-mode--show-numbers))))


(provide 'select-mode)


;; PAIRS
;;
;; I copied a lot of this from my surround package which I intend to replace
;; with this.  I didn't want any dependencies.


(defun select-mode--make-pair (char)
  "Return a cons cell of (left . right) defined by CHAR.

If CHAR is either a left or right character in `select-mode-pairs',
that pair is returned.  Otherwise a cons cell is returned made of
\(CHAR . CHAR)."
  (or (assoc char select-mode-pairs)
      (rassoc char select-mode-pairs)
      (cons char char)))


(defun select-mode--get-pair-bounds (char ends)
  "Return bounds of text surrounded by CHAR, including CHAR.

The ENDS parameter can be \\='outer to include the pairs, \\='inner
to include the region within the pairs, or \\='auto
which defaults to include the pairs unless CHAR is a
right character in `select-mode-pairs'."
  (let* ((pair (select-mode--make-pair char))
         (left (car pair))
         (right (cdr pair))
         (bounds (select-mode--pair-bounds left right)))
    (if (or (eq ends 'inner)
            (and (eq ends 'auto)
                 (string= char left)))
        (select-mode--shrink bounds)
      bounds)))


(defun select-mode--pair-bounds (left right)
  "Return the bounds of the enclosing pair LEFT and RIGHT."

  ;; If the left and right are not the same, like ( and ), then we need a
  ;; complex search that handles nesting.  For example if point is on "c" below,
  ;; we need to select the entire expression and not just the closest parens to
  ;; "c".
  ;;
  ;;     (a (b) c (d) e)
  ;;
  ;; Otherwise the open and close are the same and we won't support nesting.
  ;; We'll use a simpler technique.
  (if (string= left right)
      (cons (search-forward left nil nil -1)
            (search-forward right nil nil 1))
    (cons
     (select-mode--find-char-nestable left right -1)
     (select-mode--find-char-nestable right left 1))))


(defun select-mode--find-char-nestable (char other dir)
  "Return the position of the closest, unnested CHAR.

OTHER is the opposite pair character for CHAR.  For example if
CHAR is ( then OTHER would be ).  DIR must be -1 to search
backwards and 1 to search forward."
  ;; This searches in a single direction (-1 or 1) for `char`.  When searching
  ;; forward for the end of a pair of parens, we'd want to find the `char` ")".
  ;; We need to skip nested pairs, so we watch for "(" which we call `other'.
  ;;
  ;; The only good algorithm I know is to walk forward to the next of either
  ;; character and count the nesting level.  Since we are expecting to be
  ;; inside, we start at 1.  When we reach 0, we've found the end of the current
  ;; level.

  (save-excursion
    (if (looking-at (regexp-quote char))
        ;; We're already on the character we're looking for.  If it is the
        ;; opening paren (dir=-1), then this is the position we want.  If it is
        ;; the closing paren (dir=1), then we want the *next* point since the
        ;; selection is exclusive.
        (when (= dir 1)
          (forward-char 1))

      ;; The algorithm below is going to assume we are *inside* a pair (level =
      ;; 1), so if we're on an opening paren step in.  Otherwise the algorithm
      ;; below will find the opening paren we're already on and count it as
      ;; another open.
      (if (and (looking-at (regexp-quote other)) (= dir 1))
          (forward-char dir))

      (let ((level 1))                      ; level of nesting
        (while (> level 0)
          (let* ((current (point))
                 (charpos (search-forward char nil t dir))
                 (otherpos (progn
                             (goto-char current)
                             (search-forward other nil t dir)))
                 (chardist (select-mode--distance current charpos))
                 (otherdist (select-mode--distance current otherpos))
                 (pos (if (< chardist otherdist) charpos otherpos))
                 (diff (if (< chardist otherdist) -1 1)))

            (if (null charpos)
                (user-error "Did not find %s" char))

            (setq level (+ level diff))
            (goto-char pos)))))

    (point)))

(defun select-mode--infer-bounds (&optional strict)
  "Infer the bounds.

With an active region they are those of the region. If looking at
a symbol they are those of the symbol. For empty lines the
current point is considered both start and end.

If STRICT is t, a user error is signaled when the bounds couldn't
be inferred."
  (cond
   ((use-region-p)
    (cons (region-beginning) (region-end)))
   ((thing-at-point 'symbol)
    (bounds-of-thing-at-point 'symbol))
   ((save-excursion
      (beginning-of-line)
      (looking-at-p "[ \t]*$"))
    (cons (point) (point)))
   (strict
    (user-error "No bounds can be inferred"))))

(defun select-mode--shrink (bounds)
  "Shrink BOUNDS from outer to inner."
  (cons (1+ (car bounds)) (1- (cdr bounds))))


(defun select-mode--distance (p1 p2)
  "Return the absolute distance between the positions P1 to P2."
  ;; REVIEW: Aren't p1 and p2 positions which can't be negative?
  ;;
  ;; Also, using most-positive-fixnum does simplify other calling code, but
  ;; perhaps we can reformat the other code to not require this.
  (let* ((a1 (abs p1))
         (a2 (abs (or p2 most-positive-fixnum))))
    (- (max a1 a2) (min a1 a2))))


(defun select-mode-insert-pair (char)
  "Insert a pair (around) the current region."
  (interactive
   (list (char-to-string (read-char "character: "))))

  (let* ((pair (select-mode--make-pair char))
         (left  (car pair))
         (right (cdr pair))
         (beg (region-beginning))
         (end (region-end)))
      (goto-char end)
      (insert right)
      (goto-char beg)
      (insert left)

      ;; We are probably showing numbers for words or expressions, which doesn't
      ;; really make sense now that we've inserted a pair.  What options do we
      ;; have?
      ;;
      ;; - switch to pair mode for the pair we just entered?
      ;;  (select-mode--set-type 'pair right)
      ;; - abort select-mode (unselect)?
      (select-mode-exit)
      (deactivate-mark)))

(defun select-mode-change-pair ()
  "Replace characters at end of region with a new pair."
  (interactive)

  (unless (use-region-p)
    (error "No region"))

  (let* ((ch (read-char "replace with: "))
         (pair (select-mode--make-pair (char-to-string ch)))
         (replace (not (eq ch ?\r))) ;; delete, not replace, if RET pressed
         (beg (region-beginning))
         (end (region-end))
         (msg (if replace
                 (format "Replaced %s %s with %s %s"
                         (char-to-string (char-after beg))
                         (char-to-string (char-before end))
                         (car pair) (cdr pair))
               (format "Delete [%s] [%s]"
                       (char-to-string (char-after beg))
                       (char-to-string (char-before end))))))

    ;; Start with end in case we don't replace.  The position of the end
    ;; character will change if we just delete the beginning character.
    (goto-char end)
    (delete-char -1)
    (if replace
        (insert (cdr pair)))
    (goto-char beg)
    (delete-char 1)
    (if replace
        (insert (car pair)))

    (select-mode-exit)
    (deactivate-mark)

    (message msg)))


;;; select-mode.el ends here
