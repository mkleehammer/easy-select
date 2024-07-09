;;; select-mode.el --- Easily select  -*- lexical-binding: t -*-

;; Here I keep things a little less generic so others can generally pull my
;; commits that deal with init.el and skip commits dealing with this file.
;;
;; - [ ] Optional overlay at original position.
;; - [ ] List of region bounds so we can pop.
;; - [ ]
;;
;; * Questions
;;
;; ** What benefits are there over easy-kill?
;;
;;    - The focus is mark instead of easy-kill & easy-mark.
;;    - Numbers for expanding.
;;    - Support for direction and expanding in the opposite direction.
;;    - Easy setup and tear down for customization
;;    - Hooks

(setq select-mode-map nil)
(defvar select-mode-map (select-mode--make-keymap))

(defun select-mode--make-keymap ()
  (let ((map (make-sparse-keymap)))
    (keymap-set map "l" #'select-mode--line-select)
    (keymap-set map "z" #'select-mode-undo)
    (keymap-set map "q" #'select-mode-abort)
    (keymap-set map "RET" #'select-mode-exit)
    map))

(defvar select-mode--type nil)
(defvar select-mode--dir 'forward)

(defvar select-mode--undo-list nil
  "The list of previously selected regions used for undo.

Each element is list containing (type beg end).")


(defvar select-mode--origin nil
  "An overlay visually marking the original location of point.

All initial selections of \"things\" start from this point.")

(defface select-mode-origin '((t (:inverse-video t :inherit error)))
  "Faced used to highlight the origin."
  :group 'killing)


(defun select-mode--setup ()
  "Setup code called when entering the select hydra."
  (message "setup")

  (setq select-mode--type 'sexp)
  (setq select-mode--dir  'forward)
  (setq select-mode--undo-list nil)

  (setq select-mode--origin (make-overlay (point) (1+ (point))))
  (overlay-put select-mode--origin 'face 'select-mode-origin)

  ;; Add the property to our functions so the command hook won't abort.
  (dolist (fun (list #'select-mode-copy-or-mark #'select-mode-exit #'select-mode-abort))
    (put 'select-mode--command fun t))

  (map-keymap (lambda (_type fun)
                (message "fun %s" fun)
                (put 'select-mode--command fun t))
              select-mode-map)

  (add-hook 'pre-command-hook #'select-mode--hook-func)

  (message "setup2: %s" select-mode--origin)

  ;; Eventually I'd like either a variable for the initial type or a function
  ;; run to perform the initial select.

  (when-let ((bounds (bounds-of-thing-at-point 'sexp)))
    (message "bounds: %s" bounds)
      (push-mark (car bounds) t t)
      (goto-char (cdr bounds))))


(defun select-mode--undo-push ()
  "Add the current region to the undo list."
  (if (use-region-p)
      (let ((el (list select-mode--type (region-beginning) (region-end))))
        (unless (eq (last 'select-mode--undo-list) el)
          (push el select-mode--undo-list)))))


(defun select-mode-undo ()
  "Returns to the previous select-mode selection and type."
  (interactive)
  (message "undo!")
  (when-let ((el (pop select-mode--undo-list)))
    (message "undo: %s" el)
    (setq select-mode--type (car el))
    (goto-char (nth 1 el))
    (push-mark (point) t t)
    (goto-char (nth 2 el))))


(defun select-mode--cleanup ()
  "Internal common cleanup"

  (message "CLEANUP")
  (remove-hook 'pre-command-hook #'select-mode--hook-func)

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

This is safe to call it select-mode is not active."
  (interactive)
  (message "exit")
  (if select-mode
      (select-mode 0)
    (select-mode--cleanup)))


(defun select-mode-abort ()
  "Exits select mode and returns point to its original position."
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
  "Change type to line and select the line origin is on.

If the type is already line, select the next line."
  (interactive)
  (if (eq select-mode--type 'line)
      (select-mode--line-expand)
    (select-mode--undo-push)
    (setq select-mode--type 'line)
    (goto-char (overlay-start select-mode--origin))
    (beginning-of-line)
    (push-mark (point) t t)
    (forward-line 1)))

(defun select-mode--line-expand ()
  (select-mode--undo-push)
  (forward-line 1))
