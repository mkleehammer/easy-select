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

(defvar select-mode--type nil)
(defvar select-mode--dir 'forward)

(defvar select-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "q" #'select-mode-abort)
    (keymap-set map "RET" #'select-mode-abort)
    map))

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

  (setq select-mode--origin (make-overlay (point) (1+ (point))))
  (overlay-put select-mode--origin 'face 'select-mode-origin)

  (message "setup2: %s" select-mode--origin)

  ;; Eventually I'd like either a variable for the initial type or a function
  ;; run to perform the initial select.

  (when-let ((bounds (bounds-of-thing-at-point 'sexp)))
    (message "bounds: %s" bounds)
      (push-mark (car bounds) t t)
      (goto-char (cdr bounds))))


(defun select-mode-exit ()
  "Exits select mode, leaving the current selection.

This is safe to call it select-mode is not active."
  (interactive)
  (message "exit")
  (and (overlayp select-mode--origin)
       (delete-overlay select-mode--origin))
  (setq select-mode--origin nil))


(defun select-mode-abort ()
  "Turn off select-mode and remove the selection."
  (interactive)
  (message "abort: %s" select-mode--origin)
  (goto-char (overlay-start select-mode--origin))
  (deactivate-mark)
  )

;; (keymap-set global-map "C-M-w" 'select-mode-copy-or-mark)


(define-minor-mode select-mode
  "Provides quick commands for selecting and manipulating the region."
  :lighter "sel"
  :keymap select-mode-map

  (if select-mode
      (select-mode--setup))
  (select-mode-exit))


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
