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
;;
;; Consider using the types from thing-at-pt.
(defvar hydra-select-type nil)
(defvar hydra-select-dir 'forward)

(defvar select-mode--origin nil
  "An overlay visually marking the original location of point.

All initial selections of \"things\" start from this point.")

(defface select-mode-origin '((t (:inverse-video t :inherit error)))
  "Faced used to highlight the origin."
  :group 'killing)


(defun select-mode-init ()
  "Setup code called when entering the select hydra."
  (message "init")

  (setq hydra-select-type 'sexp)
  (setq hydra-select-dir  'forward)

  (setq select-mode--origin (make-overlay (point) (1+ (point))))
  (overlay-put select-mode--origin 'face 'select-mode-origin)

  (message "init2: %s" select-mode--origin)

  ;; Eventually I'd like either a variable for the initial type or a function
  ;; run to perform the initial select.

  (when-let ((bounds (bounds-of-thing-at-point 'sexp)))
    (message "bounds: %s" bounds)
      (push-mark (car bounds) t t)
      (goto-char (cdr bounds))))


(defun select-mode-cleanup ()
  "Cleaup code called when exiting select-mode.

This does not change the selection."
  (message "cleanup")
  (and (overlayp select-mode--origin)
       (delete-overlay select-mode--origin))
  (setq select-mode--origin nil))


(defun select-mode-copy-or-mark ()
  "Copy if region exists, otherwise start the select hydra."
  (interactive)
  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end) t)
    (hydra-select/body)))

(defun select-mode-abort ()
  "Turn off select-mode and remove the selection."
  (interactive)
  (message "abort: %s" select-mode--origin)
  (goto-char (overlay-start select-mode--origin))
  (deactivate-mark)
  )

;; (keymap-set global-map "C-M-w" 'select-mode-copy-or-mark)


;; TODO: How do we keep multiple instances of this hydra from conflicting?  Is
;; there something we can attach a plist to?


(defhydra hydra-select (:body-pre select-mode-init
                        :after-exit select-mode-cleanup
                        :hint nil
                        :color red)
  "
Select
^Types^          ^Pairs^^^^                ^Find^           ^Misc^
_w_ word         _[_ ^outer  _]_ inner     _f_ find         _c_ change dir
_s_ symbol       _(_ ^outer  _)_ inner     _t_ til          _;_ repeat
_x_ expression   _{_ ^outer  _}_ inner     _F_ find back    _z_ undo
_l_ line         _\"_ inner  ^ ^           _T_ til back     _-_ shrink
_._ sentence     _'_ ^inner  _I_ insert    ^ ^
_p_ paragraph    _`_ ^inner  _C_ change    ^ ^
_d_ function     ^ ^ ^       _D_ delete         _RET_ exit  _q_ abort
"
  ("RET" nil nil :color blue)

  ;; TODO: Can use use the lowercase i c d?  Change shrink to something else -?
  ;; Maybe change " to inner and hit again for outer?
  ;; Also conflicts with d for function

  ;; TODO: Need direction key.
  ;; Any other symbols on base?  , . / ; '

  ;; Could use + for repeat and also make it work on pairs to continue "up".

  ;; TODO: items below this are incomplete
  (";" hydra-select--word nil)
  ("z" hydra-select--word nil)
  ("w" hydra-select--word nil)
  ("s" hydra-select--WORD nil)
  ("x" hydra-select--WORD nil)
  ("l" hydra-select--WORD nil)
  ("s" hydra-select--WORD nil)
  ("p" hydra-select--WORD nil)
  ("." hydra-select--WORD nil)
  ("(" hydra-select--WORD nil)
  (")" hydra-select--WORD nil)
  ("{" hydra-select--WORD nil)
  ("}" hydra-select--WORD nil)
  ("[" hydra-select--WORD nil)
  ("]" hydra-select--WORD nil)
  ("\"" hydra-select--WORD nil)
  ("'" hydra-select--WORD nil)
  ("`" hydra-select--WORD nil)
  ("f" hydra-select--WORD nil)
  ("F" hydra-select--WORD nil)
  ("t" hydra-select--WORD nil)
  ("T" hydra-select--WORD nil)
  ("d" hydra-select--WORD nil)
  ("" hydra-select--WORD nil)
  ("-" hydra--select-word nil)
  ("I" hydra--select-word nil :color blue)
  ("C" hydra--select-word nil :color blue)
  ("D" hydra--select-word nil :color blue)
  ("q" select-mode-abort nil :color blue)
  ("g" nil nil :color blue)
  )



(defun hydra-select--word ()
  )

(defun hydra-select--WORD ()
  )
