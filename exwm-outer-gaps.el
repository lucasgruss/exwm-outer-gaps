;;; exwm-outer-gaps.el -*- lexical-binding: t; -*-
(require 'exwm-workspace)
(require 'exwm-core)
(require 'exwm)
(require 'xelb)

(defgroup exwm-outer-gaps nil
  "Outer gaps for exwm."
  :group 'appearance
  :prefix "exwm-outer-gaps")

(defcustom exwm-outer-gaps-width [15 15 15 15]
  "Value for the outer gaps to the left, right, top and bottom of
 the emacs frames.")

(defcustom exwm-outer-gaps-increment-step 5
  "Default increment/decrement value for gaps.")

(defun exwm-outer-gaps-compute-gaps ()
  "Hook to be ran after exwm-workspace--update-workareas-hook"
  (let (workareas frames)
    (dolist (w exwm-workspace--workareas)
      (setf (aref w 0) (+ (aref w 0) (aref exwm-outer-gaps-width 0)) ; x for top left corner
            (aref w 1) (+ (aref w 1) (aref exwm-outer-gaps-width 2)) ; y for top left corner
            (aref w 2) (- (aref w 2) (+ (aref exwm-outer-gaps-width 0) (aref exwm-outer-gaps-width 1))) ; width of frame
            (aref w 3) (- (aref w 3) (+ (aref exwm-outer-gaps-width 2) (aref exwm-outer-gaps-width 3))))))) ; height of frame

(defun exwm-outer-gaps-apply ()
  "Function used to apply gaps to the emacs frames."
  (exwm-workspace--update-workareas)
  (dolist (f exwm-workspace--list)
    (exwm-workspace--set-fullscreen f)))

(defun exwm-outer-gaps-set (border width increment)
  "Set border in {left, right, top, bottom} to width.
increment (bool) indicates whether the width should be used as
absolute width or as increment value."
  (aset exwm-outer-gaps-width
        border
        (+ (if increment
               (aref exwm-outer-gaps-width border)
             0)
           width)))

(defun exwm-outer-gaps-set-all (width increment)
  "Set all gaps. Increment (bool) determines whether the width
should be used as absolute width or as increment value"
  (dotimes (border 4)
    (exwm-outer-gaps-set border width increment)))

(defun exwm-outer-gaps-increment (arg)
  "Increment the outer gaps. If arg {0, 1, 2, 3} is passed,
increment the corresponding {left, right, top, bottom} border.
Else, increment all borders by the same
exwm-outer-gaps-increment-step"
  (interactive "P")
  (when exwm-outer-gaps-mode
    (if arg
        (exwm-outer-gaps-set arg exwm-outer-gaps-increment-step t)
      (exwm-outer-gaps-set-all exwm-outer-gaps-increment-step t))
    (exwm-outer-gaps-apply)))

(defun exwm-outer-gaps-decrement (arg)
  "Decrement the outer gaps. If arg {0, 1, 2, 3} is passed,
decrement the corresponding {left, right, top, bottom} border.
Else, decrement all borders by the same
exwm-outer-gaps-increment-step"
  (interactive "P")
  (when exwm-outer-gaps-mode
    (if arg
        (exwm-outer-gaps-set arg (- exwm-outer-gaps-increment-step) t)
      (exwm-outer-gaps-set-all (- exwm-outer-gaps-increment-step) t))
    (exwm-outer-gaps-apply)))

(defun exwm-outer-gaps-balance (border)
  "Set all gaps to the width of border"
  (interactive "P")
  (if border
      (exwm-outer-gaps-set-all (aref exwm-outer-gaps-width border) nil)
    (exwm-outer-gaps-set-all (aref exwm-outer-gaps-width 0) nil))
  (exwm-outer-gaps-apply))

;;;###autoload
(define-minor-mode exwm-outer-gaps-mode
  "Add useless outer gaps to exwm."
  :global t
  (if exwm-outer-gaps-mode
      (add-hook 'exwm-workspace--update-workareas-hook
                #'exwm-outer-gaps-compute-gaps)
    (remove-hook 'exwm-workspace--update-workareas-hook
                 #'exwm-outer-gaps-compute-gaps))
  (exwm-outer-gaps-apply))

(provide 'exwm-outer-gaps)
