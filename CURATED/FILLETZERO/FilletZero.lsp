;;  FilletZero.lsp [command name = FZ]
;;  Calls up Fillet mUltiple at 0 radius; when done, restores prior radius.
;;  Concept from Cadalyst CAD Tips #1365 "Fillet with Zero Radius", by
;;  William M. Bright [1997], which does only one Fillet operation.
;;  Re-written to repeat as long as desired, and to add error handling, by
;;  Kent Cooper, February 2012.

(defun C:FZ (/ *error* fr); = Fillet Zero-radius
  (defun *error* (errmsg)
    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
      (princ (strcat "\nError: " errmsg))
    ); if
    (setvar 'filletrad fr)
    (princ)
  ); defun - *error*
  (setq fr (getvar 'filletrad))
  (setvar 'filletrad 0)
  (command "_.fillet" "_mUltiple")
  (while (> (getvar 'cmdactive) 0) (command pause))
  (setvar 'filletrad fr)
  (princ)
); defun