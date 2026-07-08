;Et.LSP:  Changes a set of text objects to all read the same.
;
;________________________________________________________________________


(defun c:et ( / v1 v2 v3 newtx nme oldtx)
(setvar "cmdecho" 0)
          (prompt "\nPick text to be changed.")
          (setq v1 (ssget '((0 . "TEXT")) ))
          (setq newtx (getstring T "\nEnter new string: "))
          (setq newtx (cons 1 newtx))
          (setq v2 0)
             (if (and v1 newtx)
                 (while (< v2 (sslength v1))
                        (setq nme (ssname v1 v2))
                        (setq oldtx (assoc 1 (entget nme)))
                        (setq v3 (entget nme))
                        (entmod (subst newtx oldtx v3))
                        (entupd nme)
                        (setq v2 (+ v2 1))
                 )
             )
)


