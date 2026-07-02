; CHWIDTH.LSP,  1991  Tony Tanzillo
;
; Adds the CHWIDTH command to AutoCAD, which changes the width of a
; selected group of 2D polylines to a new specified value.
;
; Load file with (load"filename"), where "filename" is the name you
; give to this file, on your disk.

;; revised 10:32:58 Wednesday, 01-25-1995 for CGL uses by Mark Evinger

   (defun C:pWIDTH ( / ss e d i j w1 w2)
      (princ "\nSelect polyline(s) to edit,")
      (cond
         (  (not (setq ss (ssget))))
         (t (initget 5)
            (setq w1 (getdist "\nNew width for all polylines: ")
                  j  0
                  w1 (cons 40 w1)
                  w2 (cons 41 (cdr w1)))
            (repeat (setq i (sslength ss))
               (cond
                  (  (and (setq e (ssname ss (setq i (1- i))))
                          (setq d (entget e))
                          (eq (get 0 d) "*POLY*"); changed from "POLYLINE" for R14 LWpolyline compatibility
                          (zerop (logand 88 (get 70 d))))
                     (setq j (1+ j))
                     (entmod (subst w1 (assoc 40 d)
                                       (subst w2 (assoc 41 d) d))))))
            (princ (strcat "\nChanged "
                           (itoa j)
                           " polylines." ))))
      (princ)
   )

   (defun get (k l)
      (cdr (assoc k l))
   )

   ; --------------------------eof chwidth.lsp-------------------------

