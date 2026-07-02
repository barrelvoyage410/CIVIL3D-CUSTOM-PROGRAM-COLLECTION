(defun c:findshape ( / shp cnt ltype )
  (if (cgl_check_serial)
    (progn
(princ "\n*** SHAPES REFERENCED IN DRAWING ***")
(setq shp (ssget "x" '((0 . "shape"))))
(if shp (setq cnt (sslength shp))
         (prompt "\nNo Shapes found.")
)
(while cnt
   (print (entget (ssname shp (- cnt 1))))
(print)
   (setq cnt (if (eq 1 cnt)
                 nil
                 (- cnt 1)
             )
   )
)
(princ "\n***LINETYPES REFERENCED IN DRAWING ***")
(SETQ ltype  (entget (tblobjname "ltype" (cdr (assoc 2 (tblnext "ltype" t))))))
(while ltype
   (print (cdr (assoc 2 ltype)))
   ;(print (cdr (assoc 3 (entget (cdr (assoc 340 ltype))))))

   (if (assoc 340 ltype)
       ;(princ (entget (cdr (assoc 340 ltype))))
        (princ (cdr (assoc 3 (entget (cdr (assoc 340 ltype))))))
       (princ "No Shapes in this ltype")
   )
   (if (setq ltype (tblnext "ltype"))
      (setq ltype (entget (tblobjname "ltype" (cdr (assoc 2 ltype)))))
;      (setq ltype (entget (tblobjname "ltype" (cdr (assoc 3 (entget (cdr (assoc 340 ltype)))))))

   )
)
(princ)
    );end checker progn
    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
    );end if
    (princ)
)