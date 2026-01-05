(princ
  "\nThis program will move the selected SDSK point blocks to a *-F layer"
) ;_ end of princ
(princ "\nType PNTFRZME to begin")
(princ)
(defun C:PNTFRZME ()
  (while
    (setq CGL_ENT (entsel "\nPlease select SDSK Point Block: "))
     (princ)
     (setq CGL_OBJECT (entget (car CGL_ENT)))
     (cond
       ((= "POINT" (cdr (assoc 2 CGL_OBJECT)))
        (setq CGL_OBJ_CLAY (cdr (assoc 8 CGL_OBJECT)))
        (if (= (substr CGL_OBJ_CLAY (- (strlen CGL_OBJ_CLAY) 1)) "-F") ; does the layer end in -F
          (setq CGL_OBJ_FRZ CGL_OBJ_CLAY) ;yup
          (setq CGL_OBJ_FRZ (strcat CGL_OBJ_CLAY "-f")) ;nope
        )
        (if (= (tblsearch "layer" CGL_OBJ_FRZ) NIL) ; if the layer does NOT exist
          (progn
            (command "._layer" "n" CGL_OBJ_FRZ "c" "1" CGL_OBJ_FRZ "") ;create layer, color red
            (command "._layer" "f" CGL_OBJ_FRZ "") ;freeze layer
            (princ (strcat " Point has been frozen to layer " (strcase CGL_OBJ_FRZ) "."))
          ) ;end progn
          (progn
            (command "._layer" "f" CGL_OBJ_FRZ "") ;freeze layer
            (princ (strcat " Point has been frozen to layer " (strcase CGL_OBJ_FRZ) "."))
          )
        ) ;_ end if
        (setq CGL_OBJECT
               (subst (cons '8 CGL_OBJ_FRZ)
                      (assoc 8 CGL_OBJECT)
                      CGL_OBJECT
               ) ;_ end of subst
        ) ;_ end of setq
        (entmod CGL_OBJECT) ;update point block
        (princ)
       ) ; cond = point block
       ((/= "POINT" (cdr (assoc 2 CGL_OBJECT))) ; if NOT the point block
        (princ
          (strcat "\n  " (cdr (assoc 0 CGL_OBJECT)) " selected. ") ; tell them what was selected
        )
       ) ; not the SDSK point block
     ) ;cond
  ) ;while
  (princ)
) ; end defun
(princ)