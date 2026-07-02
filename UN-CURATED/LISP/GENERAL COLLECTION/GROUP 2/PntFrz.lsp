(princ
  "\nThis program will move the selected SDSK point blocks to a *-f layer"
) ;_ end of princ
(princ "\nType PNTFRZ to begin")
(princ)

(defun C:PNTFRZ	()
  (setq SSPOINTS (ssget '((0 . "INSERT") (2 . "POINT"))))
  (setq COUNT 0)
  (repeat (sslength SSPOINTS)
    (setq CGL_OBJECT (entget (ssname SSPOINTS COUNT)))
    (setq CGL_OBJ_CLAY (cdr (assoc 8 CGL_OBJECT)))
    (setq CGL_OBJ_FRZ (strcat CGL_OBJ_CLAY "-f"))
    (if	(= (tblsearch "layer" CGL_OBJ_FRZ) NIL)
      (progn
	(command "._layer" "n" CGL_OBJ_FRZ "c" "1" CGL_OBJ_FRZ "") ;create layer, color red
	(command "._layer" "f" CGL_OBJ_FRZ "") ;freeze layer
      ) ;end progn
      (command "._layer" "f" CGL_OBJ_FRZ "") ;freeze layer
    ) ;_ end if
    (setq CGL_OBJECT
	   (subst (cons '8 CGL_OBJ_FRZ)
		  (assoc 8 CGL_OBJECT)
		  CGL_OBJECT
	   ) ;_ end of subst
    ) ;_ end of setq
    (entmod CGL_OBJECT) ;update point block
    (setq COUNT (+ COUNT 1))
  ) ;repeat
  (princ)
) ; end defun
(princ)