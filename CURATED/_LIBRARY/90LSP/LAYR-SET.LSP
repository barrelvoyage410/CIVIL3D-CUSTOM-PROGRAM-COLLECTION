;
;+LAYR-SET    *+ Set a layer by selecting an entity from the drawing
; ACADemy User Group, Portland, OR
;
(defun C:LAYR-SET ( / ent slayr echo)
   (setq echo (getvar "CMDECHO"))
   (setvar "CMDECHO" 0)
   (setq ent (entsel
      "\nSelect an entity on the layer you want to Set as current: "
   )         )
   (if (NULL ent); Make sure an object was selected
      (while (NULL ent)
         (princ "\nTry again or press ^C to quit: ")
         (setq ent (entsel))
      ); While
   ): If
   (setq ent (entget (car ent)))
   (setq slayr (cdr (assoc 8 ent)))
   (COMMAND "LAYER" "S" slayr "")
   (setvar "CMDECHO" echo)
   (princ)
); Defun LAYR-SET
