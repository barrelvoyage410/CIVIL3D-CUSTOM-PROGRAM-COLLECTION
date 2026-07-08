;
;+LAYR-UN *+ Pick layer to lock from an entity in the drawing
; ACADemy User Group, Portland, OR
;
(defun C:LAYR-LOK ( / echo ent lyrlk)
   (setq echo (getvar "CMDECHO"))
   (setvar "CMDECHO" 0)
   (setq ent (entsel "\nSelect an entity on the layer to Lock: "))
   (if (NULL ent); Make sure an object was selected
      (while (NULL ent)
         (princ "\nTry again or press ^C to quit: ")
         (setq ent (entsel))
      ); While
   ): If
   (setq ent (entget (car ent)))
   (setq lyrlk (cdr (assoc 8 ent)))
   (COMMAND "LAYER" "LOCK" lyrlk "")
   (setvar "CMDECHO" echo)
   (princ)
); Defun LAYR-LOK
