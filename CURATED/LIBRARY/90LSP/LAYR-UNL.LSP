;
;+LAYR-UNL *+ Pick layer to unlock from an entity in the drawing
; ACADemy User Group, Portland, OR
;
(defun C:LAYR-UNL ( / ent lyrun echo)
   (setq echo (getvar "CMDECHO"))
   (setvar "CMDECHO" 0)
   (setq ent (entsel "\nSelect an entity on the layer to UnLock: "))
   (if (NULL ent); Make sure an object was selected
      (while (NULL ent)
         (princ "\nTry again or press ^C to quit: ")
         (setq ent (entsel))
      ); While
   ): If
   (setq ent (entget (car ent)))
   (setq lyrun (cdr (assoc 8 ent)))
   (COMMAND "LAYER" "UNLOCK" lyrun "")
   (setvar "CMDECHO" echo)
   (princ)
); Defun LAYR-UNL
