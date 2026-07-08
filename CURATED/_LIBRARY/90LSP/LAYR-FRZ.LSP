;
;+LAYR-FRZ - Freeze a layer by selecting an entity on that layer
; ACADemy User Group, Portland, OR
;  
;  This routine lets you freeze a layer by selecting an entity on that 
;  layer.
;
(defun C:LAYR-FRZ ( / flayer ent echo)
   (setq echo (getvar "CMDECHO"))
   (setvar "CMDECHO" 0)
   (setq ent (entsel "\nPick an entity on the Layer To Freeze: "))
   (if (NULL ent); Make sure an object was selected
      (while (NULL ent)
         (princ "\nTry again or press ^C to quit: ")
         (setq ent (entsel))
      ); While
   ): If
   (setq ent (entget (car ent)))
   (setq flayer (cdr (assoc 8 ent)))
   (COMMAND "LAYER" "F" FLAYER "")
   (setvar "CMDECHO" echo)
   (princ)
); Defun LAYR-FRZ
