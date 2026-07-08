;
;+LAYR-OFF *+ Pick layer to turn off from an entity in the drawing
; ACADemy User Group, Portland, OR
;
(defun C:LAYR-OFF ( / echo ent lyroff clyr)
   (setq echo (getvar "CMDECHO"))
   (setvar "CMDECHO" 0)
   (setq ent (entsel "\nSelect an entity on the layer to turn OFF: "))
   (if (NULL ent); Make sure an object was selected
      (while (NULL ent)
         (princ "\nTry again or press ^C to quit: ")
         (setq ent (entsel))
      ); While
   ): If
   (setq ent (entget (car ent)))
   (setq lyroff (cdr (assoc 8 ent)))
   (setq clyr (getvar "CLAYER"))
   (if (/= clyr lyroff); Don't attempt to turn off the current layer
      (COMMAND "LAYER" "OFF" lyroff "")
      (princ "\nLAYR-OFF will not turn off the current layer.\n")
   ); IF
   (setvar "CMDECHO" echo)
   (princ)
); Defun LAYR-OFF
