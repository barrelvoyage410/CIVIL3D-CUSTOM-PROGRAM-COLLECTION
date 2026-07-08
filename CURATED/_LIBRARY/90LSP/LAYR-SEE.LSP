;
;+LAYR-SEE  - Pick an entity to see what layer its on.
;
; ACADemy User Group, Portland, OR
;
(defun C:LAYR-SEE (/ clayr ent)
   (setq ent (entsel "\nSelect an entity to see the layer it is on: "))
   (if (NULL ent); Make sure an object was selected
      (while (NULL ent)
         (princ "\nTry again or press ^C to quit: ")
         (setq ent (entsel))
      ); While
   ): If
   (setq ent (entget (car ent)))
   (setq clayr (cdr (assoc 8 ent)))
   (prompt (strcat "\nThe Entity Selected is on Layer < "
                   clayr " >. "))
   (princ)
);Defun LAYR-SEE
