 epu is stand for edit precision units
;       Design by Ade Suharna
;       4 September 2004
;       program no. 64/09/2004
;       edit by Rick Keller <rick@NOT.NET>     not recorded
;               Ade Suharna 05/10/2004         1).
 (defun c:epu (/ sd val-sd digit)
 (vl-load-com)
 (while                                                       ; 1).
  (setq sd (entsel "\nSelect Dimension.  ")
        sd (vlax-ename->vla-object (car sd))
        val-sd (rtos (vlax-get-property sd "PrimaryUnitsPrecision"))
        digit (getreal (strcat "\nENTER NEW PRECISION VALUE" "<" val-sd 
">" 
":")))
  (vlax-put-property sd "PrimaryUnitsPrecision"  digit)
  (vlax-release-object sd)
  )
 (princ)
   )