;(defun C:PERPLINE ()
(defun C:Ties ()
  (setq CGL_CMDECHO (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq OSM (getvar "OSMODE"))
  (princ)
  (setvar "OSMODE" 1025) ; QUICK ENDPOINT - REFERENCE GUIDE FOR MORE INFO
  (while
    (setq P1 (getpoint "\nStart of line: "))
     (setvar "OSMODE" 1152) ; QUICK PERPENDICULAR
     (setq P2 (getpoint P1 "\nEnd of line: "))
     (setvar "OSMODE" 0) ; NO OSNAP
     (command "LINE" P1 P2 "")
     (setq ANG (angle P1 P2))
     (setq CGL_DIMSCALE (getvar "DIMSCALE"))
     (setq CGL_DIMASZ (getvar "DIMASZ"))
     (setq 1TH (* CGL_DIMASZ CGL_DIMSCALE))
     (setq PP2 (polar P1 ANG 1TH))
     (setq P02 (polar PP2 (+ ANG 1.570796) (/ 1TH 5.5)))
     (setq P03 (polar PP2 (- ANG 1.570796) (/ 1TH 5.5)))
     (command "SOLID" P1 P02 P03 "" "")
     (setq ANG (angle P2 P1))
     (setq PP2 (polar P2 ANG 1TH))
     (setq P02 (polar PP2 (+ ANG 1.570796) (/ 1TH 5.5)))
     (setq P03 (polar PP2 (- ANG 1.570796) (/ 1TH 5.5)))
     (command "SOLID" P2 P02 P03 "" "")
     (setvar "OSMODE" 1025);return osnap to quick end
     (princ)
  ) ;_ end while
  (princ)
  (setvar "OSMODE" OSM)
  (setvar "CMDECHO" CGL_CMDECHO)
  (princ)
) ;_ end defun