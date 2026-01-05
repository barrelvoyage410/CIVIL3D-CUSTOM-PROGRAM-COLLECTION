;; Revised for R14 compatibility 3/20/1998 by Mark Evinger
(defun C:ARRW (/ CGL_OSMODE)
  (setvar "CMDECHO" 0)
  (princ)
  (while
    (setq P1 (getpoint "\nStart of line "))
     (setq P2 (getpoint P1 "\nEnd of line "))
     (setq CGL_OSMODE (getvar "OSMODE"))
     (setvar "OSMODE" 0) ;turn off all osnaps while dwg the line
     (command "LINE" P1 P2 "")
     (setvar "OSMODE" CGL_OSMODE)
     (setq ANG (angle P1 P2))
     (setq CGL_DIMSCALE (getvar "DIMSCALE"))
     (setq CGL_DIMASZ (getvar "DIMASZ"))
     (setq 1TH (* CGL_DIMASZ CGL_DIMSCALE))
     (setq PP2 (polar P1 ANG 1TH))
     (setq P02 (polar PP2 (+ ANG 1.570796) (/ 1TH 5.5)))
     (setq P03 (polar PP2 (- ANG 1.570796) (/ 1TH 5.5)))
     (setq CGL_OSMODE (getvar "OSMODE"))
     (setvar "OSMODE" 0) ;turn off all osnaps while dwg the arrowhead
     (command "SOLID" P1 P02 P03 "" "")
     (setvar "OSMODE" CGL_OSMODE)
     (setq ANG (angle P2 P1))
     (setq PP2 (polar P2 ANG 1TH))
     (setq P02 (polar PP2 (+ ANG 1.570796) (/ 1TH 5.5)))
     (setq P03 (polar PP2 (- ANG 1.570796) (/ 1TH 5.5)))
     (setq CGL_OSMODE (getvar "OSMODE"))
     (setvar "OSMODE" 0) ;turn off all osnaps while dwg the arrowhead
     (command "SOLID" P2 P02 P03 "" "")
     (setvar "OSMODE" CGL_OSMODE)
     (princ)
  ) ;END WHILE
  (setvar "OSMODE" CGL_OSMODE)
  (princ)
) ;DEFUN C:ARRW