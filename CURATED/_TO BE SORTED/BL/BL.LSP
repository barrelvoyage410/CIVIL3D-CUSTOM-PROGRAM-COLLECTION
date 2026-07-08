;BL
;BL.LSP                    *** VERSION 1.0 ***               (2/9/91)
;TO CREATE BUILDING LINES, ETC. - WORKS IN POLYLINE MODE.
;ALLOWS RE-USE OR RESET OF SNAPANGLE, AND FORCES ORTHOMODE.
;LAWRENCE LIEBERMAN - HOGAN, SCHOCH & ASSOCIATES - SEBASTOPOL, CA.
;

;*******************************************   SET/RESET SNAPANGLE
(defun SNP ()
(setvar "snapmode" 0)
(setvar "orthomode" 0)
(setq p1 (getpoint "\nSelect Base Point: "))
(setq p2 (getpoint p1 "\nSelect Alignment Point: "))
(command "snap" "r" p1 p2)
(setvar "orthomode" 1)
(prompt "\nType ZERO to Re-set Snap Angle to 0 Degrees...  ")
)
;*******************************************   RESET SNAPANGLE TO 0
(defun C:ZERO ()
(setvar "snapang" 0)
)
;*******************************************   C:BL
(defun C:BL (/ p1 p2 kw)
(setq ortho (getvar "orthomode"))
(setq kw (strcase (getstring "\n<Any Key> to Re-Use Snap Angle, (Y)es to Reset: ")))
(if (= kw "Y")(snp))
(setvar "snapmode" 0)
(setvar "orthomode" 1)
(prompt "\nBEGIN Line... ")
(command "pline" P1 P2)
)

