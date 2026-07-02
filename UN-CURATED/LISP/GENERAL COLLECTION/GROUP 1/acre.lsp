;routine tp present area in acres
;this routine is used to redefine the AREA command

(defun c:acre ()
(setq OS (getvar "osmode"))
(setvar "osmode" 512)
(setq ENT (entsel "\nSelect Polygon "))
(setvar "osmode" os)

(command ".area" "e" ent)
(setq Area (getvar "area"))
(setq Acre 43560)

(setq a (/ area acre))
(princ "    ")
(princ a)
(princ " Acres     ")
(princ area)
(princ " S.F.")

(princ)

)