;TIP1321.LSP: ACRE.LSP Area by Acres (c)1997, Robert Vecchi

;routine to present area in acres
(defun c:acre ()
(setq OS (getvar "osmode"))
(setvar "osmode" 512)
(setq ENT (entsel "\nSelect Polygon "))
(setvar "osmode" OS)

(command ".area" "e" ENT)
(setq Area (getvar "area"))
(setq Acre 43560)
(setq a (/ area acre))
(princ " ")
(princ a) 
(princ " Acres ")
(princ area)
(princ " S.F.")

(princ)

)
