(defun C:SLP ()
  (while
    (setq P1 (getpoint "\nSelect BOTTOM of slope break: "))
     (setq P2 (getpoint P1 "\nSelect TOP of slope break: "))
     (command "INSERT" "slpe_brk" P1 "1" "1" P2)
     (command "SCALE" "L" "" P1 P2)
  ) ;end while
  (princ)
) ; END - slp
