(defun C:TWEEN ()
(setq P1 (getpoint "\nSelect first point : "))
(setq P2 (getpoint P1 "\nSelect second point : "))
(setq P3 (polar P1 (angle P1 P2) ( / (distance P1 P2) 2)))
)

