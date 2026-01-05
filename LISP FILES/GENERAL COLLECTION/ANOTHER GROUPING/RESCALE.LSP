; RESCALE command - rescale selected objects to known points

(defun C:RESCALE (/ ce p1 p1new p2 p2new rot ss scl )
	(princ "Select objects to re-scale...\n")
	(setq ss (ssget))		; General selection set
	(if ss (progn			; If any objects selected
		(princ "For the selected objects")
		(setq p1 (getpoint "\nEnter first known point: "))
		(setq p2 (getpoint "\nEnter second known point: "))
		(setq p1new (getpoint "\nEnter new position for first point: "))
		(setq p2new (getpoint "\nEnter new position for second point: "))
		(setq ce (getvar "CMDECHO"))
		(setvar "CMDECHO" 0)
		(command "MOVE" ss "" p1 p1new)
		(setq scl ( / (distance p1new p2new) (distance p1 p2)))
		(command "SCALE" ss "" p1new scl)
		(setq rot ( - (angle p1new p2new) (angle p1 p2)))
		(setq rot ( * 180.0 ( / rot PI)))	;rads to degrees
		(command "ROTATE" ss "" p1new rot)
		(setvar "CMDECHO" ce)
	))
	(prin1)
)

