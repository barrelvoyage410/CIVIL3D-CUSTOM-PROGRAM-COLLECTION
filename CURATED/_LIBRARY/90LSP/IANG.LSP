(prompt "\nBy Dynamic Enterprises...")
(prompt "\ncopyright 1993")
(defun C:IANG () (Setvar "Cmdecho" 0)
	(setq A (entsel "\nTouch first line: "))
	(If A (Progn (setq B (entget (Car A))) (If                                                                               
	(= "LINE" (Cdr (assoc 0 B))) (Progn
	(setq C (entsel "\nTouch second line: "))
	(If C (Progn (setq D (Entget (Car C)))
	(If (= "LINE" (Cdr (assoc 0 D))) (Progn
	(setq P1 (Osnap (Cadr A) "MID")) (setq P2 (Osnap (Cadr C) "MID"))
	(setq P3 (Inters (Cdr (assoc 10 B)) (Cdr (assoc 11 B))
	(Cdr (assoc 10 D)) (Cdr (assoc 11 D)) ONSEG))
	(setq ANG (- (Angle P3 P2) (Angle P3 P1)))
	(setq ANG (Angtos ANG (Getvar "Aunits") (Getvar "Auprec")))
	(Princ (Strcat "\nIncluded Angle is " ANG ".")))
	(Princ "\nObject selected is not a LINE")))
	(Princ "\nNo object found at that location")))
	(Princ "\nObject selected is not a LINE")))
	(Princ "\nNo object found at that location")) (Princ))
