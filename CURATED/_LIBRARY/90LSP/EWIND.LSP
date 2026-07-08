;	Tom A. Stoeckel 71521,450  His Sound, Fresno CA
;	This lisp routine will insert (from one end) a single window
:	with a 2"x4" jamb on each side.  All but the first selection point
;	have osnaps set. It has only been used on Release 9.  
;	I would welcome comments and criticism.  This is FREEWARE.  Enjoy!

(defun C:EWIND( / W P1 P2 P3 P4 A1 A2 J1 J2 p a b c d os)
	(setvar "CMDECHO" 0)
	(setq W (getdist "\nEnter Width of Window:  "))
		(setq W (+ W 4))
;	(menucmd "S=OSNAP")
;	(menucmd "P1=*")
	(setq P1 (getpoint "\nPick One End of Outside Window: "))
		(setvar "lastpoint" P1)
	(setq os (getvar "OSMODE"))
	(setvar "OSMODE" 128)
	(setq P2 (getpoint "\nTouch Opposite Wall: "))
	(setvar "OSMODE" 512)
	(setq P3 (getpoint "\nPick Opposite End of Outside Window: "))
	(setvar "OSMODE" os)
	(setq A1 (angle P1 P3))
	(setq P3 (polar P1 A1 W))
	(setq A2 (angle P1 P2))
	(setq P4 (polar P3 A2 (distance P1 P2)))
	(setq J1 (polar P1 A1 2))
	(setq J2 (polar P3 (+ pi A1) 2))

	(command "line" P1 P2 "")
	(command "line" P3 P4 "")

	(command "pline"
		(setq p (polar P1 A1 2))
		(setq a (polar p A2 2))
		(setq b (polar a A2 2))
		(setq p (polar b (+ pi A1) 2))
	"")

	(command "pline"	
		(setq p (polar P3 (+ pi A1) 2))
		(setq c (polar p A2 2))
		(setq d (polar c A2 2))
		(setq p (polar d A1 2))
	"")
	(command "line" a c "")
	(command "line" b d "")
	(prin1)
)
