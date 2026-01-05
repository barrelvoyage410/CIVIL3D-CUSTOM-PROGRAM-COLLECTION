;;; Josh Crawford
;;; Function to twist view based on two picked points, or rotation angle of a block or text, or the angle of a line. 
(defun c:tw (/ ang block blockdata text textdata dxf)

;;;Necessary functions

;;extract data from dotted pair
(defun dxf (code elist) (cdr (assoc code elist)))
;;radians to degrees
(defun rtd (r) (/ (* r 180.0) pi))


;;;main program
  (if (and (= 0 (getvar "tilemode"))
	   (eq
	     16384
	     (logand
	       16384
	       (dxf 90
		    (entget
		      (ssname
			(ssget
			  "x"
			  (list	(cons 0 "viewport")
				(cons 69 (getvar "cvport"))
				(cons 410 (getvar "ctab"))
			  )
			)
			0
		      )
		    )
	       )
	     )
	   )
      )
    (princ "\nCurrent Viewport is LOCKED.\nUnlock viewport and try again.")
    (progn
      (setvar "CMDECHO" 0)
      (initget 1 "Block Text Pick Line")
      (setq ang
	     (getangle
	       "\nSet viewtwist by [Block angle/Line angle/Text angle/<Pick points]: "
	     )
      )
      (setvar "errno" 0)
      (cond
	((= ang "Text")
	 (while	(and (not (numberp ang))
		     (or (setq text
				(nentsel
				  "\nSelect TEXT, MTEXT, RTEXT, ATTDEF or ATTRIB to set viewtwist: "
				)
			 )
			 (eq 7 (getvar "errno"))
		     )
		)
	   (if
	     (and
	       text
	       (member (dxf 0 (setq textdata (entget (car text))))
		       (list "TEXT" "MTEXT" "RTEXT" "ATTDEF" "ATTRIB")
	       )
	     )
	      (setq ang (dxf 50 textdata))
	      (progn
		(princ "\nSelect object not TEXT, MTEXT, RTEXT, ATTDEF or ATTRIB.")
		(setvar "errno" 0)
	      )
	   )
	 )
	)
	((= ang "Block")
	 (while
	   (and
	     (not (numberp ang))
	     (or (setq
		   block (entsel "\nSelect Block to set viewtwist: ")
		 )
		 (eq 7 (getvar "errno"))
	     )
	   )
	    (if	(and
		  block
		  (= (dxf 0 (setq blockdata (entget (car block)))) "INSERT")
		)
	      (setq ang (dxf 50 blockdata))
	      (progn
		(princ "\nSelected object not a Block.")
		(setvar "errno" 0)
	      )
	    )
	 )
	)
	((= ang "Pick")
	 (initget 1)
	 (setq ang (getangle "\nSelect angle: "))
	)
	((= ang "Line")
	 (while	(and (not (numberp ang))
		     (or (setq line
				(nentsel
				  "\nSelect LINE: "
				)
			 )
			 (eq 7 (getvar "errno"))
		     )
		)
	   (if
	     (and
	       line
	       (= (dxf 0 (setq linedata (entget (car line)))) "LINE")
	       )
	      (setq pick (cadr line)
		    beg (dxf 10 linedata)
		    end (dxf 11 linedata)
		    ang (if (< (distance beg pick)(distance end pick)) (angle beg end)(angle end beg))
		    )
	      (progn
		(princ "\nSelected object not a LINE.")
		(setvar "errno" 0)
	      )
	   )
	 )
	 )
	(t nil)
      )
    )
  )
  (if ang
    (progn
      (command "_DVIEW" "" "TW" (- 360 (rtd ang)) "")
      (setvar "SnapAng" ang)
    )
  )
  (princ)
)


