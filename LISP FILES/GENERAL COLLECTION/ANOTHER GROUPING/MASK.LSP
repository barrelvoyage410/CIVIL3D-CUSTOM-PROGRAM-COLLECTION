;;;Mask.lsp V1.5 by Joshua Tapp
;;;November 2000, with help from
;;;Tony Tanzillo on the command pause
;;;in library functions
;;;
;;;July 9, 2003 Added circular boundary
;;;and matching layer
;;;
;;;June 23, 2005 Added functionality for version 2004
;;;No longer asks for new
;;;
;;;September 28, 2005 Corrected bug with 2005
;;;
;;;Masks everything behind selected objects
;;;Based upon a polyline drawn, rectangular or polygonal options
;;;Turns wipeout and masked objects into anonymous groups

(defun c:maskany (/ pchoic cecho tst pl above under n)
  (setq cecho (getvar "CMDECHO"))
  (setq tst (entlast))
  (setvar "CMDECHO" 1)
  (initget "R P C")
  (setq
    pchoic (getkword
	     "\nRectangular, Polygonal or Circular boundary?<R>/P/C: "
	   )
  )
  (cond
    ((= pchoic "R")
     (recta)
    )
    ((= pchoic "P")
     (polyg)
    )
    ((= pchoic "C")
     (circu)
    )
    (T (recta))
  )					;cond
  (setq pl (entlast))
  (if (eq pl tst)
    (prompt "\nPolyline was not drawn.")
    (progn
      (prompt
	"\nSelect entities to appear above wipeout or hit enter if none."
      )
      (setq above    (ssget)		;select objects to mask around
	    above1   (ssname above 0)
	    abovelst (entget above1)	;gets layer from objects
	    abovelyr (assoc 8 abovelst)	;(assumes all on same layer)
      )
      (if above
	(progn
	  (command "_.wipeout" "" pl "y")
	  (setq	under	 (entlast)	;wipeout object
		underlst (entget under)	;gets wipeout's layer
		underlst (subst	(cons 8 (cdr abovelyr))
					;change it to match layer of
				(assoc 8 underlst) ;entities masking
				underlst
			 )		;subst
	  )				;setq
	  (entmod underlst)		;updates wipeout to match layer
	  (command "_.draworder" above "" "a" under "")
	)				;progn
	(command "_.wipeout" "n" pl "y")
      )
    )					;progn
  )					;if
					;(command "_.regenall")
  (command "_.-group"
	   "_create"
	   "*"
	   "Wipeout and its masked entities."
  )
  (foreach n (list under above) (command n))
  (command "")
  (setvar "CMDECHO" cecho)
  (princ)
)					;defun

(defun recta ()
  (prompt "\nSelect corner points for rectangle: ")
  (command "_.rectangle")
  (while (eq 1 (logand 1 (getvar "cmdactive")))
    (command pause)
  )					;while
)

(defun polyg ()
  (prompt "\nSelect points on a polygon: ")
  (command "_.pline")
  (while (eq 1 (logand 1 (getvar "cmdactive")))
    (command pause)
  )					;while
)

(defun circu (/ ptc ptr)
  (prompt "\nWipeout does not accept arcs, ")
  (prompt "\n20 equal-sided polygon created instead.")
  (prompt "\nSelect center of circle: ")
  (setq ptc (getpoint))
  (prompt "\nSelect radius of cirle: ")
  (setq ptr (getpoint ptc))
  (command "_.polygon" "20" ptc "c" ptr)
  (while (eq 1 (logand 1 (getvar "cmdactive")))
    (command pause)
  )					;while
)






;;;Toggles wipeout frames on and off
;;;Joshua Tapp, August 2000
;;;Frank Oquendo gave me the actual toggle
(defun c:wof (/ lst)
  (princ "\nWipeout Macro: Toggle Wipeout Frames ON/OFF")
  (if (setq lst (dictsearch (namedobjdict) "ACAD_WIPEOUT_VARS"))
    (entmod (subst (cons 70 (boole 6 (cdr (assoc 70 lst)) 1))
		   (assoc 70 lst)
		   lst
	    )
    )
  )					;if
  (if (= (cdr (assoc 70 lst)) 1)
    (prompt "\nWipeout frames are now OFF.")
    (prompt "\nWipeout frames are now ON.")
  )					;if
  (command "_.regenall")
  (princ)
)					;wof

(prompt
  "\nWOF macro loaded.  Type WOF to toggle wipeout frames on or off."
)
(prompt
  "\nMASK.LSP V1.5 by Joshua Tapp, December 2000\nType MASK to run."
)
(prompt
  "\nCommand echo is turned on so all normal AutoCAD commands used are shown."
)
(princ)