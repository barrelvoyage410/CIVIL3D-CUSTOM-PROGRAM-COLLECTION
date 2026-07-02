;|CADALYST Tip1666:DEC2000  DST.LSP  Quick Distance  (c)2000, Robert M. Jacobson

DST.LSP WRITTEN BY ROBERT M. JACOBSON
Modified by Tom Beauford: droped redundent prompts & expanded listed info.

Dst.lsp will prompt the user to pick the first point, then pick the next point,
then display the distance picked by with a colored line, every distance measured
shows a different color. the bearing & horizontal distance will be displayed in
the left corner of the status line and the bearing, horizontal distance, difference
in elevation and slope will be displayed on the command line. |;

(defun C:DST ( / 2DIST fact str1 tw CNTR PT1 PT2 ang DST PDST Pang)

  (defun 2DIST (PT) 
	(list (car pt)(cadr pt))
  ) ;defun
  
  (setq fact nil)
  (if(and(= 1 (getvar "cvport"))(trans '(1 0 0) 2 3 0))
	(progn
	  (setq fact (car (trans '(1 0 0) 2 3 0)))
	  (princ "\nPS:MS == 1:")
	  (princ(/ 1 fact))
	  (command "mspace")
	  (setq tw (- (* 2 pi)(cdr(assoc 51(entget(acet-currentviewport-ename))))))
	  (command "pspace")
	)
  )
  (setq CNTR 0 ;INITIALIZE COUNTER
		PT1 (getpoint "\nPick First Point") ;PROMPT FOR FIRST POINT
		PT2 PT1
  )
  (while PT2 ;IF YES OR ENTER
	(setq PT2 (getpoint "\nPick Next Point" PT1)) ;PROMPT FOR NEXT POINT
	(if PT2
	  (progn
		(if fact
		  (progn
			(setq DST (/ (distance (2DIST PT1)(2DIST PT2))fact) ;CONVERT TO STRING
				  PDST (distance (2DIST PT1)(2DIST PT2)) ;CONVERT TO STRING
				  CNTR (1+ CNTR) ;ADD TO COUNTER FOR COLOR CHANGE
				  Pang (angtos (angle pt1 pt2)4 4)
				  ang (angtos (+(angle pt1 pt2)tw)4 4)
				  deltaz  (/ (- (car(cddr pt2)) (car(cddr pt1)))fact)
				  slope   (/ deltaz DST)
			)
			(if(eq Pang ang)
			  (setq DST(strcat "MS Bearing= "ang ", Dist= " (rtos DST 2 2) "', PS Dist= " (rtos PDST 2 2) "\""))
			  (setq DST(strcat "MS Bearing= "ang ", Dist= " (rtos DST 2 2) "', PS Bearing= "Pang ", Dist= " (rtos PDST 2 2) "\""))
			);if
		  );progn
		  (setq DST (distance (2DIST PT1)(2DIST PT2))
				CNTR (1+ CNTR)
				ang (angtos (angle pt1 pt2)4 4)
				deltaz  (- (car(cddr pt2)) (car(cddr pt1)))
				slope   (/ deltaz DST)
				DST	 (strcat "Bearing= "ang ", Dist= " (rtos DST 2 2) "'")
		  )
		);if fact
		(prompt (strcat "\n" DST)) ;Print the distance to command line
		(if (/= 0 deltaz)(prompt (strcat "\nDelta elev= "(rtos deltaz) "   Slope= "(rtos slope 2 6))))
		(grtext -1 DST) ;Print distance in status line
		(grdraw PT1 PT2 CNTR 2) ;Draw a colored line between points
		(setq PT1 PT2) ;Change start point
	  ) ;end progn
	) ;end if PT2
  ) ;end while PT2
  (grtext -1 "") ;Clear status line
  (princ)
) ;end fun