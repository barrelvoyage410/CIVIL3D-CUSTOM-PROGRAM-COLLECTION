;*glue.lsp                   10/3/1990  PMH  
;*Version 11.00 Steel Detailer
;*Version 11.00 Plans and Elevations
;*Version 11.00 Details and Sections
;*Version 11.00 Structural Modeler
;Revised for structural products 10/3/1990 PMH

; Glue - Join two lines, arcs or text items together
;;       Paul F. Des Sureault 10/3/89
;;       revised 9/28/90 J. Solomon for text and error traps
;;       (C)1990 DCA Software, Inc.
;
; This routine joins two lines, two arcs, or two text items together. If 
; arcs are selected, they must share a common center point and radius. For
; lines there is no such restriction. Two lines of different angles can be
; joined. The program loops to allow several pairs of objects to be joined.
;
; When gluing text together, the text value of the second selected text
; item is added on to the first. A space is inserted between text items.
;
; The program could be improved to allow joining of two polylines where
; the segments do not touch, or joining a line or arc to a polyline, also
; where the segments do not touch.
;
(defun C:GLUE (/ ent1 ent2 lpt1 lpt2 lpt3 lpt4 endpt endpt1 midpt midpt1)
  (princ "\nGlue arcs, lines or text together ... ")
  (while (setq ent1 (car (entsel "\nPick first entity: ")))
    (redraw ent1 3)
    (while (not (setq ent2 (car (entsel "\nPick second entity: "))))
          (princ "\nTry again.")
    )
    (setq elist1 (entget ent1) elist2 (entget ent2))
    (cond 
      ;*************** BOTH ARCS **********************************
      ((and ent1 ent2 (not (equal ent1 ent2))
              (= (cdr (assoc 0 elist1)) "ARC")
              (= (cdr (assoc 0 elist2)) "ARC"))
        (setq pt1 (cdr (assoc 10 elist1)))
        (setq arclay (cdr (assoc 8 elist1))); GET LAYER FROM FIRST ARC
        (setq pt3 (cdr (assoc 10 elist2)))
        (if (equal (distance pt1 pt3) 0.0 0.001)(progn; SAME CENTER POINT
          (setq lpt1 (polar pt1 (cdr (assoc 50 elist1))(cdr (assoc 40 elist1))))
          (setq lpt2 (polar pt1 (cdr (assoc 51 elist1))(cdr (assoc 40 elist1))))
          (setq lpt3 (polar pt3 (cdr (assoc 50 elist2))(cdr (assoc 40 elist2))))
          (setq lpt4 (polar pt3 (cdr (assoc 51 elist2))(cdr (assoc 40 elist2))))
          (command "erase" ent1 ent2 ""); ERASE BOTH EXISTING ARCS
          (if (< (distance lpt1 lpt3)(distance lpt1 lpt4))
           (setq endpt lpt4 midpt lpt3 dist (distance lpt1 lpt3))
           (setq endpt lpt3 midpt lpt4 dist (distance lpt1 lpt4))
          )
          (if (< (distance lpt2 lpt3)(distance lpt2 lpt4))
           (setq endpt1 lpt4 midpt1 lpt3 dist1 (distance lpt2 lpt3))
           (setq endpt1 lpt3 midpt1 lpt4 dist1 (distance lpt2 lpt4))
          )
          (if (< dist dist1)           (command "arc" lpt2 midpt endpt)
           (command "arc" lpt1 midpt1 endpt1)
          )
          (command "change" (entlast) "" "p" "la" arclay "")
        )(progn
          (redraw ent1)(princ "\nArcs must have same center point")
      ))
      )
      ;*************** BOTH LINES **********************************
      ((and ent1 ent2 (not (equal ent1 ent2))
              (= (cdr (assoc 0 elist1)) "LINE")
              (= (cdr (assoc 0 elist2)) "LINE"))
        (setq lpt1 (cdr (assoc 10 elist1)))
        (setq lpt3 (cdr (assoc 10 elist2))
              lpt4 (cdr (assoc 11 elist2)))
        (command "erase" ent2 "")
        (setq orth (getvar "orthomode"))
        (setvar "orthomode" 0)
        (if (> (distance lpt1  lpt3) (distance lpt1 lpt4))
           (command "change" ent1 "" lpt3)
           (command "change" ent1 "" lpt4)
        )
        (setvar "orthomode" orth)
      )
      ;
      ;*************** BOTH TEXT **********************************
      ((and ent1 ent2 (not (equal ent1 ent2))
              (= (cdr (assoc 0 elist1)) "TEXT")
              (= (cdr (assoc 0 elist2)) "TEXT"))
        (setq text1 (strcat (cdr (assoc 1 elist1)) " " (cdr (assoc 1 elist2))))
        (setq elist1 (subst (cons 1 text1) (assoc 1 elist1) elist1))
        (entdel ent2)
        (entmod elist1)
      )
      ;
      ;*************** SAME ENTITY *******************************
      ((equal ent1 ent2)
        (redraw ent1)
        (princ (strcat "\nPick a different " (cdr (assoc 0 elist1)) "."))
      )
      ;*************** MISMATCH **********************************
      ((and ent1 ent2)
        (redraw ent1)
        (princ "\nThe 2 entities must be the same type.")
      )
    )
  )
  (princ)
)
(princ)
