;;; CADALYST 02/08  www.cadalyst.com/code 
;;; Tip 2269: XFIX.LSP	Xref Transfer	(c) 2008 Ryan Wunderlich 

; Locks all XREFs in a drawing and puts them onto their own named layers.

;;********************************************************
;;  Load up the list box
(defun get_xrefs_ins ()
  (setq XLIST2 '())
  (setq XTEMP (tblnext "block" t))
  (setq XLIST2 (append XLIST2 (list (cons -4  "<OR") ) ) )
  (while XTEMP
    (if (cdr (assoc 1 XTEMP))
	(setq XLIST2 (append XLIST2 (list  (assoc 2 XTEMP))))
    )
    (setq XTEMP (tblnext "block"))
  )
 (setq XLIST2 (append XLIST2 (list (cons -4  "OR>"))))
)

;;********************************************************
;;  Get all references of an xref in the drawing

(defun c:xfix ( / Kount Kount2 entas refs layname)

(setq Kount  0)
(setq Kount2 0)

(get_xrefs_ins)

(if (setq refs (ssget "X" XLIST2))
(progn

	(princ "\nFinding insertion layers of all xrefs - please wait")
	(while (< Kount (sslength refs))
	
	   (setq entas (entget (ssname refs Kount)))

	   (if (/= (cdr (assoc 2 entas)) (cdr (assoc 8 entas)))
	        (progn	
		(setq layname (cdr (assoc 2 entas)))
		(setq entas (subst (cons 8 (cdr (assoc 2 entas))) (assoc 8 entas) entas))
		(entmod entas)
		(command "-layer" "lock" layname "")
		(setq Kount2 (1+ Kount2))
		);progn
	   );IF

	   (setq Kount (1+ Kount))

	); while

); progn
); if
(if (> Kount 0)
  (princ (strcat "\nFixed the insertion layers of " (itoa Kount2) " out of " (itoa kount) " total xrefs."))
);if Kount >0
(princ)
); xref-fix

