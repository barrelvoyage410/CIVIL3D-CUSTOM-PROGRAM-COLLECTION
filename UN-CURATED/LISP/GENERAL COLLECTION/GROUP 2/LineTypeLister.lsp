;;; Created by Mark Evinger 04/03/02 10:57:27 PM to document all the linetypes
;;; in a drawing.  This will place 200 foot long lines 40 feet apart in a vertical column,
;;; labelled at the right hand side with a number and the linetype name.  Text
;;; for labels is in the current style, hard-coded at 4 units tall.  Entities are
;;; created on the current layer.  You may have to run a regen after using this
;;; program to get the lines to display properly.

(defun c:LineTypeLister	()
  (setq ltlist ())			;creates empty list
  (setq ltrec (tblnext "LTYPE" t))	;finds first entry in LTYPE table
  (while (/= ltrec nil)			;while entry is not nil
    (setq lt (cdr (assoc 2 ltrec)))	;sets linetype name of entry
    (setq ltlist (cons lt ltlist))	;adds linetype name to list
    (setq ltrec (tblnext "LTYPE"))	;finds next entry in table/nil if end
  )
  (setq SortedList (acad_strlsort ltlist))
					; sort linetypes alphabetically
  (setq StartPoint (getpoint "\nSelect Starting point: "))
  (setq X_up (car StartPoint))
  (setq Y_up (cadr StartPoint))
  (setq LT_Name (length SortedList))
  (setq cgl_n 0)
  (foreach LineType SortedList
    (progn
      (setq CurLtype (nth CGL_n SortedList))
      (setq
	EList1
	 (list '(0 . "LINE")
	       (cons 6 CurLtype)
	       (cons 8 (getvar "CLAYER"))
	       (list 10 X_up Y_up 0.0)
	       (list 11 (+ X_up 200.0) Y_up 0.0)
	       (cons 62
		     (cond ((= (getvar "CECOLOR") "BYLAYER") 256)
			   ((= (getvar "CECOLOR") "BYBLOCK") 0)
			   (t (atoi (getvar "CECOLOR")))
		     )
	       )
	 )
      )
      (setq LiStErTeXt
	     (strcat "(No. " (itoa (+ 1 CGL_n)) ") - " CurLtype)
      )
      (setq EList2
	     (list '(0 . "TEXT")
		   (cons 1 ListerText)
		   (cons 6 (getvar "CELTYPE"))
		   (cons 7 (getvar "TEXTSTYLE"))
		   (cons 8 (getvar "CLAYER"))
		   (list 10 (+ X_up 210.0) Y_up 0.0)
		   (cons 40 4.0)
		   (cons 62
			 (cond ((= (getvar "CECOLOR") "BYLAYER") 256)
			       ((= (getvar "CECOLOR") "BYBLOCK") 0)
			       (t (atoi (getvar "CECOLOR")))
			 )		;cond
		   )			;cons
	     )				;list
      )					;setq
      (entmake EList1)
      (entmake Elist2)
      (setq Y_up (- Y_up 40.0))
      (setq cgl_n (+ cgl_n 1))
    )					;progn
  )					;foreach
)					;end defun