;;;Written By Michael Puckett.
;;;returns list of all symbol names in symbol-table s
(defun Table (s / d r)
  (while (setq d (tblnext s (null d)))
    (setq r (cons (cdr (assoc 2 d)) r))
  )
)

 ;|
-------------------------
(xdata-has ename apid)
returns T if entity (ename) has data for application (apid)
|;
(defun xdata-has (ename apid / elist)
  (setq elist (entget ename (list apid)))
  (if (assoc -3 elist)
    t
    nil
  )
)

 ;|
xdata-remove
based on Bill Kramer's X_DATA_DEL
only changed formatting, names
|;
(defun xdata-remove (ename apid / apids elist xlist)
  (if (setq apids (table "appid"))
    (progn
      (setq elist (entget ename apids)
	    xlist (assoc -3 elist)
      )
      (if (assoc apid (cdr xlist))
	(progn
	  (setq	xlist (cdr xlist)
		xlist (append
			(list -3)
			(reverse
			  (cdr
			    (member
			      (assoc apid xlist)
			      (reverse xlist)
			    )
			  )
			)
			(cdr (member (assoc apid xlist) xlist))
		      )
		elist (subst xlist (assoc -3 elist) elist)
	  )
	  (entdel ename)
	  (entmake elist)
	)
      )
    )
  )
)

 ;|xdata-attach
Args:
  ename		entity name
  apid		application ID
  data		xdata e.g. '((1000 . "Hi.") (1070 . 1))
  overwrite	if supplied and non-nil, existing data
  		for application apid will be overwritten
RetVal:
|;
(defun xdata-attach (ename apid data overwrite / nlist xlist elist)
  (if (not (member apid (table "appid")))
    (if	(null (regapp apid))
      (exit)
    )
  )
  (if (xdata-has ename apid)
    (if	overwrite
      (setq nlist (xdata-remove ename apid)
	    xlist (list -3 (cons apid data))
	    nlist (append nlist (list xlist))
      )
    )
    (setq elist	(entget ename)
	  xlist	(list -3 (cons apid data))
	  nlist	(append elist (list xlist))
    )
  )
  (if nlist
    (entmod nlist)
    nil
  )
)

(defun c:test (/ ename apid data overwrite)
  (while (null ename)
    (setq ename (car (entsel)))
  )
  (setq	apid	  "CEI-MOT"
	data	  (list (cons 1070 (getint "Stage: ")))
	overwrite t
  )
  (xdata-attach ename apid data overwrite)
)
