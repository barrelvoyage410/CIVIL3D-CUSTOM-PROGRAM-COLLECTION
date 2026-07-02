(defun c:mlfl (/ ALLPTS	ENAME ENFO ENTPT ENTX ENTY ENTZ	INDEX QUANTITY
	       SSML)
  (setq ssml (ssget "_X" '((0 . "MULTILEADER"))))
  (setq quantity (sslength ssml))
  (setq index 0)
  (while (< index quantity)
    (setq ename (ssname ssml index))
    (setq enfo (entget ename))
    (setq allpts (sublist 10 enfo))
    (foreach n allpts
      (setq entz (caddr n))
      (if (/= entz 0.0)
	(progn
	  (setq entx (car n))
	  (setq enty (cadr n))
	  (setq entz 0.0)
	  (setq entpt (list entx enty entz))
	  (setq enfo (subst (cons '10 entpt) (cons '10 n) enfo))
	  (entmod enfo)
	)
      )
    )
    (setq index (1+ index))
  )
)

(defun sublist (key alist)
  (apply 'append
	 (mapcar
	   '(lambda (item)
	      (if (eq (car item) key)
		(list (cdr item))
	      )
	    )
	   alist
	 )
  )
)
