;|lisp for C3D2007 to report unused Point #'s.
  No error handling included, output could use some work.
  by Jeff_M Oct. 2006
  version 2 - added check for no points, all consecutive points
|;
(defun c:freenums (/	     acadobj   aecdoc	 aecobj	   freelist
		   freestring	       lst	 pnts	   ptlist
		   seed count
		  )
  ;;following function courtesy Michael Puckett
  (defun IntsToRangedString (ints / sorted result group)
    ;;  first sort em, losing any duplicates
    (setq
      sorted (vl-sort ints '<)
      result (list (list (last sorted)))
    )
    ;;  now roll 'em ...
    (foreach x (cdr (reverse sorted))
      (setq result
	     (if (eq (1+ x) (car (setq group (car result))))
	       (cons
		 (cons x (list (last group)))
		 (cdr result)
	       )
	       (cons
		 (list x)
		 result
	       )
	     )
      )
    )
    (setq result
	   (mapcar
	     '(lambda (lst / a b)
		(cond
		  ((eq 1 (length lst))
		   (itoa (car lst))
		  )
		  ((eq (1+ (setq a (car lst))) (setq b (cadr lst)))
		   (strcat (itoa a) "," (itoa b))
		  )
		  ((strcat (itoa a) "-" (itoa b)))
		)
	      )
	     result
	   )
    )
    (apply 'strcat
	   (append
	     (list (car result))
	     (mapcar
	       '(lambda (x) (strcat "," x))
	       (cdr result)
	     )
	   )
    )
  )
  ;;end function
  (if (and (setq acadobj (vlax-get-acad-object))
	   (setq aecobj	(vla-getinterfaceobject
			  acadobj
			  "AeccXUiLand.AeccApplication.5.0"
			  ;;change to 3.0 for use in 2006
			  ;; 4.0 for 2007
			  ;; 5.0 for 2008
			)
	   )
	   (setq aecDoc (vla-get-activedocument aecobj))
	   (setq pnts (vlax-get aecDoc 'points))
	   (setq count (vlax-get pnts 'count))
      )
    (progn
      (setq seed 0)
      (vl-catch-all-apply
	'(lambda ()
	   (vlax-for pnt pnts
	     (setq ptlist (cons (vlax-get pnt 'number) ptlist))
	   )
	   (mapcar 'vlax-release-object
		   (list pnts aecdoc aecobj acadobj)
	   )
	   (setq ptlist (vl-sort ptlist '<))
	   (while (<= (setq seed (1+ seed)) (last ptlist))
	     (if (not (member seed ptlist))
	       (setq freelist (cons seed freelist))
	     )
	   )
	 )
      )
      (if (not freelist)
	(setq freestring
	       (if (= 0 count)
		 "1+"
		 (strcat (itoa (1+ (last ptlist))) "+")
	       )
	)
	(setq freestring
	       (strcat (intstorangedstring freelist)
		       ","
		       (itoa (1+ (last ptlist)))
		       "+"
	       )
	)
      )
      (princ (strcat "Available point #'s: " freestring))
    )
  )
  (princ)
)

