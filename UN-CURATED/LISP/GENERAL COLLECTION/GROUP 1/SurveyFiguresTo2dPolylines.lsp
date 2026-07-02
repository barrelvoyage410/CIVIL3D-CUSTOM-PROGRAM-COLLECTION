(defun _ConvertFigureToPolyline	(oFigure      /		   
				 arr2DPoints  arrSpace	   fr
				 i	      lst2dPoints  lstBulge
				 lstVariables objPolyline  ss
				 x	 y	      z
				)


  ;;====================
  ;; Begin Main Routine 
  ;;====================

  (setq	i  -1
	x  0
	fr (_safelist (vlax-invoke-method oFigure 'GetPoints 1))
  )
  (repeat (/ (length fr) 3)
    (setq y (1+ x)
	  z (1+ y)
	  lst2DPoints
	   (append lst2DPoints (list (nth x fr) (nth y fr)))
	  lstBulge
	   (append lstBulge
		   (list
		     (vlax-invoke-method
		       oFigure
		       'GetBulgeAtPoint
		       (vlax-3d-point
			 (list (nth x fr) (nth y fr) (nth z fr))
		       )
		     )
		   )
	   )
	  x (+ x 3)
    )
  )
  (setq	arrSpace
	 (vlax-make-safearray
	   vlax-vbdouble
	   (cons 0 (- (length lst2DPoints) 1))
	 )
  )
  (setq arr2DPoints (vlax-safearray-fill arrSpace lst2DPoints))
  (vlax-make-variant arr2DPoints)
  (_SetSysVar "clayer" (vla-get-layer oFigure))
  ;(if (not (tblsearch "LAYER" strBreaklineLayer))
  ;  (_AddLayer strBreaklineLayer)
  ;)
  ;(vla-put-layer oFigure strBreaklineLayer)
  (setq	objPolyline
	 (vlax-invoke-method
	   (_Space)
	   'AddLightWeightPolyline
	   arr2DPoints
	 )
  )
  (foreach n lstBulge
    (vla-setbulge objPolyline (setq i (1+ i)) n)
  )
)

(defun c:CFP (/		   _AddLayer	_Space
		      _SetSysVar   *Error*	strBreakline
		      _safelist	   _variantvalue
		      ss	   oFigure
		     )
  (vla-startundomark
    (vla-get-activedocument (vlax-get-acad-object))
  )
  ;;===================================
  ;; Initiliaze user defined variables 
  ;;===================================
  (setq strBreaklineLayer "breakline")
  ;;===================
  ;; Defun subroutines 
  ;;===================

  ;;_ Returns the active space object
  (defun _Space	(/ *DOC* space)
    (setq *DOC* (vla-get-activedocument (vlax-get-acad-object)))
    (setq space	(if (= 1 (vla-get-activespace *DOC*))
		  (vla-get-modelspace *DOC*) ;_ Model Space
		  (if (= (vla-get-mspace *DOC*) :vlax-true)
		    (vla-get-modelspace *DOC*) ;_ Model Space through Viewport
		    (vla-get-paperspace *DOC*) ;_ Paper Space
		  )
		)
    )
  )

  (defun *error* (strMessage /)
    (if	(or
	  (= strMessage "Function cancelled") ; If user cancelled
	  (= strMessage "quit / exit abort") ; If user aborted
	  (null strMessage)		; End quietly
	)				; End or sequence
      (princ)				; Exit quietly
      (princ (strcat "\nError: " strMessage)) ; Report Error
    )
    (if	lstVariables
      (mapcar '(lambda (x) (setvar (car x) (cdr x))) lstVariables)
    )
    (vla-endundomark
      (vla-get-activedocument (vlax-get-acad-object))
    )
    (princ)
  )

  ;;_ Adjust system variable and save original to list
  (defun _SetSysVar (name value /)
    (if	lstVariables
      (if (null (assoc name lstVariables))
	(setq lstVariables
	       (append lstVariables
		       (list (cons name (getvar name)))
	       )
	)
      )
      (setq lstVariables (list (cons name (getvar name))))
    )
    (setvar name value)
  )

 ;_ Add a new layer to the layers collection
 ;_ Syntax (AddLayer "layername")
 ;_ Function returns T if successful nil if not
  (defun _AddLayer (strLayerName / objLayer)
    (if	(and (= (type strLayerName) 'STR)
	     (not (tblsearch "LAYER" strLayerName))
	)
      (progn
	(setq
	  objLayer (vla-add
		     (vla-get-layers
		       (vla-get-activedocument (vlax-get-acad-object))
		     )
		     strLayerName
		   )
	)
	(vlax-release-object objLayer)
	t
      )
    )
  )

 ;_ Convert Safearray to list
  (defun _safelist (value)
    (if	(= (type value) 'VARIANT)
      (setq value (_variantvalue value))
    )
    (setq value (vl-catch-all-apply 'vlax-safearray->list (list value)))
    (if	(vl-catch-all-error-p value)
      nil
      value
    )
  )
 ;_ Get value of variant
  (defun _variantvalue (value)
    (setq value (vl-catch-all-apply 'vlax-variant-value (list value)))
    (if	(vl-catch-all-error-p value)
      nil
      value
    )
  )

  (princ
    "\rConvert a feature line or survey figure to polyline... "
  )
  (setq myss (ssget '((0 . "AECC_SVFIGURE,AECC_FEATURE_LINE"))) i 0)
  (repeat (sslength myss)
    (setq oFigure (vlax-ename->vla-object (ssname myss i)))
    (_ConvertFigureToPolyline oFigure)
    (setq i (1+ i))
  )  
    
    
  
;;;  (if (setq ss (ssget ":S:E" '((0 . "AECC_SVFIGURE,AECC_FEATURE_LINE"))))
;;;    (progn
;;;      (setq oFigure
;;;	     (vlax-ename->vla-object (ssname ss 0))
;;;      )
;;;      (_ConvertFigureToPolyline oFigure)
;;;    )
;;;  )
  (*error* nil)
)