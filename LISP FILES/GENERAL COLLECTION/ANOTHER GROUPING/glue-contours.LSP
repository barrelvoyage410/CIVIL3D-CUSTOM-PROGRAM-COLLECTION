;| Routine to glue 2 LDT Contours together. Contours must be of the
   same elevation. NOTE - Contours may be glued that are some distance
   apart, but the tie between the 2 will be a straight segment!
   by Jeff Mishler, April 2003
|;

(defun c:cglue ()
  (glue_contours)
  (princ)
  )

(defun glue_contours (/ CONT1 CONT2 COORD1 COORD2 LABEL2 LIST1 LIST2 OBJ_CONT1 OBJ_CONT2 X Y)
  (command "undo" "begin")
  (setq cont1 (car (entsel "\nSelect first contour: ")))
  (redraw cont1 3)
  (setq	cont2 (car (entsel "\nSelect contour to attach: ")))
  (redraw cont2 3)
  (setq	obj_cont1 (vlax-ename->vla-object cont1)
	obj_cont2 (vlax-ename->vla-object cont2)
	)
  (cond
    ((or (/= (cdr (assoc 0 (entget cont1))) "AECC_CONTOUR")
	 (/= (cdr (assoc 0 (entget cont2))) "AECC_CONTOUR")
	 )
     (progn
       (alert "You must select AECC_Contour entities!\n\tTry again.....")
       (redraw cont1 4)(redraw cont2 4)
       (exit)
       )
     )
    ((/= (vla-get-elevation obj_cont1)(vla-get-elevation obj_cont2))
     (progn
       (alert "Contours must be same elevation to be able to glue!\n\tTry again....")
       (redraw cont1 4)(redraw cont2 4)
       (exit)
       )
     )
    (t (prompt "\nGluing contours......"))
    );cond
  (redraw cont1 4)(redraw cont2 4)
  (setq coord1 (vla-get-coordinates obj_cont1)
	coord2 (vla-get-coordinates obj_cont2)
	list1 (vlax-safearray->list (vlax-variant-value coord1))
	list2 (vlax-safearray->list (vlax-variant-value coord2))
	list1 (append list1 list2)
	label2 (vlax-safearray->list (vlax-variant-value (vlax-get-property obj_cont2 'labelpoints)))
	)
  (setq coord1 (vlax-safearray-fill (vlax-make-safearray vlax-vbdouble (cons 0 (1- (length list1)))) list1))
  (setq coord1 (vlax-make-variant coord1))
  (vla-put-coordinates obj_cont1 coord1)
  (while label2
    (setq x (car label2)
	  y (cadr label2)
	  label2 (cdr (cdr (cdr label2)))
	  )
    (vlax-invoke-method obj_cont1 'addlabelat x y)
    )
  (vla-delete obj_cont2)
  (command "undo" "end")
  (princ)
  )

(princ "\nGlue contours loaded......type 'cglue' to use....")
