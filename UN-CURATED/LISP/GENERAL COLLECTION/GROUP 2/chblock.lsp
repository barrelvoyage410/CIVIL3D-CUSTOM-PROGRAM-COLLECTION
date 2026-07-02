;;;   CHBlock.lsp
;;;   Copyright (C) 1990 by Autodesk, Inc.
;;;  
;;;   Permission to use, copy, modify, and distribute this software and its
;;;   documentation for any purpose and without fee is hereby granted.  
;;;
;;;   THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY. 
;;;   ALL IMPLIED WARRANTIES OF FITNESS FOR ANY PARTICULAR PURPOSE AND OF 
;;;   MERCHANTABILITY ARE HEREBY DISCLAIMED.
;;; 
;;;   by Jan S. Yoder
;;;   01 February 1990
;;;
;;;--------------------------------------------------------------------------;
;;; DESCRIPTION
;;;   Change the X, Y, or Z block scales independently.
;;;   Also changes to the insertion point and rotation angle are allowed.
;;;   Either of these operations may be done by dragging an instance of
;;;   the block on-screen or by specifying real values.
;;;
;;;   Multiple entitites may be selected for manipulation;  they will be
;;;   accessible one at a time in the order of selection.
;;;
;;;      BLOCK SCALES:  X - 1.0  Y - 1.0  Z - 1.0
;;;      Change scale. All/X/Y/Z/<Exit>: 
;;;--------------------------------------------------------------------------;

(defun cs (/ cb_ver temp temp1 ename ent x y z) ; change block scales

  (setq cb_ver "1.00")                ; Reset this local if you make a change.

  ;;
  ;; Internal error handler defined locally
  ;;
  (defun chb_er (s)                   ; If an error (such as CTRL-C) occurs
                                      ; while this command is active...
    (if (/= s "Function cancelled")
      (if (= s "quit / exit abort")
        (princ)
        (princ (strcat "\nError: " s))
      )
    )
    (command "undo" "end")
    (if chb_oe                        ; If an old error routine exists
      (setq *error* chb_oe)           ; then, reset it 
    )
    (setvar "cmdecho" chb_oc)         ; Reset command echoing on error
    (princ)
  )
  
  (if *error*                         ; Set our new error handler
    (setq chb_oe *error* *error* chb_er) 
    (setq *error* chb_er) 
  )

  (setq chb_oc (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  

  (princ (strcat "\nChange block,  Version " cb_ver
                  ", (c) 1990 by Autodesk, Inc. "))
  (setq sset (ssget)
        ssl  (if sset (sslength sset) 0)
  )
  (while (and sset (> (setq ssl (1- ssl)) -1))
    (setq ent (entget (setq ename (ssname sset ssl))))
    (if (= (cdr(assoc 0 ent)) "INSERT")
      (setq x   (cdr(assoc 41 ent))
            y   (cdr(assoc 42 ent))
            z   (cdr(assoc 43 ent))
            temp nil
      )
      (setq temp "Exit") ; skip this entity
    )
    (if (null temp)
      (if temp1
        (princ "\nNext object...")
        (progn
          (setq temp1 T)
          (princ "\nFirst object...")
        )
      )
    )  
    (while (and (= (cdr(assoc 0 ent)) "INSERT") (not (= temp "Exit")))
      (command "undo" "group")
      (redraw ename 3)
      
      (princ "\nInsertion point/Rotation/Scale/<Exit>: ")
      (initget "Insertion Rotation Scale Exit")
      (setq temp (getkword))
      (cond
        ((= temp "Insertion")
          (command "move" ename "" (cdr(assoc 10 ent)) pause)
          (setq ent (entget ename))
        )
        ((= temp "Rotation")
          (command "rotate" ename "" (cdr(assoc 10 ent)))
          (princ (strcat
            "\nNew rotation angle <" (angtos (cdr(assoc 50 ent))) ">: "))
          (command pause)
          (setq ent (entget ename))
        )
        ((= temp "Scale")
          (while (/= temp "Exit")
            (princ (strcat "\nBLOCK SCALES: \tX - " (rtos x)
                                           "\tY - " (rtos y)
                                           "\tZ - " (rtos z)))
            (princ "\nChange scale. All/X/Y/Z/<Exit>: ")
            (initget "All X Y Z Exit")
            (setq temp (getkword))
            (cond
              ((= temp "X")
                (setq x (getdist (strcat "\nNew X scale <" (rtos x) ">: ")))
                (setq ent (subst (cons 41 x) (assoc 41 ent) ent))
              )
              ((= temp "Y")
                (setq y (getdist (strcat "\nNew Y scale <" (rtos y) ">: ")))
                (setq ent (subst (cons 42 y) (assoc 42 ent) ent))
              )
              ((= temp "Z")
                (setq z (getdist (strcat "\nNew Z scale <" (rtos z) ">: ")))
                (setq ent (subst (cons 43 z) (assoc 43 ent) ent))
              )
              ((= temp "All")
                (initget "X Y Z Exit")
                (setq scale (getdist (strcat 
                  "\nNew global scale X/Y/Z/<" (rtos x) ">: ")))
                (cond
                  ((= scale "Y") (setq scale y))
                  ((= scale "Z") (setq scale z))
                  ((= (type scale) 'REAL) (princ))
                  (T             (setq scale x))
                )
                (setq x scale
                      y scale
                      z scale
                )
                (setq ent (subst (cons 41 x) (assoc 41 ent) ent)
                      ent (subst (cons 42 y) (assoc 42 ent) ent)
                      ent (subst (cons 43 z) (assoc 43 ent) ent)
                )
              )
              (T
                (setq temp "Exit")
              )
            )
            (entmod ent)
          )
          (setq temp T)
        )
        (T
          (setq temp "Exit")
          (redraw ename 4)
        )
      )
    )
  )
  (command "select" sset "")
  (command "undo" "end")
  (setvar "cmdecho" chb_oc)         ; Reset command echoing on error
  (princ)
)
(defun c:chblock  () (cs) ) ; change block scales
(princ "\n\tC:CHBlock loaded.  Start command with CHBlock.")
(princ)
