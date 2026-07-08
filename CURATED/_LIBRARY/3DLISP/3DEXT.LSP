
;#: 40630 S0/General
;    17-Apr-89  21:07:32
;Sb: 3d intersection
;Fm: Paul Stocks 76310,201
;To: Jaime
;
;Jamie
; 
;Here is a stand alone function that requires a 3dface and a line.
;Flatland must be 0  hope you have better luck!!!
(defun C:EXT3D ()
  (setq REV nil)
  (setq FACE (entsel "\Select target face: "))
  (setq VECTOR (entsel "\Select line to extend: "))
  (command "UCS" "E" (car FACE))
  (setq ELIST (entget (car VECTOR)))
;get line start and end
  (setq PT1 (cdr (assoc 10 ELIST))
        PT2 (cdr (assoc 11 ELIST))
  )
;determine 3d length
  (setq DIST3D (distance PT1 PT2))
;translate the points into current UCS
  (setq PT1 (trans PT1 (car VECTOR) 1)
        PT2 (trans PT2 (car VECTOR) 1)
  )
;put Z values of start/end into list for possible reversal
  (setq ZLIST (list (abs (caddr PT1)) (abs (caddr PT2))))
  (if (apply '< ZLIST) (setq ZLIST (reverse ZLIST) REV T))
  (setq HIZ (car ZLIST) LOZ (cadr ZLIST))
;determine target length
  (setq NEWLEN (/ HIZ (/ (- HIZ LOZ) DIST3D)))
;modify appropriate endpoint
  (cond (REV
         (command "UCS" "3" PT2 PT1 "")
         (setq PTDEST (polar '(0 0) 0.0 NEWLEN))
         (entmod (subst (cons 10 (trans PTDEST 1 (car VECTOR)))
                        (assoc 10 ELIST)
                        ELIST
                 )
         )
        )
        (T
         (command "UCS" "3" PT1 PT2 "")
         (setq PTDEST (polar '(0 0) 0.0 NEWLEN))
         (entmod (subst (cons 11 (trans PTDEST 1 (car VECTOR)))
                        (assoc 11 ELIST)
                        ELIST
                 )
         )
        )
  )
;recover starting UCS
  (repeat 2
   (command "UCS" "P")
  )
(princ)
)

