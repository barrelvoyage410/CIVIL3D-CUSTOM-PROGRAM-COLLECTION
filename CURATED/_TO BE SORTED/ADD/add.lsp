;;;This is an online calculator
;;;*****************************************
;;;   Superior Designs - Custom programming available
;;;   Copyright (C) 1996-1997
;;;   Written by Craig Carr -  1996
;;;   http://www.inil.com/users/ccarr/sdi/acad.htm
;;;   e-mail ccarr@inil.com
;;;*****************************************
;;;Online Calculator.- Bonus winner in 11/96 Cadalyst-lisp



(DEFUN C:ADD()
(SETVAR "CMDECHO" 0)
;(setq ADDver "1.4")
(setq ADDerr *error*) 
(defun *error* (msg)(setq *error* ADDerr)(setvar "cmdecho" 1) (princ) )

;;****************
(DEFUN STRNG2(TXT1 TXT2)    ;;;FORMAT- (STRNG "IS" "THIS") RETURNS # OF OCCURENCES
                        (SETQ II 0)(SETQ J 1)
                        (WHILE (<= J (STRLEN TXT2))
                               (IF (= (SUBSTR TXT2 J (STRLEN TXT1)) TXT1)
                                   (PROGN (SETQ II(1+ II))(SETQ LPOS (- J 1)  ) )   )    ;;  END IF
                        (SETQ J(1+ J)) ) ; END WHILE
                        (IF (= II 0)(SETQ II nil)(SETQ II II))  ) ; END FUNCTION STRNG
;;;;**************

(SETQ ADIN 1)
(SETQ TOTAL 0.0)
(if (= ADDMEM nil)(SETQ ADDMEM 0))
(WHILE (= ADIN 1)

(PRINC "\n")
(PRINC TOTAL)

(SETQ NUM(strcase(GETSTRING "\nFUNCTION & NUMBER: ")))

     (IF (= (SUBSTR NUM 2 1) "R") (SETQ NUM (STRCAT (SUBSTR NUM 1 1) (RTOS ADDMEM)) ))


(COND
     ( (= (SUBSTR NUM 1 1) "*") (SETQ TOTAL(* TOTAL (ATOF (SUBSTR NUM 2) ))) )
     ( (= (SUBSTR NUM 1 1) "/") (SETQ TOTAL(/ TOTAL (ATOF (SUBSTR NUM 2) ))) )
     ( (STRNG2 "/" NUM)         (SETQ TOTAL(/ (ATOF (SUBSTR NUM 1 LPOS)) (ATOF (SUBSTR NUM (+ LPOS 2))) )) )
     ( (= (SUBSTR NUM 1 1) "C") (SETQ TOTAL 0.0) )
     ( (= (SUBSTR NUM 1 1) "E") (SETQ ADIN nil) )
     ( (= (SUBSTR NUM 1 1) "M") (SETQ TOTAL(* TOTAL 25.4)) )
     ( (= (SUBSTR NUM 1 1) "I") (SETQ TOTAL(/ TOTAL 25.4)) )
     ( (= (SUBSTR NUM 1 1) "S") (SETQ ADDMEM TOTAL) )
     ( (= (SUBSTR NUM 1 1) "R") (SETQ TOTAL ADDMEM) )
     ( (= (SUBSTR NUM 1 1) "^") (setq total(expt total (atof (substr num 2) ))) )
     ( (= (SUBSTR NUM 1 1) "?") (progn (princ "\n BUTTON FUNCTIONS")
                                       (PRINC "\n * - MULTIPLY  / - DIVIDE")
                                       (PRINC "\n ^ - POWER     C - CLEAR")
                                       (PRINC "\n - - SUBTRACT  + - ADD")
                                       (PRINC "\n M - CONVERT TO METRIC")
                                       (PRINC "\n I - CONVERT TO INCHES")
                                       (princ "\n S - STORE")
                                       (PRINC "\n E - EXIT             ")
                                       (PRINC "\n TYPING A '+' IS OPTIONAL")
                                  )); END PROGN END COND

     ( T (SETQ TOTAL(+ TOTAL (ATOF NUM) )) )

); END COND


); END WHILE

(setq *error* ADDerr) (setvar "cmdecho" 1) (princ TOTAL)
)