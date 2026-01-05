(defun stpinit ()
(IF (NOT STA1) (SETQ sta0 (getreal "\nBeginning Station: ")  
                     STA1 (GETPOINT "\nPick Starting Point: ")
                     STA2 (GETPOINT "\nPick Direction: ")
                );SETQ
 );IF
(princ)
)

(DEFUN C:STP ()
(stpinit)
(while (setq pt (getpoint "\nStation Point: "))
  (princ (stp pt))
);while
);defun

(defun stp (pt / off lr)

(strcat "STA. " (stpsta pt) ", " (stpoff pt) );strcat
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; real nnnnn.nn --> NNN+NN.NN  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rtosta (n / fp lp z)
 (stpinit)
 (setq fp (fix (/ n 100)))
 (setq lp (- n (* fp 100)))
 (if (<= (abs lp) 10 ) (setq z "0") (setq z ""))
; (strcat  (rtos fp 2 0) "+" z (rtos (abs lp) 2 2))
  (strcat  (rtos fp 2 0) "+" z (rtos (abs lp) 2 (GETVAR "LUPREC")))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN stpsta (p3 / p4)
 (stpinit)
 (setq p4 (polar p3 (+ (angle STA1 STA2) (/ pi 2.0)) 1000))
 (setq p4 (inters STA1 STA2 p3 p4 nil))

 (if (equal (angle STA1 p4) (angle STA1 STA2) 0.01)
 (setq sta (+ sta0 (distance STA1 p4)))
 (setq sta (- sta0 (distance STA1 p4)))
 );if 
(rtosta sta)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN stpoff (p3 / p4 off)
 (stpinit)
 (setq p4 (polar p3 (+ (angle STA1 STA2) (/ pi 2.0)) 1000))
 (setq p4 (inters STA1 STA2 p3 p4 nil))

 (if (equal (+ (/ pi 2.0) (angle STA1 STA2)) (angle p4 p3) 0.01)
   (setq off (* -1.0 (distance p3 p4)))
   (setq off (distance p3 p4))
 );if

(if (>= off 0) (setq lr "RT.") (setq lr "LT."))
(strcat (rtos (abs off) 2 (GETVAR "LUPREC")) " " lr)
)
