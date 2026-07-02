
;MELBARD PROFILE ROUTINE  Modified by CDP, March 1994

(defun C:PSU () ; ******* Profile Set-Up
   (prompt "Start of Profile Setup: ")(terpri)
   (prompt "Pick Point for Datum Elev. and Start Sta. ")(terpri)
   (setq orpnt (getpoint "Use Object Snap to be Exact: "))(terpri)
   (setq spnt (getreal "Enter Starting Station: "))
   (setq de (getreal "Enter Datum Elevation: "))
   (setq vert (getreal "Enter Vertical Ratio: "))
   (SUOP) ; ********* Run Set-UP Out-Put ( SUOP ) Program
)
(defun SUOP (/ x xx) ; ******** Set-Up Out-Put
      (setq x (fix (/ spnt 100)))
      (setq xx (- spnt (* x 100)))
      (if (or (< (fix xx) 10) (> (fix xx) 0))
         (progn
         (prompt "Start STA  ")(princ x)(prompt "+0")(princ (rtos xx 2 2))
         (prompt "    Datum ELEV  ")(princ (rtos de 2 2))
         (prompt "    Vertical Ratio (V / H)  ")(princ vert)(terpri)
         )
      )
      (if (or (> (fix xx) 10) (< (fix xx) 0))
         (progn
            (prompt "Start STA  ")(princ x)(prompt "+")(princ (rtos xx 2 2))
            (prompt "    Datum ELEV  ")(princ (rtos de 2 2))
            (prompt "    Vertical Ratio (V / H)  ")(princ vert)(terpri)
            )  
         )
)

(defun C:PRO (/ fp mfp np mnp fx fy nx ny x xx)
   (if (= spnt nil)
      (progn
      (prompt "*ERROR* Must run Profile Set Up  < PSU >  first: ")(terpri)
      (getstring "Press any key to continue: ")
      (exit)
      )
   )
   (SUOP) : ****** Run Set-Up Out-Put Program
   (prompt "Start of Profile Routine: ") (terpri)
      (setq fp (getpoint "\nEnter First < STATION , ELEVATION >  "))
      (setq fx (+ (- (car fp) spnt) (car orpnt)))
      (setq fy (* (- (cadr fp) de) vert))
      (setq fy (+ (cadr orpnt) fy))
      (setq mfp (list fx fy))
      (setq np (getpoint "\nEnter Next <STATION , ELEVATION >  "))
      (setq nx (+ (- (car np) spnt) (car orpnt)))
      (setq ny (* (- (cadr np) de) vert))
      (setq ny (+ (cadr orpnt) ny))
      (setq mnp (list nx ny))
      (command "line" mfp mnp "")
      (while np
         (setq mfp (list (car mnp) (cadr mnp)))
         (princ "\nLast < STA , ELEV > Entered:  ")(princ (car np))
            (prompt " , ")(princ (cadr np))
         (setq np (getpoint "\nEnter Next <STATION , ELEVATION >  "))
         (setq nx (+ (- (car np) spnt) (car orpnt)))
         (setq ny (* (- (cadr np) de) vert))
         (setq ny (+ (cadr orpnt) ny))
         (setq mnp (list nx ny))
         (command "line" mfp mnp "")
   )
)

(defun C:PID (/ x xx) ; ******** Point ID
   (if (= spnt nil)
      (progn
      (prompt "*ERROR* Must run Profile Set Up  < PSU >  first: ")(terpri)
      (getstring "Press any key to continue: ")
      (exit)
      )
   )
   (setq pp (getpoint "Snap to a Profile Point for STA. and ELEV. ")) (terpri)
   (setq pidsta (+ (- (car pp) (car orpnt)) spnt))
      (setq x (fix (/ pidsta 100)))
      (setq xx (abs (- pidsta (* x 100))))
   (setq pidelev (- (cadr pp) (cadr orpnt)))
   (setq pidelev (+ (/ pidelev vert) de))
   (if (or (< (fix xx) 10) (> (fix xx) 0))
      (progn
      (prompt "STA. =  ")(princ x)(prompt "+0")(princ (rtos xx 2 2))
      (prompt "     ELEV. =  ")(princ (rtos pidelev 2 2)) (terpri)
      )
   )
   (if (or (> (fix xx) 10) (< (fix xx) 0))
      (progn
      (prompt "STA. =  ")(princ x)(prompt "+")(princ (rtos xx 2 2))
      (prompt "     ELEV. =  ")(princ (rtos pidelev 2 2)) (terpri)
      )
   )
)
(defun C:CSU () ; ******** Current Set-Up
   (if (/= spnt nil) (SUOP)
      (progn
         (prompt "*ERROR* Must run Profile Set Up  < PSU >  first: ")
      )
   )
)
;END PROFILE ROUTINE

(prompt "Done\n") (princ)



