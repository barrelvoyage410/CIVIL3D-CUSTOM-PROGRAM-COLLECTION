;****************************************************************************
; -----TW----- DVIEW TWIST & SNAP ANG                           
;****************************************************************************

;;;  LISP  uses two points or an input angle to
;;;    twist the current view using the the dview command.
;;;    the snapangle variable is reset to make the cursor
;;;    appear horizontal.

;;;;The dtr funtion converts degrees to radians
;;;;The rtd funtion converts radians to degrees

(defun RTD (/ANG) (/ (* ANG 180.0) pi))

(defun DTR (/ANG) (* pi (/ ANG 180.0)))


(defun C:TW (/ P1 P2 ANG)
;;;  store the cmdecho, osnapcoord, osmode and orhtomode system varibles
  (setq CE-SAV (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (setq OSCOORD-SAV (getvar "osnapcoord"))
  (setvar "osnapcoord" 1)
  (setq OSMODE-SAV (getvar "osmode"))
  (setvar "osmode" 512)
  (setq ORTHOMODE-SAV (getvar "orthomode"))
  (setvar "orthomode" 0)
  (princ
    "\nTwist view and cursor angle by picking two point or entering an angle
"
  )					; _ end ofPRINC
  (if (setq P2 (getpoint
		 "\nFirst point(left) <Press Enter to input an angle>: "
	       )			; _ end of GETPOINT
      ) ;_ end of SETQ
    (progn (initget 1)
	   (setq P1  (getpoint "\nSecond point(right): " P2)
		 ANG (angle P2 P1)
	   ) ;_ end of SETQ
	   (command "DVIEW" "L" "" "TW" (- 360 (RTD ANG)) "")
	   (setvar "SNAPANG" ANG)
    ) ;_  end of PROGN
    (progn (setq ANG (getangle "\nAngle: "))
	   (command "DVIEW" "L" "" "TW" (- 360 (RTD ANG)) "")
	   (setvar "SNAPANG" ANG)
    ) ;_ end of PROGN
  ) ;_  end of IF
  (setvar "cmdecho" CE-SAV)
  (setvar "osnapcoord" OSCOORD-SAV)
  (setvar "OSMODE" OSMODE-SAV)
  (setvar "orthomode" ORTHOMODE-SAV)
;;; print the rotation angle to the screen:  converts string to local units
  and
  precision
  (princ
    (strcat "\nTwist angle: "
	    (angtos ANG (getvar "aunits") (getvar "auprec"))
    ) ;_ end of strcat
  ) ;_ end of princ
  (princ)
) ;_  end of DEFUN

;****************************************************************************
; -----SNAG----- SET SNAP ANG TO DVIEW TWIST
;****************************************************************************

(defun c:snag (/ VANG SANG)
  (setq VANG (getvar "viewtwist"))
  (setq SANG (- (* 2 pi) VANG))
  (setvar "snapang" SANG)
  !
)
