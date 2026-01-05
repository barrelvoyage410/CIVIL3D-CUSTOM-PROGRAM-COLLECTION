;;; revised 03-20-2007 to check serial number             ;;;ME
;;; revised 08/10/2007 4:56 PM to trim properly under C3D2008  ME;;;
;;; revised 11/12/2007 11:02 AM to reset RAD variable to nil between text inserts  ME;;;

(defun C:ZPR ()
;  (if (cgl_check_serial)
;    (progn
      (defun *ERROR* (MSG)
	(RESET)
	(prompt (strcat "\N\N" MSG "     "))
      )					; DEFUN
      (setq OSM (getvar "OSMODE"))
      (setq ORTHO (getvar "ORTHOMODE"))
      (setvar "ORTHOMODE" 0)
      (setvar "BLIPMODE" 0)
      (setvar "CMDECHO" 0)
      (setq PICKBX (getvar "PICKBOX"))
      (setq APERTRE (getvar "APERTURE"))
      (setvar "OSMODE" 512)		; SETS OSNAP TO "NEAREST"
      (while
	(setq P1 (getpoint "\nSelect contour: "))
	 (setvar "OSMODE" 0)		; RESETS OSNAP TO NONE
	 (setq NUM (rtos (caddr P1) 2 0))
	 (setq TXT NUM)
	 (if (= NUM "0")
	   (prompt (setq TXT (getstring "Contour ELEVATION = ? ")))
	 )				; END IF
	 (setq RAD (* 2.0 (getvar "TEXTSIZE")))
	 (command "._CIRCLE" P1 RAD)
	 (setvar "PICKBOX" 5); pickbox 0 doesn't work in C3D2008
	 (setvar "APERTURE" 1)
	 (setq P2 (polar P1 50 0.00005)); original value of 0.5 was too large for R12+ video display
	 (command "._TRIM" "L" "" P2 "")
	 (command "._ERASE" "P" "")
	 (setvar "OSMODE" OSM)		; RESETS OSNAP TO NONE
	 (princ "\nText rotation angle: ")
	 (command "._TEXT" "J" "M" P1 PAUSE TXT )
	 (setvar "PICKBOX" PICKBX)
	 (setvar "APERTURE" APERTRE)
	 (setq TXTEL (caddr P1))
	 (setq DIFF (- 0 TXTEL))
	 (setq NEWELV (strcat "@0,0," (rtos DIFF 2 4)))
	 (command ".MOVE" "L" "" "0,0,0" NEWELV)
	 (setvar "OSMODE" 512)
        (setq RAD nil)
      )					; END WHILE
      (princ)
      (setvar "PICKBOX" PICKBX)
      (setvar "APERTURE" APERTRE)
      (setvar "ORTHOMODE" ORTHO)
      (setvar "OSMODE" OSM)
      (setq RAD nil)
      (princ)
;    )					;end checker progn
;    (alert
;      "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd."
;    )
;  )					;end if
;  (princ)

)					;END ZPR