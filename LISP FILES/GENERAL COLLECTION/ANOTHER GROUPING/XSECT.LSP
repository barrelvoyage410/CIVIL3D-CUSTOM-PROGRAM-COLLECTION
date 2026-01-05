;;;-----------------------------------------------------------------------;;;
;;;                     Created by Mark Evinger                           ;;;
;;;                 Compuserve Id Number 73162,1676                       ;;;
;;;-----------------------------------------------------------------------;;;
;;; revised 15:50:40 Wednesday, 09-14-1994 to prompt & show scales        ;;;
;;; revised 3:23 PM 10/27/2005 to save/restore OSMODE                     ;;;v1.04
;;; revised 03/20/2007 to check for cgl serial number                     ;;;v1.06
(defun c:XSECT ()
  (if (cgl_check_serial)
    (progn
  (setq VERSION "1.06")
  (setq CGL_OSNAP (getvar "OSMODE"))
  (princ
    (strcat
      "\r XSECT version "
      VERSION
      " -- copyright © Cowhey Gudmundson Leder, Ltd. 1994-2007. "
    )
  )
;;; This bit is designed more for a dual monitor system, as the version will
;;; scroll off a three line display....

  ;;---------------------------------------------------------------------------
  ;; save specified mode settings
  ;;---------------------------------------------------------------------------
  (defun MODESAVE (A_A)
    (setq MLST '())
    (repeat (length A_A)
      (setq
	MLST (append MLST (list (list (car A_A) (getvar (car A_A)))))
      ) ;_ end setq
      (setq A_A (cdr A_A))
    ) ;_ end repeat
  ) ;_ end defun
  ;;---------------------------------------------------------------------------
  ;; restore saved mode settings
  ;;---------------------------------------------------------------------------
  (defun MODER ()
    (repeat (length MLST)
      (setvar (caar MLST) (cadar MLST))
      (setq MLST (cdr MLST))
    ) ;_ end repeat
  ) ;_ end defun
  ;;---------------------------------------------------------------------------
  ;;error handler
  ;;---------------------------------------------------------------------------
  (defun XSECT_err (s)			;If an error (such as CTRL-C) occurs
					;while this command is active...
    (if	(/= s "Function cancelled")
      (princ (strcat "\nError: " s))
    )
    (setq *error* old_err)		;Restore old *error* handler
    (MODER)				;return modes to old values
    (princ)
  )


  (setq	old_err	*error*			;use special error handling function
	*error*	XSECT_err
  )

  (print)
  (MODESAVE '("BLIPMODE"   "CMDECHO"	"COORDS"     "DIMZIN"
	      "GRIPS"	   "OSMODE"	"AUNITS"     "CELTYPE"
	     )
  )					;save modes
  (setvar "BLIPMODE" 0)			;turn off blips
  (setvar "CMDECHO" 0)			;turn off command echo
  (setvar "COORDS" 2)			;sets coords to show distance & angle
  (setvar "DIMZIN" 0)			;turn off dimzin (controls leading/trailing 0's)
  (setvar "GRIPS" 0)			;turn off grips
  (setvar "OSMODE" 0)			;turn off osnaps
  (setvar "AUNITS" 0)			;set angular units to degrees only
  (setvar "CELTYPE" "BYLAYER")		; set CurrentEntity Linetype to bylayer
  (setvar "OSMODE" 0)
  (princ)
  (setq	REF (getpoint
	      "\n Select known reference point on profile grid: "
	    )
  )
  (setq REFSTA (getreal "\n Enter STATION of grid reference point: "))
  (setq REFEL (getreal "\n Enter ELEVATION of grid reference point: "))

  ;;test for USERR2 not equal to LTSCALE, get user input if not equal
  (setq SCAL (getvar "USERR2"))
  (setq TEMP_SCAL (getvar "LTSCALE"))
  (cond
    ((/= SCAL TEMP_SCAL)
     (setq SCAL	(getreal (strcat "\nEnter HORIZONTAL scale: >>"
				 (rtos (getvar "ltscale") 2 1)
				 "<< "
			 )
		)
     )
     (if (null SCAL)
       (setq SCAL (getvar "LTSCALE"))
     )					;end if
     (setvar "USERR2" SCAL)
    )					;end cond HSCAL /= LTSCAL
  )					;end cond

  (if (or (= (getvar "USERR2") nil)
	  (or (= (getvar "USERR2") 0) (= (getvar "USERR2") 1))
      )
    (progn
      (initget (+ 1 2 4))
      (setvar
	"USERR2"
	(getreal
	  "\n Drawing scale currently 0 or 1. Enter intended plot scale: "
	)
      )
    )					;end progn
  )

  (setq HSCAL (getvar "USERR2"))
  (setq TEMP_SCL (getvar "ltscale"))
  (setq TEMP_ANS (/ HSCAL 10))
  (setq	VSCAL (getreal (strcat "\n Enter VERTICAL SCALE: >>"
			       (rtos TEMP_ANS 2 1)
			       "<< "
		       )
	      )
  )
  (if (null VSCAL)
    (setq VSCAL TEMP_ANS)
  )
  (setq VFAC (/ HSCAL VSCAL))
  ;;(setq VSCAL (getreal "\n Enter vertical scale: "))
  ;;(setq VFAC (/ HSCAL VSCAL))
  (setq STA (getreal "\n Enter STATION of first x-section point: "))
  (setq RIM (getreal "\n Enter ELEV of first x-section point: "))
  (setq HDIFF (- STA REFSTA))
  (setq VDIFF (- RIM REFEL))
  (if (> HDIFF 0.0)
    (setq HANG 0.0)
    (setq HANG (* 1.0 pi))
  )
  (if (> VDIFF 0.0)
    (setq VANG (* 0.5 pi))
    (setq VANG (* 1.5 pi))
  )
  (setq NEWSTA (polar REF HANG (abs HDIFF)))
  (setq FIRST_POINT (polar NEWSTA VANG (abs (* VDIFF VFAC))))
					;(command "line" NEWSTA MIDRIM "")
  (while
    (setq STA (getreal "\n Enter STATION of next x-section point: "))
     (setq RIM (getreal "\n Enter ELEV of next x-section point: "))
     (setq HDIFF (- STA REFSTA))
     (setq VDIFF (- RIM REFEL))
     (if (> HDIFF 0.0)
       (setq HANG 0.0)
       (setq HANG (* 1.0 pi))
     )
     (if (> VDIFF 0.0)
       (setq VANG (* 0.5 pi))
       (setq VANG (* 1.5 pi))
     )
     (setq NEWSTA (polar REF HANG (abs HDIFF)))
     (setq NEXT_POINT (polar NEWSTA VANG (abs (* VDIFF VFAC))))
     (command "line" FIRST_POINT NEXT_POINT "")
     (setq FIRST_POINT NEXT_POINT)
  )					; end while
  (MODER)				; return modes to previous values
  (print)
  (print)
  );end checker progn
    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
    );end if
    (princ)  
)					; end xsect