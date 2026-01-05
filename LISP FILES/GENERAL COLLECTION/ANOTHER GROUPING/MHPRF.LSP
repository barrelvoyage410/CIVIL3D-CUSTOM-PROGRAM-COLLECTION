;;;-----------------------------------------------------------------------;;;
;;;                   Original lisp provided by Julian Laskey             ;;;
;;;                         Modified by Mark Evinger                      ;;;
;;;-----------------------------------------------------------------------;;;
;;; revised 19:05:24 Thursday, 06-23-1994 to include better error handling;;;
;;; revised 14:56:00 Friday, 06-02-1995 to include AUNITS in MODESAVE     ;;;
;;; revised 07/23/98 5:16:24 PM to R14 and S8 compatibility               ;;;
;;; revised 02/25/1999 11:06:06 AM to fix error function                  ;;;
;;; revised 06/29/2006 12:46 PM to ignore running osnaps                  ;;; v1.24
;;; revised 11/09/2006 3:59 PM to work properly given any REFEL           ;;; v1.26
;;; revised 12/07/2006 5:37 PM to check for CGL Acad Serial number.       ;;; v1.28
;;; revised 03/19/2007 to check for cgl serial number                     ;;; v1.28


;;; This will return T if it is the right serial number 
;;; or nil if it is not 
;(defun CGL_Check_serial ( / check_no) 
;  (setq check_no "000-00000000") 
;  (= check_no (getvar "_pkser")) 
;) 


(defun C:MHPRF ()
  (if (cgl_check_serial)
    (progn
  (graphscr)
  (setq VERSION "1.28"
        REDRAW-COMMAND
         ".REDRAWALL"
  ) ;_ end of setq
  (princ
    (strcat
      "\r MHPRF version "
      VERSION
      " -- copyright © Cowhey Gudmundson Leder, Ltd. 1995-2007. "
    ) ;_ end of strcat
  ) ;_ end of princ
;;; This bit is designed more for a dual monitor system, as the version will
;;; scroll off a three line display....
 ;---------------------------------------------------------------------------
 ; save specified mode settings
 ;---------------------------------------------------------------------------
  (defun MODESAVE (A_A)
    (setq MLST '())
    (repeat (length A_A)
      (setq
        MLST (append MLST (list (list (car A_A) (getvar (car A_A)))))
      ) ;_ end of setq
      (setq A_A (cdr A_A))
    ) ;_ end of repeat
  ) ;_ end of defun
 ;---------------------------------------------------------------------------
 ; restore saved mode settings
 ;---------------------------------------------------------------------------
  (defun MODER ()
    (repeat (length MLST)
      (setvar (caar MLST) (cadar MLST))
      (setq MLST (cdr MLST))
    ) ;_ end of repeat
  ) ;_ end of defun
 ;---------------------------------------------------------------------------
 ;error handler
 ;---------------------------------------------------------------------------
  (defun MHPRF_ERR (S) ;If an error (such as CTRL-C) occurs
    (if (/= S "Function cancelled") ;while this command is active...
      (princ (strcat "\nError: " S))
    ) ;_ end of if
    (setq *ERROR* OLD_ERR) ;Restore old *error* handler
    (MODER) ;return modes to old values
    (princ)
  ) ;_ end of defun
 ;---------------------------------------------------------------------------
  (MODESAVE '("BLIPMODE" "CMDECHO" "COORDS" "DIMZIN" "GRIPS" "OSMODE" "AUNITS"
             )
  ) ;save modes
  (setvar "BLIPMODE" 0) ;turn off blips
  (setvar "CMDECHO" 0) ;turn off command echo
  (setvar "COORDS" 2) ;sets coords to show distance & angle
  (setvar "DIMZIN" 0) ;turn off dimzin (controls leading/trailing 0's)
  (setvar "GRIPS" 0) ;turn off grips
  (setvar "OSMODE" 0) ;turn off osnaps
  (setvar "AUNITS" 0) ;set angular units to degrees only
 ;---------------------------------------------------------------------------
  (defun GET_INFO ()
    (print)
    (setq REF (getpoint
                "\n Select known reference point on profile grid: "
              ) ;_ end of getpoint
    ) ;_ end of setq
    (setq REFSTA (getreal "\n Enter STATION of grid reference point: "))
    (setq
      REFEL (getreal "\n Enter ELEVATION of grid reference point: ")
    ) ;_ end of setq
    (setq HSCAL (getvar "USERR2"))
    (setq TEMP_HSCAL (getvar "LTSCALE"))
    (cond
      ((/= HSCAL TEMP_HSCAL)
       (setq HSCAL (getreal (strcat "\nEnter HORIZONTAL scale: >>"
                                    (rtos (getvar "ltscale") 2 1)
                                    "<< "
                            ) ;_ end of strcat
                   ) ;_ end of getreal
       ) ;_ end of setq
       (if (null HSCAL)
         (setq HSCAL (getvar "LTSCALE"))
       ) ;end if
       (setvar "USERR2" HSCAL)
      ) ;end cond HSCAL /= LTSCAL
    ) ;end cond
    (setq TEMP_SCL (getvar "ltscale"))
    (setq TEMP_ANS (/ TEMP_SCL 10))
    (setq VSCAL (getreal (strcat "\n Enter VERTICAL SCALE: >>"
                                 (rtos TEMP_ANS 2 1)
                                 "<< "
                         ) ;_ end of strcat
                ) ;_ end of getreal
    ) ;_ end of setq
    (if (null VSCAL)
      (setq VSCAL TEMP_ANS)
    ) ;_ end of if
    (setq VFAC (/ HSCAL VSCAL))
  ) ; end get_info

  (setq OLD_ERR *ERROR* ;use special error handling function
        *ERROR* MHPRF_ERR
  ) ;_ end of setq

  (prompt
    "\n Indicate direction of profile (L)-->R  or L<--(R) "
  ) ;_ end of prompt
  (print)
  (initget 1 "L R")
  (setq ANS (getkword "(L)eft or (R)ight: "))
  (if (not (null ANS))
    (setq OPT ANS)
  ) ;_ end of if

  (cond
    ((= OPT "R")
     (prompt "\n You chose option Right to Left  L<--R ")
     (GET_INFO)
     (while
       (setq STA (getreal "\n Enter STATION of manhole to draw: "))
        (setq RIM (getreal "\n Enter RIM ELEV of manhole to draw: "))
        (setq INV (getreal "\n Enter INV ELEV of manhole to draw: "))
        (setq DEPTH (- RIM INV))
        (setq HDIFF (- STA REFSTA))
        (setq VDIFF (- RIM REFEL))

        (setq HANG (* 1.0 pi))
 ;(if (> HDIFF 0.0) (setq HANG 0.0) (setq HANG (* 1.0 pi)))
        (if (> VDIFF 0.0)
          (setq VANG (* 0.5 pi))
          (setq VANG (* 1.5 pi))
        ) ;_ end of if

        (setq NEWSTA (polar REF HANG HDIFF))
        (setq MIDRIM (polar NEWSTA VANG (* (abs VDIFF) VFAC))) ;use abs VDIFF to guarantee the right direction above REFEL
        (setq MIDINV (polar MIDRIM (* 1.5 pi) (* DEPTH VFAC)))
        (setq D1R (polar MIDRIM (* 1.0 pi) 1.50))
        (setq D1I (polar MIDINV (* 1.0 pi) 5.0))
        (setq D2R (polar MIDRIM 0.0 1.50))
        (setq D2I (polar MIDINV 0.0 5.0))
        (setvar "OSMODE" 0) ;kill any running osnaps

        (command "line" D1R D2R D2I D1I D1R "")
     ) ; end while
    ) ; end OPT L<--Rgraham
    ((= OPT "L")
     (prompt "\n You chose option Left to Right  L-->R ")
     (GET_INFO)
     (while
       (setq STA (getreal "\n Enter STATION of manhole to draw: "))
        (setq RIM (getreal "\n Enter RIM ELEV of manhole to draw: "))
        (setq INV (getreal "\n Enter INV ELEV of manhole to draw: "))
        (setq DEPTH (- RIM INV))
        (setq HDIFF (- STA REFSTA))
        (setq VDIFF (- RIM REFEL))

        (if (> HDIFF 0.0)
          (setq HANG 0.0)
          (setq HANG (* 1.0 pi))
        ) ;_ end of if
        (if (> VDIFF 0.0)
          (setq VANG (* 0.5 pi))
          (setq VANG (* 1.5 pi))
        ) ;_ end of if

        (setq NEWSTA (polar REF HANG (abs HDIFF)))
        (setq MIDRIM (polar NEWSTA VANG (* (abs VDIFF) VFAC))) ;use abs VDIFF to guarantee the right direction above REFEL
        (setq MIDINV (polar MIDRIM (* 1.5 pi) (* DEPTH VFAC)))
        (setq D1R (polar MIDRIM (* 1.0 pi) 1.50))
        (setq D1I (polar MIDINV (* 1.0 pi) 5.0))
        (setq D2R (polar MIDRIM 0.0 1.50))
        (setq D2I (polar MIDINV 0.0 5.0))
        (setvar "OSMODE" 0) ;kill any running osnaps

        (command "line" D1R D2R D2I D1I D1R "")
     ) ; end while
    ) ; end OPT L-->R
  ) ; end cond
  (MODER)
  (print)
  (print)
  (princ)
  );end checker progn
    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
    );end if
    (princ)
) ; end mhprf