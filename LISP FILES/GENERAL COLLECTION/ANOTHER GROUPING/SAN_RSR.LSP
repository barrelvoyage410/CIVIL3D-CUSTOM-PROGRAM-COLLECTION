;;;-----------------------------------------------------------------------;;;
;;;                   Original lisp provided by Julian Laskey             ;;;
;;;                         Modified by Mark Evinger                      ;;;
;;;-----------------------------------------------------------------------;;;
;;; revised 19:05:24 Thursday, 06-23-1994 to include better error handling;;; v1.00
;;; revised 09:15:33 Wednesday, 11-16-1994 to draw sanitary risers        ;;; v1.01
;;; revised 15:07:03 Friday, 06-02-1995 to include AUNITS in MODESAVE     ;;; v1.02
;;;-----------------------------------------------------------------------;;;
;;; revised 08/13/98 12:02:52 AM to account for OSMODE concerns           ;;; v1.03
;;;-----------------------------------------------------------------------;;;
;;; REVISED 03-20-2007 to check for serial number                         ;;; v1.04
(defun C:SAN_RSR ()
  (if (cgl_check_serial)
    (progn
  (graphscr)
  (setq VERSION "1.04"
        REDRAW-COMMAND
         ".REDRAWALL"
  ) ;_ end setq
  (princ
    (strcat
      "\r SAN_RSR version "
      VERSION
      " -- copyright © Cowhey Gudmundson Leder, Ltd. 1995-2007. "
    ) ;_ end strcat
  ) ;_ end princ
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
      ) ;_ end setq
      (setq A_A (cdr A_A))
    ) ;_ end repeat
  ) ;_ end defun
 ;---------------------------------------------------------------------------
 ; restore saved mode settings
 ;---------------------------------------------------------------------------
  (defun MODER ()
    (repeat (length MLST)
      (setvar (caar MLST) (cadar MLST))
      (setq MLST (cdr MLST))
    ) ;_ end repeat
  ) ;_ end defun
 ;---------------------------------------------------------------------------
 ;error handler
 ;---------------------------------------------------------------------------
  (defun MHPRF_ERR (S) ;If an error (such as CTRL-C) occurs
    (if (/= S "Function cancelled") ;while this command is active...
      (princ (strcat "\nError: " S))
    ) ;_ end if
    (setq *ERROR* OLD_ERR) ;Restore old *error* handler
    (MODER) ;return modes to old values
    (princ)
  ) ;_ end defun
 ;---------------------------------------------------------------------------
  (setq CGL_OSMODE (getvar "OSMODE"))
  (MODESAVE '("BLIPMODE" "CMDECHO" "COORDS" "DIMZIN" "GRIPS" "OSMODE" "AUNITS")
  ) ;save modes
  (setvar "BLIPMODE" 0) ;turn off blips
  (setvar "CMDECHO" 0) ;turn off command echo
  (setvar "COORDS" 2) ;sets coords to show distance & angle
  (setvar "DIMZIN" 0) ;turn off dimzin (controls leading/trailing 0's)
  (setvar "GRIPS" 0) ;turn off grips
  (setvar "OSMODE" 0) ;turn off osnaps
  (setvar "AUNITS" 0) ;set angular units to degrees only
 ;---------------------------------------------------------------------------
  (setvar "OSMODE" CGL_OSMODE)
  (defun GET_INFO ()
    (print)
    (setq REF (getpoint
                "\n Select known reference point on profile grid: "
              ) ;_ end getpoint
    ) ;_ end setq
    (setvar "OSMODE" 0)
    (setq REFSTA (getreal "\n Enter STATION of grid reference point: "))
    (setq
      REFEL (getreal "\n Enter ELEVATION of grid reference point: ")
    ) ;_ end setq

 ;test for USERR2 not equal to LTSCALE, get user input if not equal
    (setq SCAL (getvar "USERR2"))
    (setq TEMP_SCAL (getvar "LTSCALE"))
    (cond
      ((/= SCAL TEMP_SCAL)
       (setq SCAL (getreal (strcat "\nEnter HORIZONTAL scale: >>"
                                   (rtos (getvar "ltscale") 2 1)
                                   "<< "
                           ) ;_ end strcat
                  ) ;_ end getreal
       ) ;_ end setq
       (if (null SCAL)
         (setq SCAL (getvar "LTSCALE"))
       ) ;end if
       (setvar "USERR2" SCAL)
      ) ;end cond HSCAL /= LTSCAL
    ) ;end cond

    (setq HSCAL (getvar "USERR2"))
    (setq TEMP_SCL (getvar "ltscale"))
    (setq TEMP_ANS (/ TEMP_SCL 10))
    (setq VSCAL (getreal (strcat "\n Enter VERTICAL SCALE: >>"
                                 (rtos TEMP_ANS 2 1)
                                 "<< "
                         ) ;_ end strcat
                ) ;_ end getreal
    ) ;_ end setq
    (if (null VSCAL)
      (setq VSCAL TEMP_ANS)
    ) ;_ end if
    (setq VFAC (/ HSCAL VSCAL))
  ) ; end get_info

  (setq OLD_ERR *ERROR* ;use special error handling function
        *ERROR  MHPRF_ERR
  ) ;_ end setq

  (prompt
    "\n Indicate direction of profile (L)-->R  or L<--(R) "
  ) ;_ end prompt
  (print)
  (initget 1 "L R")
  (setq ANS (getkword "(L)eft or (R)ight: "))
  (if (not (null ANS))
    (setq OPT ANS)
  ) ;_ end if

  (cond
    ((= OPT "R")
     (prompt "\n You chose option Right to Left  L<--R ")
     (GET_INFO)
     (while
       (setq
         STA (getreal "\n Enter STATION of sanitary riser to draw: ")
       ) ;_ end setq
        (setq
          RIM (getreal "\n Enter RIM ELEV of sanitary riser to draw: ")
        ) ;_ end setq
        (setq
          INV (getreal "\n Enter INV ELEV of sanitary riser to draw: ")
        ) ;_ end setq
        (setq DEPTH (- RIM INV))
        (setq HDIFF (- STA REFSTA))
        (setq VDIFF (- RIM REFEL))

        (setq HANG (* 1.0 pi))
 ;(if (> HDIFF 0.0) (setq HANG 0.0) (setq HANG (* 1.0 pi)))
        (if (> VDIFF 0.0)
          (setq VANG (* 0.5 pi))
          (setq VANG (* 1.5 pi))
        ) ;_ end if

        (setq NEWSTA (polar REF HANG HDIFF))
        (setq MIDRIM (polar NEWSTA VANG (* VDIFF VFAC)))
        (setq MIDINV (polar MIDRIM (* 1.5 pi) (* DEPTH VFAC)))
        (setq D1R (polar MIDRIM (* 1.0 pi) 1.0))
        (setq D1I (polar MIDINV (* 1.0 pi) 1.0))
        (setq D2R (polar MIDRIM 0.0 1.0))
        (setq D2I (polar MIDINV 0.0 1.0))
        (setvar "OSMODE" 0)

        (command "line" D1R D2R D2I D1I D1R "")
     ) ; end while
    ) ; end OPT L<--R
    ((= OPT "L")
     (prompt "\n You chose option Left to Right  L-->R ")
     (GET_INFO)
     (while
       (setq
         STA (getreal "\n Enter STATION of sanitary riser to draw: ")
       ) ;_ end setq
        (setq
          RIM (getreal "\n Enter RIM ELEV of sanitary riser to draw: ")
        ) ;_ end setq
        (setq
          INV (getreal "\n Enter INV ELEV of sanitary riser to draw: ")
        ) ;_ end setq
        (setq DEPTH (- RIM INV))
        (setq HDIFF (- STA REFSTA))
        (setq VDIFF (- RIM REFEL))

        (if (> HDIFF 0.0)
          (setq HANG 0.0)
          (setq HANG (* 1.0 pi))
        ) ;_ end if
        (if (> VDIFF 0.0)
          (setq VANG (* 0.5 pi))
          (setq VANG (* 1.5 pi))
        ) ;_ end if

        (setq NEWSTA (polar REF HANG (abs HDIFF)))
        (setq MIDRIM (polar NEWSTA VANG (* VDIFF VFAC)))
        (setq MIDINV (polar MIDRIM (* 1.5 pi) (* DEPTH VFAC)))
        (setq D1R (polar MIDRIM (* 1.0 pi) 1.0))
        (setq D1I (polar MIDINV (* 1.0 pi) 1.0))
        (setq D2R (polar MIDRIM 0.0 1.0))
        (setq D2I (polar MIDINV 0.0 1.0))
        (setvar "OSMODE" 0)

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
) ; end san_rsr