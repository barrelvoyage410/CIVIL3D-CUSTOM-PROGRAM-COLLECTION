;;; WM_SER.LSP created by Mark Evinger  CIS id 73162,1676                ;;;
;;; Draws water services to CGL standards, uses two points to define     ;;;
;;; direction, and inserts circle at first point                         ;;;
;;;                                                                      ;;;
;;; final revisions 10:26:24 Friday, 06-24-1994 to include FLAT_LAND and ;;;
;;; better error handling                                                ;;;
;;; revised 15:05:00 Friday, 06-02-1995 to include AUNITS in MODESAVE    ;;;
;;; revised 11:34:14 AM 07/24/98 to check for USERR2 /= LTSCALE  (R14)   ;;;
;;;----------------------------------------------------------------------;;;
;;; revised 08/13/98 1:14:52 PM to account for OSMODE concerns           ;;;
;;;----------------------------------------------------------------------;;;
;;; revised 12/8/99 8:37:06 AM to change layer colors                    ;;;v1.04
;;; color 10 =  30                                                       ;;;
;;; color 11 =  97                                                       ;;;
;;; color 12 = 114                                                       ;;;
;;; color 13 = 204                                                       ;;;
;;; color 14 = 200                                                       ;;;
;;; These color changes were to match the old DOS R12 color schemes as   ;;;
;;; closely as possible.  The new colors show up better in the Windows   ;;;
;;; 256 color scheme.                                                    ;;;
;;;                                                                      ;;;
;;;----------------------------------------------------------------------;;;
;;; revised 09-26-2001 5:21 PM to correctly use MODER to return grips to ;;;
;;; initial state.  (per request of Nicole Woods, thank you for pointing ;;;
;;; out the error!                                                       ;;;
;;;----------------------------------------------------------------------;;;
;;; revised 1/29/2002 1:33 PM to allow for existing and proposed services;;;v1.06
;;; routine now asks exist  or  proposed and  uses  a  circle  or  donut ;;;
;;; respectively                                                         ;;;
;;;----------------------------------------------------------------------;;;
;;; revised 5/6/05 10:44 AM to use new standard layers of 6E_WMSERV and  ;;;
;;; 6P_WMSERV.                                                           ;;;v1.08
;;;                                                                      ;;;
;;; revised 03-20-2007 to check for serial number                        ;;;v1.10
 ;---------------------------------------------------------------------------
 ; save specified mode settings
 ;---------------------------------------------------------------------------
(defun MODESAVE (A_A)
  (setq MLST '())
  (repeat (length A_A)
    (setq MLST (append MLST (list (list (car A_A) (getvar (car A_A))))))
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
(defun WM_SER_ERR (S) ;If an error (such as CTRL-C) occurs
  (if (/= S "Function cancelled") ;while this command is active...
    (princ (strcat "\nError: " S))
  ) ;_ end if
  (setq *ERROR* OLD_ERR) ;Restore old *error* handler
  (MODER) ;return modes to old values
  (princ)
) ;_ end defun
 ;---------------------------------------------------------------------------
 ;strip out Z elevations
 ;---------------------------------------------------------------------------
(defun FLAT_LAND ()
  (setq P1X (car P1))
  (setq P1Y (cadr P1))
  (setq P1Z (caddr P1))
  (setq P2X (car P2))
  (setq P2Y (cadr P2))
  (setq P2Z (caddr P2))
  (if (or (/= P1Z P2Z) (/= P1Z 0))
    (progn
      (alert
        "      C A U T I O N\n You are working in 3D!!\nWM_SER.LSP will strip out all\n       Z-elevations"
      ) ;_ end alert
      (setq P1_FLAT (list P1X P1Y 0))
      (setq P2_FLAT (list P2X P2Y 0))
    ) ;end progn
    (progn
      (setq P1_FLAT (list P1X P1Y P1Z))
      (setq P2_FLAT (list P2X P2Y P2Z))
    ) ;end progn
  ) ;end if
  (setq P1 P1_FLAT)
  (setq P2 P2_FLAT)
) ;end flat_land
 ;---------------------------------------------------------------------------
(defun C:WM_SER ()
  (if (cgl_check_serial)
    (progn
  (graphscr)

  (setq CURRENT_LAYER (getvar "clayer"))
  (setq CGL_OSMODE (getvar "OSMODE"))

  (MODESAVE '("BLIPMODE" "CMDECHO" "COORDS" "DIMZIN" "GRIPS" "OSMODE" "AUNITS")
  ) ;save modes
  (setvar "BLIPMODE" 0) ;turn off blips
  (setvar "CMDECHO" 0)  ;turn off command echo
  (setvar "COORDS" 2)   ;sets coords to show distance & angle
  (setvar "DIMZIN" 0)   ;turn off dimzin (controls leading/trailing 0's)
  (setvar "GRIPS" 0)    ;turn off grips
  (setvar "OSMODE" 0)   ;turn off osnaps
  (setvar "AUNITS" 0)   ;set angular units to degrees only

  (setq OLD_ERR *ERROR* ; use special error handling function
        *ERROR* WM_SER_ERR
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

  (if (or (= (getvar "USERR2") NIL) (= (getvar "USERR2") 0))
    (progn
      (initget (+ 1 2 4))
      (setvar
        "USERR2"
        (getreal "\n Drawing scale not found. Enter current scale: "
        ) ;_ end getreal
      ) ;_ end setvar
    ) ;_ end progn
  ) ;_ end if
  (setq SCAL (getvar "userr2"))

  (setq RAD (* SCAL 0.025))
  (print)
  (setvar "OSMODE" CGL_OSMODE)
  (princ "\n      (1)=Ex. WATERMAIN SERVICE // (2)=Prop. WATERMAIN SERVICE ")
  (print)
  (if (or (= UTOPT NIL) (and (/= UTOPT 1) (/= UTOPT 2)))
    (progn
      (initget (+ 1 2 4))
      (setq UTOPT
             (getint "\n      Draw WATERMAIN SERVICE - enter type 1 or 2 : ")
      ) ;_ end setq
      (while (> UTOPT 2)
        (progn
          (print)
          (princ
            "\n      **** Your choice was not 1 or 2!!  Please try again!! **** "
          ) ;_ end princ
          (princ "\n      (1)=Ex. WATERMAIN SERVICE // (2)=Prop. WATERMAIN SERVICE ")
          (initget (+ 1 2 4))
          (setq
            UTOPT (getint
                    "\n      Draw WATERMAIN SERVICE - enter type 1 or 2 : "
                  ) ;_ end getint
          ) ;_ end setq
        ) ;end progn
      ) ;end while
    ) ;end progn
    (progn
      (initget (+ 2 4))
      (setq ANS
             (getint
               (strcat "\n      Draw WATERMAIN SERVICE - enter type 1 or 2  << "
                       (itoa UTOPT)
                       " >>: "
               ) ;_ end strcat
             ) ;_ end getint
      ) ;_ end setq
      (if (not (null ANS))
        (setq UTOPT ANS)
      ) ;_ end if
      (while (> UTOPT 2)
        (progn
          (print)
          (princ
            "\n      **** Your choice was not 1 or 2!!  Please try again!! **** "
          ) ;_ end princ
          (princ "\n      (1)=Ex. WATERMAIN SERVICE // (2)=Prop. WATERMAIN SERVICE ")
          (initget (+ 1 2 4))
          (setq
            UTOPT (getint
                    "\n      Draw WATERMAIN SERVICE - enter type 1 or 2 : "
                  ) ;_ end getint
          ) ;_ end setq
        ) ;end progn
      ) ;end while
    ) ;end progn
  ) ;end if

  (while
    (setq P1 (getpoint "\nPick B-BOX location: "))
     (print)
     (setq P2 (getpoint P1 "\nPick connection location: "))
     (setvar "OSMODE" 0)
     (FLAT_LAND)
     (setq DIR (angle P1 P2))
     (setq D1 P1)
     (progn
       (cond ((= UTOPT 1)
              (setq WMLYR "6E_WMSERV")
                (if (= (tblsearch "layer" WMLYR) NIL)
                 (command "._layer" "n" WMLYR "c" "30" WMLYR "lt" "water2" WMLYR "")
                ) ; end if
              (command "layer" "s" WMLYR "")
              (command "circle" P1 RAD)
              ) ;existing watermain
             ((= UTOPT 2)
              (setq WMLYR "6P_WMSERV")
                (if (= (tblsearch "layer" WMLYR) NIL)
                  (command "._layer" "n" WMLYR "c" "114" WMLYR "lt" "water2" WMLYR "")
                ) ; end if
              (command "layer" "s" WMLYR "")
              (command "donut" "0.00001" (* 2 RAD) P1 "")
              ) ;proposed watermain
       ) ;end cond
       (setq D1 (polar P1 DIR RAD))
       (command "line" D1 P2 "")
       (command "layer" "s" CURRENT_LAYER "")
     ) ; end progn
     (setvar "OSMODE" CGL_OSMODE)
  ) ; end while


  (MODER) ; return saved modes - leave it like you started!!
  (terpri)
  (princ) ; suck up any "nil" returns, exit quietly
    );end checker progn
    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
    );end if
    (princ)  
) ; end WM_SER