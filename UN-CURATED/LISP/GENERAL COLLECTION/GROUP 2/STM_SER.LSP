;;; STM_SER.LSP created by Mark Evinger  CIS id 73162,1676               ;;;
;;; Draws storm services to CGL standards, uses two points to define     ;;;
;;; direction, and inserts storm hook at midpoint of pick points.        ;;;
;;;                                                                      ;;;
;;; final revisions 09:52:23 Friday, 06-24-1994 to include FLAT_LAND and ;;;
;;; better error handling                                                ;;;
;;; revised 15:03:29 Friday, 06-02-1995 to include AUNITS in MODESAVE    ;;;
;;; revised 4:18:00 PM 07/24/98 to account for USERR2 /= LTSCALE for R14 ;;;
;;; revised 08/13/98 12:43:20 AM to account for OSMODE concerns          ;;;
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
;;;-----------------------------------------------------------------------;;;
;;;                                                                       ;;;
;;; revised 4/4/2002 10:57 AM for Land Desktop R3 / Acad 2002 use         ;;; v3.0.0
;;; routine now uses complex linetypes for structure to structure linework;;;
;;; This takes advantage of the linetype stuff available since R13.       ;;;
;;;                                                                       ;;;
;;; revised 5/6/05 11:02 AM for LDT2005 rollout, changed default layers to;;;
;;; 6E_STMSERV and 6P_STMSERV.                                            ;;; v3.0.1
;;;                                                                       ;;;
;;; revised  03-20-2007 to check for serial number                        ;;; v3.0.2


(defun C:STM_SER ()
;  (if (cgl_check_serial)
;    (progn
  (graphscr)
 ;---------------------------------------------------------------------------
 ; Linetype load_checker
 ;---------------------------------------------------------------------------
  (defun CGL_LINETYPE_LOAD (CGL_LTYPE)
    (if (null (tblsearch "ltype" CGL_LTYPE))
      (progn
        (command "_linetype" "_L" CGL_LTYPE "cgl_shp.lin" "")
        (if (null (tblsearch "ltype" CGL_LTYPE))
          (progn
            (princ (strcat "\nCould not load linetype " CGL_LTYPE " - using CONTINUOUS "))
            (setq CGL_LTYPE "CONTINUOUS")
          )
        )
      )
    )
    CGL_LTYPE
  ) ;end defun
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
  (defun STM_SER_ERR (S) ;If an error (such as CTRL-C) occurs
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
          "      C A U T I O N\n You are working in 3D!!\nSTM_SER.LSP will strip out all\n       Z-elevations"
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
  (setq CURRENT_LAYER (getvar "clayer"))
  (setq CGL_OSMODE (getvar "OSMODE"))

  (MODESAVE '("BLIPMODE" "CMDECHO" "COORDS" "DIMZIN" "GRIPS" "OSMODE" "AUNITS" "CELTYPE")
  ) ;save modes
  (setvar "BLIPMODE" 0) ;turn off blips
  (setvar "CMDECHO" 0) ;turn off command echo
  (setvar "COORDS" 2) ;sets coords to show distance & angle
  (setvar "DIMZIN" 0) ;turn off dimzin (controls leading/trailing 0's)
  (setvar "GRIPS" 0) ;turn off grips
  (setvar "OSMODE" 0) ;turn off osnaps
  (setvar "AUNITS" 0) ;set angular units to degrees only
  (setvar "CELTYPE" "BYLAYER") ; set CurrentEntity Linetype to bylayer

  (CGL_LINETYPE_LOAD "cgl_stm_svc") ; loads from the CGL_SHP.LIN file

  (setq OLD_ERR *ERROR* ; use special error handling function
        *ERROR* STM_SER_ERR
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

;============================================================
  (princ "\n      (1)=Ex. STORM SERVICE // (2)=Prop. STORM SERVICE ")
  (print)
  (if (or (= UTOPT NIL) (and (/= UTOPT 1) (/= UTOPT 2)))
    (progn
      (initget (+ 1 2 4))
      (setq UTOPT
             (getint "\n      Draw STORM SERVICE - enter type 1 or 2 : ")
      ) ;_ end setq
      (while (> UTOPT 2)
        (progn
          (print)
          (princ
            "\n      **** Your choice was not 1 or 2!!  Please try again!! **** "
          ) ;_ end princ
          (princ "\n      (1)=Ex. STORM SERVICE // (2)=Prop. STORM SERVICE ")
          (initget (+ 1 2 4))
          (setq
            UTOPT (getint
                    "\n      Draw STORM SERVICE - enter type 1 or 2 : "
                  ) ;_ end getint
          ) ;_ end setq
        ) ;end progn
      ) ;end while
    ) ;end progn
    (progn
      (initget (+ 2 4))
      (setq ANS
             (getint
               (strcat "\n      Draw STORM SERVICE - enter type 1 or 2  << "
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
          (princ "\n      (1)=Ex. STORM SERVICE // (2)=Prop. STORM SERVICE ")
          (initget (+ 1 2 4))
          (setq
            UTOPT (getint
                    "\n      Draw STORM SERVICE - enter type 1 or 2 : "
                  ) ;_ end getint
          ) ;_ end setq
        ) ;end progn
      ) ;end while
    ) ;end progn
  ) ;end if

  (cond
    ((= UTOPT 1)
     (setq STMLYR "6E_STMSERV")
     (setq TAIL "E_STMARR")
    ) ;existing
    ((= UTOPT 2)
     (setq STMLYR "6P_STMSERV")
     (setq TAIL "P_STMARR")
    ) ;Proposed
  ) ;end cond

  (if (= (tblsearch "layer" STMLYR) NIL)
     (progn
      (cond
        ((= UTOPT 1)
         (setq STMLYR "6E_STMSERV")
         (command "_layer" "_n" STMLYR "c" "30" STMLYR "_LT" "CGL_STM_SVC" STMLYR "") ; uses new CGL_STM linetype
        ) ;existing
        ((= UTOPT 2)
         (setq STMLYR "6P_STMSERV")
         (command "_layer" "_n" STMLYR "c" "114" STMLYR "_LT" "CGL_STM_SVC" STMLYR "") ; uses new CGL_STM linetype
        ) ;Proposed
      ) ;end cond
    ) ;end progn   
  ) ; end if



;============================
  
  (setq SCAL (getvar "userr2"))
  (setq RAD (* SCAL 0.0625))
  (setq TAILSCL (* SCAL 0.75))
  (setq TAIL "P_STMARR")
  (setvar "OSMODE" CGL_OSMODE)
  (while
    (setq P1 (getpoint "\n Pick first point: "))
     (print)
     (setq P2 (getpoint P1 "\n Pick next point: "))
     (setvar "OSMODE" 0)
     (FLAT_LAND)
     (setq DIR (angle P1 P2))
;;;     (setq INSDIR (angtos DIR 0 4))
;;;     (print)
;;;     (setq FULL_LENGTH (distance P1 P2))
;;;     (setq HALF_LENGTH (* FULL_LENGTH 0.5))
;;;     (setq D1 (polar P1 DIR HALF_LENGTH))
;;;     (setq D2 (polar D1 DIR RAD))
     (command "layer" "s" STMLYR "")
     (setq STUB1 (polar P1 (+ DIR (/ pi 2.0)) (/ RAD 2.0)))
     (setq STUB2 (polar P1 (- DIR (/ pi 2.0)) (/ RAD 2.0)))
     (command "line" STUB1 STUB2 "")
    (if (< (distance P1 P2) 13.8) ;if the distance is under 13.8'
      (progn
        (setvar "CELTSCALE" 0.5); half size the entity linetype
        (command "._line" P1 P2 "")
        (setvar "CELTSCALE" 1.0)
        );end progn
     (command "._line" P1 P2 ""); otherwise draw the line normal
      );end if
;     (command "line" D2 P2 "")
;     (command "insert" TAIL D2 TAILSCL "" INSDIR)
     (command "layer" "s" CURRENT_LAYER "")
     (setvar "OSMODE" CGL_OSMODE)
  ) ;end while
  (MODER)
  (princ)
  (princ)
;    );end checker progn
;    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
;    );end if
;    (princ)  
) ; end stm_ser