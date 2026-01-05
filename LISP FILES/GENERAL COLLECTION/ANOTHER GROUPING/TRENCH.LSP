;;;            created by Mark Evinger  CIS id 73162,1676                 ;;;
;;; TRENCH.LSP : Draws trench backfill in plan & profile sheets           ;;;
;;; Routine finds current linetype scale and uses it to calc the width of ;;;
;;; the backfill to be 0.125"                                             ;;;
;;; revised 10:12:05 Friday, 06-24-1994 to include FLAT_LAND and better   ;;;
;;; error handling                                                        ;;;
;;; revised 15:01:58 Friday, 06-02-1995 to include AUNITS in MODESAVE     ;;;
;;;-----------------------------------------------------------------------;;;
;;; revised 08/13/98 12:48:10 AM to account for OSMODE concerns           ;;;
;;; changed routine to be a repeater - works until to point picked        ;;;
;;;-----------------------------------------------------------------------;;;
;;; revised 12/8/99 8:37:06 AM to change layer colors                     ;;;v1.04
;;; color 10 =  30                                                        ;;;
;;; color 11 =  97                                                        ;;;
;;; color 12 = 114                                                        ;;;
;;; color 13 = 204                                                        ;;;
;;; color 14 = 200                                                        ;;;
;;; These color changes were to match the old DOS R12 color schemes as    ;;;
;;; closely as possible.  The new colors show up better in the Windows    ;;;
;;; 256 color scheme.                                                     ;;;
;;;                                                                       ;;;
;;;-----------------------------------------------------------------------;;;
;;; revised 03-20-2007 to check for serial number                         ;;; v1.06

;;;---------------------------------------------------------------------------
;;; save specified mode settings
;;;---------------------------------------------------------------------------
(defun MODESAVE (A_A)
  (setq MLST '())
  (repeat (length A_A)
    (setq MLST (append MLST (list (list (car A_A) (getvar (car A_A))))))
    (setq A_A (cdr A_A))
  ) ;_ end repeat
) ;_ end defun
;;;---------------------------------------------------------------------------
;;; restore saved mode settings
;;;---------------------------------------------------------------------------
(defun MODER ()
  (repeat (length MLST)
    (setvar (caar MLST) (cadar MLST))
    (setq MLST (cdr MLST))
  ) ;_ end repeat
) ;_ end defun
;;;---------------------------------------------------------------------------
;;;error handler
;;;---------------------------------------------------------------------------
(defun TRENCH_ERR (S) ;If an error (such as CTRL-C) occurs
  (if (/= S "Function cancelled") ;while this command is active...
    (princ (strcat "\nError: " S))
  ) ;_ end if
  (setq *ERROR* OLD_ERR) ;Restore old *error* handler
  (MODER) ;return modes to old values
  (princ)
) ;_ end defun
;;;---------------------------------------------------------------------------
;;;strip out Z elevations
;;;---------------------------------------------------------------------------
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
        "      C A U T I O N\n You are working in 3D!!\nTRENCH.LSP will strip out all\n       Z-elevations"
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
;;;---------------------------------------------------------------------------;----------------------------------------------------------------------------
;;; convert angle in degrees to angle in rads
;;;----------------------------------------------------------------------------
(defun DTR (A)
  (* pi (/ A 180.00))
) ;_ end DEFUN
;;;----------------------------------------------------------------------------
;;; acquire information for trench backfill
;;;----------------------------------------------------------------------------
(defun GPUSER ()
  (setvar "OSMODE" CGL_OSMODE)
  (princ)
  (while
    (setq SP (getpoint "\nStart point of trench backfill: "))
     (setq EP (getpoint "\nEnd point of trench backfill: " SP))
     (setvar "OSMODE" 0)

     (setq P1 SP)
     (setq P2 EP)
     (FLAT_LAND)
     (setq SP P1)
     (setq EP P2)

     (setq HTCHSCL (getvar "LTSCALE"))
     (setq H_WIDTH (* 0.125 HTCHSCL))

     (setq PANGLE (angle SP EP))
     (setq PLENGTH (distance SP EP))
     (setq HWIDTH (/ H_WIDTH 2))
     (setq ANGP90 (+ PANGLE (DTR 90))) ; BACKFILL ANGLE +90 DEG
     (setq ANGM90 (- PANGLE (DTR 90))) ; BACKFILL ANGLE -90 DEG
 ;----------------------------------------------------------------------------
 ; draw outline of trench backfill
 ;----------------------------------------------------------------------------
     (setvar "OSMODE" 0)
     (command "PLINE"
              (setq P (polar SP ANGM90 HWIDTH))
              (setq P (polar P PANGLE PLENGTH))
              (setq P (polar P ANGP90 H_WIDTH))
              (polar P (+ PANGLE (DTR 180)) PLENGTH)
              "CLOSE"
     ) ;_ end COMMAND
     (princ)
     (princ)
 ;----------------------------------------------------------------------------
 ; draw in hatching
 ;----------------------------------------------------------------------------
     (command "layer" "set" "4P_trench" "")
     (setq HTCHSCL (* 0.50 HTCHSCL))
     (command "UCS" "Z" SP EP)
     (command "HATCH" "DOTS" HTCHSCL "45" "L" "")
     (command "UCS" "P")
     (command "ERASE" "P" "")
     (command "layer" "s" CURRENT_LAYER "")
     (setvar "OSMODE" CGL_OSMODE)
     (princ)
     (princ)
  ) ;_ end while
) ;_ end defun
;;;----------------------------------------------------------------------------
;;; main routine
;;;----------------------------------------------------------------------------
(defun TRENCH_1 ()
  (graphscr)
  (setq OLD_ERR *ERROR* ;use special error handling function
        *ERROR* TRENCH_ERR
  ) ;_ end setq

  (setq CURRENT_LAYER (getvar "CLAYER"))
  (setq CGL_OSMODE (getvar "OSMODE"))
  (MODESAVE '("BLIPMODE" "CMDECHO" "COORDS" "DIMZIN" "GRIPS" "OSMODE" "AUNITS")
  ) ;_ end MODESAVE
 ;save modes
  (setvar "BLIPMODE" 0) ;turn off blips
  (setvar "CMDECHO" 0) ;turn off command echo
  (setvar "COORDS" 2) ;sets coords to show distance & angle
  (setvar "DIMZIN" 0) ;turn off dimzin (controls leading/trailing 0's)
  (setvar "GRIPS" 0) ;turn off grips
  (setvar "OSMODE" 0) ;turn off osnaps
  (setvar "AUNITS" 0) ;set angular units to degrees only

  (if (= (tblsearch "layer" "4p_trench") NIL)
    (command "._layer" "n" "4P_trench" "c" "114" "4P_trench" "")
  ) ;_ end if
  (GPUSER)
  (princ)
  (MODER)
  (princ)
) ;end defun trench
(princ)
 ;----------------------------------------------------------------------------
;;(defun C:TR () (vmon) (TRENCH_1))
(defun C:TRENCH ()
  (if (cgl_check_serial)
    (progn
  (vmon)
  (TRENCH_1)
    );end checker progn
    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
    );end if
    (princ)  
  )
(princ "\n\tc:TRENCH loaded. Start command with TRENCH.")
(princ)