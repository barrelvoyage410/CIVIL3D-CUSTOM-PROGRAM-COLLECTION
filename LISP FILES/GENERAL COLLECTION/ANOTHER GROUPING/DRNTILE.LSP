;;;          created 08-25-1995 by Mark Evinger, cis # 73162,1676         ;;;
;;; draws storm draintile, modified to Cowhey & Associates layer standards;;;
;;;-----------------------------------------------------------------------;;;
;;; revised 07/24/98 4:20:45 PM to test for USERR2 /= LTSCALE for R14     ;;;
;;;                                                                       ;;;
;;; revised 08/13/98 9:38:43 AM for prompts and cosmetic changes. Checked ;;;v1.02
;;; OSMODE settings, making sure the MODER function works as advertised.  ;;;
;;;                                                                       ;;;
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
;;;                                                                       ;;;
;;; revised 4/4/2002 10:57 AM for Land Desktop R3 / Acad 2002 use         ;;; v3.0.0
;;; routine now uses complex linetypes for structure to structure linework;;;
;;; This takes advantage of the linetype stuff available since R13.       ;;;
;;;                                                                       ;;;
;;;-----------------------------------------------------------------------;;;
;;; revised 03-20-2007 to check for serial number                         ;;; v3.0.2

(defun DRNTILE ()
  (setq VERSION "3.0.2")
  (princ
    (strcat
      "\r DRNTILE version "
      VERSION
      " -- copyright © Cowhey & Associates 1998-2007. "
    )
  )
;;; This bit is designed more for a dual monitor system, as the version will
;;; scroll off a three line display....

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
      )
      (setq A_A (cdr A_A))
    )
  )
 ;---------------------------------------------------------------------------
 ; restore saved mode settings
 ;---------------------------------------------------------------------------
  (defun MODER ()
    (repeat (length MLST)
      (setvar (caar MLST) (cadar MLST))
      (setq MLST (cdr MLST))
    )
  )
 ;---------------------------------------------------------------------------
 ;error handler
 ;---------------------------------------------------------------------------
  (defun DRNTILE_ERR (S) ;If an error (such as CTRL-C) occurs
    (if (/= S "Function cancelled") ;while this command is active...
      (princ (strcat "\nError: " S))
    )
    (setq *ERROR* OLD_ERR) ;Restore old *error* handler
    (MODER) ;return modes to old values
    (princ)
  )
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
          "      C A U T I O N\n You are working in 3D!!\nFORCMAIN.LSP will strip out all\n       Z-elevations"
        )
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
  (setq CURRENT_LAYER (getvar "CLAYER"))
 ;---------------------------------------------------------------------------
 ;                       REPEATING STORM SEWER ROUTINE
 ;---------------------------------------------------------------------------
  (defun SANLOOP ()

    (setvar "OSMODE" 0)
    (command "._LAYER" "S" CURRENT_LAYER "")
    (print)
    (setq ANS NIL)
    (initget "Y N")
    (setq
      ANS (getkword
            "\n Insert MANHOLE at  last point? - (Y)es or (N)o: <Y> "
          )
    )
    (if (or (= ANS "N") (= ANS "n"))
      (setq D2 P2)
      (progn
        (command "._layer" "s" MHLYR "")
        (command "._insert" MH P2 SCAL "" INSDIR)
        (setq D2 (polar P2 (+ DIR pi) RAD))
        (command "._LAYER" "S" CURRENT_LAYER "")
      ) ;end progn
    ) ;end if
    (command "._layer" "s" SANLYR "")
    (if (<= CHKDIST 50.0)
      (setvar "CELTYPE" "CGL_DRN_Small")
    )
    (command "line" D1 D2 "")
    (setq CHKDIST 0) ; re-zero chkdist
    (setvar "CELTYPE" CGL_CELTYPE) ; linetype back to original value
 ;    (command "._layer" "s" ARRLYR "")
 ;    (command "._insert" TAIL D1 SCAL "" INSDIR)
    (command "._LAYER" "S" CURRENT_LAYER "")
    (setvar "OSMODE" CGL_OSMODE)
    (princ)
  ) ;end defun sanloop
  (princ)
 ;---------------------------------------------------------------------------
 ;                       MAIN STORM SEWER ROUTINE
 ;---------------------------------------------------------------------------
  (defun DRAWSAN ()
    (setvar "OSMODE" CGL_OSMODE)
    (print)
    (setq P1 (getpoint "\n Pick first point: "))
    (while (= P1 NIL)
      (progn
        (prompt "\nYou must pick a start point! ")
        (setq P1 (getpoint "\n Pick first point: "))
      ) ;end progn
    ) ;end while
    (print)
    (setvar "OSMODE" CGL_OSMODE)
    (setq P2 (getpoint P1 "\n Pick next point: "))
    (setvar "OSMODE" 0) ; turn off osnaps for drawing lines
    (FLAT_LAND)
    (setq CHKDIST (distance P1 P2))
    (setq DIR (angle P1 P2))
    (setq INSDIR (angtos DIR 0 4))
    (print)
    (setvar "OSMODE" 0)
    (setq GAP (* SCAL 0.0625))
    (setq RAD (* SCAL 0.0625))

    (print)
    (setq ANS NIL)
    (initget "Y N")
    (setq ANS
           (getkword
             "\n Insert MANHOLE at first point? - (Y)es or (N)o: <Y> "
           )
    )
    (if (or (= ANS "N") (= ANS "n"))
      (setq D1 P1)
      (progn
        (command "._layer" "s" MHLYR "")
        (command "._insert" MH P1 SCAL "" INSDIR)
        (setq D1 (polar P1 DIR RAD))
        (command "._LAYER" "S" CURRENT_LAYER "")
      ) ;end progn
    ) ;end if
 ;    (setq D2 (polar D1 DIR SEG))
 ;    (command "._layer" "s" SANLYR "")
 ;    (command "._line" D1 D2 "")
    (command "._LAYER" "S" CURRENT_LAYER "")
    (setvar "OSMODE" CGL_OSMODE)

    (SANLOOP)

    (setvar "OSMODE" CGL_OSMODE)
    (while (setq NXTPT (getpoint P2 "\n Pick next point: "))
      (setvar "OSMODE" 0)
      (setq P1 P2)
      (setq P2 NXTPT)
      (FLAT_LAND)
      (setq CHKDIST (distance P1 P2))
      (setq DIR (angle P1 P2))
      (setq INSDIR (angtos DIR 0 4))
      (print)
      (setvar "OSMODE" CGL_OSMODE)
      (setvar "OSMODE" 0)
      (if (or (= ANS "N") (= ANS2 "F") (= ANS2 "S"))
        (setq D1 P1)
        (setq D1 (polar P1 DIR RAD))
      )
 ;      (setq D2 (polar D1 DIR SEG))
 ;      (command "._layer" "s" SANLYR "")
 ;      (command "._line" D1 D2 "")
      (command "._LAYER" "S" CURRENT_LAYER "")

      (SANLOOP)
    ) ;while
    (princ)
  ) ; end drawsan
  (princ)
 ;---------------------------------------------------------------------------
 ;                       MAIN SEWER DRAWING ROUTINE
 ;---------------------------------------------------------------------------
  (setq CURRENT_LAYER (getvar "clayer"))
  (setq CGL_OSMODE (getvar "OSMODE"))

  (setq OLD_ERR *ERROR* ; use special error handling function
        *ERROR* DRNTILE_ERR
  )

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

  (setq CGL_CELTYPE (getvar "CELTYPE")) ;save current linetype
  (CGL_LINETYPE_LOAD "cgl_DRN") ; loads from the CGL_SHP.LIN file
  (CGL_LINETYPE_LOAD "cgl_DRN_Small") ; loads from the CGL_SHP.LIN file

  (if (= (tblsearch "layer" "4E_DRAINTILE") NIL)
    (command "._layer" "n" "4E_DRAINTILE" "c" "30" "4E_DRAINTILE" "LT" "CGL_DRN" "4E_DRAINTILE" "")
 ;uses new linetypes
  )
  (if (= (tblsearch "layer" "4P_DRAINTILE") NIL)
    (command "._layer" "n" "4P_DRAINTILE" "c" "2" "4P_DRAINTILE" "LT" "CGL_DRN" "4P_DRAINTILE" "")
 ;use new linetypes
  )
  (if (= (tblsearch "layer" "4P_stmMH") NIL)
    (command "._layer" "n" "4P_stmMH" "c" "114" "4P_stmMH" "")
  )
 ;(if (= (tblsearch "layer" "4P_stmarr") NIL)
 ;  (command "._layer" "n" "4P_stmarr" "c" "114" "4P_stmarr" "")
 ;)

  (setq SEGIN 40.0)
 ;---------------------------------------------------------------------------
 ;  get drawing scale -  this is designed to use Softdesk var userr2 for
 ;  horizontal scale

 ;test for USERR2 not equal to LTSCALE, get user input if not equal
  (setq SCAL (getvar "USERR2"))
  (setq TEMP_SCAL (getvar "LTSCALE"))
  (cond
    ((/= SCAL TEMP_SCAL)
     (setq SCAL (getreal (strcat "\nEnter HORIZONTAL scale: >>"
                                 (rtos (getvar "ltscale") 2 1)
                                 "<< "
                         )
                )
     )
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
        )
      )
    )
  )
  (setq SCAL (getvar "userr2"))
 ;---------------------------------------------------------------------------
  (graphscr)
  (princ "\n      (1)=Ex. DRAINTILE // (2)=Prop. DRAINTILE ")
  (print)
  (if (or (= UTOPT NIL) (and (/= UTOPT 1) (/= UTOPT 2)))
    (progn
      (initget (+ 1 2 4))
      (setq
        UTOPT (getint
                "\n      Draw DRAINTILE lines - enter type 1 or 2 : "
              )
      )
      (while (> UTOPT 2)
        (progn
          (print)
          (princ
            "\n      **** Your choice was not 1 or 2!!  Please try again!! **** "
          )
          (princ "\n      (1)=Ex. DRAINTILE // (2)=Prop. DRAINTILE ")
          (initget (+ 1 2 4))
          (setq UTOPT
                 (getint
                   "\n      Draw draintile lines - enter type 1 or 2 : "
                 )
          )
        )
      )
    )
    (progn
      (initget (+ 2 4))
      (setq ANS
             (getint
               (strcat
                 "\n      Draw draintile lines - enter type 1 or 2  << "
                 (itoa UTOPT)
                 " >>:"
               )
             )
      )
      (if (not (null ANS))
        (setq UTOPT ANS)
      )
      (while (> UTOPT 2)
        (progn
          (print)
          (princ
            "\n      **** Your choice was not 1 or 2!!  Please try again!! **** "
          )
          (princ "\n      (1)=Ex. DRAINTILE // (2)=Prop. DRAINTILE ")
          (initget (+ 1 2 4))
          (setq UTOPT
                 (getint
                   "\n      Draw DRAINTILE lines - enter type 1 or 2 : "
                 )
          )
        ) ;end progn
      ) ;end while
    ) ;end progn
  ) ;end if

  (cond
    ((= UTOPT 1)
     (setq MH "E_stmMH")
     (setq TAIL "stub")
     (setq SANLYR "4E_DRAINTILE") ;THESE ARE
     (setq MHLYR  "4E_DRAINTILE") ;THE LAYERS for the
     (setq ARRLYR "4E_DRAINTILE") ;ABOVE BLOCKS
     (DRAWSAN)
    )

    ((= UTOPT 2)
     (setq MH "P_stmMH")
     (setq TAIL "stub")
     (setq SANLYR "4P_DRAINTILE") ;THESE ARE
     (setq MHLYR  "4P_stmMH")     ;THE layers for
     (setq ARRLYR "4P_DRAINTILE") ;THE ABOVE BLOCKS
     (DRAWSAN)
    )
  ) ; end condition

  (print)
 ;---------------------------------------------------------------------------
  (command "._LAYER" "S" CURRENT_LAYER "")
  (princ)
  (MODER)
  (princ)
) ;end defun DRNTILE
(princ)
(princ)
(princ)
;;;-----------------------------------------------------------------------
;;; c: definitions
;;;-----------------------------------------------------------------------
;(defun C:DT ()
;  (if (cgl_check_serial)
;    (progn
;  (vmon)
;  (DRNTILE)
;    );end checker progn
;    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
;    );end if
;    (princ)
;  )
;(defun C:DRNTILE ()
;  (if (cgl_check_serial)
;    (progn 
;  (vmon)
;  (DRNTILE)
;    );end checker progn
;    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
;    );end if
;    (princ)
;  )
(princ
  "\n\tc:DRNTILE loaded.  Start command with DT or DRNTILE."
)
(princ)
;;; Happy Lisping!! The AutoCad Guru (aka The Cad Elf *<:)-|-< )
