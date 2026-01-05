;;;          created 08-03-1993 by Mark Evinger, cis # 73162,1676         ;;;
;;; draws sanitary sewer forcemain, modified to Cowhey & Associates layer ;;;
;;; standards.                                                            ;;;
;;; revised 17:52:00 Thursday, 06-23-1994 to include FLAT_LAND and better ;;;
;;; error checking...                                                     ;;;
;;; revised 14:31:44 Friday, 06-02-1995 to use the AUNITS var in MODESAVE ;;;
;;; revised 10:26:44 Friday, 08-25-1995 to fix the hard-coded 40' segment ;;;
;;; length when asked for segment length.                                 ;;;
;;; revised 07/24/98 4:26:29 PM to check for USERR2 /= LTSCALE for R14    ;;;
;;;                                                                       ;;;
;;; revised 08/13/98 10:18:06 AM to check for OSMODE concerns, also for   ;;;
;;; cosmetic changes to prompts and displays.                             ;;;
;;;                                                                       ;;;
;;; revised 12/8/99 8:37:06 AM to change layer colors                     ;;;v1.26
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
;;;                                                                       ;;;
;;; revised 03-20-2007 to check for serial number                         ;;; v3.0.2

(defun FORCMAIN ()
  (setq VERSION "3.0.2")
  (princ
    (strcat
      "\r FORCMAIN version "
      VERSION
      " -- copyright © Cowhey & Associates 1993-2007. "
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
  (defun FORCMAIN_ERR (S) ;If an error (such as CTRL-C) occurs
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
 ;                       REPEATING STORM SEWER ROUTINE
 ;---------------------------------------------------------------------------
  (defun SANLOOP ()
    (setq ANS NIL)
    (initget "Y N")
    (setq
      ANS (getkword
            "\n Insert manhole at  last point? - (Y)es or (N)o: <Y> "
          )
    ) ;setq
    (if (or (= ANS "N") (= ANS "n"))
      (setq D2 P2)
      (progn
        (command "layer" "s" MHLYR "")
        (command "insert" MH P2 SCAL "" INSDIR)
        (setq D2 (polar P2 (+ DIR pi) RAD))
        (command ".LAYER" "S" CURRENT_LAYER "")
      ) ;end progn
    ) ;end if

    (command "layer" "s" SANLYR "")
    (if (< CHKDIST 40.0)
      (setvar "CELTYPE" "CGL_SANFRC_Small")
    )
    (command "line" D1 D2 "")
    (setq CHKDIST 0) ; re-zero chkdist
    (setvar "CELTYPE" CGL_CELTYPE) ; linetype back to original value
    (command ".LAYER" "S" CURRENT_LAYER "")
    (setvar "OSMODE" CGL_OSMODE)
    (princ)
  ) ;end defun sanloop
  (princ)
 ;---------------------------------------------------------------------------
 ;                       MAIN STORM SEWER ROUTINE
 ;---------------------------------------------------------------------------
  (defun DRAWSAN ()
    (print)
    (setvar "OSMODE" CGL_OSMODE)
    (setq P1 (getpoint "\n Pick first point: "))
    (while (= P1 NIL)
      (progn
        (prompt "\nYou must pick a start point! ")
        (setq P1 (getpoint "\n Pick first point: "))
      ) ;end progn
    ) ;end while
    (print)
    (setq P2 (getpoint P1 "\n Pick next point: "))
    (setvar "OSMODE" 0)
    (FLAT_LAND)
    (setq CHKDIST (distance P1 P2))
    (setq DIR (angle P1 P2))
    (setq INSDIR (angtos DIR 0 4))
    (print)
    (setvar "OSMODE" CGL_OSMODE)
    (setq GAP (* SCAL 0.0625))
    (setq RAD (* SCAL 0.0625))
    (print)
    (setq ANS NIL)
    (initget "Y N")
    (setq ANS
           (getkword
             "\n Insert manhole at first point? - (Y)es or (N)o: <Y> "
           )
    )
    (if (or (= ANS "N") (= ANS "n"))
      (setq D1 P1)
      (progn
        (command "layer" "s" MHLYR "")
        (command "insert" MH P1 SCAL "" INSDIR)
        (setq D1 (polar P1 DIR RAD))
        (command ".LAYER" "S" CURRENT_LAYER "")
      ) ;end progn
    ) ;end if

    (SANLOOP)

    (while (setq NXTPT (getpoint P2 "\n Pick next point: "))
      (setq P1 P2)
      (setq P2 NXTPT)
      (FLAT_LAND)
      (setq CHKDIST (distance P1 P2))
      (setq DIR (angle P1 P2))
      (setq INSDIR (angtos DIR 0 4))
      (print)
      (setvar "OSMODE" CGL_OSMODE)
      (if (or (= ANS "N") (= ANS2 "F") (= ANS2 "S"))
        (setq D1 P1)
        (setq D1 (polar P1 DIR RAD))
      )

      (SANLOOP)

    ) ;while
    (princ)
  ) ; end drawsan
  (princ)
 ;---------------------------------------------------------------------------
 ;                       MAIN SEWER DRAWING ROUTINE
 ;---------------------------------------------------------------------------
  (setq OLD_ERR *ERROR* ; use special error handling function
        *ERROR* FORCMAIN_ERR
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

  (setq CHKDIST 0)
  (setq CURRENT_LAYER (getvar "CLAYER"))
  (setq CGL_OSMODE (getvar "OSMODE"))
  (setq CGL_CELTYPE (getvar "CELTYPE")) ;save current linetype
  (CGL_LINETYPE_LOAD "CGL_SANFRC") ; loads from the CGL_SHP.LIN file
  (CGL_LINETYPE_LOAD "CGL_SANFRC_Small") ; loads from the CGL_SHP.LIN file

  (if (= (tblsearch "layer" "4E_sanforcemain") NIL)
    (command "layer" "n" "4E_SanForceMain" "c" "30" "4E_sanforcemain" "LT" "CGL_SANFRC" "4e_sanforcemain" "")
  )
  (if (= (tblsearch "layer" "4P_SanForceMain") NIL)
    (command "layer" "n" "4P_sanforcemain" "c" "2" "4P_sanforcemain" "LT" "CGL_SANFRC" "4p_sanforcemain" "")
  )
  (if (= (tblsearch "layer" "4P_sanMH") NIL)
    (command "layer" "n" "4P_sanMH" "c" "114" "4P_sanMH" "")
  )
;;;  (if (= (tblsearch "layer" "4P_sanfrc") NIL)
;;;    (command "layer" "n" "4P_sanfrc" "c" "114" "4P_sanfrc" "")
;;;  )

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
  (princ "\n      (1)=Ex. FORCEMAIN // (2)=Prop. FORCEMAIN ")
  (print)
  (if (or (= UTOPT NIL) (and (/= UTOPT 1) (/= UTOPT 2)))
    (progn
      (initget (+ 1 2 4))
      (setq
        UTOPT (getint
                "\n      Draw forcemain lines - enter type 1 or 2 : "
              )
      )
      (while (> UTOPT 2)
        (progn
          (print)
          (princ
            "\n      **** Your choice was not 1 or 2!!  Please try again!! **** "
          )
          (princ "\n      (1)=Ex. FORCEMAIN // (2)=Prop. FORCEMAIN ")
          (initget (+ 1 2 4))
          (setq UTOPT
                 (getint
                   "\n      Draw forcemain lines - enter type 1 or 2 : "
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
                 "\n      Draw forcemain lines - enter type 1 or 2  << "
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
          (princ "\n      (1)=Ex. FORCEMAIN // (2)=Prop. FORCEMAIN ")
          (initget (+ 1 2 4))
          (setq UTOPT
                 (getint
                   "\n      Draw forcemain lines - enter type 1 or 2 : "
                 )
          )
        ) ;end progn
      ) ;end while
    ) ;end progn
  ) ;end if

  (cond
    ((= UTOPT 1)
     (setq MH "E_sanMH")
     (setq TAIL "p_sanfrc")
     (setq SANLYR "4E_SanForceMain") ;THESE ARE
     (setq MHLYR "4E_Sanforcemain") ;THE LAYERS for the
     (setq ARRLYR "4E_sanforcemain") ;ABOVE BLOCKS
     (DRAWSAN)
    )

    ((= UTOPT 2)
     (setq MH "P_sanMH")
     (setq TAIL "P_sanfrc")
     (setq SANLYR "4P_sanforcemain") ;THESE ARE
     (setq MHLYR "4P_sanMH") ;THE layers for
     (setq ARRLYR "4P_sanfrc") ;THE ABOVE BLOCKS
     (DRAWSAN)
    )
  ) ; end condition

  (print)
 ;---------------------------------------------------------------------------
  (command "._LAYER" "S" CURRENT_LAYER "")
  (princ)
  (MODER)
  (princ)
) ;end defun forcmain
(princ)
(princ)
(princ)
;;;-----------------------------------------------------------------------
;;; c: definitions
;;;-----------------------------------------------------------------------
(defun C:FM ()
  (if (cgl_check_serial)
    (progn
  (vmon)
  (FORCMAIN)
    );end checker progn
    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
    );end if
    (princ)  
  )
(defun C:FORCMAIN ()
    (if (cgl_check_serial)
    (progn
  (vmon)
  (FORCMAIN)
    );end checker progn
    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
    );end if
    (princ)  
  )
(princ
  "\n\tc:FORCMAIN loaded.  Start command with FM or FORCMAIN."
)
(princ)
;;; Happy Lisping!! The AutoCad Guru (aka The Cad Elf *<:)-|-< )