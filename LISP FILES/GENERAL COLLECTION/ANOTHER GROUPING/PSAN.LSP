;;;-----------------------------------------------------------------------;;;
;;;                   Original lisp provided by Julian Laskey             ;;;
;;;                         Modified by Mark Evinger                      ;;;
;;;-----------------------------------------------------------------------;;;
;;; revised 19:59:21 Thursday, 06-23-1994 to include FLAT_LAND and better ;;;
;;; error correction.                                                     ;;;
;;;-----------------------------------------------------------------------;;;
;;; revised 14:41:07 Friday, 06-02-1995 to account for AUNITS in MODESAVE ;;;
;;; also, changed the way segment distances are input                     ;;;
;;;-----------------------------------------------------------------------;;;
;;; revised 10:39:14 Friday, 08-25-1995 to fix the hard-coded 40' segment ;;;
;;; length when asked for segment length.                                 ;;;
;;;-----------------------------------------------------------------------;;;
;;; revised 08/13/98 11:53:43 AM to account for OSMODE concerns, also     ;;;
;;; modified prompts and other cosmetic changes                           ;;;
;;;-----------------------------------------------------------------------;;;
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
;;;-----------------------------------------------------------------------;;;
;;;                                                                       ;;;
;;; revised 3/19/2007 to use serial number check.                         ;;; v3.0.2

(defun C:PSAN ()
;                  (if (cgl_check_serial)
;                     (progn
  (graphscr)
  (setq VERSION "3.0.2"
        REDRAW-COMMAND
         ".REDRAWALL"
  ) ;_ end setq
  (princ
    (strcat
      "\r PSAN version "
      VERSION
      " -- copyright © Cowhey Gudmundson Leder, Ltd. 1995-2007. "
    ) ;_ end strcat
  ) ;_ end princ
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
  (defun PSAN_ERR (S) ;If an error (such as CTRL-C) occurs
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
          "      C A U T I O N\n You are working in 3D!!\nPSAN.LSP will strip out all\n       Z-elevations"
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
;;;-----------------------------------------------------------------------;;;
;;;                             Main Routine                              ;;;
;;;-----------------------------------------------------------------------;;;

  (setq CURRENT_LAYER (getvar "clayer"))
  (setq CGL_OSMODE (getvar "OSMODE"))

  (setq OLD_ERR *ERROR* ;use special error handling function
        *ERROR* PSAN_ERR
  ) ;_ end setq

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
  (CGL_LINETYPE_LOAD "cgl_PSAN") ; loads from the CGL_SHP.LIN file
  (CGL_LINETYPE_LOAD "cgl_PSAN_Small") ; loads from the CGL_SHP.LIN file

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

  (if (= (tblsearch "layer" "4P_SAN") NIL)
 ;(command "layer" "n" "4P_SAN" "c" "2" "4P_SAN" "")
    (command "_layer" "_n" "4P_SAN" "c" "2" "4P_SAN" "_LT" "CGL_PSAN" "4P_SAN" "") ; uses new CGL_STM linetype
  ) ;_ end if
  (if (= (tblsearch "layer" "4P_SANMH") NIL)
    (command "layer" "n" "4P_SANMH" "c" "114" "4P_SANMH" "")
  ) ;_ end if
 ;  (if (= (tblsearch "layer" "4P_SANARR") NIL)
 ;    (command "layer" "n" "4P_SANARR" "c" "114" "4P_SANARR" "")
 ;  ) ;_ end if

  (setq MH "P_SANMH")
  (setq TAIL "P_SANARR")
  (setq RAD (* SCAL 0.0625))
 ;(setq SEGIN 60.0)
  (setq CHKDIST 0) ;zero out CHKDIST

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
  (setq P2 (getpoint P1 "\n Pick next point: "))
  (setvar "OSMODE" 0)
  (FLAT_LAND)
  (setq CHKDIST (distance P1 P2))
  (setq DIR (angle P1 P2))
  (setq INSDIR (angtos DIR 0 4))
  (print)
  (setq ANS NIL)
  (initget "Y N")
  (setq
    ANS (getkword
          "\n Insert manhole at first point? - (Y)es or (N)o: <Y> "
        ) ;_ end getkword
  ) ;_ end setq
  (if (or (= ANS "N") (= ANS "n"))
    (setq D1 P1)
    (progn
      (command "layer" "s" "4P_SANMH" "")
      (command "insert" MH P1 SCAL "" INSDIR)
      (setq D1 (polar P1 DIR RAD))
      (command "layer" "s" CURRENT_LAYER "")
    ) ;_ end progn
  ) ;_ end if

  (print)
  (setq ANS NIL)
  (initget "Y N")
  (setq ANS (getkword
              "\n Insert manhole at last point? - (Y)es or (N)o: <Y> "
            ) ;_ end getkword
  ) ;_ end setq
  (if (or (= ANS "N") (= ANS "n"))
    (progn
      (setq D2 P2)
      (setq LASTMH "N")
    ) ;_ end progn
    (progn
      (command "layer" "s" "4P_SANMH" "")
      (command "insert" MH P2 SCAL "" INSDIR)
      (setq D2 (polar P2 (+ DIR pi) RAD))
      (setq LASTMH "Y")
      (command "layer" "s" CURRENT_LAYER "")
    ) ;_ end progn
  ) ;_ end if

  (command "layer" "s" "4P_SAN" "")
  (if (<= CHKDIST 50.0)
    (setvar "CELTYPE" "CGL_PSAN_Small")
  )
  (command "line" D1 D2 "")
  (setq CHKDIST 0) ; re-zero chkdist
  (setvar "CELTYPE" CGL_CELTYPE) ; linetype back to original value
  (command "layer" "s" CURRENT_LAYER "") ; layer back to original value

  (setq P1 P2)
  (setvar "OSMODE" CGL_OSMODE)

  (while
    (setq P2 (getpoint P1 "\n Pick next point: "))
     (setvar "OSMODE" 0)
     (FLAT_LAND)
     (setq CHKDIST (distance P1 P2))
     (setq DIR (angle P1 P2))
     (setq INSDIR (angtos DIR 0 4))
     (print)
     (if (= LASTMH "N")
       (setq D1 P1)
     ) ;_ end if
     (if (= LASTMH "Y")
       (setq D1 (polar P1 DIR RAD))
     ) ;_ end if

     (print)
     (setq ANS NIL)
     (initget "Y N")
     (setq
       ANS (getkword
             "\n Insert manhole at last point? - (Y)es or (N)o: <Y> "
           ) ;_ end getkword
     ) ;_ end setq
     (if (or (= ANS "N") (= ANS "n"))
       (progn
         (setq D2 P2)
         (setq LASTMH "N")
       ) ;_ end progn
       (progn
         (command "layer" "s" "4P_SANMH" "")
         (command "insert" MH P2 SCAL "" INSDIR)
         (setq D2 (polar P2 (+ DIR pi) RAD))
         (setq LASTMH "Y")
         (command "layer" "s" CURRENT_LAYER "")
       ) ;_ end progn
     ) ;_ end if

     (command "layer" "s" "4P_SAN" "")
     (if (<= CHKDIST 50.0)
       (setvar "CELTYPE" "CGL_PSAN_Small")
     )
     (command "line" D1 D2 "")
     (setq CHKDIST 0) ; re-zero chkdist
     (setvar "CELTYPE" CGL_CELTYPE) ; linetype back to original value
     (command "layer" "s" CURRENT_LAYER "") ; layer back to original value

     (setq P1 P2)
     (setvar "OSMODE" CGL_OSMODE)
  ) ;_ end while
  (command "LAYER" "S" CURRENT_LAYER "")
  (MODER)
  (print)
  (print)
  (princ)
  (princ)
;                      );end checker progn
;                        (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
;                            );end if
;                         (princ)
) ;end psan