;;;-----------------------------------------------------------------------;;;
;;;                   Original lisp provided by Julian Laskey             ;;;
;;;                         Modified by Mark Evinger                      ;;;
;;;-----------------------------------------------------------------------;;;
;;; revised to ver. 1.01 - includes storm inlet cleanouts 8-2-1993 ME     ;;;
;;;-----------------------------------------------------------------------;;;
;;; revised to ver 2.0  6-14-94 to filter out z-elevations (flat_land)    ;;;
;;;-----------------------------------------------------------------------;;;
;;; revised 12:08:21 Friday, 06-17-1994 to include better error-checkers  ;;; v2.1
;;;-----------------------------------------------------------------------;;;
;;; revised 14:19:37 Friday, 06-02-1995 to ver 2.1.1 To account for angle ;;;
;;; units (specifically RADS) and also changed the way you input segment  ;;;
;;; distances to let user pick 2 points to input a distance in dwg units. ;;;
;;; revised 09:58:28 Friday, 08-25-1995 to fix the hard-coded 40' segment ;;;
;;; length when asked for segment length.                                 ;;; v2.1.1
;;;-----------------------------------------------------------------------;;;
;;; REVISED 07/24/98 5:20:32 PM to check for USERR2 /= LTSCALE for R14    ;;; v2.1.2
;;;-----------------------------------------------------------------------;;;
;;; revised 08/13/98 12:26:03 AM to account for OSMODE concerns           ;;; v2.1.4
;;;-----------------------------------------------------------------------;;;
;;; revised 12/8/99 8:37:06 AM to change layer colors                     ;;; v2.1.6
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
;;; revised 03-20-2007 to check for serial number                         ;;; v3.0.2
;;; revised 01-21-2008 to use new e_stmfes and p_stmfes insertion point   ;;; v3.0.4
(defun C:STM ()
;                  (if (cgl_check_serial)
;                  (progn
  (graphscr)
;;; This bit is designed more for a dual monitor system, as the version will
;;; scroll off a three line display....
  (setq VERSION "3.0.4")
  (princ
    (strcat
      "\r STM version "
      VERSION
      " -- copyright © Cowhey Gudmundson Leder, Ltd. 1993-2008. "
    ) ;_ end strcat
  ) ;_ end princ
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
  (defun STM_ERR (S) ;If an error (such as CTRL-C) occurs
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
          "      C A U T I O N\n You are working in 3D!!\nSTM.LSP will strip out all\n       Z-elevations"
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
 ;structure insert questions - first structure
 ;---------------------------------------------------------------------------
  (defun STRUCTURE_FIRST ()
    (print)
    (setq ANS NIL)
    (initget "Y N")
    (setq ANS
           (getkword
             "\n Insert structure at first point? - (Y)es or (N)o: <Y> "
           ) ;_ end getkword
    ) ;_ end setq
    (if (or (= ANS "N") (= ANS "n"))
      (setq D1 P1)
      (progn
        (print)
        (setq ANS2 NIL)
        (initget "M C I F S CL")
        (setq ANS2
               (getkword
                 "\n Insert (M)anhole,(C)atch basin,(I)nlet,(CL)eanout,(F)lared end or (S)tub ?: <M> "
               ) ;_ end getkword
        ) ;_ end setq
        (if (= ANS2 NIL)
          (setq ANS2 "M")
        ) ;_ end if
        (cond
          ((= ANS2 "M")
           (command "layer" "s" MHLYR "")
           (command "insert" MH P1 SCAL "" INSDIR)
           (setq D1 (polar P1 DIR RAD))
           (command ".LAYER" "S" CURRENT_LAYER "")
          ) ; end manhole
          ((= ANS2 "C")
           (command "layer" "s" CBLYR "")
           (command "insert" CB P1 SCAL "" INSDIR)
           (setq D1 (polar P1 DIR RAD))
           (command ".LAYER" "S" CURRENT_LAYER "")
          ) ; end catch basin
          ((= ANS2 "I")
           (setvar "OSMODE" CGL_OSMODE)
           (setq INLDIR
                  (getpoint P1
                            (strcat "\n Indicate rotation for inlet: ")
                  ) ;_ end getpoint
           ) ;_ end setq
           (setvar "OSMODE" 0)
           (command "layer" "s" CBLYR "")
           (command "insert"
                    INL
                    P1
                    SCAL
                    ""
                    (angtos (angle P1 INLDIR) 0 4)
           ) ;_ end command
           (setq D1 (polar P1 DIR RAD))
           (command ".LAYER" "S" CURRENT_LAYER "")
          ) ; end inlet
          ((= ANS2 "CL")
           (setvar "OSMODE" CGL_OSMODE)
           (setq
             INLDIR (getpoint
                      P1
                      (strcat "\n Indicate rotation for cleanout: ")
                    ) ;_ end getpoint
           ) ;_ end setq
           (setvar "OSMODE" 0)
           (command "layer" "s" CBLYR "")
           (command "insert"
                    INLCO
                    P1
                    SCAL
                    ""
                    (angtos (angle P1 INLDIR) 0 4)
           ) ;_ end command
           (setq D1 (polar P1 DIR RAD))
           (command ".LAYER" "S" CURRENT_LAYER "")
          ) ; end storm cleanout
          ((= ANS2 "F")
           (setq D1 (polar P1 DIR (* 0.12 SCAL)))
           (command "layer" "s" CBLYR "")
           (command "insert" FES P1 SCAL "" INSDIR);changed insert point to P1 from D1 to reflect new FES insertion point for C3D point styles
           (command ".LAYER" "S" CURRENT_LAYER "")
          ) ; end flared end section
          ((= ANS2 "S")
           (setq D1 P1)
           (command "layer" "s" STMLYR "")
           (command "insert" "STUB" P1 SCAL "" INSDIR)
           (command ".LAYER" "S" CURRENT_LAYER "")
          ) ;end stub
        ) ;cond structure YES
      ) ;orig if y/n struc insert
    ) ;end if
    (setvar "OSMODE" CGL_OSMODE)
  ) ;end structure_first
 ;---------------------------------------------------------------------------
 ;                       REPEATING STORM SEWER ROUTINE
 ;---------------------------------------------------------------------------
  (defun STMLOOP ()
    (setvar "OSMODE" 0) ;make sure OSMODE is off for line draws
    (print)
    (setq ANS NIL)
    (initget "Y N")
    (setq
      ANS (getkword
            "\n Insert structure at last point? - (Y)es or (N)o: <Y> "
          ) ;_ end getkword
    ) ;_ end setq
    (if (or (= ANS "N") (= ANS "n"))
      (setq D2 P2) ;if NO to insert structure
 ;else YES, then insert what kind of structure?
      (progn
        (print)
        (setq ANS2 NIL)
        (initget "M C I F S CL")
        (setq ANS2
               (getkword
                 "\n Insert (M)anhole,(C)atch basin,(I)nlet,(CL)leanout,(F)lared end or (S)tub ?: <M> "
               ) ;_ end getkword
        ) ;_ end setq
        (if (= ANS2 NIL)
          (setq ANS2 "M")
        ) ;_ end if
        (cond
          ((= ANS2 "M")
           (command "layer" "s" MHLYR "")
           (command "insert" MH P2 SCAL "" INSDIR)
           (setq D2 (polar P2 (+ DIR pi) RAD))
           (command ".LAYER" "S" CURRENT_LAYER "")
          ) ; end manhole
          ((= ANS2 "C")
           (command "layer" "s" CBLYR "")
           (command "insert" CB P2 SCAL "" INSDIR)
           (setq D2 (polar P2 (+ DIR pi) RAD))
           (command ".LAYER" "S" CURRENT_LAYER "")
          ) ; end catch basin
          ((= ANS2 "I")
           (setvar "OSMODE" CGL_OSMODE)
           (setq INLDIR
                  (getpoint P2
                            (strcat "\n Indicate rotation for inlet: ")
                  ) ;_ end getpoint
           ) ;_ end setq
           (setvar "OSMODE" 0)
           (command "layer" "s" CBLYR "")
           (command "insert"
                    INL
                    P2
                    SCAL
                    ""
                    (angtos (angle P2 INLDIR) 0 4)
           ) ;_ end command
           (setq D2 (polar P2 (+ DIR pi) RAD))
           (command ".LAYER" "S" CURRENT_LAYER "")
          ) ; end inlet
          ((= ANS2 "CL")
           (setvar "OSMODE" CGL_OSMODE)
           (setq
             INLDIR (getpoint
                      P2
                      (strcat "\n Indicate rotation for cleanout: ")
                    ) ;_ end getpoint
           ) ;_ end setq
           (setvar "OSMODE" 0)
           (command "layer" "s" CBLYR "")
           (command "insert"
                    INLCO
                    P2
                    SCAL
                    ""
                    (angtos (angle P2 INLDIR) 0 4)
           ) ;_ end command
           (setq D2 (polar P2 (+ DIR pi) RAD))
           (command ".LAYER" "S" CURRENT_LAYER "")
          ) ; end storm cleanout
          ((= ANS2 "F")
           (setq D2 (polar P2 (+ DIR pi) (* 0.12 SCAL)))
           (command "layer" "s" CBLYR "")
           (command "insert" FES P2 SCAL "" (angtos (+ DIR pi) 0 4));changed FES insert to P2 from D2 to reflect new block insertion point for C3D label styles
           (command ".LAYER" "S" CURRENT_LAYER "")
          ) ; end flared end section
          ((= ANS2 "S")
           (setq D2 P2)
           (command "layer" "s" STMLYR "")
           (command "insert" "STUB" P2 SCAL "" (angtos (+ DIR pi) 0 4))
           (command ".LAYER" "S" CURRENT_LAYER "")
          ) ; end stub
        ) ;end cond (structure=yes)
      ) ;orig if y/n struc insert
    ) ;end if
    (command "layer" "s" STMLYR "")
    (if (<= CHKDIST 50)
      (progn
        (setvar "CELTYPE" "CGL_STM_small")
        (command "._line" D1 D2 "")
        (setvar "CELTYPE" "BYLAYER")
        (setq CHKDIST 0)
      ) ;end progn
      (command "._line" D1 D2 "")
    ) ;end if
    (command ".LAYER" "S" CURRENT_LAYER "")
    (setvar "OSMODE" CGL_OSMODE)
  ) ;end stmloop
 ;---------------------------------------------------------------------------
 ;                       MAIN STORM SEWER ROUTINE
 ;---------------------------------------------------------------------------
  (defun DRAWSTM ()
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
    (setq DIR (angle P1 P2))
    (setq INSDIR (angtos DIR 0 4))
    (setq CHKDIST (distance P1 P2))
    (setq CHKDIST_TEMP CHKDIST)
    (print)
    (setvar "OSMODE" 0)
    (setq SEG (* SCAL (/ SEGIN SCAL)))
    (setq GAP (* SCAL 0.0625))
    (setq RAD (* SCAL 0.0625))

    (STRUCTURE_FIRST) ; go ask is there a structure at start point

    (setvar "OSMODE" 0)

    (STMLOOP) ; draw the linework and ask structure at end point

    (setvar "OSMODE" CGL_OSMODE)
    (while (setq NXTPT (getpoint P2 "\n Pick next point: "))
      (setq P1 P2)
      (setq P2 NXTPT)
      (setvar "OSMODE" 0)
      (FLAT_LAND)
      (setq CHKDIST (distance P1 P2))
      (setq DIR (angle P1 P2))
      (setq INSDIR (angtos DIR 0 4))
      (print)
      (setvar "OSMODE" CGL_OSMODE)
      (if (or (= ANS "N") (= ANS2 "F") (= ANS2 "S"))
        (setq D1 P1)
        (setq D1 (polar P1 DIR RAD))
      ) ;_ end if
      (setq D2 (polar D1 DIR SEG))

      (STMLOOP) ; draw the linework and ask structure at end point

    ) ;end while
    (princ)
  ) ; end defun drawstm
 ;---------------------------------------------------------------------------
 ;                       MAIN SEWER DRAWING ROUTINE
 ;---------------------------------------------------------------------------
  (setq CURRENT_LAYER (getvar "CLAYER"))
  (setq CGL_OSMODE (getvar "OSMODE"))
  (setq OLD_ERR *ERROR* ; use special error handling function
        *ERROR* STM_ERR
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

  (CGL_LINETYPE_LOAD "cgl_stm") ; loads from the CGL_SHP.LIN file
  (CGL_LINETYPE_LOAD "cgl_stm_small") ; loads from the CGL_SHP.LIN file

  (if (= (tblsearch "layer" "4E_STM") NIL)
    (command "_layer" "_n" "4E_STM" "_c" "30" "4E_STM" "_LT" "cgl_stm" "4E_STM" "") ; uses new CGL_STM linetype
  ) ;_ end if
  (if (= (tblsearch "layer" "4P_STM") NIL)
    (command "_layer" "_n" "4P_STM" "_c" "2" "4P_STM" "_LT" "cgl_stm" "4P_STM" "") ; uses new CGL_STM linetype
  ) ;_ end if
  (if (= (tblsearch "layer" "4P_STMINL") NIL)
    (command "layer" "n" "4P_STMINL" "c" "114" "4P_STMINL" "")
  ) ;_ end if
  (if (= (tblsearch "layer" "4P_STMMH") NIL)
    (command "layer" "n" "4P_STMMH" "c" "114" "4P_STMMH" "")
  ) ;_ end if
;;;  (if (= (tblsearch "layer" "4P_STMARR") NIL)
;;;    (command "layer" "n" "4P_STMARR" "c" "114" "4P_STMARR" "")
;;;  ) ;_ end if
;;; no more storm arrows with new linetypes
  (setq SEGIN 40.0) ; set basic segment length for checking purposes
  (setq CHKDIST 0) ; zero out CHKDIST

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
    ) ;end progn
  ) ;_ end if
  (setq SCAL (getvar "userr2"))

  (princ "\n      (1)=Ex. STORM // (2)=Prop. STORM ")
  (print)
  (if (or (= UTOPT NIL) (and (/= UTOPT 1) (/= UTOPT 2)))
    (progn
      (initget (+ 1 2 4))
      (setq UTOPT
             (getint "\n      Draw sewer lines - enter type 1 or 2 : ")
      ) ;_ end setq
      (while (> UTOPT 2)
        (progn
          (print)
          (princ
            "\n      **** Your choice was not 1 or 2!!  Please try again!! **** "
          ) ;_ end princ
          (princ "\n      (1)=Ex. STORM // (2)=Prop. STORM ")
          (initget (+ 1 2 4))
          (setq
            UTOPT (getint
                    "\n      Draw sewer lines - enter type 1 or 2 : "
                  ) ;_ end getint
          ) ;_ end setq
        ) ;end progn
      ) ;end while
    ) ;end progn
    (progn
      (initget (+ 2 4))
      (setq ANS
             (getint
               (strcat "\n      Draw sewer lines - enter type 1 or 2  << "
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
          (princ "\n      (1)=Ex. STORM // (2)=Prop. STORM ")
          (initget (+ 1 2 4))
          (setq
            UTOPT (getint
                    "\n      Draw sewer lines - enter type 1 or 2 : "
                  ) ;_ end getint
          ) ;_ end setq
        ) ;end progn
      ) ;end while
    ) ;end progn
  ) ;end if

  (cond
    ((= UTOPT 1)
     (setq MH "E_STMMH")
     (setq CB "E_STMCB")
     (setq INL "E_STMINL")
     (setq INLCO "E_STMCLO")
     (setq FES "E_STMFES")
     (setq TAIL "E_STMARR")
     (setq STMLYR "4E_STM") ;THESE ARE
     (setq MHLYR "4E_STM") ;THE LAYERS
     (setq CBLYR "4E_STM") ;FOR THE
     (setq ARRLYR "4E_STM") ;ABOVE BLOCKS
     (DRAWSTM)
    ) ;end existing option

    ((= UTOPT 2)
     (setq MH "P_STMMH")
     (setq CB "P_STMCB")
     (setq INL "P_STMINL")
     (setq INLCO "P_STMCLO")
     (setq FES "P_STMFES")
     (setq TAIL "P_STMARR")
     (setq STMLYR "4P_STM") ;THESE ARE
     (setq MHLYR "4P_STMMH") ;THE
     (setq CBLYR "4P_STMINL") ;LAYERS FOR
     (setq ARRLYR "4P_STMARR") ;THE ABOVE BLOCKS
     (DRAWSTM)
    ) ;end proposed option

  ) ;end cond

  (print)
 ;---------------------------------------------------------------------------
 ;ending stuff
 ;---------------------------------------------------------------------------
  (MODER) ; return modes to previous values
  (command "._LAYER" "S" CURRENT_LAYER "")
  (princ) ;exit quietly
  (princ) ;...even more quietly
;                        );end checker progn
;                       (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
;                         );end if
;                        (princ)  
) ;end c:stm