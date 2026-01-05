 ; Created by Mark Evinger                               CIS id 73162,1676
 ; revised 11-3-93 by Mark Evinger to include "existing" and "proposed"
 ; feature for text; existing in parenthesis, proposed not.
 ; revised 03-23-94 to add choice of TC or FL in spot elevations
 ; revised 09:45:56 Friday, 04-01-1994 to add rim option
 ; revised 10:35:12 Thursday, 05-18-1995 to fix null entry on "x" rotation
 ; revised 8/4/2003 to toggle osnaps for the grad_X placement and text placement - v1.15
 ; revised 02-27-2004 to add EP option - v1.16
 ; revised 03-01-2004 to do 3d GRAD_X block insertions - v1.20
 ; revised 07-13-2006 to add "TDC=" option for labelling - v1.22 ME
 ; revised 03-20-2007 to check for serial number - v 1.24 ME
 ;-------------------------------------------------------------------------
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
(defun gradlbl_err (S) ;If an error (such as CTRL-C) occurs
  (if (/= S "Function cancelled") ;while this command is active...
    (princ (strcat "\nError: " S))
  ) ;_ end if
  (setq *ERROR* OLD_ERR) ;Restore old *error* handler
  (MODER) ;return modes to old values
  (princ)
) ;_ end defun
 ;---------------------------------------------------------------------------

(defun EX_PR ()
  (if (null OPT2)
    (setq OPT2 "PR")
  )
  (initget "EX PR")
  (setq ANS2 (getkword
               (strcat "\n   (EX)isting or (PR)roposed grades?  >> "
                       OPT2
                       " << "
               )
             )
  )
  (if (not (null ANS2))
    (setq OPT2 ANS2)
  )
  (print)
) ; end defun ex_pr
 ;-------------------------------------------------------------------------

(defun 2D_3D ()
  (if (null OPT2d)
    (setq OPT2d "3D")
  )
  (initget "2D 3D")
  (setq ANS2d (getkword
                (strcat "\n   (2D) or (3D) elevations on GRAD_X blocks?  >> " OPT2d " << ")
              )
  )
  (if (not (null ANS2d))
    (setq OPT2d ANS2d)
  )
  (print)
) ; end defun 2D_3D
 ;-------------------------------------------------------------------------

(defun GRADLBL ()
  (setq VERSION "1.24"
        REDRAW-COMMAND
         ".REDRAWALL"
  )
  (princ
    (strcat
      "\r GRADLBL version "
      VERSION
      " -- copyright © Cowhey & Associates 1993-2007."
    )
  )
  (princ
    "\nPrefix \"T\" for TC=; \"D\" for TDC=; \"F\" for FL=; \"R\" for RIM=; and \"E\" for EP=;"
  )
 ;(princ "\n  Use \" F \" in front of elevation for flowline shots")
  (princ "\n  T789.55 or D789.00 or F555.33 or R699.36 or E700.50 etc.")
;;;     (princ "\nUse \"T\" in front of elevation for top of curb shots; use \"F\" for flowline shots,")
;;;     ;(princ "\n  Use \" F \" in front of elevation for flowline shots")
;;;     (princ "\nUse \"R\" in front of elevation for rim grades and \"E\" for Edge of pavement.  T789.55 or F555.33 or R699.36, E700.50 etc.")
  (ex_pr) ;existing or proposed text
  (2d_3d) ;construct in 2d or 3d
  (setq SCL (getvar "LTSCALE"))
;;;
;;;     main program bit...
;;;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  (setq CURRENT_LAYER (getvar "CLAYER"))
  (setq CGL_OSMODE (getvar "OSMODE"))
  (setq OLD_ERR *ERROR* ; use special error handling function
        *ERROR* gradlbl_err
  ) ;_ end setq
  (MODESAVE '("BLIPMODE" "CMDECHO" "COORDS" "DIMZIN" "GRIPS" "OSMODE" "AUNITS" "CELTYPE"
             )
  ) ;save modes
  (setvar "BLIPMODE" 0) ;turn off blips
  (setvar "CMDECHO" 0) ;turn off command echo
  (setvar "COORDS" 2) ;sets coords to show distance & angle
  (setvar "DIMZIN" 0) ;turn off dimzin (controls leading/trailing 0's)
 ;  (setvar "GRIPS" 0) ;turn off grips
 ;  (setvar "OSMODE" 0) ;turn off osnaps
  (setvar "AUNITS" 0) ;set angular units to degrees only
  (setvar "CELTYPE" "BYLAYER") ; set CurrentEntity Linetype to bylayer  
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  (cond
    ((= OPT2 "PR")
     (setq CGL_OSMODE (getvar "OSMODE")) ; gather snap mode
     (while
       (setq P1 (getpoint "\n Insertion point of 'X': "))
        (initget 1)
        (setq P2 (getpoint P1 "\n Rotation of 'X': "))
        (princ)
        (command ".insert" "GRAD_X" P1 SCL "" P2)
        (princ)
        (setq NUM (getstring T "\nSpot grade elevation = ? "))
        (setq TFR (strcase (substr NUM 1 1)))
        (setq NUM_1 (substr NUM 2))
        (princ)
        (cond
          ((= TFR "T") ;is it TC?
           (setq TXT (strcat "TC=" NUM_1))
          )
          ((= TFR "F") ; is it FL?
           (setq TXT (strcat "FL=" NUM_1))
          )
          ((= TFR "R") ;is it RIM?
           (setq TXT (strcat "RIM=" NUM_1))
          )
          ((= TFR "E") ;is it EP?
           (setq TXT (strcat "EP=" NUM_1))
          )
          ((= TFR "D") ;is it TDC?
           (setq TXT (strcat "TDC=" NUM_1))
          )
          (t
           (setq TXT NUM) ; not TC FL or RIM
           (setq NUM_1 NUM) ; make NUM_1 = NUM for use in the 3d point below
          )
        ) ; end cond
        (setvar "OSMODE" 0) ; no snaps
        (setq P3 (getpoint "\n Insertion point of text = ? "))
        (if (= OPT2d "3D")
          (progn
            (command "._erase" "L" "") ;remove the just-created GRAD_X block
            (setq P1 (list (car p1) (cadr p1) (atof num_1))) ; make P1 a 3d point
            (command ".insert" "GRAD_X" P1 SCL "" P2) ; insert a new GRAD_X block in 3d
            (command ".TEXT" P3 pause TXT) ; place the text string
          ) ;end progn
          (command ".TEXT" P3 pause TXT) ; else just insert the text
        ) ;end if
        (setvar "OSMODE" CGL_OSMODE) ;restore snaps
        (princ)
     ) ; end while
    ) ; end cond pr
    ((= OPT2 "EX")
     (setq CGL_OSMODE (getvar "OSMODE")) ;gather OSNAP mode
     (while
       (setq P1 (getpoint "\n Insertion point of 'X': "))
        (initget 1)
        (setq P2 (getpoint P1 "\n Rotation of 'X': "))
        (princ)
        (command ".insert" "GRAD_X" P1 SCL "" P2)
        (princ)
        (setq NUM (getstring T "\nSpot grade elevation = ? "))
        (setq TFR (strcase (substr NUM 1 1)))
        (setq NUM_1 (substr NUM 2))
        (princ)
        (cond
          ((= TFR "T") ;is it TC?
           (setq TXT (strcat "(" "TC=" NUM_1 ")"))
          )
          ((= TFR "F") ; is it FL?
           (setq TXT (strcat "(" "FL=" NUM_1 ")"))
          )
          ((= TFR "R") ;is it RIM?
           (setq TXT (strcat "(" "RIM=" NUM_1 ")"))
          )
          ((= TFR "E") ;is it EP?
           (setq TXT (strcat "(" "EP=" NUM_1 ")"))
          )
          ((= TFR "D") ;is it TDC?
           (setq TXT (strcat "(" "TDC=" NUM_1 ")"))
          )
          (t
           (setq TXT (strcat "(" NUM ")")) ; not TC FL or RIM
           (setq NUM_1 NUM) ; make NUM_1 = NUM for use in the 3d point below           
          )
        ) ; end cond
        (setvar "OSMODE" 0) ;no snaps
        (setq P3 (getpoint "\n Insertion point of text = ? "))
        (if (= OPT2d "3D")
          (progn
            (command "._erase" "L" "") ;remove the just-created GRAD_X block
            (setq P1 (list (car p1) (cadr p1) (atof num_1))) ; make P1 a 3d point
            (command ".insert" "GRAD_X" P1 SCL "" P2) ; insert a new GRAD_X block in 3d
            (command ".TEXT" P3 pause TXT) ;place the text string
          ) ;end progn
          (command ".TEXT" P3 pause TXT) ; else just insert the text
        ) ;end if
        (setvar "OSMODE" CGL_OSMODE) ; restore snaps
        (princ)
     ) ; end while
    ) ; end cond ex
  ) ;end cond
  (moder) ;return saved values  
  (princ)
  (princ)
  (princ)
  (princ)

) ; end GRADLBL
;;;
;;; c: definitions
;;;
(defun c:gl ()
  (if (cgl_check_serial)
    (progn
  (vmon)
  (gradlbl)
    );end checker progn
    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
    );end if
    (princ)  
  )
(defun c:gradlbl ()
  (if (cgl_check_serial)
    (progn
  (vmon)
  (gradlbl)
    );end checker progn
    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
    );end if
    (princ)  
  )
(princ
  "\n\tc:GRADLBL loaded.  Start command with GL or GRADLBL."
)
(princ)
