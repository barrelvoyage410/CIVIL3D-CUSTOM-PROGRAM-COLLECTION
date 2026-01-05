;;;          created 08-25-1995 by Mark Evinger, cis # 73162,1676         ;;;
;;;         draws wavy pipe ends on profile pipes to CGL standards        ;;;
;;;-----------------------------------------------------------------------;;;
;;;                                                                       ;;;
;;;revised 02/25/99 11:28:20 AM for prompts and cosmetic changes. Checked ;;;
;;;OSMODE settings, making sure the MODER function works as advertised.   ;;;

;;; revised 03-20-2007 to check for serial number

(defun C:PIPE_END ()
;                      (if (cgl_check_serial)
;                      (progn
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
  (defun PIPE_END_ERR (S) ;If an error (such as CTRL-C) occurs
    (if (/= S "Function cancelled") ;while this command is active...
      (princ (strcat "\nError: " S))
    ) ;_ end if
    (setq *ERROR* OLD_ERR) ;Restore old *error* handler
    (MODER) ;return modes to old values
    (princ)
  ) ;_ end defun
 ;---------------------------------------------------------------------------
  (MODESAVE '("BLIPMODE" "CMDECHO" "COORDS" "DIMZIN" "GRIPS" "OSMODE" "AUNITS")
  ) ;save modes
  (setvar "BLIPMODE" 0) ;turn off blips
  (setvar "CMDECHO" 0) ;turn off command echo
  (setvar "COORDS" 2) ;sets coords to show distance & angle
 ;  (setvar "DIMZIN" 0) ;turn off dimzin (controls leading/trailing 0's)
 ;  (setvar "GRIPS" 0) ;turn off grips
 ;  (setvar "OSMODE" 0) ;turn off osnaps
 ;  (setvar "AUNITS" 0) ;set angular units to degrees only
 ;---------------------------------------------------------------------------
  (setq OLD_ERR *ERROR* ;use special error handling function
        *ERROR* PIPE_END_ERR
  ) ;_ end setq
  (setq CGL_OSMODE (getvar "OSMODE"))
  (setvar "OSMODE" 1)
 ;---------------------------------------------------------------------------
  (while
    (setq P1 (getpoint "\nPick end of pipe: "))
     (setq P2 (getpoint P1 "\nPick other end: "))
     (setvar "osmode" 0)
     (setq SCL (distance P1 P2))
     (command ".INSERT" "PIPE_END" P1 SCL "" P2)
     (setvar "OSMODE" 1)
  ) ;_ end while
 ;---------------------------------------------------------------------------
  (MODER)
                (setvar "OSMODE" CGL_OSMODE)
  (princ)
;                  );end checker progn
;                  (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
;                  );end if
;                 (princ)  
) ;_ end DEFUN PIPE_END