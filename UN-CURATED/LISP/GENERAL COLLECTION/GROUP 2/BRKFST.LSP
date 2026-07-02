;;;-----------------------------------------------------------------------;;;
;;;                   Original lisp provided by Cadalyst Tip              ;;;
;;;                         Modified by Mark Evinger                      ;;;
;;;-----------------------------------------------------------------------;;;
;; THIS LISP IS FOR MULTIPLE BREAKS WITH THE F, (FIRST), OPTION
;; IT SHOULD WORK ON ACAD 10-12
;;; revised 15:06:07 Friday, 06-02-1995 to include AUNITS in MODESAVE     ;;;
;;; revised 14:57:42 Friday, 07-26-1996 to include while loop (repeating  ;;;
;;;         command)                                                      ;;;
;;; revised 03-20-2007 to check for serial number.  ME                    ;;;
;---------------------------------------------------------------------------
; save specified mode settings
;---------------------------------------------------------------------------
(defun MODESAVE (a_A)
  (setq MLST '())
  (repeat (length a_A)
    (setq MLST (append MLST (list (list (car a_A) (getvar (car a_A))))))
  (setq a_A (cdr a_A)))
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
(defun nwerr (s)                     ;If an error (such as CTRL-C) occurs
  (if (/= s "Function cancelled") ;while this command is active...
    (princ (strcat "\nError: " s))
  )
  (setq *error* old_err)          ;Restore old *error* handler
  (moder)                         ;return modes to old values
  (princ)
)
;---------------------------------------------------------------------------
;;; === BRKFST.LSP ===

(DEFUN C:BRKFST ()
  (if (cgl_check_serial)
    (progn
  (graphscr)
  (SETQ OLD_ERR *ERROR*
  *ERROR* NWERR)
  (modesave '("BLIPMODE" "CMDECHO" "COORDS" "DIMZIN" "GRIPS" "OSMODE" "AUNITS"));save modes
  (setvar "BLIPMODE" 0)    ;turn off blips
  (setvar "CMDECHO" 0)     ;turn off command echo
  (setvar "COORDS" 2)      ;sets coords to show distance & angle
  (setvar "DIMZIN" 0)      ;turn off dimzin (controls leading/trailing 0's)
  (setvar "GRIPS" 0)       ;turn off grips
  ;(setvar "OSMODE"  0)     ;turn off osnaps
  (setvar "AUNITS" 0)      ;set angular units to degrees only
  ; 14:57:02 Friday, 07-26-1996 ME
  ; added while loop to make this a repeating command
  (while
    (SETQ ENT1 (ENTSEL "\nSelect line to BREAK: ")
      ENT (CAR ENT1)
    )
    (REDRAW ENT 3)
    (SETQ PT2 (GETPOINT "\nFirst point : ")
      PT3 (GETPOINT "\nSecond point : ")
    )
    (COMMAND ".BREAK" ENT1 "F" PT2 PT3)
  ); end while
  (moder)
  (PRINC)
    );end checker progn
    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
    );end if
    (princ)

) ; END BRKFST

