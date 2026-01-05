;;; revised 03-20-2007 to include serial number checking           ;;; ME
;;; revised 08-10-2007 to work under C3D2008, text command requires extra "" for input.  ME;;;


(defun C:ZPNT () ; Z-PRINT WITH NO LINE TRIMMING
  (if (cgl_check_serial)
    (progn
  (defun *ERROR* (MSG)
    (RESET)
    (prompt (strcat "\N\N" MSG "     "))
  ) ; DEFUN
  (setq OSM (getvar "OSMODE"))
  (setq ORTHO (getvar "ORTHOMODE"))
  (setvar "ORTHOMODE" 0)
  (setvar "BLIPMODE" 0)
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 512) ; SETS OSNAP TO "NEAREST"
  (while
    (setq P1 (getpoint "\nSelect contour: "))
     (setvar "OSMODE" OSM) ; RESETS OSNAP TO ORIGINAL
     (setq NUM (rtos (caddr P1) 2 0))
     (setq TXT NUM)
     (if (= NUM "0")
       (prompt (setq TXT (getstring "Contour ELEVATION = ? ")))
     ) ; END IF
     (princ "\nText rotation angle: ")
     (command "._TEXT" "J" "M" P1 PAUSE TXT "")
     (setq TXTEL (caddr P1))
     (setq DIFF (- 0 TXTEL))
     (setq NEWELV (strcat "@0,0," (rtos DIFF 2 4)))
     (command "._MOVE" "L" "" "0,0,0" NEWELV)
     (setvar "OSMODE" 512) ; SETS OSNAP TO "NEAREST"
  ) ; END WHILE
  (setvar "ORTHOMODE" ORTHO)
  (setvar "OSMODE" OSM)
  (princ)
    );end checker progn
    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
    );end if
    (princ)

) ;END ZPNT