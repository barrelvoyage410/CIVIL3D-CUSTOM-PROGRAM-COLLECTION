;;; PIPEVIEW - Draws ellipses for sewer pipe end views in profile.
;;; uses constant pipe width of 1.5, user-defined pipe height.
;;; Created by Mark Evinger   CompuServe ID 73162,1676     August 26, 1993
;;; final revisions 19:57:02 Thursday, 06-23-1994 -> text-touchups only;;;
;;;                                                                    ;;;
;;; revised 03-20-2007 to check for serial number  ME                  ;;;
;-------------------------------------------------------------------------
;         Get Pipe Size
;-------------------------------------------------------------------------
(defun GETSIZE ()
     (initget 6)
     (setq PIPESIZE (getreal (strcat "\nEnter PIPE SIZE in inches : ")))
     (if (not (= PIPESIZE nil))
          (progn
               (setq SCL (getvar "ltscale"))
               (setq HGT (/ PIPESIZE 12))
               (setq VFAC (/ SCL VSCAL))
               (setq PIPE_HGT (* HGT VFAC))
               (setq TOP_PNT (strcat "@"(rtos PIPE_HGT)"<90"))
               (setq SIDE_PNT "@1.5<0")
          ); end progn
     ); end if
); end defun getsize
;-------------------------------------------------------------------------
;         Get OPEN_CLOSE Information
;-------------------------------------------------------------------------
(defun OPEN_CLOSE ()
     (setq OPT "O")
     (initget "O C")
     (setq ANS (getkword (strcat "\nIs pipe (O)pen or (C)losed ?:>>"OPT"<< ")))
     (if (not (null ANS)) (setq OPT ANS))
); end defun open_close
;-------------------------------------------------------------------------
;         Draw Pipe Ellipse
;-------------------------------------------------------------------------
(defun DRAW_PIPE ()
     (cond
          ((= OPT "O")
               (setq INSPT (getpoint "\nSelect invert point of pipe: "))
               (command ".ELLIPSE" INSPT TOP_PNT SIDE_PNT)
          ); end opt "o"
          ((= OPT "C")
               (setq INSPT (getpoint "\nSelect invert point of pipe: "))
               (command ".ELLIPSE" INSPT TOP_PNT SIDE_PNT)
               (command "UCS" "W" "")
               (command ".HATCH" "LINE" "1" "45" "L" "")
               (command "UCS" "p" "")
          ); end opt "c"
     ); end condtion
); end defun draw_pipe
;;;=======================================================================
;;;     error handler
;;;=======================================================================
(defun pipeview_err (s)            ;If an error (such as CTRL-C) occurs
                                   ;while this command is active...
     (if (/= s "Function cancelled")
          (princ (strcat "\nError: " s))
     )
     (setq *error* old_err)        ;Restore old *error* handler
     (princ)
); end defun pipeview_err
;-------------------------------------------------------------------------
;         Main Program
;-------------------------------------------------------------------------
(defun PIPEVIEW ()
     (setq VERSION "1.02")
     (princ
          (strcat
               "\r PIPEVIEW version "VERSION
               " -- Copyright Cowhey Gudmundson Leder, Ltd. 1993-2007."
          ); end strcat
     ); end princ
     (setq TEMP_SCL (getvar "ltscale"))
     (setq TEMP_ANS (/ TEMP_SCL 10))
     (setq TEMP_ME (getreal (strcat "\n Enter VERTICAL SCALE: >>"(rtos TEMP_ANS 2 1)"<< ")))
     (if (not (null TEMP_ME)) (setq VSCAL TEMP_ME ) (setq VSCAL TEMP_ANS))
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     (setq old_err *error*          ; Use special error handling function.
     *error* pipeview_err)
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     (while
          (getsize)         ;<-- this while loop is the "guts"
          (open_close)      ;<-- of the lisp.... it calls the
          (draw_pipe)       ;<-- separate functions.
     );end while
     (princ)
     (princ)
     (princ)                ;<-- exit lisp "quietly"
); end pipeview defun
;;;-----------------------------------------------------------------------
;;; c: definitions
;;;-----------------------------------------------------------------------
(defun c:pv ()
  (if (cgl_check_serial)
    (progn
  (vmon)
  (pipeview)
    );end checker progn
    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
    );end if
    (princ)  
  )
(defun c:pipeview ()
  (if (cgl_check_serial)
    (progn
  (vmon)
  (pipeview)
    );end checker progn
    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
    );end if
    (princ)  
  )
(princ "\n\tc:PIPEVIEW loaded.  Start command with PV or PIPEVIEW.")
(princ)
;;; Happy Lisping!! The AutoCad Guru (aka The Cad Elf *<:)-|-< )
