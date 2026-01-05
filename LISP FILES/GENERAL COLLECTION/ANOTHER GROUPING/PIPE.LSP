;; By Mark Evinger            CIS ID 73162,1676                   10-20-93
;; revised 10:46:21 Wednesday, 03-30-1994 to account for DIMZIN variable
;; trouble (many thanks to TonyT on CI$)
;; revised 19:43:40 Thursday, 06-23-1994 to final form.
;; revised 10:14:00 Friday, 01-12-1996 to use Ductile Iron in sanitary pipe

;; revised 03-20-2007 to check for serial number     ME

;;========================================================================
;;     error handler
;;========================================================================
(defun pipelbl_err (s)                ;If an error (such as CTRL-C) occurs
  (if (/= s "Function cancelled")  ;while this command is active...
    (princ (strcat "\nError: " s))
  )
  (setq *error* old_err)        ;Restore old *error* handler
  (return)
  (princ)
); end defun pipelbl_err
;-------------------------------------------------------------------------
(defun rtd (TEXT_ANGLE)
  (/ (* 180 TEXT_ANGLE) pi)
); end defun rads_to_degrees
;-------------------------------------------------------------------------
(defun PLAN_TEXT ()
  (setq INSPT (getpoint "\nSelect insertion point of text: "))
  (command ".text" INSPT "0" PIPE_TEXT)
  (princ)
);end defun plan_text
;-------------------------------------------------------------------------
(defun PROFILE_TEXT ()
  (setq INSPT (getpoint "\nSelect center point of text: "))
  (command ".text" "j" "c" INSPT TEXT_ANG PIPE_TEXT)
  (princ)
);end defun profile_text
;-------------------------------------------------------------------------
(defun PL_PR ()
  (print)
  (if (null OPT2) (setq OPT2 "PL"))
  (initget "PL PR")
  (setq ANS2 (getkword (strcat "\n   (PL)an or (PR)ofile text?  >> "OPT2" << ")))
  (if (not (null ANS2)) (setq OPT2 ANS2))
  (print)
); end defun pl_pr
;-------------------------------------------------------------------------
(defun PVC_DI ()
  (print)
  (if (null OPT2a) (setq OPT2a "P"))
  (initget "P D")
  (setq ANS2a (getkword (strcat "   (P)VC or (D)uctile Iron pipe?  >> "OPT2a" << ")))
  (if (not (null ANS2a)) (setq OPT2a ANS2a))
  (print)
); end defun pl_pr
;-------------------------------------------------------------------------
(defun STORM_PIPE ()
  (setq PIPE_LENGTH (getstring T "\nEnter PIPE LENGTH: "))
  (setq PIPE_SIZE   (getstring T "\nEnter PIPE SIZE: "))
  (initget "3 4")
  (setq PIPE_CLASS  (getkword     "\nEnter PIPE CLASS in numerical notation (3-4): "))
  (while (= PIPE_CLASS nil)
    (progn
      (prompt "\nNo pipe class entered! please enter pipe class (3-4): ")
      (initget "3 4")
      (setq PIPE_CLASS  (getkword     "\nEnter PIPE CLASS in numerical notation (3-4): "))
    );end progn
  );end while
  (initget 5)
  (setq PIPE_SLOPE  (getreal     "\nEnter PIPE SLOPE in percent: "))
  (cond
    ((= PIPE_CLASS "3")
    (setq PIPE_CLS "III"))
    ((= PIPE_CLASS "4")
    (setq PIPE_CLS "IV"))
  ); end cond
  (setq OLD_DIMZIN (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (setq PIPE_SLP (rtos PIPE_SLOPE 2 2))
  (setvar "DIMZIN" OLD_DIMZIN)
  (setq PIPE_TEXT (strcat PIPE_LENGTH" L.F. - "PIPE_SIZE(chr 34)" RCP CL %%u%%o"PIPE_CLS"%%u%%o @ "PIPE_SLP"%"))
); end defun storm_pipe
;-------------------------------------------------------------------------
(defun SAN_PIPE ()
  (PVC_DI)
  (cond
    ((= OPT2a "P")
    (setq ANS2a_TEXT "PVC"))
    ((= OPT2a "D")
    (setq ANS2a_TEXT "D.I."))
  );end cond
  (setq PIPE_LENGTH (getstring T "\nEnter PIPE LENGTH: "))
  (setq PIPE_SIZE   (getstring T "\nEnter PIPE SIZE: "))
  (initget 5)
  (setq PIPE_SLOPE  (getreal     "\nEnter PIPE SLOPE in percent: "))
  (setq OLD_DIMZIN (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (setq PIPE_SLP (rtos PIPE_SLOPE 2 2))
  (setvar "DIMZIN" OLD_DIMZIN)
  (setq PIPE_TEXT (strcat PIPE_LENGTH" L.F. - "PIPE_SIZE(chr 34)" "ANS2a_TEXT" SAN @ "PIPE_SLP"%"))
); end defun san_pipe
;-------------------------------------------------------------------------
(defun STORM_PVC ()
  (setq PIPE_LENGTH (getstring T "\nEnter PIPE LENGTH: "))
  (setq PIPE_SIZE   (getstring T "\nEnter PIPE SIZE: "))
  (initget 5)
  (setq PIPE_SLOPE  (getreal     "\nEnter PIPE SLOPE in percent: "))
  (setq OLD_DIMZIN (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (setq PIPE_SLP (rtos PIPE_SLOPE 2 2))
  (setvar "DIMZIN" OLD_DIMZIN)
  (setq PIPE_TEXT (strcat PIPE_LENGTH" L.F. - "PIPE_SIZE(chr 34)" PVC STORM @ "PIPE_SLP"%"))
); end defun storm_pvc
;-------------------------------------------------------------------------
(defun FIND_LINE ()
  (setq PICK (entsel "\nPick line for text alignment : "))
  (if (= PICK nil)
    (progn
      (prompt "\nNothing selected! Please try again... ")
      (setq PICK (entsel "\nPick line for text alignment : "))
      (if (= PICK nil)
        (progn
          (prompt "\nNothing selected! Setting TEXT ANGLE to 0")
          (setq TEXT_ANG 0)
        ); end progn
        (progn
          (setq LINE_NAME (car PICK))
          (setq ENT_LIST (entget LINE_NAME))
          (setq ENT_TYPE (cdr (assoc 0 ENT_LIST)))
          (while (/= ENT_TYPE "LINE")
            (prompt "\nEntity picked was not a LINE. ")
            (setq PICK (entsel "\nPlease select again: "))
            (if (/= PICK nil)
              (progn
                (setq LINE_NAME (car PICK))
                (setq ENT_LIST (entget LINE_NAME))
                (setq ENT_TYPE (cdr (assoc 0 ENT_LIST)))
              );end progn
              (progn
                (prompt "\nNothing selected! Please try again...")
                (setq PICK (entsel "\nSelect line to align text to : "))
              );end progn
            );end if
          );end while
          (setq SP (cdr (assoc 10 ENT_LIST)))
          (setq EP (cdr (assoc 11 ENT_LIST)))
          (setq TEXT_ANG (angle SP EP))
          (setq TEMP_ANG (rtd TEXT_ANG))
          (cond
            ((<= TEMP_ANG 90)
              (setq TEXT_ANG TEMP_ANG)
            ); end TEMP_ANG < 90
            ((and (<= TEMP_ANG 180) (> TEMP_ANG 90))
              (setq TEXT_ANG (+ TEMP_ANG 180))
            ); end 180 < TEMP_ANG < 90
            ((and (<= TEMP_ANG 270) (> TEMP_ANG 180))
              (setq TEXT_ANG (- TEMP_ANG 180))
            ); end 270 < TEXT_ANG < 180
            ((and (<= TEMP_ANG 360) (> TEMP_ANG 270))
              (setq TEXT_ANG TEMP_ANG)
            ); end 270 < TEXT_ANG < 360
          ); end angle condition
        );end progn
      );end if
    );end progn
    (progn
      (setq LINE_NAME (car PICK))
      (setq ENT_LIST (entget LINE_NAME))
      (setq ENT_TYPE (cdr (assoc 0 ENT_LIST)))
      (while (/= ENT_TYPE "LINE")
        (prompt "\nEntity picked was not a LINE. ")
        (setq PICK (entsel "\nPlease select again: "))
        (if (/= PICK nil)
          (progn
            (setq LINE_NAME (car PICK))
            (setq ENT_LIST (entget LINE_NAME))
            (setq ENT_TYPE (cdr (assoc 0 ENT_LIST)))
          );end progn
          (progn
            (prompt "\nNothing selected! Please try again...")
            (setq PICK (entsel "\nSelect line to align text to : "))
          );end progn
        );end if
      );end while
      (setq SP (cdr (assoc 10 ENT_LIST)))
      (setq EP (cdr (assoc 11 ENT_LIST)))
      (setq TEXT_ANG (angle SP EP))
      (setq TEMP_ANG (rtd TEXT_ANG))
      (cond
        ((<= TEMP_ANG 90)
          (setq TEXT_ANG TEMP_ANG)
        ); end TEMP_ANG < 90
        ((and (<= TEMP_ANG 180) (> TEMP_ANG 90))
          (setq TEXT_ANG (+ TEMP_ANG 180))
        ); end 180 < TEMP_ANG < 90
        ((and (<= TEMP_ANG 270) (> TEMP_ANG 180))
          (setq TEXT_ANG (- TEMP_ANG 180))
        ); end 270 < TEXT_ANG < 180
        ((and (<= TEMP_ANG 360) (> TEMP_ANG 270))
          (setq TEXT_ANG TEMP_ANG)
        ); end 270 < TEXT_ANG < 360
      ); end angle condition
    );end progn
  );end if
); end FIND_LINE
;-------------------------------------------------------------------------
(DEFUN PIPELBL ()
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  (setq old_err *error*          ; Use special error handling function.
  *error* pipelbl_err)
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  (setq OPT "X")
  (while (not (null OPT))
    (prompt "\n Indicate type of pipe for text or e(X)it: ")
    (initget "S SA ST X")
    (setq ANS (getkword (strcat "\nType of pipe (S)torm rcp, (SA)nitary pvc or (ST)orm pvc: >>"OPT"<< ")))
    (if (not (null ANS)) (setq OPT ANS))
    (cond
      ((= OPT "X")
        (setq OPT nil)
      ); end cond "X"
      ((= OPT "S")
        (PL_PR)
        (cond
          ((= OPT2 "PL")
            (STORM_PIPE)
            (PLAN_TEXT)
          ); end cond PL
          ((= OPT2 "PR")
            (STORM_PIPE)
            (FIND_LINE)
            (PROFILE_TEXT)
          ); end cond PR
        ); end cond PL or PR
      ); end cond "S"
      ((= OPT "SA")
        (PL_PR)
        (cond
          ((= OPT2 "PL")
            (SAN_PIPE)
            (PLAN_TEXT)
          ); end opt PL
          (( = OPT2 "PR")
            (SAN_PIPE)
            (FIND_LINE)
            (PROFILE_TEXT)
          ); end opt PR
        ); end plan or profile
      ); end cond "SA"
      ((= OPT "ST")
        (PL_PR)
        (cond
          ((= OPT2 "PL")
            (STORM_PVC)
            (PLAN_TEXT)
          ); end opt PL
          ((= OPT2 "PR")
            (STORM_PVC)
            (FIND_LINE)
            (PROFILE_TEXT)
          ); end opt PR
        ); end condition PL or PR
      ); end cond "ST"
    ); end cond overall
  );end while
  (princ)
  (princ)
);end pipelbl
;                (defun c:pipelbl ()
;                      (if (cgl_check_serial)
;                              (progn
;                                       (vmon)
;                                      (pipelbl)
;                             );end checker progn
;                         (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
;                         );end if
;                         (princ)  
;                  ); defun pipelbl
;                     (defun c:pipe ()
;                         (if (cgl_check_serial)
;                               (progn
;                                 (vmon)
;                                      (pipelbl)
;                                         );end checker progn
;                         (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
;                           );end if
;                           (princ)  
;                     ); defun pipe
(princ "\n\tc:PIPELBL loaded. Start command with PIPE or PIPELBL.")
(princ)
;;; Happy Lisping!  The Cad Elf  *<:)-|-<
