;;;                                                                                     ;;;
;;;                 Created 7/19/2000 4:51:47 PM by Mark Evinger                        ;;;
;;;                                                                                     ;;;
;;;  NEW_TF.lsp - changes the text values in the strings "TF=xxx.xx" or xxx.xx          ;;;
;;; to add or subtract an adjustment value to all the text or mtext in the selection    ;;;
;;; set.  This routine assumes that the strings on in the form of "TF= xxx.xx"          ;;;
;;; or "xxxxx = xxx.xx" or just xxx.xx.   The routine will test for 1 or 2 decimal      ;;;
;;; places and return the adjusted number rounded to the appropriate value.             ;;;
;;;                                                                                     ;;;
;;;                                                                                     ;;;
;;; revised 7/25/2000 4:10:53 PM to adjust both TEXT and MTEXT, parsing strings with    ;;;
;;; an equal sign or not.  Routine will now adjust ALL text selected, those strings that;;;
;;; have no numbers will be ignored, as will those with a % in them.                    ;;; v1.0.2
;;;                                                                                     ;;;
;;;  So, routine now ignores plain text strings, text with "%" in it, and adjusts plain ;;;
;;; numbers.  Text that has an "=" in it will be split into 2 parts, the text after the ;;;
;;; "=" sign will be replaced with the adjustment value.  This all means that this      ;;;
;;; routine was designed to work on ONLY individual numbers "999.99" and text strings   ;;;
;;; which represent an elevation in the form of "<string> = xxx.xx".                    ;;;
;;;                                                                                     ;;;
;;; revised 02-27-2004 to account for ( and ) in text, allowing for existing elevations ;;; v1.1.4
;;; to be changed as well                                                               ;;;
;;;                                                                                     ;;;
;;; revised 08-02-2004 to toggle for truncation of trailing zeros - converting .xx      ;;; v1.1.6
;;; values to .x precision.  This allows one to convert to "preliminary" values for     ;;;
;;; elvevations.  Note that this ONLY works on values ending in zero,                   ;;;
;;;                                                                                     ;;;
;;;Mark Evinger  email: Mark.Evinger@cgl-ltd.com                                        ;;;
;;;MIS Manager                                                                          ;;;
;;;Cowhey Gudmundson Leder, Ltd.                                                        ;;;
;;;300 Park Boulevard                                                                   ;;;
;;;Suite 350                                                                            ;;;
;;;Itasca, IL 60143                                                                     ;;;
;;;Phone: 630-250-9595 x242  Fax: 630-250-9644                                          ;;;
;;;Website: http://www.cgl-ltd.com                                                      ;;;
;;;                                                                                     ;;;
;;; revised 03-20-2007 to check for serial number                                       ;;; v 1.1.8

(defun ME_SPINNER () ; draws the spinner at the command prompt.
  (if (not #SPIN)
    (setq #SPIN "-")
  ) ;if
  (cond
    ((equal #SPIN "-") (setq #SPIN "\\"))
    ((equal #SPIN "\\") (setq #SPIN "|"))
    ((equal #SPIN "|") (setq #SPIN "/"))
    (t (setq #SPIN "-"))
  ) ;cond close
  (princ (strcat "\rWorking... " #SPIN))
) ;defun ME_spinner

 ; ************************************************************************
 ;                             SPLITSTR.LSP
 ;
 ; This routine accepts a string and a delimiter character
 ; It returns a list containing two strings
 ; The first returned string contains all of the characters from the beginning
 ; of the string to the delimiter
 ; The second string in the returned list contains the remainder of the string
 ; starting after the delimiter
 ;
 ; External routines used:
 ; Internal routines used:
 ;
 ; Permission is hereby granted to use this routine on the condition that
 ; this header remains with the routine. No warrantity is implied or
 ; expressed. The routine is not to be modified without permission of the
 ; author.
 ;
 ; Original version    3/6/94      John Gibb/Paladin
 ;                                 Compuserve #71530,3004
 ; ************************************************************************

(defun SPLIT_STR
                 (LINE CHAR / TEMPVAR TEMP-LIST CNT LOOP LINE-LEN TEMP-LIST)
  (setq TEMPVAR  ""
        CNT      1
        LOOP     t
        LINE-LEN (strlen LINE)
  ) ;_ end of setq
  (if (= LINE-LEN 0)
    (setq TEMPVAR "")
    (while LOOP
      (if (> CNT LINE-LEN)
        (setq LOOP NIL)
        (progn
          (if (and (= (substr LINE CNT 1) CHAR) (/= CNT LINE-LEN))
            (setq LINE (substr LINE (1+ CNT) (- (strlen LINE) CNT))
                  LOOP NIL
            ) ;_ end of setq
            (progn
              (setq TEMPVAR (strcat TEMPVAR (substr LINE CNT 1)))
              (if (= CNT LINE-LEN)
                (setq LOOP NIL)
              ) ;end if
            ) ;end progn
          ) ;end if
        ) ;end progn
      ) ;end if cnt > length
      (setq CNT (1+ CNT))
    ) ;end while
  ) ;end if line length = 0
  (setq TEMP-LIST (cons LINE TEMP-LIST)
        TEMP-LIST (cons TEMPVAR TEMP-LIST)
  ) ;_ end of setq
) ;end split_str
 ;***************************************************************

(defun C:NEW_TF (/              CGL_NEW_TF     CGL_OLD_TEXT   CGL_SPLIT_STR  CGL_TEXT1
                 CGL_OLD_NUM    OLD_NUM        NEW_NUM        NEW_TEXT       EN
                 L              SS             N              CGL_VERSION    CGL_BACK_PAREN
                 CGL_Back_Half  CGL_FRONT_HALF CGL_STR_TRUNCATE
                )
  (if (cgl_check_serial)
    (progn
;;; This bit is designed more for a dual monitor system, as the version will
;;; scroll off a three line display....
  (setq CGL_VERSION "1.1.8")
  (princ
    (strcat
      "\r NEW_TF version "
      CGL_VERSION
      " -- copyright © Cowhey & Associates 2000 - 2007. "
    ) ;_ end strcat
  ) ;_ end princ
 ;---------------------------------------------------------------------------

  (setq CGL_BACK_PAREN nil)
  (setq CGL_STR_TRUNCATE "No")
  (prompt
    "\nSelect TEXT or MTEXT entities to change Grading value: "
  ) ;_ end of prompt
  (setq SS (ssget '((0 . "*TEXT"))))
  (if SS
    (progn
      (initget 1 "Yes No")
      (setq CGL_STR_TRUNCATE
             (getkword
               "\nDo you want trailing zeros truncated from elevations? <Yes or No>: "
             ) ;_ end of getkword
      ) ;_ end of setq
      (setq CGL_NEW_TF (getreal "\nEnter Grading adjustment value: "))
      (setq N (sslength SS))
      (setq L 0)
      (while (< L N)
        (setq EN (entget (ssname SS L)))
        (setq CGL_OLD_TEXT (cdr (assoc 1 EN)))
        (cond

          ((= t (wcmatch CGL_OLD_TEXT "*=*")) ; if there is an "=" anywhere in text
           (setq CGL_SPLIT_STR (SPLIT_STR CGL_OLD_TEXT "=")) ; split the string in 2
           (if (= (substr (cadr CGL_SPLIT_STR) 1 1) " ") ; if the 1st char of 2nd string is " "
             (setq CGL_TEXT1 (strcat (car CGL_SPLIT_STR) "= ")) ;true, has form of "TF= "
             (setq CGL_TEXT1 (strcat (car CGL_SPLIT_STR) "=")) ;false, has form of "TF="
           ) ;end if
           (if
             (wcmatch (cadr cgl_split_str) "*[)]*") ; back half has ) in it
              (progn
                (setq CGL_BACK_PAREN T)
                (setq CGL_Back_Half (vl-string-right-trim ")" (cadr cgl_split_str))) ;remove the back )
                (setq CGL_OLD_NUM CGL_Back_Half)
                (setq OLD_NUM (atof CGL_OLD_NUM))
                (setq NEW_NUM (+ CGL_NEW_TF OLD_NUM))
              ) ;end progn true condition
              (progn
                (setq CGL_OLD_NUM (cadr CGL_SPLIT_STR))
                (setq OLD_NUM (atof CGL_OLD_NUM))
                (setq NEW_NUM (+ CGL_NEW_TF OLD_NUM))
              ) ;end progn back half does not have )
           ) ;end if
           (if
             (/= (substr CGL_OLD_NUM (- (strlen CGL_OLD_NUM) 2) 1) ".") ;test for number of decimal places
              (setq NEW_TEXT (strcat CGL_TEXT1 (rtos NEW_NUM 2 1))) ; to 0.0
              (setq NEW_TEXT (strcat CGL_TEXT1 (rtos NEW_NUM 2 2))) ; to 0.00
           ) ;end if not 2 decimal places
           (if (and (= CGL_STR_TRUNCATE "Yes")
                    (= (substr CGL_OLD_NUM (strlen CGL_OLD_NUM) 1) "0")
               ) ;_ end of and
             (setq NEW_TEXT (strcat CGL_TEXT1 (rtos NEW_NUM 2 1))) ;clip to 0.0
           ) ; end if
           (if (= T CGL_BACK_PAREN)
             (setq NEW_TEXT (strcat NEW_TEXT ")")) ;add the closing parenthesis on the string
             (setq CGL_BACK_PAREN nil) ; zero out back_paren var
           ) ;end if
           (setq EN
                  (subst (cons '1 NEW_TEXT)
                         (assoc 1 EN)
                         EN
                  ) ;_ end of subst
           ) ;_ end of setq
           (entmod EN)
           (setq CGL_BACK_PAREN nil) ; clear flag for )
          ) ;end string has "=" in it

          ((= T (wcmatch CGL_OLD_TEXT "*(*,*)*")) ;if a () in the string
           (if (= (substr CGL_OLD_TEXT 1 1) "(") ; if 1st char of 1st string is (
             (progn
               (setq CGL_FRONT_HALF (vl-string-left-trim "(" CGL_OLD_TEXT)) ;remove ( from start of string
               (setq CGL_FRONT_HALF (vl-string-right-trim ")" CGL_FRONT_HALF)) ;remove ) from end of string
             ) ;end progn
           ) ;end if
           (if (wcmatch CGL_FRONT_HALF "*[A-Z]*,*[a-z]*") ;if there is text in the string between ()
             (setq NEW_TEXT CGL_FRONT_HALF) ;do nothing
             (progn
               (setq OLD_NUM (atof CGL_FRONT_HALF))
               (setq NEW_NUM (+ CGL_NEW_TF OLD_NUM))
               (if (/= (substr CGL_FRONT_HALF (- (strlen CGL_FRONT_HALF) 2) 1) ".") ;test for number of decimal places
                 (setq NEW_TEXT (rtos NEW_NUM 2 1)) ; to 0.0
                 (setq NEW_TEXT (rtos NEW_NUM 2 2)) ; to 0.00
               ) ;end if not 2 decimal places
               (if (and (= CGL_STR_TRUNCATE "Yes")
                        (= (substr CGL_front_half (strlen CGL_front_half) 1) "0")
                   ) ;_ end of and
                 (setq NEW_TEXT (rtos NEW_NUM 2 1)) ;clip to 0.0
               ) ; end if
             ) ;end progn
           ) ;end if
           (setq NEW_TEXT (strcat "(" NEW_TEXT ")")) ; add the parens back in
           (setq EN
                  (subst (cons '1 NEW_TEXT)
                         (assoc 1 EN)
                         EN
                  ) ;_ end of subst
           ) ;_ end of setq
           (entmod EN) ; update the text
          ) ; text has () in it

          ((= t (wcmatch CGL_OLD_TEXT "*%*")) ; if there is a "%" anywhere in text
           (setq NEW_TEXT CGL_OLD_TEXT) ;do nothing
           (setq EN
                  (subst (cons '1 NEW_TEXT)
                         (assoc 1 EN)
                         EN
                  ) ;_ end of subst
           ) ;_ end of setq
           (entmod EN) ; do nothing the long way...
          ) ;end slope percent

          ((= t (wcmatch CGL_OLD_TEXT "*[A-Z]*,*[a-z]*")) ; if there is text anywhere in text
           (setq NEW_TEXT CGL_OLD_TEXT) ;do nothing
           (setq EN
                  (subst (cons '1 NEW_TEXT)
                         (assoc 1 EN)
                         EN
                  ) ;_ end of subst
           ) ;_ end of setq
           (entmod EN) ; do nothing the long way...
          ) ;end plain text string

          ((/= t (wcmatch CGL_OLD_TEXT "*=*")) ; no text, no "=", no "%", just numbers
           (setq OLD_NUM (atof CGL_OLD_TEXT))
           (setq NEW_NUM (+ CGL_NEW_TF OLD_NUM))
           (if (/= (substr CGL_OLD_TEXT (- (strlen CGL_OLD_TEXT) 2) 1) ".") ;test for number of decimal places
             (setq NEW_TEXT (rtos NEW_NUM 2 1)) ; to 0.0
             (setq NEW_TEXT (rtos NEW_NUM 2 2)) ; to 0.00
           ) ;end if not 2 decimal places
           (if (and (= CGL_STR_TRUNCATE "Yes")
                    (= (substr CGL_OLD_text (strlen CGL_OLD_text) 1) "0")
               ) ;_ end of and
             (setq NEW_TEXT (rtos NEW_NUM 2 1)) ;clip to 0.0
           ) ; end if
           (setq EN
                  (subst (cons '1 NEW_TEXT)
                         (assoc 1 EN)
                         EN
                  ) ;_ end of subst
           ) ;_ end of setq
           (entmod EN)
          ) ;end string has no = in it

        ) ;end conditional
        (setq L (1+ L))
        (ME_SPINNER)
      ) ;end while
      (princ)
      (prompt "\n")
      (prompt "\n")
      (prompt "\n")
      (if (= CGL_STR_TRUNCATE "Yes")
        (progn
          (prompt
            (strcat "\n"
                    (itoa L)
                    " Grading numbers changed by value of "
                    (rtos CGL_NEW_TF 2)
                    " with trailing zeros truncated."
            ) ;_ end of strcat
          ) ;end prompt with truncate on
        ) ;end progn yes
        (progn
          (prompt
            (strcat "\n"
                    (itoa L)
                    " Grading numbers changed by value of "
                    (rtos CGL_NEW_TF 2)
            ) ;_ end of strcat
          ) ;end prompt with truncate off
        ) ;end progn no
      ) ;end if
    ) ;end progn
    (alert "No TEXT or MTEXT in selection set!")
  ) ;end if
  (princ)
    );end checker progn
    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
    );end if
    (princ)  
) ; end C:NEW_TF