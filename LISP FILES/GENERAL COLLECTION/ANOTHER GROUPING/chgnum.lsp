;;;
;;; function:  c:ChgNum Version 1.0 by Miles Baker 9/4/96 8:07AM
;;;
;;; Copyright (c) 1996 by ExecuCAD Inc.  All Rights Reserved.
;;; (800) 728-9223    http://www.execucad.com
;;;
;;; description:  This function will search all selected text strings
;;;               and add the specified number to the numeric components
;;;
;;; Warning:      This function does not take into account complex
;;;               representations of numbers such as 1.25E10 or
;;;               signed numbers such as -10
;;;
;;; This complete file is distributed "as-is" as "freeware".
;;; This file may be distributed freely provided the header information
;;; remains in tact.
;;;
(defun c:ChgNum ( / ss sChg i iss en ed sText sOld sNew s
                    Lf_IsNumber Lf_IncrStr Lf_ChgVal )
  ;;..........................................
  ;; Local Function:  Lf_IsNumber Version 1.0 by Miles Baker 9/4/96 9:00AM
  ;; input:           pc - single character
  ;; process:         Checks to see if the character is a number
  ;; output:          returns T if a number nil if not a number
  ;;
  (defun Lf_IsNumber (pc / LgRet)
    (if (member pc (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
      (setq LgRet T)
      (setq LgRet nil)
    );; endif
    LgRet
  );; end Lf_IsNumber

  ;;..........................................
  ;; Local Function:  Lf_IncrStr Version 1.0 by Miles Baker 9/4/96 9:19AM
  ;; input:           ps - string containing only numeric values
  ;;                  psIncr - string numeric value to increment string with
  ;; process:         Adds real value to string
  ;; return:          Incremented String Value
  ;;
  (defun Lf_IncrStr (ps psIncr / rs fh sRet )
    (setq rs (+ (read ps) (read psIncr)))
    (if (equal (type rs) (quote REAL))
      (progn
        (setq fh (open "delete.gbg" "w"))   ;; this section is to ensure the
        (prin1 rs fh)                       ;; same number of decimal places are
        (setq fh (close fh))                ;; maintained instead of using the
        (setq fh (open "delete.gbg" "r"))   ;; rtos function
        (setq sRet (read-line fh))
        (setq fh (close fh))
      );; en dprogn
      (setq sRet (itoa rs))
    );; end if
    sRet
  );; end Lf_IncrStr

  ;;..........................................
  ;; Local function:  Lf_ChgVal Version 1.0 by Miles Baker 9/4/96 8:22AM
  ;; input:     psText - Text String, prchg - Increment/Decrement value
  ;; process:   Searches text string for any numeric values, pulls out
  ;;            numeric values and adds the  change value, then replaces
  ;;            the number with the new value in the string
  ;; output:    returns the modified string
  ;;
  (defun Lf_ChgVal (psText psChg / i iLen c LgCNum LgNxtNum sNum sRet LgDec )
    (setq i 1)
    (setq iLen (strlen psText))
    (setq sRet "")                 ;; initialize return string
    (repeat iLen
      (setq c (substr psText i 1)) ;; next character
      (setq LgCNum (Lf_IsNumber c))
      (cond
        ((equal c ".")
          (if (<= (+ i 1) iLen)
            (setq LgNxtNum (Lf_IsNumber (substr psText (+ i 1) 1)))
            (setq LgNxtNum nil)
          );; en dif
          (cond
            ((and sNum LgDec)
              (setq sNum (Lf_IncrStr sNum psChg))
              (setq sRet (strcat sRet sNum c))
              (setq LgDec nil)              ;; Used Decimal Flag - Reset
              (setq sNum nil)
            );; end cond
            ((and sNum (not LgDec) LgNxtNum)
              (setq LgDec T)                ;; Used Decimal Flag - True
              (setq sNum (strcat sNum c))
            );; add decimal which is part of real
            ((and sNum (not LgDec) (not LgNxtNum))
              (setq sNum (Lf_IncrStr sNum psChg))
              (setq sRet (strcat sRet sNum c))
              (setq LgDec nil)              ;; Used Decimal Flag - Reset
              (setq sNum nil)
            );; end cond number terminated
            ((and (not sNum) LgNxtNum)      ;; leading decimal number i.e. .001
              (setq sNum c)
              (setq LgDec T)
            );; end cond leading decimal
            (T
              (setq sRet (strcat sRet c)))
          );; end cond
        );; end cond decimal values
        ((and (not sNum) LgCNum)
          (setq sNum c)
        );; end cond start new number
        ((and sNum LgCNum)
          (setq sNum (strcat sNum c))
        );; end cond add next numeric character
        ((and sNum (not LgCNum))
          (setq sNum (Lf_IncrStr sNum psChg))
          (setq sRet (strcat sRet sNum c))
          (setq LgDec nil)              ;; Used Decimal Flag - Reset
          (setq sNum nil)
        );; end sNum
        (T
          (setq sRet (strcat sRet c))
          (setq sNum nil)
        );; end cond T
      );; end cond
      (setq i (+ i 1))
    );; end repeat
    (if sNum
      (progn
        (setq sNum (Lf_IncrStr sNum psChg))
        (setq sRet (strcat sRet sNum ))
      );; endprogn
    );; end if
    sRet                           ;; return modified string
  );; end Lf_ChgVal
  ;;................................................
  ;; The Main Function Starts Below
  ;;
  (prompt "\nSelect Text Entities to Change Numeric Values of: ")
  (if (setq ss (ssget (list (cons 0 "*TEXT"))))
    (progn
      (initget 1)
      (setq sChg (getstring "\nNumeric Change (use a - for decreased values): "))
      (setq i 0)
      (setq iss (sslength ss))
      (repeat iss
        (setq en (ssname ss i))
        (setq ed (entget en))
        (setq sText (cdr (assoc 1 ed)))
        (setq sNew  (Lf_ChgVal sText sChg))
        (if (not (equal sText sNew))
          (entmod (setq ed (subst (cons 1 sNew) (assoc 1 ed) ed)))
        );; end if
        (setq i (+ i 1))
      );; end repeat
    );; end progn text entities selected to process
    (prompt "\nNo text selected to process.  ")
  );; end if
  (prompt "\nChgNum Function Complete.")
  (princ)
);; end c:ChgNum function
