 ;  THIS PROGRAM ALLOWS CONVERT AN ATTRIBUTE DEFINITION INTO A TEXT 
 ;  ENTITY WITH THE SAME SIZE AND ORIENTATION AS THE ATTRIBUTE.  THE TEXT
 ;  WILL HAVE THE SAME TEXT VALUE AS ATTRIBRUTE TAG.
 ;  
 ;  COPYRIGHTED 1993 BY BBK SOFTWARE 
 ;  
 ;
;; **heavily** revised 8/23/06 6:42 PM by Mark Evinger to allow for selection set processing
;;; revised 03-20-2007 to check for serial number             ;;;ME

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
  (princ (strcat "\rWorking... " #SPIN " Item " (itoa (1+ l)))
  ) ;_ end of princ
) ;defun ME_spinner

(DEFUN ATT2TXT1 ()
  (SETQ INPTL (CDR (ASSOC 10 E)) ; text insert point
        INPTJ (CDR (ASSOC 11 E)) ; justified insert point
        LYR   (CDR (ASSOC 8 E)) ; layer name
        STY   (CDR (ASSOC 7 E)) ; text style name
        ANL   (CDR (ASSOC 50 E)) ; text rotation angle
        THT   (CDR (ASSOC 40 E)) ; text height
        JFH   (CDR (ASSOC 72 E)) ; text justification
        JFV   (CDR (ASSOC 74 E)) ; vertical justification
        TV    (CDR (ASSOC 2 E)) ; text value
        CLR   (CDR (ASSOC 62 E)); Color
  ) ;_ end of SETQ
  (IF (= 0 JFH)
    (SETQ JFHT "L")
  ) ;_ end of IF
  (IF (= 1 JFH)
    (SETQ JFHT "C")
  ) ;_ end of IF
  (IF (= 2 JFH)
    (SETQ JFHT "R")
  ) ;_ end of IF
  (IF (= 4 JFH)
    (SETQ JFHT "M")
  ) ;_ end of IF
  (IF (= 0 JFV)
    (SETQ JFVT "")
  ) ;_ end of IF
  (IF (= 2 JFV)
    (SETQ JFVT "M")
  ) ;_ end of IF
  (IF (= 3 JFV)
    (SETQ JFVT "T")
  ) ;_ end of IF
  (IF (= 1 JFV)
    (SETQ JFVT "B")
  ) ;_ end of IF
  (SETQ JFT (STRCAT JFVT JFHT)) ; text justification

  (if (= CLR nil)
    (setq CLR 256)
    );if 

  (entmake (list
             '(0 . "TEXT")
             '(100 . "AcDbEntity")
             '(100 . "AcDbText")
             (list 10 0.0 0.0 0.0)
             (cons 11 INPTJ) ;Start point of justified text
             (cons 8 LYR) ;layer Name 
             (cons 40 THT) ;Text height 
             (cons 1 TV) ;Text/attrib value 
             (cons 50 ANL) ;Start Rotation angle 
             '(41 . 1.0)
             '(51 . 0.0)
             (cons 7 STY) ;Text style 
             '(71 . 0) ;Text generation flag 
             (cons 72 JFH) ;Text justification 
             '(100 . "AcDbText")
             '(73 . 0)
             (cons 62 CLR); Entity color
           ) ;list 
  ) ;entmake 

;;;  (COMMAND "._-LAYER" "S" LYR "")
;;;  (IF (= JFT "L")
;;;    (COMMAND "._TEXT" "S" STY INPTL (/ (* ANL 180) PI) TV )
;;;    (COMMAND "._TEXT" "S" STY "J" JFT INPTJ (/ (* ANL 180) PI) TV );draw in the new text
;;;  ) ;_ end of IF
  (entdel (cdr (assoc -1 E))) ; erase the attribute entity
  (princ)
) ;end defun
(DEFUN C:ATT2TXT ()
  (if (cgl_check_serial)
   (progn

  (PROMPT
    "\nSelect set of ATTRIBUTE DEFINITIONS to change to TEXT"
  ) ;_ end of PROMPT
  (SETQ SS (SSGET '((0 . "ATTDEF"))))
 ;select only attribute definition entities
  (setq n (sslength SS))
  (setq l 0)
  (while (< l n)
    (setq E (ENTGET (SSNAME SS l)))
    (ATT2TXT1)
    (me_spinner)
    (setq l (1+ l))
    (princ)
  ) ;end while
    );end checker progn
    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
    );end if
    (princ)

) ;_ end of DEFUN
(PRINC)
 ;(C:ATT2TXT)