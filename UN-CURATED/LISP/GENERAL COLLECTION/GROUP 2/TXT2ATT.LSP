;  THIS PROGRAM CONVERTS TEXT  INTO A PRESET ATTRIBUTE DEFINITION 
;  WITH THE SAME SIZE AND ORIENTATION AS THE TEXT.  THE ATTRIBUTE TAG AND
;  DEFAULT VALUE WILL HAVE THE SAME VALUE AS THE TEXT STRING.
;  
;  COPYRIGHTED 1993 BY BBK SOFTWARE 
;  
;  Added MidLeft justification 12/14/93  lcc
;
(DEFUN TXT2ATT1 ()
  (SETQ INPTL (CDR (ASSOC 10 E ))
    INPTJ (CDR (ASSOC 11 E ))
    LYR (CDR (ASSOC 8 E ))   
    STY (CDR (ASSOC 7 E ))
    ANL (CDR (ASSOC 50 E ))
    THT (CDR (ASSOC 40 E ))
    JFH (CDR (ASSOC 72 E ))
    JFV (CDR (ASSOC 73 E ))
    TV (CDR (ASSOC 1 E ))
    ltv (strlen tv)
    ttv tv
    flag t
    i 0
  )
  (while flag
    (if (= (ascii ttv) 32)(setq flag nil))
    (setq i (1+ i)
      ttv (substr ttv 2)
    )
    (if (> i ltv)(setq flag nil))  
  )
  (setq tv1 (substr tv 1 (1- i)))
  (IF (= 0 JFH)(SETQ JFHT "L"))
  (IF (= 1 JFH)(SETQ JFHT "C"))
  (IF (= 2 JFH)(SETQ JFHT "R"))
  (IF (= 4 JFH)(SETQ JFHT "M"))
  (IF (= 02 JFH)(SETQ JFHT "ML"))
  (IF (= 0 JFV)(SETQ JFVT ""))
  (IF (= 1 JFV)(SETQ JFVT "B"))
  (IF (= 2 JFV)(SETQ JFVT "M"))
  (IF (= 3 JFV)(SETQ JFVT "T"))
  (SETQ JFT (STRCAT JFVT JFHT))
  (COMMAND "LAYER" "S" LYR "")
  (SETVAR "AFLAGS" 8)
  (SETQ STY "GHTEXT" )
  (IF (= JFT "L")
    (COMMAND "ATTDEF" "" TV1 "" TV "S"  STY INPTL THT (/(* ANL 180)PI))
    (COMMAND "ATTDEF" "" TV1 "" TV "S"  STY JFT INPTJ THT (/(* ANL 180)PI))
  )
  (COMMAND "ERASE" SS "")
  (REDRAW (ENTLAST))
) 
;
(DEFUN C:TXT2ATT ()
  (PROMPT "\nSelect text ")
  (SETQ SS (SSGET PT)
    E (ENTGET (SSNAME SS 0))
  )
  (if (= (cdr (assoc 0 E)) "TEXT")
    (TXT2ATT1)
    (PROMPT "\nThis is not text")
  )
)
(PRINC)
(C:TXT2ATT)
