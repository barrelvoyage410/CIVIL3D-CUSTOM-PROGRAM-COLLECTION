;;;;;************                     USED TO CHANGE TEXT TO DEFAULT
;;;*****************************************
;;;   Superior Designs - Custom programming available
;;;   Copyright (C) 1994-1997
;;;   Written by Craig Carr -  1994
;;;   http://www.inil.com/users/ccarr/sdi/acad.htm
;;;   e-mail ccarr@inil.com
;;;*****************************************
;;;Lisp to Standardize/modify selected text in a dwg

(DEFUN C:CHT()(SETVAR "CMDECHO" 0)
(PRINC "\nPICK TEXT TO STANDARDIZE..")
(SETQ SSTXT(SSGET))(SETQ TXTQTY(SSLENGTH SSTXT))
(SETQ NUM 0)(WHILE (< NUM TXTQTY)
          (IF (= (cdr(ASSOC 0 (ENTGET(SSNAME SSTXT NUM)))) "TEXT")
              (COMMAND "CHANGE" (SSNAME SSTXT NUM) "" "" "" "STANDARD" "" "" "" ) ); END IF
     (SETQ NUM(1+ NUM)) ); END WHILE  
(PRINC)); END FUNCTION
;;;;;;*************
