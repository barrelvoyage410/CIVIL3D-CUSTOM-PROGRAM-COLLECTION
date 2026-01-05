;;Have this load whenever someone opens AutoCAD. 
(DEFUN AcadUserLog(/ LOGN DAT XREC XNAME XLIST) 
(setq logn (getvar "loginname")) 
(SETQ DAT (RTOS (GETVAR "CDATE") 2 4)) 
(SETQ LOGN (STRCAT LOGN " on " (SUBSTR DAT 5 2) "/" (SUBSTR DAT 7 2) "/" (SUBSTR DAT 1 4) " @ " (SUBSTR DAT 10 2) ":" (SUBSTR DAT 12 2))) 
(REGAPP "Loguser") 


(if (SETQ XLIST (DICTSEARCH (NAMEDOBJDICT) "LOGLIST")) 
   (PROGN 
   (MAPCAR '(LAMBDA (Y) 
         (IF (NOT (ASSOC Y XLIST)) (SETQ XLIST(APPEND XLIST (LIST (CONS Y "-"))))) 
         ) 
        '(1 2 3 4) 
   ) 
   (ENTDEL (CDAR XLIST)) 
   (SETQ XREC (SUBST (CONS 1 LOGN) (ASSOC 1 XLIST) XLIST) 
         XREC (subst (cons 2 (cdr(assoc 1 xlist))) (assoc 2 XLIST) XREC) 
         XREC (subst (cons 3 (cdr(assoc 2 xlist))) (assoc 3 xlist) XREC) 
         XREC (subst (cons 4 (cdr(assoc 3 xlist))) (assoc 4 xlist) XREC) 
   ) 
   ) 

(SETQ XREC (LIST (CONS 0 "XRECORD")(CONS 100 "AcDbXrecord")(CONS 1 LOGN)(CONS 2 "-")(CONS 3 "-")(CONS 4 "-"))) 
) 



;(SETQ XREC (SUBST (CONS 1 LOGN)(ASSOC 1 XREC) XREC)) 
  (setq xname (entmakex xrec)) 
  (dictadd (namedobjdict) "LOGLIST" xname) 

) 


(DEFUN C:log (/ xlist) 

(setq dan (getvar "dwgname")) 

(IF (= (getenv "AccoSYS") "ON")(progn 
(PRINC "\nAcco Systems, Inc. - Drawing Log") 
(PRINC "\n--------------------------------") 
(Princ "\nTime listed is when ") 
(princ dan) 
(princ " was opened.") 
(Princ "\n") 
(SETQ XLIST (DICTSEARCH (NAMEDOBJDICT) "LOGLIST")) 

(PRINT (CDR(ASSOC 2 XLIST))) 
(PRINT (CDR(ASSOC 3 XLIST))) 
(PRINT (CDR(ASSOC 4 XLIST))) 
(PRINC "\n\n") 
(PRINC) 
(textscr) 
)) 
;(PRINC XLIST)(PRINC) 
(princ) 
) 


(MAPCAR '(LAMBDA (Y) 
         (IF (NOT (ASSOC Y XLIST)) (SETQ XLIST(APPEND XLIST (LIST (CONS Y "-"))))) 
         ) 
        '(1 2 3 4) 
)