(defun c:2lwrcase (/ INDEX LEN SSET NME TXT CAPTXT)
(princ "\nPick text to convert to all lower case: ")
(setq SSET (ssget))
(setq LEN (sslength SSET)                   ;get sel set length
        INDEX 0)
  (while (< INDEX LEN)
(setq NME (entget (ssname SSET INDEX)))     ;get entity list
(setq TXT (cdr (assoc 1 NME)))              ;get text
(setq CAPTXT (strcase TXT T))               ;make lower case
(setq CAPTXT   
   (subst (cons 1 CAPTXT)                   ;update entity list
	(assoc 1 NME)
		NME))
(entmod CAPTXT)                             ;modify to screen
     (setq INDEX (1+ INDEX))                ;go next
)                                           ;end while
(setq INDEX LEN SSET nil NME nil TXT nil CAPTXT nil)
(prin1)
)                                           ;end C:2LWRCASE
