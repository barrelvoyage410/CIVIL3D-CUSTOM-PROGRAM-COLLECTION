;;;    GSR.LSP / 12APR95
;;;    
;;;    Copyright (C) 1995 by Thomas Berger
;;;
;;;    FREEWARE:
;;;    Permission to use, copy, modify, and distribute this software
;;;    for any purpose and without fee is hereby granted, provided
;;;    that the above copyright notice appears in all copies and
;;;    that both that copyright notice and the limited warranty and
;;;    restricted rights notice below appear in all supporting
;;;    documentation.
;;;
;;;    THOMAS BERGER PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;;    THOMAS BERGER SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;;    MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  THOMAS BERGER
;;;    DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;;    UNINTERRUPTED OR ERROR FREE.
;;;
;;;
;;;    ****************************************************************
;;;    GSR: Global Search and Replace
;;;
;;;    GSR asks for two strings and replaces the first one in all 
;;;    TEXT entities inside the drawing file. GSR also finds and replaces
;;;    nested TEXT entities inside BLOCK INSERTIONs, ATTRIBUTEs and
;;;    DIMENSIONs, GSR does not handle R13 MTEXT entities.
;;;
;;;    DOS2WIN calls GSR with a replacelist to convert German Umlauts
;;;            from DOS to WIN ASCII
;;;
;;;    WIN2DOS calls GSR with a replacelist to convert German Umlauts
;;;            from WIN to DOS ASCII
;;;
;;;    GLOBAL functions only: C:GSR GSR REPLACESTRING C:DOS2WIN C:WIN2DOS
;;;
;;;           C:GSR is the command line function
;;;
;;;           (GSR startentityname replacelist) the AutoLISP function
;;;              i.e. (GSR nil '(("AB" "ab") ("CD" "y")))
;;;              will start with the drawings first entity
;;;              and replace all "AB"s with "ab"s and all "CD"s with "y"s
;;;
;;;           (REPLACESTRING text searchstring) a function to replace
;;;              a string inside another string
;;;    ****************************************************************


(defun c:gsr (/ old new)
	(setvar "cmdecho" 0)
	(command "._undo" "_group")
	(setq old (getstring "\nold string: "))
	(if (/= "" old)
		(progn
			(setq new (getstring "\nnew string: "))
			(gsr nil (list (list old new)))
			(command "._regen")
		)
	)
	(command "._undo" "_end")
	(princ)
)

(defun c:win2dos ()
	(setvar "cmdecho" 0)
	(command "._undo" "_group")
	(gsr nil '(("ƒ" "é") ("÷" "ô") ("‹" "ö") ("‰" "Ñ") ("ˆ""î") ("¸" "Å") ("ﬂ" "·")))
	(command "._regen")
	(command "._undo" "_end")
	(princ)
)

(defun c:dos2win ()
	(setvar "cmdecho" 0)
	(command "._undo" "_group")
	(gsr nil '(("é" "ƒ") ("ô" "÷") ("ö" "‹") ("Ñ" "‰") ("î" "ˆ") ("Å" "¸") ("·" "ﬂ")))
	(command "._regen")
	(command "._undo" "_end")
	(princ)
)

(defun gsr ( en replist / el txt)
	(if (= 'ENAME (type en)) nil (setq en (entnext)))
	(while en
		(cond
			( (= "TEXT" (cdr (assoc 0 (setq el (entget en)))))
				(setq txt (cdr (assoc 1 el)))
				(setq txt 
					(progn (mapcar
						'(lambda (x)
							(setq txt (replacestring txt (car x) (cadr x)))
						)
						replist
					) txt)
				)
				(setq el (subst (cons 1 txt) (assoc 1 el) el))
				(entmod el)
				(setq en (entnext en))
			)
			( (= "ATTRIB" (cdr (assoc 0 (setq el (entget en)))))
				(setq txt (cdr (assoc 1 el)))
				(setq txt 
					(progn (mapcar
						'(lambda (x)
							(setq txt (replacestring txt (car x) (cadr x)))
						)
						replist
					) txt)
				)
				(setq el (subst (cons 1 txt) (assoc 1 el) el))
				(entmod el)
				(setq en (entnext en))
			)
			( (= "DIMENSION" (cdr (assoc 0 (setq el (entget en)))))
				(setq txt (cdr (assoc 1 el)))
				(if (/= "" txt)
					(progn
						(setq txt 
							(progn (mapcar
						'(lambda (x)
							(setq txt (replacestring txt (car x) (cadr x)))
						)
						replist
					) txt)
						)
						(setq el (subst (cons 1 txt) (assoc 1 el) el))
						(entmod el)
					)
				)
				(setq en (entnext en))
			)
			( (= "INSERT" (cdr (assoc 0 (setq el (entget en)))))
				(gsr (cdr (assoc -2 (tblsearch "BLOCK" (cdr (assoc 2 el))))) replist)
				(setq en (entnext en))
			)
			(T
				(setq en (entnext en))
			)
		)
		
	)
	(princ)
)


(defun replacestring (txt sea rep / tle sle part front 1st ret)
	(setq tle (strlen txt) sle (strlen sea) part "")
	(while (< 0 tle)
		(cond
			((< tle sle)
				(setq part (strcat part txt) txt "" tle 0)
			)
			(T
				(setq 1st (substr txt 1 1)
							front (substr txt 1 sle)
				)
				(if (= front sea)
					(progn
					(setq ret (cons rep (cons part ret))
								txt	(substr txt (1+ sle))
								tle 	(strlen txt)
								part ""
					)
					
					)
					(setq part (strcat part 1st)
								txt (substr txt 2)
								tle 	(strlen txt)
					)
				)
			)
		)
	)
	(apply 'strcat (reverse (cons part ret)))
)


(princ "\nGSR: GLOBAL SEARCH and REPLACE of TEXT entities")
(princ "\n     (also nested text in block insertions, attributes or dimensions).")
(princ "\n     FREEWARE from Tom Berger, imago mundi CAD Systemhaus GmbH")
(princ "\n                               Friedrich-Ebert-Str. 78")
(princ "\n                               D-34119 Kassel")
(princ "\n                               Tel +49 561 103006, Fax +49 561 14172")
(princ "\n                               CIS: Thomas Berger, 100326,2312")
(princ "\n                               Internet: 100326.2312@compuserve.com")
(princ "\n     Use GSR for global search and replace, or")
(princ "\n     DOS2WIN to convert German Umlauts from DOS to WIN, or")
(princ "\n     WIN2DOS to convert German Umlauts from WIN to DOS.")
(princ "\n     You can also search and replace %%nnn strings!")
(princ)
