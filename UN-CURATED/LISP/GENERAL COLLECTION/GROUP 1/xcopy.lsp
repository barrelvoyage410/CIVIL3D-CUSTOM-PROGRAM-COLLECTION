;Copies selected entity in an xref to current drawing on the current layer

(DEFUN C:xcopy ( / ENT VER LAYER OLDLIST REVLIST NSTYLE OLDSTYLE OLDLAYER NLAYER)
(setq ent nil)
(sETVAR "CMDECHO" 0)
;********ERROR HANDLER**********************
(defun *Error* (msg)
  (if (/= msg "Function cancelled")
    (princ (strcat "\nError: " msg))
  )
(terpri)
                         
  (setq *error* olderr)        
  (princ)
)
;***********************************************
;(check)
;(if cont (exit))
(setvar "cmdecho" 0)
(setq layer (getvar "clayer"))
(while (= ent nil) 
(setq ent (nentsel "\nSelect entity to copy to current drawing: "))
)
(setq oldlist (entget (car ent)))
(setq revlist (cdr oldlist))
;check for style (text)

(if (= (cdr (assoc 0 revlist)) "TEXT")
(if (wcmatch (cdr (assoc 7 revlist)) "*|*")
(progn
(setq NSTYLE (cons 7 "L100"))
(setq oldstyle (assoc 7 revlist))
(setq revlist (subst NSTYLE oldstyle revlist))
)
))

;change layer
(setq oldlayer (assoc 8 revlist))
(setq NLAYER (cons 8 layer))
(setq revlist (subst NLAYER oldlayer revlist))
;
(entmake revlist)
(princ (strcat "\nA " (cdr (assoc 0 revlist)) " was created.")) 
(setvar "cmdecho" 0)
(PRINC)
)