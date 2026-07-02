;Converts attributes (attr. definitions, tags) to plain texts
(defun C:A2T ( / ss ssl i e new grp grplst addg)
 (if (setq ss (ssget '((0 . "ATTDEF"))))
   (progn
     (setq ssl (sslength ss)
    i 0
    )
     (setq grplst (list 7 8 10 11 39 40 41 50 51 62 71 72 73))
     (while (< i ssl)
(setq e (ssname ss i))
(setq ent (entget e))
(setq new '((0 . "TEXT")))
(setq new (append new (list (cons 1 (cdr (assoc 2 ent))))))
(foreach grp grplst
  (setq addg (assoc grp ent))
  (if (/= addg nil)
    (setq new (append new (list (assoc grp ent))))
    )
  )
(entmake new)
(entdel e)
(setq i (1+ i))
)
     )
   (princ "\nNo attribute Selected")
   )
 (princ)
 ) ; defun