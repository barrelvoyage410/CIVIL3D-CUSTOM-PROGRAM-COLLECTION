(defun DelXdata (ent app / entlst tmplst)
  (setq entlst (entget ent app))
  (foreach memb (cdr (assoc -3 entlst))
    (setq tmplst (cons -3 (list (cons (car memb) nil)))
          entlst (subst tmplst (assoc -3 entlst) entlst)
          entlst (entmod entlst)
    )
  )
)


(defun C:DelAllXdata (/ curass countr CGL_Xdata_Type)
  (setq CGL_XDATA_TYPE (getstring "enter string to match: "))
  (setq curass (ssget "X" '((-3 (CGL_Xdata_Type))))
        countr 0
  )
  (if curass
    (repeat (sslength curass)
      (DelXdata (ssname curass countr) '(CGL_Xdata_Type))
      (setq countr (1+ countr))
    )
  )
  (princ)
) ;end defun
