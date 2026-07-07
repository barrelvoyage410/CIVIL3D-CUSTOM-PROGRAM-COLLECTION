;;; CADALYST 03/08  www.cadalyst.com/code 
;;; Tip 2272: XCC.lsp	Layer Color Change	(c) 2008 Ryan Wunderlich 


(defun c:lcc (/ obj layer color)
  (while (setq obj (nentsel "\nSelect entity on layer: "))
    (setq layer	(entget
		  (tblobjname "layer" (cdr (assoc 8 (entget (car obj)))))
		)
	  color	(acad_colordlg (cdr (assoc 62 layer)))
    )
    (if	color
      (entmod (subst (cons 62 color) (assoc 62 layer) layer))
    )
  )
  (princ)
)