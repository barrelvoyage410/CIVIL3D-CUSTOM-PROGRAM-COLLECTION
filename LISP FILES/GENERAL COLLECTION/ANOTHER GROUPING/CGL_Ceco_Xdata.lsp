(defun strip_xdata (ename)
  (entmod
    (append (entget ename) (list (list -3 (list "pacsoft196168"))))
  )
) ; strip_xdata single item


(defun c:cgl_kill_xdata ()
  (setq stripitem (car (entsel "\nSelect entity to strip data from: ")))
  (strip_xdata stripitem)
)
