(defun C:XDTest ( / e1 xlst)
  
  (setq xlst (list "Test" 123456 123.35 "Hey You" "Get off of " "My Cloud" 123.23 23))
  (while (not e1)
    (setq e1 (entsel "\nSelect entity to write xdata to: "))
  )
  (setq e1 (car e1))
  
  (_XData e1 xlst "Tester")
  (princ)
)
