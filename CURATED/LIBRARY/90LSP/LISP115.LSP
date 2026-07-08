(defun c:lisp115 ()
  (setq a (getstring "\nName of layer  "))
  (setq b (ssget "x" (list (cons 8 a))))
  (command "erase" b "")
)
