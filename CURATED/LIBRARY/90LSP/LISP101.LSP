(defun ss ()
  (setq cd (getint "\nEnter Associative Code  "))
  (setq par (getstring "\nEnter parameter  "))
  (ssget "x" (list (cons cd par)))
)
