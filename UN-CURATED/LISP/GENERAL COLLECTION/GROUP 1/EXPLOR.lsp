(defun c:wex()  ;Change "wex" to any command you wish to use
   (startapp "explorer" (getvar "dwgprefix"))
   (princ)
) 