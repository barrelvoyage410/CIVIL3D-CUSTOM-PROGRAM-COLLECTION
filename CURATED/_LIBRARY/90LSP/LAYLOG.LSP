;Laylog.LSP: Records the current status of your layers to a file.
;
;_______________________________________________________________________

(DEFUN Convert_date ()
(Setq dte (rtos (getvar "cdate") 2 4))
(setq yr (substr dte 1 4))
(setq mo (substr dte 5 2))
(setq dy (substr dte 7 2))
(setq sdate (strcat "DATE: "mo "/" dy "/" yr "\t"))
)

(defun taber (item / leng)
  (setq leng (strlen item))
  (cond
    ((< leng 8) "\t\t\t\t")
    ((< leng 16) "\t\t\t")
    ((< leng 24) "\t\t")
    ((< leng 32) "\t")
  )
)

(DEFUN FEXST (FNAME)
(SETQ appnder (OPEN FNAME "r"))
)

(defun creat (fname / lname lcolr ltype tab1 record lafile)
(if appnder
  (progn
    (princ (strcat "\nAppending  current layer list to " fname))
    (setq lafile (open fname "a"))
  )
  (setq lafile (open fname "w"))
)
(princ "\n" lafile)
(princ "_____________________________________________________________________\n" LAFILE)
(convert_date)
(princ sdate lafile)
(princ (strcat "Drawing Name: " fname) lafile)
  (princ "\n\n" lafile)
  (princ "Layer Name\t\t\tColor\tLine Type\tRemarks\n" LAFILE)
  (princ "_____________________________________________________________________\n" LAFILE)
  (setq record (tblnext "layer" T))
    (while record
      (setq lname (cdr(assoc 2 record))
            lcolr (itoa (cdr(assoc 62 record)))
            ltype (cdr(assoc 6 record))
      )
      (setq tab1 (taber lname))
      (setq lname (strcat lname tab1)
            lcolr (strcat lcolr "\t")
      )
      (princ lname lafile)
      (princ lcolr lafile)
      (princ ltype lafile)
      (princ "\n" lafile)
      (setq record (tblnext "layer"))
    );end while
;  (princ "\n" lafile)
  (close lafile)
(PRINC)
)




(defun c:laylog (/ fname exst nme laynme laylst oldlst appnder)
    (setq nme (getvar "dwgname"))
    (setq fname (getstring (strcat "\nEnter name of layer file <" nme ">: " )))
    (if (= fname "")
        (setq fname nme)
    )
    (setq fname (strcat fname ".lay"))
    (fexst fname)
    (creat fname)
    (princ)
)



