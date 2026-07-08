;Hpglin.LSP: Imports AutoCAD HPGL plot files into a drawing. Not tried with
;	     other HPGL files. Can be used to import 3D plots for 2D conversion.
;__________________________________________________________________________



(defun sclcd (val / )
 (rtos (/ (atof val) 1016.0) 2 4)
)

(defun coord (/ tstr tstra tstrb)
(Setq tstr "")
(setq char-b (chr (read-char plfile)))
(while (/= char-b ";")
   (if (/= char-b ",")
     (setq tstr (strcat tstr char-b))
   )
   (setq char-b (chr (read-char plfile)))
   (if (= char-b ",")
       (progn
       (setq tstra (sclcd tstr))
       (setq tstr "")
       );progn
   );if
);while
(setq tstrb (sclcd tstr))
(Setq tstr (strcat tstra "," tstrb))
tstr
)

(defun getpen (/ pen1)
(setq lastpn pen)
(while (and
          (/= col "end")
          (/= pen1 "SP")
          (/= pen1 "PU")
          (/= pen1 "PD")
          (/= pen1 "PA")
       )
    (setq char-b (chr (read-char plfile)))
    (setq pen1 (strcat char-a char-b))
    (Setq char-a char-b)

)
pen1
)

(defun c:hpglin (/ pen char-a char-b col fname plfile blp lastpt lastpn)
(Setvar "cmdecho" 0)
(setq blp (getvar "blipmode"))
(setvar "blipmode" 0)
(setq fname (getstring "\nEnter name of HPGL plot file: "))
(princ "\nReading file - Please wait....")
(command "zoom" "c" "0,0" "")
(setq plfile (open fname "r"))
(setq char-a (chr (read-char plfile)))
(while (/= pen "SP")
    (setq char-b (chr (read-char plfile)))
    (setq pen (strcat char-a char-b))
    (Setq char-a char-b)
)

(while (/= col "end")

(if (= pen "SP")
  (progn
  (setq char-b (chr(read-char plfile)))
  (if (= (type (read char-b)) 'INT)
    (setq col char-b)
    (setq col "end")
  )
  (setq pen (getpen))
  )
)
(if (= pen "PU")
  (progn
  (setq llst (append llst (list "")))
(setq pwidth "0")
(cond
  ((= col "1")(setq pwidth ".001"))
  ((= col "2")(setq pwidth ".001"))
  ((= col "3")(setq pwidth ".010"))
  ((= col "4")(setq pwidth ".010"))
  ((= col "5")(setq pwidth ".013"))
  ((= col "6")(setq pwidth ".013"))
  ((= col "7")(setq pwidth ".018"))
  ((= col "8")(setq pwidth ".018"))
  ((= col "9")(setq pwidth ".025"))
  ((= col "10")(setq pwidth ".025"))
  ((= col "11")(setq pwidth ".035"))
  ((= col "12")(setq pwidth ".035"))
)
 (command "color" col
           "pline" first "W" pwidth "")
  (foreach n llst (command n))
  (setq pen (getpen))
  )
)
(if (= pen "PD")
  (progn
       (Setq llst '())
       (if (= lastpn "PU")
          (setq first lastpt)
       )
       (setq pen (getpen))
       (while (= pen "PA")
         (setq llst (append llst (list (setq lastpt (coord)))))
         (Setq pen (getpen))
       )
  )
)
(if (= pen "PA")
   (progn
       (setq first (coord))
       (setq pen (getpen))
   )
)

);while
(close plfile)
(setvar "blipmode" blp)
(princ)
)