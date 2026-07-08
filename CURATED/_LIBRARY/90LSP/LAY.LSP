(defun laydel ()
(prompt "\nSub-command not in use in this version.")(princ)
)

(defun layadd ()
(setq fil(opf 2 "a"))
(if (not (null fil))
(progn
(setq lname(getstring "Enter layer Name: "))
(while (>(setq lclr(getint "Enter Color Number: "))255)(alert"Max Color Number=255"))
(while(null(tblsearch "ltype" (setq llt(getstring "Enter Linetype: "))))(alert"Linetype Not Loaded!"))
(setq txt(writedat fil lname lclr llt))
(if (null txt)(alert "Addition Unsuccessful!"))
(close fil)
))
)

(defun layload ();main layer load routine
(setq fil(opf 2 "r"))
(if (not(null fil))
(progn
(while (not(null(setq nst(read-line fil))))
(if (null nst)(princ nst)(makelay nst)))
(prompt "\n")
(princ lct)
(prompt " layer(s) loaded.")(princ)
))
)

(defun makelay (l)
(setq t(-(findlrecord l 1)1))
(setq lay (substr l 1 t))
(setq 2st (+ t 2))
(setq t2(findlrecord l 2))
(setq t (- t2 2st))
(setq cl(substr l 2st t))
(setq 3st (+ t2 1))
(setq lt(substr l 3st))
(if (null(tblsearch "ltype" lt))(command "linetype" "load" lt "js" ""))
(if (null(tblsearch "layer" lay))
(progn(command "layer" "m" lay "c" (atoi cl) "" "lt" lt "" "")(setq lct (+ lct 1)))
)
)


(defun findlrecord (st num)
(setq ct2 0)
(setq ct 1)
(setq len(strlen st))
(while (>= len ct)
(setq ch(substr st ct 1))
(if (= (ascii ch) 59)(setq ct2(+ ct2 1)))
(if (= ct2 num)(progn(setq pos ct)(setq len (- ct 1)))(setq ct (+ ct 1)))
)
(setq pos ct)
)


(defun laysave ();main layer save routine
(setq z 1)
(setq tbl(tblnext "layer" 1))
(setq f (opf 1 "w"))
(if (not(null f))
(progn
(while (= z 1)
(setq tbl (tblnext "layer"))
(if (not (null tbl))
(progn
(setq ly(cdr(assoc 2 tbl)))
(setq cl(cdr(assoc 62 tbl)))
(setq lt(cdr(assoc 6 tbl)))
(writedat f ly cl lt))(setq z 0))
)
(tblnext "layer" 1)
(prompt"\n")
(princ sct)
(prompt " layer(s) saved.")(princ)
(close fdesc)
(princ)
))
)

(defun writedat (fd l c t);writes data to file
(setq s (strcat l";"(itoa c)";"t))
(setq sct(+ sct 1))
(write-line s fd)
)

(defun opf (var fun);displays dialog for file open
(setq fnme(getfiled "Select a Layer Data File" "/acad12/" "ldt" var))
(if (not(null fnme))(setq fdesc (open fnme fun))(setq fnme nil))
)

(defun c:lay ();main program
(setq lct 0)
(setq sct 0)
(initget 1 "D A S L Q")
(setq ans (getkword "\nAdd/Delete/Save/Load/Quit:"))
(cond  ((= (strcase ans) "S")(laysave))
       ((= (strcase ans) "L")(layload))
       ((= (strcase ans) "A")(layadd))
       ((= (strcase ans) "D")(laydel))
       ((= (strcase ans) "Q")(bye))
       (t nil)
)
)

(defun bye();exit routine
(princ)
)


(setq vers "1.00")
(prompt "\nLayer Manager v")
(princ vers)
(prompt " Loaded..")
(prompt "\nType LAY to start.")
(prompt "\nBy Dynamic Enterprises..")
(prompt "\ncopyright 1993")
(princ)


