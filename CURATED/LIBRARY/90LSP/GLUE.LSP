;GLUE.LSP
;
;Draws a single line between the two most-distant points of two
;user-selected lines.  Erases the two user-selected lines.  Assigns
;to the new line the same properties of Layer, Color, Linetype, and
;Thickness as those of the first user-selected line.
;
;For AutoCAD Release 10
;
;Written 02/19/1989  Brad Zehring  Autodesk, Inc. Training Department
;
(defun C:GLUE (/ l1 l2 e1 e2 dl ml temp p pt1 pt2
                 old_flatland old_cmdecho)
  (setq old_flatland (getvar "flatland"))
  (setq old_cmdecho (getvar "cmdecho"))
  (setvar "flatland" 0)
  (setvar "cmdecho" 0)
  (setq l1 (entsel "\nSelect first line: "))
  (setq l2 (entsel "\nSelect second line: "))
  (if
    (or (eq l1 nil) (eq l2 nil))
    (prompt "\nRequires two lines. *Invalid*")
    (progn
      (if
        (not
          (and
            (eq "LINE" (cdr (assoc 0 (setq e1 (entget (car l1))))))
            (eq "LINE" (cdr (assoc 0 (setq e2 (entget (car l2))))))
          )
        )
        (prompt "\nRequires two lines. *Invalid*")
        (progn
          (setq dl nil)
          (setq dl
            (cons
              (list (distance (cdr (assoc 10 e1)) (cdr (assoc 10 e2))) 11)
              dl
            )
          )
          (setq dl
            (cons
              (list (distance (cdr (assoc 10 e1)) (cdr (assoc 11 e2))) 12)
              dl
            )
          )
          (setq dl
            (cons
              (list (distance (cdr (assoc 11 e1)) (cdr (assoc 10 e2))) 21)
              dl
            )
          )
          (setq dl
            (cons
              (list (distance (cdr (assoc 11 e1)) (cdr (assoc 11 e2))) 22)
              dl
            )
          )
          (setq ml nil temp dl)
          (repeat 4
            (setq ml (cons (car (car temp)) ml))
            (setq temp (cdr temp))
          )
          (setq p (car (cdr (assoc (eval (cons 'MAX ml)) dl))))
          (cond
            ((= p 11)
             (setq pt1 (cdr (assoc 10 e1)))
             (setq pt2 (cdr (assoc 10 e2)))
            )
            ((= p 12)
             (setq pt1 (cdr (assoc 10 e1)))
             (setq pt2 (cdr (assoc 11 e2)))
            )
            ((= p 21)
             (setq pt1 (cdr (assoc 11 e1)))
             (setq pt2 (cdr (assoc 10 e2)))
            )
            ((= p 22)
             (setq pt1 (cdr (assoc 11 e1)))
             (setq pt2 (cdr (assoc 11 e2)))
            )
            (t)
          )
          (command ".line" pt1 pt2 "")
          (command ".chprop" (entlast) ""
                   "LA" (cdr (assoc 8 e1))
                   "C"  (if
                          (null (cdr (assoc 62 e1)))
                          ""
                          (cdr (assoc 62 e1))
                        )
                   "LT" (if
                          (null (cdr (assoc 6 e1)))
                          ""
                          (cdr (assoc 6 e1))
                        )
                   "T"  (if
                          (null (cdr (assoc 39 e1)))
                          ""
                          (cdr (assoc 39 e1))
                        )
                   ""
          )
          (command ".erase" l1 l2 "")
        )
      )
    )
  )
  (setvar "flatland" old_flatland)
  (setvar "cmdecho" old_cmdecho)
  (prin1)
)
