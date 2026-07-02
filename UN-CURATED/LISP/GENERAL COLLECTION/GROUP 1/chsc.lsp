;Tip1361.LSP:  CHSC.LSP  Change Scale of Blocks  (c)1997, V. Levin

(defun C:CHSC ( / n ss sv i e m)
  (setvar "cmdecho" 0)
  (graphscr)
  (prompt "\n Select blocks to change scale or rotation angle of -")
  (setq ss (ssget '((0 . "INSERT"))))
  (if ss
    (progn
      (initget "X Y Z All R")
      (setq n (getkword "\n Property to change - X/Y/Z/All scales/Rotation angle/<NONE>: "))
    )
  )
  (if n
    (progn
      (initget (if (= n "R") 1 3))
      (setq sv (getreal
                 (strcat "\n New "
                   (if (= n "R")
                     "Rotation angle"
                     (if (= n "All") "XYZ-scale" n)
                   )
                   " for the selected blocks: "
                 )
               )
            i 0
            n (cond
                ((= n "All") '(41 42 43))
                ((= n "R") (setq sv (* (/ pi 180) sv)) '(50))
                (T (list (- (ascii n) 47)))
              )
      )
      (repeat (sslength ss)
        (setq e (entget (ssname ss i))  i (1+ i))
        (foreach m n (setq e (subst (cons m sv) (assoc m e) e)))
        (entmod e)
      )
      (setq ss nil)
    )
  )
  (princ)
)

(prompt "\n CHSC - changes X,Y,Z scale and Rotation angle of selected blocks.")
(princ)