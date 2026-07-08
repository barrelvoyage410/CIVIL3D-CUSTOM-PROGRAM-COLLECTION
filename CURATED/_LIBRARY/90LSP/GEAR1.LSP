(defun grinfo ()
   (setq c (getpoint "\nCenter of Gear: ")) 
   (setq pc (getpoint "\nPitch Circle: "))
   (setq teeth (getint "\nNumber of teeth: ")) 
   (while (< teeth 14) (setq teeth (getint "\nMust be at least 14 teeth: "))) 
   (while (> teeth 50) (setq teeth (getint "\nMust be no more than 50 teeth: "))) 
   (while (< teeth 14) (setq teeth (getint "\nMust be at least 14 teeth: "))) 
   (while (> teeth 50) (setq teeth (getint "\nMust be no more than 50 teeth: "))) 
)
(defun calgear ()
   (setq d (* 2 (distance c pc))) (setq pc (polar c 0 (/ d 2)))
   (setq ang3 2.79248)
   (setq ang5 (/ 360.000000 teeth))
   (setq wang -0.785385)
   (setq wang2 0.785385)
   (setq dp (/ teeth d))
   (setq ht (/ 0.32 dp))
   (setq adm (/ 1.0 dp))
   (setq y (* (/ 2.257 dp) 1.0642))
)
(defun drawgear ()
   (command "PLINE"
      (setq p (polar pc 0 adm))
      "a" 
      "ce" 
      c 
      "l" 
      ht 
      "l" 
      (setq p (polar (getvar "lastpoint") ang3 y)) 
      "a" 
      "ce" 
      c 
      "l" 
      ht 
      ""
   )
   (SETQ TOP (GETVAR "LASTPOINT")) 
   (command "MIRROR" 
      "L" 
      ""
      (setq p (polar pc 0 adm)) 
      c 
      "n" 
   )
   (command "ARRAY"
       "w"
       (POLAR TOP 4.712389 (* Y 1.5))
       (polar TOP 0 Y)
       ""
       "c"
       c
       ang5
       "-360"
       "Y"
   )
)
(defun C:GEAR ()
    (grinfo)
    (setq stdots (getvar "blipmode"))
    (setq stech (getvar "cmdecho"))
    (setvar "blipmode" 0)
    (setvar "cmdecho" 0)
    (calgear)
    (drawgear)
    (setvar "blipmode" stdots)
    (setvar "cmdecho" stech)
)
