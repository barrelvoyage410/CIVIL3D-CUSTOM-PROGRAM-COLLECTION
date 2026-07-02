(defun C:SPLINE2PLINE (/ SPLINES PLINETYPE OSMODE I SPL ED CODEPAIR)
  (if
    (setq SPLINES (ssget (list (cons 0 "spline"))))
     (progn
       (if
         (zerop (setq PLINETYPE (getvar "plinetype")))
          (setvar "plinetype" 1)
          ) ;if
       (setq OSMODE (getvar "osmode"))
       (setvar "osmode" 0)
       (setq I 0)
       (while
         (setq SPL (ssname SPLINES I))
          (setq    I  (1+ I)
                ED (entget SPL)
                ) ;setq
          (command ".pline")
          (foreach
               CODEPAIR
                       ED
            (if
              (= 10 (car CODEPAIR))
               (command (cdr CODEPAIR))
               ) ;if
            ) ;foreach
          (command "")
          (command ".pedit" "l" "s" "")
          ) ;while
       (if PLINETYPE
         (setvar "plinetype" PLINETYPE)
         )
       (setvar "osmode" OSMODE)
       ) ;progn
     ) ;if
  (princ)
  ) ;defun