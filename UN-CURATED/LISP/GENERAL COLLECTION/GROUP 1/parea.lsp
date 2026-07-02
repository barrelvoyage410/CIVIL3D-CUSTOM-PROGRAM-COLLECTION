Calculates the area of one or more closed polylines and 
    displays the result in an AutoCAD Alert Window. 

	Command prompt: "PAREA"
-----------------------------------------------------------------------------------------
(defun c:parea (/ a ss n du)
  (setq a  0
        ;du (getvar "dimunit")
        ss (ssget '((0 . "*POLYLINE")))
  )
  (if ss
    (progn
      (setq n (1- (sslength ss)))
      (while (>= n 0)
        (command "_.area" "_o" (ssname ss n))
        (setq a (+ a (getvar "area"))
              n (1- n)
        )
      )
      (alert
        (strcat "The total area of the selected\nobject(s) is:\n\n "
                ;(if (or (= du 3) (= du 4) (= du 6))
                  (strcat
                    (rtos a 2 3)
                    " Square Feet,\nor\n "
                    (rtos (/ a 9) 2 3)
                    " Square Yards,\nor\n"
                    (rtos (/ a 43560) 2 3)
                    " Acres"
                  )
                ;)
        )
      )
    )
    (alert "\nNo Polylines selected!")
  )
  (princ)
)
