;cumulative distance
;Don Jacobsen
;Kennewick, WA, USA
;drjcjj@3-cities.com
;this routine is just like the Autocad Distance command with the
;exception that it allows you to pick more than 2 consecutive points.
;the routine will display the cumulative distance and the distance
;between the last two points picked on the command line.
; Tested in 2000 - 08/31/00

(defun c:cd ()
 (setvar "cmdecho" 0)
 (graphscr)
 (setq 
  p1 (getpoint "\nPick start point ")
  p2 (getpoint p1 "\nPick next point ")
  d1 (distance p1 p2)
  prdist (strcat "\nDistance: " (rtos d1))
 )
 (princ prdist)
 (setq p3 (getpoint p2 "\nPick next point or RETURN if done "))
 (while p3
  (setq
   d0 (distance p2 p3)
   d1 (+ (distance p2 p3) d1)
   p2 p3
   prdist (strcat "\nDistance: " (rtos d0) ", Cumulative distance: " (rtos d1))
  )
  (princ prdist)
  (setq p3 (getpoint p2 "\nPick Next Point "))
 )
 (setq cumd (strcat "Cumulative distance --> " (rtos d1)))
 (prompt cumd)
 (princ)
)
(princ "\nType CD to run Cumulative Distance")
(princ)
     
