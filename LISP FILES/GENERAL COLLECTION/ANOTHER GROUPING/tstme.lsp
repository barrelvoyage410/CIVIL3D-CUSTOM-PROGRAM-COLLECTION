(defun
      c:testme  ()
  (defun
        calc
            (ent pnt)
    (setq pntobj (vlax-curve-getclosestpointto ent pnt))
    (setq sta (vlax-curve-getdistatpoint ent pntobj))
    (setq off (distance pntobj pnt))
    (princ (strcat
             "\rSTA="
             (rtos sta 2 2)
             ", OFFSET="
             (rtos off 2 2)
             " Press <enter> to quit"
             ) ;_ end of strcat
           ) ;_ end of princ
    ) ;_ end of defun
  (setq ent (car (entsel "\n Select Centerline: ")))
  (setq track t)
  (while track
    (setq track (cadr (grread t 1)))
    (setq pnt track)
    (calc ent pnt)
    ) ;_ end of while
  ) ;_ end of defun
