(defun c:lisp107 (/ blm a n i)
  (setq blm (getvar "blipmode"))
  (setvar "blipmode" 0)
  (setq a (ssget))
  (setq n (sslength a))
  (setq i 0)
  (repeat n
    (redraw (ssname a i) 2)
    (setq i (+ i 1))
  )
  (setvar "blipmode" blm)
)
