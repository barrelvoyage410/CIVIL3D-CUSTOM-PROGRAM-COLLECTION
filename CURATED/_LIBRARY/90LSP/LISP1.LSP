(defun c:lisp1 (/ os pt1 pt2)
  (setq os (getvar "osmode"))
  (setvar "osmode" 33)
  (setq pt1 (getpoint "\nPick first line "))
  (setq pt2 (getpoint "\nPick second line "))
  (command "line" pt1 pt2 "")
  (setvar "osmode" os)
)
