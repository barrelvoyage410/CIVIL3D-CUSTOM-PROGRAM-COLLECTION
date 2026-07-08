(defun c:lisp117 (/ part pt1)
  (setq part (getstring "\nPart name  "))
  (setq pt1 (getpoint "\nInsertion point  "))
  (command "insert" part pt1 "" "" "")
  (command "array" "l" "" "r")
)
