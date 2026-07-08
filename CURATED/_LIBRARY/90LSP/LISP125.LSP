(defun c:lisp125 (/ flg cnt e)
  (setq flg 1)
  (setq cnt 0)
  (while (setq e (tblnext "layer" flg))
    (setq cnt (+ cnt 1))
    (setq flg nil)
  )
  (repeat cnt
  (command "purge" "la" "y")
  )
)
