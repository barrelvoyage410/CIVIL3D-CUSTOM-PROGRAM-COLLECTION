(defun c:renum ()
  (prompt "\nSelect entities to change number value: ")
  (setq  ss  (ssget  '((0  . "TEXT"))))
  (if ss
    (progn
      (setq NEW_RAD (getreal "\nEnter lot number adjustment value: "))
      (setq n (sslength ss))
      (setq l  0)
      (while (< l n)
        (setq en (entget (ssname ss l)))
        ;(setq old_num (assoc 1 (entget (ssname ss l))))
        (setq old_num (atoi (cdr (assoc 1 en))))
        (setq new_num (+ NEW_RAD old_num))
        (setq new_text (rtos new_num 2 0))
        (setq en
          (subst (cons '1 new_text)
            (assoc 1 en)
          en)
        )
        (entmod en)
        (setq l (1+  l))
      );end while
      (prompt (strcat "\n" (itoa l) " lot numbers changed by value of " (rtos NEW_RAD 2 0)))
    );end progn
    (alert "No text in selection set!")
  );end if
  (princ)
)
