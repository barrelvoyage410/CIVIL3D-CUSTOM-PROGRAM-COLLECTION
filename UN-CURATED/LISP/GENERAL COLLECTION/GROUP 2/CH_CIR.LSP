(defun  c:ch_cir  ( / ss NEW_RAD n l)
  (prompt "\nSelect entities to change CIRCLE radius: ")
  (setq  ss  (ssget  '((0  . "CIRCLE"))))
  (if ss
    (progn
      (initget 7)
      (setq NEW_RAD (getreal "\nEnter new radius for ALL circles: "))
      (setq n (sslength ss))
      (setq l  0)
      (while (< l n)
        (command "._change" (ssname ss l) "" "" NEW_RAD )
        (setq l (1+  l))
      );end while
      (prompt (strcat "\n" (itoa l) " Circles changed to radius " (rtos NEW_RAD 2 2)))
    );end progn
    (alert "No circles in selection set!")
  );end if
  (princ)
);end ch_cir

