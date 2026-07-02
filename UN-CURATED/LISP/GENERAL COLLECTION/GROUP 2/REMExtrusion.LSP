(defun C:RemExtrusion ()
  (setq ss (ssget))
  (setq n (sslength ss)
  count 0       )
  (while (< count n)
    (setq EN (entnext (ssname ss n)))
    (setq ED (entget EN))
    (if (= (assoc 210 ED) nil)
      (setq count (+ count 1.0))
      (progn
        (setq ED (subst (cons 210 "0,0,1") (assoc 210 ED) ED))
        (entmod ED)
      );end progn
    );end if
  ); end while
); end defun
