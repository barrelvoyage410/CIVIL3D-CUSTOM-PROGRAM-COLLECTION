(defun c:CBrka (/ ss tmp i j ent elst inc st)
  (vl-load-com)

  (or Cbrk:num (setq Cbrk:num 2))

  (cond ((setq ss (ssget "_:L" '((0 . "ARC,CIRCLE"))))
         
         (initget 6)
         (and (setq tmp (getint (strcat "\nSpecify Number of Sections <" (itoa Cbrk:num) "> : ")))
              (setq Cbrk:num tmp))

         (setq i -1)
         (while (setq ent (ssname ss (setq i (1+ i))))
           (setq elst (entget ent) j -1)

           (cond (  (eq "CIRCLE" (cdr (assoc 0 elst)))
                    (setq inc (/ (* 2 pi) cBrk:num) st 0.))
                 (t (setq inc (/ (- (cdr (assoc 51 elst))
                                    (setq st (cdr (assoc 50 elst)))) cBrk:num))))

           (repeat cbrk:num
             (entmake (list (cons   0 "ARC")
                            (assoc  8  elst)
                            (assoc 10  elst)
                            (assoc 40  elst)
                            (cons  50 (+ st (* (setq j (1+ j)) inc)))
                            (cons  51 (+ st (* (1+ j) inc)))))
             
             (entmake (list (cons   0 "LINE")
                            (assoc  8   elst)
                            (assoc 10   elst)
                            (cons  11 (polar (cdr (assoc 10 elst))
                                             (+ st (* j inc))
                                             (cdr (assoc 40 elst)))))))

           (if (eq "ARC" (cdr (assoc 0 elst)))
             (entmake (list (cons   0 "LINE")
                            (assoc  8   elst)
                            (assoc 10   elst)
                            (cons  11 (polar (cdr (assoc 10 elst))
                                             (+ st (* (1+ j) inc))
                                             (cdr (assoc 40 elst)))))))
           
         (entdel ent))))

  (princ))