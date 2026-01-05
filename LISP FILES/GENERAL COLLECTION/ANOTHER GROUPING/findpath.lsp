;++++++++++++ FINDPATH +++++++++++++++++++++++++++++++++++++++++++ 
;;;Returns ECS Point Values Of PLINE 
(defun findpath (en / pl ed sp_flg cl_flg bf nl i vp bf vf pl_flg) 
  (and (/= (type en) 'ENAME) 
       (princ "\n*** FindPath Error *** ") 
       (exit)) 
  (setq ed (entget en)) 
  (and (/= "POLYLINE" (cdr (assoc 0 ed))) 
       (princ "\n*** POLYLINEs Only *** ") 
       (exit)) 
  (setq pl_flg (cdr (assoc 70 ed))) 
  (and (= (logand pl_flg 1) 1) 
       (setq cl_flg T)) 
  (and (= (logand pl_flg 4) 4) 
       (setq sp_flg T)) 
  (and (or (= (logand pl_flg 16) 16) 
           (= (logand pl_flg 64) 64)) 
       (princ "\nInvalid POLYLINE Mesh") 
       (exit)) 
  (while (/= "SEQEND" (cdr (assoc 0 (entget (entnext en))))) 
         (setq en (entnext en) 
               ed (entget en) 
               vp (cdr (assoc 10 ed)) 
               bf (cdr (assoc 42 ed)) 
               vf (cdr (assoc 70 ed))) 
         (cond; ((= "SEQEND" (cdr (assoc 0 (entget (entnext en))))) 
              ;  (setq pl (cons vp pl))) 
               ((and (/= bf 0.0) 
                     (/= "SEQEND" (cdr (assoc 0 (entget (entnext en)))))) 
                (add_arc vp (cdr (assoc 10 (entget (entnext en)))) bf)) 
               ((and (/= bf 0.0) 
                     cl_flg 
                     (= "SEQEND" (cdr (assoc 0 (entget (entnext en)))))) 
                (add_arc vp (last pl) bf)) 
               ((and (= bf 1.0) 
                     (not cl_flg) 
                     (= "SEQEND" (cdr (assoc 0 (entget (entnext en)))))) 
                (princ)) 
               ((and sp_flg 
                     (= bf 0.0) 
                     (= (logand vf 8) 8)) 
                (setq pl (cons vp pl))) 
               ((and (not sp_flg) 
                     (= bf 0.0) 
                     (/= (logand vf 8) 8)) 
                (setq pl (cons vp pl))))) 
  (if (and cl_flg 
           (not (equal (car pl) (last pl)))) 
      (setq pl (cons (last pl) pl))) 
  (setq i 0) 
  (while (< i (length pl)) 
         (while (equal (nth i pl) (nth (1+ i) pl) 0.0001) 
                (setq i (1+ i))) 
         (and (nth i pl) 
              (setq nl (cons (nth i pl) nl))) 
         (setq i (1+ i))) 
   nl) 

(defun add_arc (sp ep bulge / alist x1 x2 y1 y2 cotbce 
                ce ra sa ea ia inc qty na temp) 
  (setq x1 (car sp);;Modified Bulge 
        x2 (car ep);;Conversion By 
        y1 (cadr sp);;Duff Kurland 
        y2 (cadr ep);;Autodesk, Inc. 
    cotbce (/ (- (/ 1.0 bulge) bulge) 2.0) 
        ce (list (/ (+ x1 x2 (- (* (- y2 y1) cotbce))) 2.0) 
                 (/ (+ y1 y2    (* (- x2 x1) cotbce) ) 2.0) 
                 (caddr sp)) 
        ra (distance ce sp) 
        sa (atan (- y1 (cadr ce)) (- x1 (car ce))) 
        ea (atan (- y2 (cadr ce)) (- x2 (car ce)))) 
  (if (minusp sa) 
      (setq sa (+ sa (* 2.0 pi)))) 
  (if (minusp ea) 
      (setq ea (+ ea (* 2.0 pi)))) 
  (if (minusp bulge) 
      (setq temp sa sa ea ea temp)) 
  (if (> sa ea) 
      (setq ia (+ (- (* pi 2.0) sa) ea)) 
      (setq ia (- ea sa))) 
  (setq qty (abs (fix (/ ia (/ pi 16))))) 
  (if (< qty 2) 
      (setq qty 2)) 
  (setq na sa 
       inc (/ (abs ia) qty)) 
  (repeat (1+ qty) 
      (setq alist (cons (polar ce na ra) alist) 
               na (+ sa inc) 
               sa na)) 
  (if (not (equal sp (car alist) 0.0001)) 
      (setq alist (reverse alist))) 
  (foreach a alist 
      (setq pl (cons a pl)))) 


(defun c:fpth (/ s ss ppl fn wf) 
  (while (or (not ss) 
             (> (sslength ss) 1)) 
          (princ "\nSelect 1 PLINE ( Heavy )") 
          (setq ss (ssget '((0 . "POLYLINE"))))) 
  (setq s (ssname ss 0)) 
  (setq ppl (findpath s)) 
  (while (or (not fn) 
             (not (snvalid fn))) 
         (setq fn (getstring "\nOutput File Name:   "))) 


  (setq wf (open (strcat fn ".DAT") "w")) 


  (write-line "(setq path_list '(" wf) 
  (foreach p ppl 
   (write-line 
    (strcat "(" (rtos (car p) 2 12) " " 
                (rtos (cadr p) 2 12) " " 
                (rtos (caddr p) 2 12) ")") wf)) 
  (write-line "))" wf) 
  (close wf) 
  (if (= (getvar "ACADVER") "15.0") 
      (progn 
         (initget "Yes No") 
         (if (= "Yes" (getkword "\nMake A SPLINE From The Path Yes/No <N>: 
")) 
             (progn 
               (command "_.SPLINE") 
               (foreach p ppl (command p)) 
               (command "") 
               (command "") 
               (command ""))))) 


  (prin1)) 

