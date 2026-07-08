;   Length/Area of Polyline by Layer
;   David Bethel May 2004 from an original idea by David Watson
;   This command will give a total area or length for all polylines on a specified layer.
;
(defun c:zone ( / ss la rv i tv op en) 

   (while (not ss) 
          (princ "\nPick any object on the required layer") 
          (setq ss (ssget))) 

   (initget "Length Area") 
   (setq rv (getkword "\nWould you like to measure Length/<Area> : ")) 
   (and (not rv) 
        (setq rv "Area")) 

   (setq la (cdr (assoc 8 (entget (ssname ss 0)))) 
         ss (ssget "X" (list (cons 0 "*POLYLINE") 
                             (cons 8 la))) 
          i (sslength ss) 
         tv 0 
         op 0) 
   (while (not (minusp (setq i (1- i)))) 
          (setq en (ssname ss i)) 
          (command "_.AREA" "_E" en) 
          (cond ((= rv "Length") 
                 (setq tv (+ tv (getvar "PERIMETER")))) 
                (T 
                 (setq tv (+ tv (getvar "AREA"))) 
                 (if (/= (logand (cdr (assoc 70 (entget en))) 1) 1) 
                     (setq op (1+ op)))))) 

   (princ (strcat "\nTotal " rv 
                  " for layer " la 
                  " = " (rtos tv 2 2) 
                  " in " (itoa (sslength ss)) " polylines\n" 
                  (if (/= rv "Length") 
                      (strcat (itoa op) " with open polylines") ""))) 
   (prin1))