;   Flatten a 3D drawing 
;   Written by Eduard 
;   This command will set all elevations and points to zero, efectively flattening any 3D drawing. 
; 
(defun c:flat (/ total-nabor) 
  (vl-load-com) 
  (if 
    (setq total-nabor (ssget "x" '((410 . "model")))) 
     (progn 
       (setq total-nabor 
        (mapcar 'vlax-ename->vla-object 
          (mapcar 'cadr 
            (ssnamex total-nabor) 

          ) ;_ end of mapcar 
        ) ;_ end of mapcar 
       ) ;_ end of setq 
       (foreach  i '(1e99 -1e99) 
   (mapcar (function (lambda (x) 
           (vla-move x 
               (vlax-3d-point (list 0 0 0)) 
               (vlax-3d-point (list 0 0 i)) 
           ) ;_ end of vla-move 
         ) ;_ end of lambda 
     ) ;_ end of function 
     total-nabor 
   ) ;_ end of mapcar 
       ) ;_ end of foreach 
     ) ;_ end of progn 
  ) ;_ end of if 
  (princ) 
) ;_ end of defun