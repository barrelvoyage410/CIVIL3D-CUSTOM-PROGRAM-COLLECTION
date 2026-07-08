;   Delete Layer(s)
;   Written by Mark Thomas
;   This file contains 2 functions:
;   del-layer will delete all objects on a selected layer and purge it
;   mdel-layer will delete all objects on selected layers and purge them 

(defun c:del-layer (/ ent l_name ss cntr amt ssent) 

;;; 
;;; erases all objects on selected layer then purges that layer 
;;; 

  (setvar 'clayer "0") ; set layer to 0 

  (if 
    ; make sure we get something 
    (setq ent (car (entsel "\nSelect layer to remove: "))); test 
    (progn 
      ; extract the layer name from the entity 
      (setq l_name (cdr (assoc 8 (entget ent)))) 

      ; create a selection set of all entites on layer 'l_name' 
      (setq ss (ssget "X" (list (cons 8 l_name))) 
            ; set 'cntr' to number of items in selection set 
            cntr (1- (sslength ss)) 
            amt (itoa cntr); make a string from an integer 
            ) 

      (if 
        ; does the sel set have anything in it 
        (> cntr 0); test 

        (while 
          ; as long as 'cntr' is greater than or equal to 0 
          ; keep looping 
          (>= cntr 0) 

          ; extract the ename from the sel set 
          (setq ssent (ssname ss cntr)) 
          (entdel ssent); delete that entity 
          (setq cntr (1- cntr)); subtract 1 from cntr 
          ) 

        ) 
      ) 
    ) 
  (command "_.purge" "LA" l_name "N") 
  (princ (strcat "\nErased " amt " items")) 
  (princ) 
  ) 

;;; 
;;; erases all objects on selected layers and purges them 
;;; 

(defun c:mdel-layer (/ lst) 
  (setq cmd (getvar 'cmdecho)) 

  ; turn off the echo! 
  (setvar 'cmdecho 0) 

  (if 
    ; make sure we get a list before we continue 
    (setq lst (fx-make-layer-list)); test 

    ; now that we have a list run 'fx-rm-layer' on each of those items 
    (mapcar '(lambda (x) (fx-rm-layer x)) lst) 
    ) 

  ; reset the variable 
  (setvar 'cmdecho cmd) 
  (princ) 
  ) 

;;; 
;;; functions ======================================================
;;; 

(defun fx-rm-layer (l_name / ss sntr amt ssent) 

;;; 
;;; erase and purges all entites on 'l_name', a string 
;;; returns the number of objects erased 
;;; 

  (setvar 'clayer "0") 

  (if 
    ; make sure the layer exists in the dwg 
    (tblsearch "layer" l_name); test 
    (progn ; continue 
      (setq ss (ssget "X" (list (cons 8 l_name))) 
            cntr (1- (sslength ss)) 
            amt cntr 
            ) 

      (if (> cntr 0) 
        (while 
          (>= cntr 0) 
          (setq ssent (ssname ss cntr)) 
          (entdel ssent) 
          (setq cntr (1- cntr)) 
          ) 
        ) 
      ) 
    ) 
  (if (> amt 0) 
    (command "_.purge" "LA" l_name "N") 
    ) 
  ; return the amount of entites erased 
  amt 
  ) 


(defun fx-make-layer-list (/ ent l_name entlst) 

;;; 
;;; generate a list of layer names based on user selection 
;;; and returns that list 
;;; 

  (while 
    ; while user is selecting something continue loop 
    (setq ent (car (entsel "\nSelect Item on Layer: "))); test 

    ; extract layer name from selected entity 
    (setq l_name (cdr (assoc 8 (entget ent)))) 
    (prompt l_name); output the layer 

    (if 
      ; make sure the layer isn't already in the list 
      (not (vl-position l_name entlst)); test 

      ; if not then add it to the list 
      (setq entlst (cons l_name entlst)) 
      ) 
    ) 
  ) 

