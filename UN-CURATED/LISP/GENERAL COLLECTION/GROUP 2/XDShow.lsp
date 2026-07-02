;Shows stored xdata on selected entity.  Only shows most common types of String, Real and Int.

;Written by David Noble - July 6th, 1998

;Copyright 2000 MW Technologies  All Rights Reserved
;www.mwtech.com

;''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
; You are free to use this code within your own applications,
; but you are expressly forbidden from selling or otherwise 
; distributing this source code without prior written consent.
; This includes both posting free demo projects made from this 
; code as well as reproducing the code in text or html format. 
;''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

;MW Technologies provides this program "As Is" and with all faults.  MW Technologies
;specifically disclaims any implied warranty of fitness for a particular use.
;MW Technologies does not warrant that the operation of the program will be
;uninterrupted or error free.



;REVISIONS
;---------
;None

(defun C:XDShow ( / applst appnum cmd cnt cnt1 dcl_id e1 tmp1 x_elst x_lst xd xname xname_lst)
 
  (setvar "cmdecho" 0)
  (setq olderr *error*   *error* _XDERR) 
  
  ;Create Appid List
  (setq tmp1 (tblnext "APPID" 'T))         
  (while tmp1                              
    (if (/= (cdr (assoc 2 tmp1)) "ACAD")
      (setq applst (cons (cdr (assoc 2 tmp1)) applst))
    )
    (setq tmp1 (tblnext "APPID"))
  )

  (setq applst (reverse applst)
        cmd T
        cnt 0
  )
  (while cmd
    (setq e1   nil
          cnt1 0
          xname_lst nil
    )
    (while (not e1)
      (setq e1 (entsel "\nSelect entity to display its xdata: "))
    )
    (princ "\n                                                  ")
  
    (setq e1     (car e1)
          x_elst (entget e1 applst)   ;check to see if xdata exists
    )
        
    (setq x_elst (cdr (assoc -3 x_elst)) ;now just the -3 data list
          cnt    0
    )
    
    (repeat (length x_elst)     ;xname_lst contains list of all appnames for this entity
      (setq xname_lst (append xname_lst (list (car (nth cnt x_elst))))
            cnt       (1+ cnt)
      )
    )

    (setq x_lst  (cdr (car x_elst)))
    
    ;(princ x_lst)

    (if x_lst
     (progn
      (setq dcl_id (load_dialog "XDShow.dcl"))
      (if (not (new_dialog "XShow" dcl_id))          
        (exit)
      )
    
      (start_list "applst")                      ;fill out registered app list.
        (mapcar ' add_list xname_lst)  
      (end_list)
  
      (set_tile "applst" "0")
    
      (start_list "xlst")
        (mapcar '(lambda (X) 
         (add_list 
           (strcat (itoa (car X)) "   " 
             (cond
              ((or (= (car X) 1000)(= (car X) 1002))
               (cdr X)
              )
              ((= (car X) 1040)
                (rtos (cdr X))
              )
              ((= (car X) 1071)
                (rtos (cdr X) 2 0)
              )
              ((= (car X) 1070)
               (itoa (cdr X))
              )
              (t  "**CANNOT DISPLAY")
             )
           )
         ))
         x_lst
        ) 
      (end_list)
      (set_tile "xlst" "0")
  
      (action_tile "again"                  ;Display another button
        (strcat "(progn "
                "(done_dialog)"
                "(unload_dialog dcl_id))"
        )
      )
      
      (action_tile "applst"                  ;Display another button
        (strcat "(progn "
                "(setq appnum (atoi (get_tile \"applst\")))"
                "(XDPopControl appnum))"
        )
      )
      
      (action_tile "done"
        (strcat "(progn (done_dialog)"
                "(unload_dialog dcl_id)"
                "(setq cmd nil))"
        )
      )
      
      (start_dialog)
    
     )
    ;else
     (princ "\r>>No XData is associated with that entity, Choose again.")
    );if
  )
  
  (princ)
)


(defun XDPopControl ( appnum / )

  ;appnum = position of appname in list

  (setq x_lst (cdr (nth appnum x_elst)))
  (start_list "xlst")
    (mapcar '(lambda (X) 
     (add_list 
       (strcat (itoa (car X)) "   " 
         (cond
          ((or (= (car X) 1000)(= (car X) 1002))
           (cdr X)
          )
          ((= (car X) 1040)
            (rtos (cdr X))
          )
          ((= (car X) 1070)
           (itoa (cdr X))
          )
          (t  "**CANNOT DISPLAY")
         )
       )
     ))
     x_lst
    ) 
  (end_list)
  (set_tile "xlst" "0")
  
)


;Standard error handler
(defun _XDERR (msg)
  
  (setq *error* olderr)
  (setvar "cmdecho" 1)

  (if (/= msg "quit / exit abort")
   (progn
    (princ "\n \nError: ")
    (princ msg)
   )
  )
  (princ)
) 
