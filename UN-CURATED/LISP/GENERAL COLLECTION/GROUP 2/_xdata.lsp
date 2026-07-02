;This function is used to place all x-data onto entities.  It only handles the most
;used x-data forms of: String, Real or Int

; Written by David Noble - March 1998
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

;For more information, please contact:  dnoble@mwtech.com   -or-  (250) 370-5250


(defun _XData (e1 xlst appname / applst cnt cnt1 elst lst tmp val )

  ;e1      = ename to place x-data on
  ;xlst    = list of all info to place as x-data (string int real etc...)
  ;appname = registered or non-registered appname for x-data
  
  (if (not (tblsearch "APPID" appname))
    (regapp appname)
  )
  (setq applst (list appname))
  
  ;List {  }  = 1002 code 
  ;String     = 1000 code
  ;Real       = 1040 code
  ;INT        = 1070 code
  ;Long       = 1040 code (converted long int to real)
  
  (setq cnt  0
        elst (entget e1)
  )
  
  (repeat (length xlst)           ;pull out all data from xlst one by one and
    (setq val (nth cnt xlst))     ;construct dotted pairs for each.
    (cond
     ((= (type val) 'LIST)  ;if xlst is a list of sublists, we must go deeper to get values
      (setq cnt1   0
            lst    val
            tmp    (cons 1002 "{")            ;open list bracket for x-data
            applst (append applst (list tmp))
      )
      
      (repeat (length lst)
        (setq val (nth cnt1 lst))
        (cond
         ((= (type val) 'STR)
          (setq tmp (cons 1000 val))  
         )
         ((= (type val) 'REAL)
          (setq tmp (cons 1040 val))  ;real
         )
         ((= (type val) 'INT)
          (if (or (> val 32767)(< val -32767))
            (setq tmp (cons 1040 (+ val 0.0)))  ;32-bit signed long integer
          ;else
            (setq tmp (cons 1070 val))  ;16-bit integer
          )
         )
        )
        (setq applst (append applst (list tmp))
              cnt1   (1+ cnt1)
        )
      )
      (setq tmp (cons 1002 "}"))        ;close list bracket for x-data
     )
      
     ((= (type val) 'STR)
      (setq tmp (cons 1000 val))  
     )
     
     ((= (type val) 'REAL)
      (setq tmp (cons 1040 val))  ;real
     )
     
     ((= (type val) 'INT)
      (if (or (> val 32767)(< val -32767))
        (setq tmp (cons 1040 (+ val 0.0)))  ;32-bit signed long integer - make a real so ALisp can read
      ;else
        (setq tmp (cons 1070 val))  ;16-bit integer
      )
     )
    )
    
    (setq applst (append applst (list tmp))
          cnt    (1+ cnt)
    )
  )
  
  (setq applst (cons -3 (list applst))
        elst   (append elst (list applst))
  )
  
  (entmod elst) ;update database 
)

