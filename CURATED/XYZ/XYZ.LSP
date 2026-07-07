;;;---------------------------------------------------------------------;;;
;;;  XYZ.lsp								;;;
;;;  Created by Jonathan Norton						;;;
;;;  jonathann3891@gmail.com						;;;
;;;									;;;
;;;  Copyright © 2016                                                   ;;;
;;;                                                                     ;;;
;;;  FUNCTION:                                                          ;;;
;;;  Identify coordinates/elev of a single point			;;;
;;;									;;;
;;;                                                                     ;;;
;;;  PLATFORMS:                                                         ;;;
;;;  Tested on 2012					                ;;;
;;;                                                                     ;;;
;;;									;;;
;;;                                                                     ;;;
;;;  THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED       ;;;
;;;  WARRANTY. ALL IMPLIED WARRANTIES OF FITNESS FOR ANY PARTICULAR     ;;;
;;;  PURPOSE AND OF MERCHANTABILITY ARE HEREBY DISCLAIMED.              ;;;
;;;                                                                     ;;;
;;;									;;;
;;;---------------------------------------------------------------------;;;


(defun c:XYZ ()
  (command-s "UCS" "WORLD")
  (setq os (getvar 'osmode))
  (setvar 'osmode 7)
  (setq flag 4)
  (setq PT1 ( getpoint "\nChoose a Point : ")
	  x (rtos (car   pt1))        
          y (rtos (cadr  pt1))        
          z (rtos (caddr pt1))         
  )
  (setq dcl_id (load_dialog "XYZ.dcl"))
  (while (> flag 2)
  (if(not(new_dialog "XYZ" dcl_id))
    (exit)
	  )
	   (set_tile "X" x)
  	   (set_tile "Y" y)
  	   (set_tile "Z" z)
	   (action_tile "Pick" "(done_dialog 4)")
	   (action_tile "Cancel" "(done_dialog 0)")
  (setq flag (start_dialog))
    (if (= flag 4)
      (progn
	(setq PT1 ( getpoint "\nChoose a Point : ")
	  x (rtos (car   pt1))        
          y (rtos (cadr  pt1))        
          z (rtos (caddr pt1))
	      )
	)
      )
    )
  (unload_dialog dcl_id)
  (setvar 'osmode os)
  (princ)
  )