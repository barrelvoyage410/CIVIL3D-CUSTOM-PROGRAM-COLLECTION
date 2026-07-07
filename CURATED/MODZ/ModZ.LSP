;;;---------------------------------------------------------------------;;;
;;;  ModZ.lsp								;;;
;;;  Created by Jonathan Norton						;;;
;;;  jonathann3891@gmail.com						;;;
;;;									;;;
;;;  Copyright © 2016                                                   ;;;
;;;                                                                     ;;;
;;;  FUNCTION:                                                          ;;;
;;;  Modify Objects "Z" Elevation w/DCL					;;;
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

(defun c:ModZ (/ dcl_id flag SaveVars ss ssi)
  (vl-load-com)

  (defun SaveVars(/ Elv)
    (setq Elv(distof(get_tile "Elv")))
    )
    
  (if(not Elv)
    (setq Elv "0.00")
    )
  (command-s "UCS" "WORLD")
  (setq flag 4)
  (setq dcl_id (load_dialog "ModZ.dcl"))
  (while (> flag 2)
  (if (not (new_dialog "ModZ" dcl_id))
    (exit)
  )
  (set_tile "Elv" Elv)
  (action_tile "Elv" "(setq Elv $value)")
  (action_tile "Accept" "(SaveVars)(done_dialog 2)")
  (action_tile "Pick" "(SaveVars)(done_dialog 3)")
  (action_tile "GetZ" "(done_dialog 4)")
  (action_tile "Cancel"  "(done_dialog 0)(setq Elv nil)")
  (setq flag (start_dialog))
  
    (if (= flag 2)
      (progn
	(setq ssi (ssget "X"))
	   (unload_dialog dcl_id)
		(command "_.CHANGE" ssi "" "_P" "_E" Elv "")
	(princ
	  (strcat "Object(s) Elevation set to " Elv))
	);progn
      );if
  
    (if (= flag 3)
      (progn
	(setq ss (ssget))
	(unload_dialog dcl_id)
	(command "_.CHANGE" ss "" "_P" "_E" Elv "")
	(princ
	  (strcat "Object(s) Elevation set to " Elv))
	);progn
      );if

  (if (= flag 4)
    (progn
      (setq OS (getvar 'osmode))
      (setvar 'osmode 7)
      (setq Pnt (getpoint "\nPick Point on Elevation: "))
      (setq PtZ# (caddr Pnt)
	    Elv (rtos PtZ# 4 8)    
	    );setq
      );progn
    );if
    );while
  (princ)
  );defun

