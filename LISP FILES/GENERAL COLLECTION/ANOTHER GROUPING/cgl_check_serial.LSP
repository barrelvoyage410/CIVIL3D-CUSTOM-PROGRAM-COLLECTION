(defun CGL_Check_Serial (/ f check_no)
  ;(setq CGL_CheckSerial nil)

  (setq f (open "X:\\X-Cowhey\\Support\\CGL\\cgl_sncheck.cgl"  "r"))
  (setq check_no (read-line f))
  (close f)
  (= check_no (getvar "_pkser"))


  ;(if (/= (getvar "_PKSER") (read-line f))
  ;  (alert "This command will only run on CGL authorized Autocad installs.")
  ;  (setq CGL_CheckSerial T)
  ;) ;_ end of if
  ;(close f)
  ;(princ)
  ;(princ)
) ;_ end of defun


;;;  (if (cgl_check_serial)
;;;    (progn
;;;<<do code here>>
;;;    );end checker progn
;;;    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
;;;    );end if
;;;    (princ)

;;; cgl_sncheck.cgl is plaintext of form 000-00000000