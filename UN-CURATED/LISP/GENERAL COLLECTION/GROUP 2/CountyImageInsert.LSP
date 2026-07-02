(defun ME_SPINNER () ; draws the spinner at the command prompt.
  (if (not #SPIN)
    (setq #SPIN "-")
  ) ;if
  (cond
    ((equal #SPIN "-") (setq #SPIN "\\"))
    ((equal #SPIN "\\") (setq #SPIN "|"))
    ((equal #SPIN "|") (setq #SPIN "/"))
    (t (setq #SPIN "-"))
  ) ;cond close
  (setq cgl_count (+ cgl_count 1))
  (princ (strcat "\rWorking... "(rtos cgl_count 2 0) " " #SPIN))
) ;defun ME_spinner

(defun CountyImageInsert ()
  (setq cgl_count 0)
 ;(setq CGL_County "F:\\AerialsExpress\\Chicago2002\\KaneCountyIL\\A\\")
 ;(setq CGL_County_MapList "jpglist.txt")
 ;(setq CGL_FILE1 (open (strcat CGL_County CGL_County_MapList) "r"))
  (setq CGL_FILE1 (getfiled "Select the JPGLIST.TXT File: "
                            "F:\\AerialsExpress\\"
                            "txt"
                            128
                  )
  )
  (setq f (open CGL_FILE1 "r"))
 ;(setq CGL_Prefix (substr CGL_FILE1 1 (- (strlen CGL_FILE1) 11)))
  (setvar "filedia" 0)
  (while (setq s (read-line f))
 ;(command "_mapiInsert" (strcat CGL_Prefix s) "Y" "")
    (command "_mapiInsert" s "Y" )
    (ME_SPINNER)
  ) ;end while
  (close f)
  (setvar "filedia" 1)
  (princ "\nDONE!!")
  (princ)
) ;end defun
(defun c:cii ()
  (if (cgl_check_serial)
    (progn
  (vmon)
  (CountyImageInsert)
    );end checker progn
    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
    );end if
    (princ)
  )
(defun c:CountyImageInsert ()
  (if (cgl_check_serial)
    (progn
  (vmon)
  (CountyImageInsert)
    );end checker progn
    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
    );end if
    (princ)
  )
(princ "\n\tc:CountyImageInsert loaded.  Start command with CII or CountyImageInsert. ")
(princ)