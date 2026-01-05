;;; created 4:44 PM 9/18/2003 to toggle saving plot files on or off in Land Desktop
;;; Copyright (c) Mark Evinger  All Rights Reserved

(defun cglsaveplot ( / OPT2 ANS2)
    (if (findfile "c:/temp/cgl-saveplot.cgl");if the file is there
    (setq OPT2 "Yes");default to yes
    (setq OPT2 "No");otherwise set to no
  ); end if
  (initget "Yes No")
  (setq ANS2 (getkword (strcat "\n   Save a copy of plotfiles?  >> " OPT2 " << ")))
  (if (not (null ANS2))
    (setq OPT2 ANS2)
  )
  (print)
  (cond
    ((= OPT2 "Yes")
     (princ "\nPlotfiles will be saved in the C:\\Temp\\SavePlot folder!!")
     (princ)
     (setq f (open "c:/temp/cgl-saveplot.cgl" "w"))
     (write-line (strcat "CGL-SavePlot toggled on at: " (rtos (getvar "cdate") 2 16)) f)
     (close f)
    ) ; end cond Yes
    ((= OPT2 "No")
     (princ "\nPlotfiles will NOT be saved at all")
     (princ)
     (vl-file-delete "c:/temp/cgl-saveplot.cgl")
    ) ; end cond No
  ) ;end cond
  (princ)
);end cglsaveplot.lsp
(defun c:csp () (vmon) (cglsaveplot))
(defun c:CGLSavePlot () (vmon) (cglsaveplot))
(princ)