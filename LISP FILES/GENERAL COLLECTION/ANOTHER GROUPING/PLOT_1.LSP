;; Updated for R14 compatibility 7-14-1998  ME
;; Added INITDIA and removed LISPPLOT 7-31-1998 ME
(setq CGL_CMDECHO (getvar "cmdecho"))
(setvar "cmdecho" 0)
(defun c:PLOT_1 ()
     (command "._undefine" "plot")
     (defun c:PLOT ()
          (princ "\nAuto-Updating drawing stamp!!")
          (If (not c:stmp_me) (load "stmp_me")) (c:stmp_me)
;;          (if (not LISPPLOT) (xload "lispplot")) (lispplot) ;LispPlot for Windows
          (initdia); new to R14
          (command "._PLOT")
          (princ)
     );end defun PLOT
  (setvar "cmdecho" CGL_CMDECHO)
);end defun PLOT_1
