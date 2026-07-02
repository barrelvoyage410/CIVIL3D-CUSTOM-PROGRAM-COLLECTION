;;; Created 08/10/98 2:33:31 PM by Mark Evinger (Acad_Guru@Compuserve.com)
;;; Routine created to add a PREfix and SUFfix to Softdesk Point Block
;;; ELEVATION attributes.
;;;
;;; Copyright © 1998 Mark Evinger - All rights reserved.
;;; revised 03-20-2007 to include serial number check   ME

(defun ME_SPINNER (/) ; draws the spinner at the command prompt.
  (if (not #SPIN)
    (setq #SPIN "-")
  ) ;if
  (cond
    ((equal #SPIN "-") (setq #SPIN "\\"))
    ((equal #SPIN "\\") (setq #SPIN "|"))
    ((equal #SPIN "|") (setq #SPIN "/"))
    (t (setq #SPIN "-"))
  ) ;cond close
  (princ (strcat "\rWorking... " #SPIN))
) ;defun ME_spinner

(princ
  "\nThis routine will add entered text around all POINT BLOCK elevations."
)
(princ "\nType elevchg to begin")
(defun C:ELEVCHG (/ COUNT OBJECT ELEV CGL_PRE CGL_SUF CGL_ELEV)
    (if (cgl_check_serial)
    (progn
  (setq CGL_PRE (getstring 1 "\nEnter elevation PREfix :"))
  (setq CGL_SUF (getstring 1 "\nEnter elevation SUFfix :"))
  (setq SSPOINTS (ssget '((0 . "INSERT") (2 . "POINT"))))
  (if (not SSPOINTS)
    (princ "\nNo Softdesk Point blocks in selected!!")
    (progn
      (setq COUNT 0) ;initialize counter
      (repeat (sslength SSPOINTS) ;repeat for each point
	(setq OBJECT (entget (entnext (ssname SSPOINTS COUNT)))) ;get entity data
	(if (= "ELEV" (cdr (assoc 2 OBJECT))) ;check if first attribute is elevation
	  (progn
	    (setq CGL_ELEV (cdr (assoc 1 OBJECT))) ;get elevation attribute
	    (setq CGL_ELEV (strcat CGL_PRE CGL_ELEV CGL_SUF))
	    (entmod (subst (cons 1 CGL_ELEV) (assoc 1 OBJECT) OBJECT))
	    (entupd (ssname SSPOINTS COUNT))
	    (setq COUNT (+ COUNT 1)) ;increment counter
	    (ME_SPINNER) ; let user know something is happening
	  ) ;progn
	) ;if
      ) ;repeat
      (princ)
      (princ (strcat "\n"(itoa COUNT)" Point block elevations modified.")) ; let user know what happened
      (princ)
    ) ;progn
  ) ;if
  (princ)
    );end checker progn
    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
    );end if
    (princ)
) ;defun
(princ)