;;;     Copyright 2000, 2001 by D. Duane Lewis
;;;     duane_lewis@hotmail.com
;;;     Kansas City, Kansas
;;;     March 6, 1999
;;;
;;;     Permission to use, copy, modify, and distribute this software
;;;     for any purpose and without fee is hereby granted, provided
;;;     that the above copyright notice appears in all copies and 
;;;     that both that copyright notice and the limited warranty and 
;;;     restricted rights notice below appear in all supporting 
;;;     documentation.
;;;
;;;    PROGRAMMER PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;;    PROGRAMMER SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;;    MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  PROGRAMMER
;;;    DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;;    UNINTERRUPTED OR ERROR FREE.
;;;
;;;    Use, duplication, or disclosure by the U.S. Government is subject to
;;;    restrictions set forth in FAR 52.227-19 (Commercial Computer
;;;    Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii) 
;;;    (Rights in Technical Data and Computer Software), as applicable.
;;;
;;;
;;;    ! ! ! This program will not work without the "acad.unt" file ! ! !
;;;                 be sure it is located in AutoCAD's path
;;;
;;;
;;;    use the the following commands for text output of square feet and acreage
;;;
;;;    AR   =  select boundary for sqft & acres, text will be displayed at the command line for a visual or copyclip and paste into other text.
;;;
;;;    AR2 =  pick a point inside a closed boundary, text of the sqft & acres will be placed at the previous point picked.
;;;
;;;
(defun C:AR ()
	(ecof) 
	(prompt "\nSelect closed polyline for square feet and acres...")
	(command "_area" "e" pause)
	(setq gar (getvar "area") 
	        parea (cvunit gar "sq ft" "acre"))
	(princ (strcat "\n" (rtos gar 2 0) " SQUARE FEET OR " (rtos parea 2 2) " ACRES"))
	(ecoprev)
)
(defun C:AR2 ()
 	(ecof) 
	(setq boupnt (getpoint "\nPick point inside area for square feet and acres...\n"))
	(command "_bpoly" boupnt "")
	(setq _tmpbou (entlast))
	(command "_area" "e" _tmpbou)
	(command "_erase" _tmpbou "")
	(setq gar (getvar "area") 
	        parea (cvunit gar "sq ft" "acre"))
	(princ (setq _tx (strcat "\n" (rtos gar 2 0) " SQUARE FEET OR " (rtos parea 2 2) " ACRES")))
	(princ "\n\n")
	(command  "_mtext" boupnt "j" "mc" "w" 0 _tx "")
	(ecoprev)
)
(defun ECOF ()
     (setvar "cmdecho" 0)
     (setq olderr *error* *error* clerr)
)
(defun ECOPREV ()
     (setvar "cmdecho" 1)
     (setq *error* olderr)     (prin1)
)
(defun CLERR (s)
   (if (/= s "Function cancelled") (princ ))
     (setvar "cmdecho" 1)
     (setq *error* olderr new nil) 
     (prin1)
)
(prompt "\nEnter AR or AR2 to start command...")