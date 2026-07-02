;; TOELEV.LSP
;; Copyright (C) 1997 by Autodesk, Inc.
;;
;; Created 12/8/97 by Toby Jutras
;; 
;; Permission to use, copy, modify, and distribute this software in object 
;; code form for any purpose and without fee is hereby granted, provided that 
;; the above copyright notice appears in all copies and that both that 
;; copyright notice and the limited warranty and restricted rights notice 
;; below appear in all supporting documentation.
;; AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.  AUTODESK 
;; SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF MERCHANTABILITY OR FITNESS 
;; FOR A PARTICULAR USE.  AUTODESK, INC. DOES NOT WARRANT THAT THE OPERATION 
;; OF THE PROGRAM WILL BE UNINTERRUPTED OR ERROR FREE.  Use, duplication, or 
;; disclosure by the U.S. Government is subject to restrictions set forth in 
;; FAR 52.227-19 (Commercial Computer Software - Restricted Rights) and DFAR 
;; 252.227-7013(c)(1)(ii) (Rights in Technical Data and Computer Software), as 
;; applicable.
;;
;; Commands Defined:
;;  c:toelev - Allows Softdesk point blocks to be set to the elevation described
;;             in the elevation attribute of the point block.  Also prompts to 
;;             add AutoCAD points to the current drawing to be used by other
;;             programs.
;;            
;; Description:
;;  The "toelev" command is designed to read a Softdesk point block, and move it
;;  to the elevation described in the elevation attribute of the point block.
;;  In order to work properly, you must select Softdesk point blocks.
;;  
;;  This is most useful in 3D drafting, where symbols need to be inserted at
;;  the appropriate 3D location.  By utilizing this utility, you can use the 
;;  node osnap to select the point in the point block, and it will reflect the
;;  correct elevation.
;;  
;;  The prompt to "... add AutoCAD points to the drawing?" will add AutoCAD points
;;  (nodes) to the drawing at the same location as the point block (AutoCAD X & Y,
;;  northings and eastings), and at the elevation stated in the elevation attribute.
;;  This feature has been requested to allow users to "convert" Softdesk points to
;;  a format that could be read by other software.
;;  
;; NOTE: 
;;  Because this utility reads the point block, the elevation will NOT change
;;  if changes are made to the point database, and changes made to the point
;;  elevation will NOT be reflected in the database.  Also, if points are 
;;  re-imported into the drawing, this utility must be re-run for the points
;;  to be located at the respective elevations.
;;  
(princ "\nThis program will move the selected points to the")
(princ "\nelevation described in the point block.")
(princ "\nType toelev to begin")
(defun c:toelev( / opt lay count object elev)                                     ;;program name is toelev
   (setq ssPoints (ssget '((0 . "INSERT")(2 . "POINT"))))                         ;;restrict selection set
   (initget "Yes No")                                                             ;;initialize keywords
   (setq opt (getkword "\nWould you like to add AutoCAD point to the drawing?"))  ;;find out if adding nodes
   (if (= opt "Yes")                                                              ;;selected yes?
      (while (= lay nil)                                                          ;;prompt for layer name until
         (setq lay (getstring "\nEnter layer name for AutoCAD Points: "))         ;;user enters one
      )                                                                           ;;while
   )                                                                              ;;if
   (setq count 0)                                                                 ;;initialize counter
   (repeat (sslength ssPoints)                                                    ;;repeat for each point
      (setq object (entget (entnext (ssname ssPoints count))))                    ;;get entity data
      (if (= "ELEV" (cdr (assoc 2 object)))                                       ;;check if first attribute
         (progn                                                                   ;;is elevation
            (setq elev (atof (cdr (assoc 1 object))))                             ;;get elevation attribute
            (moveup (ssname ssPoints count) elev opt lay)                         ;;run move up function
            (setq count (+ count 1))                                              ;;increment counter
         )                                                                        ;;progn
      )                                                                           ;;if
   )                                                                              ;;repeat
   (princ)
)                                                                                 ;;defun
(defun moveup(pntblock elev opt lay / )                                           ;;moveup function takes
    (setq pntblock (entget pntblock))                                             ;;pntblock & elev parameters
    (setq pntblock (subst (list 10 (cadr (assoc 10 pntblock))                     ;;substitute new elev for old
                                   (caddr (assoc 10 pntblock)) 
                                    elev) (assoc 10 pntblock) 
                    pntblock))
    (entmod pntblock)                                                             ;;update point block
    (if (= opt "Yes")                                                             ;;if user chose yes earlier
       (entmake (list '(0 . "POINT") (assoc 10 pntblock) (cons 8 lay)))           ;;make an AutoCAD point
    )                                                                             ;;if
    (princ)
)                                                                                 ;;defun
(princ)
