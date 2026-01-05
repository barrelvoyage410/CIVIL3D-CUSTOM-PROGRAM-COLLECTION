;;  Revised 07-16-1998 to use both POLYLINES and LWPOLYLINES for R14 compatibility
;;  Routine now searches for *POLY* instead of "POLYLINE" to update.

;;;---------------------------------------------------------------------------;
;;;
;;;   PLUD.LSP   Version 0.5
;;;
;;;   (C) Copyright 1988-1992 by Autodesk, Inc.
;;;  
;;;   This program is copyrighted by Autodesk, Inc. and is  licensed
;;;   to you under the following conditions.  You may not distribute
;;;   or  publish the source code of this program in any form.   You
;;;   may  incorporate this code in object form in derivative  works
;;;   provided  such  derivative  works  are  (i.) are  designed and
;;;   intended  to  work  solely  with  Autodesk, Inc. products, and
;;;   (ii.)  contain  Autodesk's  copyright  notice  "(C)  Copyright
;;;   1988-1992 by Autodesk, Inc."
;;;
;;;   AUTODESK  PROVIDES THIS PROGRAM "AS IS" AND WITH  ALL  FAULTS.
;;;   AUTODESK  SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF  MER-
;;;   CHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK,  INC.
;;;   DOES  NOT  WARRANT THAT THE OPERATION OF THE PROGRAM  WILL  BE
;;;   UNINTERRUPTED OR ERROR FREE.
;;;   
;;;   by Kieran V. McKeogh
;;;   22 October 1991
;;;   
;;;---------------------------------------------------------------------------;
;;;  DESCRIPTION  
;;;
;;;  An AutoLISP routine allowing users to toggle the selected polylines
;;;  between the two modes of linetype generation, either generation
;;;  between the vertices or the new method, along the complete polyline. 
;;;
;;;---------------------------------------------------------------------------;

;;;---------------------------------------------------------------------------;
;;; Internal error handler.
;;;---------------------------------------------------------------------------;

(defun plud_err (s)                   ;If an error (such as CTRL-C) occurs
                                      ;while this command is active...
  (if (/= s "Function cancelled")
    (princ (strcat "\nError: " s))
  )
  (setq *error* old_err)              ;Restore old *error* handler
  (princ)
)
;;;---------------------------------------------------------------------------;

;;;---------------------------------------------------------------------------;
;;; The Main Function.
;;;---------------------------------------------------------------------------;

(defun c:plud(/ a method plud_ss pline_info old new)

  (setq old_err *error*            ; Use special error handling function.
        *error* plud_err)

  (initget "Full Segment")
  (setq method (getkword "Full/Segment <Full>: "))
  (setq a 0)
  (cond
    ((= "Segment" method)          ; find all polylines with 128 bit set  includes LWPOLYLINES
      (setq plud_ss 
              (ssget (list (cons 0 "*POLY*")(cons -4 "&=")(cons 70 128)))
      )
    ) 
    (T                             ; find all 2D polylines with 128 bit not set includes LWPOLYLINES    
      (setq plud_ss (ssget (list (cons 0 "*POLY*")
                                 (cons -4 "<NOT")
                                    (cons -4 "&")(cons 70 248)
                                 (cons -4 "NOT>")   
      )))
    )
  )
  (if plud_ss
    (progn
      (while (< a (sslength plud_ss))
        (setq pline_info (entget (ssname plud_ss a)))
        (setq old (assoc '70 pline_info))
        (if (= "Segment" method)
          (setq new (cons 70 (logand (cdr old) (~ 128))))  ; remove 128 bit
          (setq new (cons 70 (logior (cdr old) 128)))  ; add 128 bit
        )
        (entmod (subst new old pline_info)) 
        (setq a (1+ a))
      )
    )
  )
  (princ (strcat (itoa a) " Polyline(s) updated."))
  (princ)
)

;;;---------------------------------------------------------------------------;
