;;; --------------------------------------------------------------------------;
;;; AXROT.LSP
;;;   Copyright (C) 1990 by Autodesk, Inc.
;;;  
;;;   Permission to use, copy, modify, and distribute this software and its
;;;   documentation for any purpose and without fee is hereby granted.  
;;;
;;;   THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY. 
;;;   ALL IMPLIED WARRANTIES OF FITNESS FOR ANY PARTICULAR PURPOSE AND OF 
;;;   MERCHANTABILITY ARE HEREBY DISCLAIMED.
;;;
;;;   By Jan S. Yoder                                          May 11, 1988
;;;   Modified for AutoCAD Rel 11                              July    1990
;;; --------------------------------------------------------------------------;
;;; DESCRIPTION
;;;
;;;   A routine to do 3 axis rotation of a selection set.
;;;
;;; ---------------------------- Main program --------------------------------;

(defun c:axrot (/ axerr s olderr obpt oce ogm ohl oucsf ssel kwd dr bpt) 
  (if (and (= (getvar "cvport") 1) (= (getvar "tilemode") 0)) 
    (progn
      (prompt "\n *** Command not allowed in Paper space ***\n") 
      (princ)
    ) 
    (progn
      ;; Internal error handler
      
      (defun axerr (s)                ; If an error (such as CTRL-C) occurs
        ;; while this command is active...
        (if (/= s "Function cancelled") 
          (princ (strcat "\nError: " s))
        ) 
        (setq *error* olderr)         ; restore old *error* handler
        (setvar "gridmode" ogm)       ; restore saved modes
        (setvar "highlight" ohl)
        (setvar "ucsfollow" oucsf)
        (command "ucs" "restore" "_AXROT_")
        (command "ucs" "del" "_AXROT_")
        (command "undo" "e")         ; complete undo group
        (setvar "cmdecho" oce)
        (princ)
      ) 
      
      (setq olderr *error* 
            *error* axerr)
      (setq oce (getvar "cmdecho") 
            ogm (getvar "gridmode") 
            ohl (getvar "highlight") 
            oucsf (getvar "ucsfollow")
      )
      (setvar "cmdecho" 0)
      (command "undo" "group") 
      (command "ucs" "save" "_AXROT_")
      (setvar "gridmode" 0)
      (setvar "ucsfollow" 0)
      (setq ssel (ssget))
      
      (if ssel
        (progn
          (setvar "highlight" 0)
          (initget 1 "X Y Z") 
          (setq kwd (getkword "\nAxis of rotation X/Y/Z: "))
          (setq dr (getreal "\nDegrees of rotation <0>: "))
          (if (null dr) 
            (setq dr 0)
          ) 
          (setq bpt (getpoint "\nBase point <0,0,0>: "))
          (if (null bpt) 
            (setq bpt (list 0 0 0))
          ) 
          (setq bpt (trans bpt 1 0))
          (cond ((= kwd "X") (command "ucs" "Y" "90")) 
            ((= kwd "Y") (command "ucs" "X" "-90")) 
            ((= kwd "Z") (command "ucs" "Z" "0"))
          ) 
          (setq bpt (trans bpt 0 1))
          (command "rotate" ssel "" bpt dr) 
          (command "ucs" "p")            ;restore previous ucs
          (command "'redrawall") 
        )
        (princ "\nNothing selected. ")
      )
      (setvar "gridmode" ogm)         ;restore saved modes
      (setvar "highlight" ohl)
      (setvar "ucsfollow" oucsf)
      (command "ucs" "del" "_AXROT_")
      (command "undo" "e")            ;complete undo group
      (setvar "cmdecho" oce)
      (princ)
    )
  )
) 
(princ "\n\tC:AXROT loaded.  Start command with AXROT.")
(princ)
;;; --------------------------------------------------------------------------;

