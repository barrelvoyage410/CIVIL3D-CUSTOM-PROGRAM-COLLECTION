;###############################################################################
;
; poly2to3.lsp
;
; Convert 2-D polyline(s) (pline) to 3-D polyline(s) (3dpoly) using
; user-specified z-value for 3-D polyline.  Creates new 3-D polyline and
; erases old 2-D polyline.
;
; User is prompted for:
;    1) the z-value to assign to the 3-D polyline(s)
;    2) the selection set
;
; NOTE:  this may not properly handle all possible combination of attributes.
;
; Please feel free to direct any comments, criticisms, and/or suggestions to
; me at one of the below listed locations.
;
; by:  Michael J. Read               Compuserve:  71571,2073
;      Read Engineering
;      1714 Ben Crenshaw Way
;      Austin, TX 78746
;      (512)327-9776
;
; January 5, 1990
;
;#############################  Revision History  ##############################
; mjr 900105 - V1.00 - initial release
; mjr 911026 - V1.10 - cleaned up, added selection set capability, attribute
;                      handling
;###############################################################################


(princ "\n\n Lisp by:  Michael J. Read\tREAD ENGINEERING\t(512)327-9776\tLoading...")

(defun header(/ ver)                           ; display header info
   (setq ver 1.10)
   (princ "\n READ ENGINEERING --  ")
   (princ (strcat "poly2to3 V(" (rtos ver 2 2) ") -- change 2-D polyline(s) to 3-D polyline(s)\n"))
)

; return the value associated with a particular entity field
(defun fld (num list)
   (cdr (assoc num list))
)

; save specified mode settings
(defun MODES (a)
   (setq MLST '())
   (repeat (length a)
      (setq MLST (append MLST (list (list (car a) (getvar (car a))))))
      (setq a (cdr a)))
)

; restore saved mode settings
(defun MODER ()
   (repeat (length MLST)
      (setvar (caar MLST) (cadar MLST))
      (setq MLST (cdr MLST))
   )
)

; main routine
; convert 2-d polyline to 3-d polyline
;
(defun c:poly2to3(/ e1 ec ed emax en et el f1 f2 nument numver ss1 x y xyz zval)
   (header)                           ; display header info
   (modes '("BLIPMODE" "CMDECHO" ))   ; save modes
   (setvar "cmdecho" 0)               ; turn off command echo
   (setvar "blipmode" 0)              ; turn off blips

   (princ "changes 2-D polyline to 3-D and assigns user-specified z-value\n")
   (setq zval (getreal "Enter z-value:  "))       ; get z-value

   (graphscr)
   (princ "\nSelect 2-D polyline(s) to convert to 3-D polylines(s)...\n")
   (if (setq ss1 (ssget))             ; ask the user to build a selection set
      ; if the set isn't empty
      ;   build selection set of polylines only
      ;   by deleting all of the non-polylines in the set
      (progn
      (setq nument 0
            emax (sslength ss1))
      (prompt (strcat "\n" (itoa emax) " entitie(s) selected.\n"))
      (while (< nument emax)
         (setq en (ssname ss1 nument)
               ed (entget en))
         (if (/= "POLYLINE" (fld 0 ed))
            (progn
            (ssdel en ss1)
            (setq nument (1- nument)
                  emax (1- emax))
            )
         )
         (setq nument (1+ nument))
      ); end while

      (setq nument 0
            emax (sslength ss1))               ; number of polylines
      (prompt (strcat (itoa emax) " polyline(s) found.\n"))

      ; loop through polylines
      ; add 3-D 3dpoly vertex for each 2-D vertex
      
      (if (> emax 0)
         (progn
         (while (< nument emax)
            (redraw (ssname ss1 nument) 3)     ; highlight each polyline
            (setq nument (1+ nument))
         );end while

         (setq nument 0
               emax (sslength ss1))            ; number of polylines

         (while (< nument emax)
            (setq en (ssname ss1 nument))
            (setq e1     en
                  ed     (entget en)
                  numver 0
                  ec     (fld 62 ed)
                  f1     (fld 70 ed)
            )

            (while (and (setq en (entnext en))
                        (setq ed (entget en))
                        (/= "SEQEND" (fld 0 ed)))
               (setq xyz    (fld 10 ed)
                     x      (car   xyz)
                     y      (cadr  xyz)
                     el     (fld  8 ed)
                     et     (fld  0 ed)
                     f2     (fld 70 ed)
               )
               (if (= et "POLYLINE" )
                  (princ (strcat "Converting pline to 3dpoly using z-value: "
                                 (rtos zval 2 2) "\n"))
               )
;               (princ (strcat "### f1=" (itoa f1) ", f2=" (itoa f2) "\n"))
               ;
               ; add 3dpoly vertex for each ACTUAL 2-D vertex
               ; ignore spline or curve fit vertices
               (if (cond
                     ((and (= et "VERTEX") (= 0 f2)) 1)
                     ((and (= et "VERTEX") (= 2 (logand f1 2)) (= 0 f2)) 1)
                     ((and (= et "VERTEX") (= 4 (logand f1 4)) (= 16 f2)) 1)
                     (t nil)
                   )
                  (progn
                  (setq numver (+ numver 1))
                  (if (= 1 numver) (command "3dpoly"))     ; start 3dpoly
                  (command (list x y zval))                ; add vertex
                  );end progn
               );end if
            ); end while entnext

            (setq nument (1+ nument))
            (redraw e1 1)
            (if (> numver 0)                   ; if we had any valid vertices
               (progn
               (command "")                    ; end 3dpoly
               ;
               ; assign attributes of old polyline to new polyline
               ;
               (if (= (logand 1 f1) 1) (command "pedit" "last" "c" ""))
               (if (= (logand 2 f1) 2) (command "pedit" "last" "s" ""))
               (if (= (logand 4 f1) 4) (command "pedit" "last" "s" ""))
               (entdel e1)                     ; delete old polyline
               (command "change" "last" "" "prop" "layer" el "")   ;change layer
               (if (not (null ec))
                  (command "change" "last" "" "prop" "color" ec "");change color
               )
               (princ (strcat "   added 3dpoly with " (itoa numver)
                              " vertices \n"))
               );end progn
            ;else
               (princ "Can't change polyline!\n")
            );end if numver

         );end while entity
         );end progn

      );end if emax > 0
      );end progn

   );end if ss1

   (moder)                                     ; reset modes
   (princ)
) ;end of poly2to3

(header)                                       ; display header info
(princ)
