;;;  Program File ......... REVCLOUD.LSP
;;;  Main Function ........ C:RC
;;;  Written By ........... Steven H. Guthrie
;;;
;;;  Compatibility ........ AutoCAD Release 10, 11, 12, 13, 14
;;;
;;;>REVISED BY MARK EVINGER 73162,1676 FOR COWHEY & ASSOCIATES NEEDS!<
;;;>REVISED JULY 20, 1993<
;;;  See Revcloud.old for original code as dowloaded from Cserve!
;;;
;;;  Notes: This routine draws revision clouds using pline arcs with
;;;         a 0.0 beginning width and a (0.02 x Scale Factor) ending
;;;         width to give a hand drawn appearance.  Clouds should be
;;;         drawn in a counter-clockwise direction for arc bulges to
;;;         lay toward the drawing extents and a clockwise direction
;;;         for arc bulges to lay towards drawing interior.
;;;
;;;         The Scale Factor value used for determining ending width
;;;         of each arc is obtained from system variable "LTSCALE".
;;;
;;;---------------------------------------------------------------------
;;;  Revision History:
;;;
;;;    Version ..... 1.0    10-10-91    Initial Release
;;;    Version ..... 1.1    01-08-92    Revise internal *error* function
;;;                                     and Added Undo/Close options
;;;    Version ..... 2.0    01-15-92    Final Release
;;;    Version ..... 2.01   08-10-93    Revised by ME at Cowhey & Assoc.
;;;    Version ..... 2.112  10-28-1998  Revised by Mark Evinger @ Cowhey & Associates
;;;    Version ..... 3.0    03-06-2001  Revised by Cathy Lam @ Cowhey & Associates
;;;                                     to use current layer.
;;;       
;;;---------------------------------------------------------------------
;;; revised 12/8/99 8:37:06 AM to change layer colors                     ;;;v2.112a
;;; color 10 =  30                                                        ;;;
;;; color 11 =  97                                                        ;;;
;;; color 12 = 114                                                        ;;;
;;; color 13 = 204                                                        ;;;
;;; color 14 = 200                                                        ;;;
;;; These color changes were to match the old DOS R12 color schemes as    ;;;
;;; closely as possible.  The new colors show up better in the Windows    ;;;
;;; 256 color scheme.                                                     ;;;
;;;                                                                       ;;;


(defun C:RCLOUD (/          M:BM       M:OM       M:OER      M:P1
                 M:P2       M:PL       M:CLS      M:ANG      M:CNT
                 M:DST      M:SC       SCAL       TEMP_SCAL  CGL_REVNUMBER
                 CGL_REVTRI_INSP
                )

  (princ "\n> Revision Cloud, ver 3.0 ... 03-06-2001")
  (setq M:BM (getvar "BLIPMODE"))
  (setq M:OM (getvar "ORTHOMODE"))
  (setq M:OSNP (getvar "OSMODE"))
  (setq M:CLYR (getvar "CLAYER"))
  (setq M:OER *ERROR*)

  (defun *ERROR* (S)
    (setvar "BLIPMODE" M:BM)
    (setvar "ORTHOMODE" M:OM)
    (setvar "OSMODE" M:OSNP)
    (setvar "CLAYER" M:CLYR)
    (princ (strcat "\Exit..." S))
    (if M:OER
      (setq *ERROR* M:OER)
    ) ;_ end if
    (princ)
  ) ;_ end defun

  (setvar "BLIPMODE" 0)
  (setvar "ORTHOMODE" 0)
  (setvar "OSMODE" 0)


 ;test for USERR2 not equal to LTSCALE, get user input if not equal
  (setq SCAL (getvar "USERR2"))
  (setq TEMP_SCAL (getvar "LTSCALE"))
  (cond
    ((/= SCAL TEMP_SCAL)
     (setq SCAL (getreal (strcat "\nEnter HORIZONTAL scale: >>"
                                 (rtos (getvar "ltscale") 2 1)
                                 "<< "
                         ) ;_ end strcat
                ) ;_ end getreal
     ) ;_ end setq
     (if (null SCAL)
       (setq SCAL (getvar "LTSCALE"))
     ) ;end if
     (setvar "USERR2" SCAL)
    ) ;end cond HSCAL /= LTSCAL
  ) ;end cond  

; following IF statement commented out to place revcloud and triangle on the 
; current layer, NOT on layer "1REVCLOUD"
;  (if (= (tblsearch "layer" "1REVCLOUD") NIL)
;    (command "._layer" "n" "1REVCLOUD" "c" "200" "1REVCLOUD" "")
;  ) ;_ end if
;  (command "._layer" "set" "1revcloud" "")

  (initget 7)
  (setq CGL_REVNUMBER (strcat (getstring "\nEnter Revision Number: ") "."))

  (setq CGL_REVTRI_INSP
         (getpoint "\nSelect insertion point of Revision Triangle: ")
  ) ;_ end setq

  (command "._insert"
           "REVTRI"
           CGL_REVTRI_INSP
           (getvar "USERR2")
           ""
           0
           CGL_REVNUMBER
  ) ;_ end command


  (setq M:P1  (getpoint "\nDraw RevCloud From Point: ")
        M:SC  (* 0.75 (getvar "LTSCALE")) ; changed from DIMSCALE 10/28/1998  ME
        M:CNT 0
        M:PL  (list (list M:CNT M:P1))
  ) ;_ end setq

  (if M:P1
    (command "pline" M:P1 "A")
  ) ;_ end if
  (while M:P1
    (if (/= M:CNT 0)
      (progn
        (initget "Undo Close")
        (setq M:P2 (getpoint M:P1 "\nUndo/Close/<Endpoint>: "))
        (cond ((= M:P2 "Close")
               (setq M:CLS "Close"
                     M:P2  (cadr (last M:PL))
               ) ;_ end setq
              )
              ((= M:P2 "Undo")
               (repeat (car (nth 0 M:PL)) (command "u"))
               (setq M:PL (cdr M:PL))
               (setq M:CNT (car (nth 0 M:PL))
                     M:P1  (cadr (nth 0 M:PL))
               ) ;_ end setq
               (if (= M:CNT 0)
                 (command "A")
               ) ;_ end if
              )
              ((= M:P2 NIL)
               (command "W" "0.0" "0.0" "")
               (setq M:P1 NIL)
              )
        ) ;_ end cond
      ) ;_ end progn
      (setq M:P2 (getpoint M:P1 "\nEndpoint: "))
    ) ;_ end if
    (if (= (type M:P2) 'list)
      (progn
        (setq M:ANG (angle M:P1 M:P2)
              M:CNT (/ (distance M:P1 M:P2) (* 0.300 M:SC))
        ) ;_ end setq
        (cond ((and (>= M:CNT 1.0) (< M:CNT 2.0))
               (setq M:CNT 2)
              )
              ((< M:CNT 1.0)
               (setq M:CNT 1)
              )
              (t
               (setq M:CNT (fix M:CNT))
              )
        ) ;_ end cond
        (setq M:DST (/ (distance M:P1 M:P2) M:CNT))
        (repeat M:CNT
          (command "w"
                   "0.0"
                   (* 0.00 M:SC)
                   "S"
                   (polar
                     (polar M:P1 M:ANG (/ M:DST 2.0))
                     (- M:ANG 1.57)
                     (/ M:DST 2.666666666)
                   ) ;_ end polar
                   (setq M:P1 (polar M:P1 M:ANG M:DST))
          ) ;_ end command
        ) ;_ end repeat
        (setq M:PL (cons (list M:CNT M:P2) M:PL))
        (if (= M:CLS "Close")
          (progn
            (command "W" "0.0" "0.0" "")
            (setq M:P1 NIL)
          ) ;_ end progn
        ) ;_ end if
      ) ;_ end progn
    ) ;_ end if
  ) ;_ end while
  (setvar "BLIPMODE" M:BM)
  (setvar "ORTHOMODE" M:OM)
  (setvar "OSMODE" M:OSNP)
  (setvar "CLAYER" M:CLYR)
  (prin1)
) ;_ end defun
 ;(PROMPT "\n     Revcloud loaded, start command with RC.")
 (PROMPT "\n     Revcloud loaded, start command with RCLOUD.")
 (princ)