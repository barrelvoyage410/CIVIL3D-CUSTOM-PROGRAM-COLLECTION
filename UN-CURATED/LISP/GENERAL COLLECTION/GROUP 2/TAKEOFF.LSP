;==========================================================
; TAKEOFF.LSP 
; Copyright 1993, 1994 by Looking Glass Microproducts
;==========================================================
; Report the accumulated lengths of lines, circles
; arcs, and polylines entities on specified layers.
;==========================================================
(defun C:TAKEOFF (/ OLD_ERROR ERROR FILTER CBC_GETSTRING 
                  CBC_MAPSET CBC_BITSET CBC_VBLIST 
                  CBC_PERIMETER CBC_SEGLEN LENGTHOF 
                  ONE_TAKEOFF LAYERS SS1 TOTAL 2PI)
   ;=======================================================
   ; Selection set filter for Lines, Circles, Arcs,
   ; 2D Polylines and 3D Polylines
   ;=======================================================
   (setq
      FILTER (list
                '(-4 . "<or")
                '(0 . "LINE")
                '(0 . "CIRCLE")
                '(0 . "ARC")
                '(-4 . "<and")
                '(0 . "POLYLINE")
                '(-4 . "<not")
                '(-4 . "&")
                '(70 . 80)
                '(-4 . "not>")
                '(-4 . "and>")
                '(-4 . "or>")
             )
      2PI    (+ pi pi)
   )
   ;=======================================================
   ; Error Handler
   ;=======================================================
   (defun ERROR (S)
      (if (not
             (member
                S
                '("Function cancelled" "console break")
             )
          )
         (princ S)
      )
      (setq *error* OLD_ERROR)
      (princ)
   )
   ;=======================================================
   ; Get String with default
   ;=======================================================
   (defun CBC_GETSTRING (CR PRMPT DEFAULT / TEMP)
      (cond
         ((=
             ""
             (setq
                TEMP (getstring
                        CR
                        (if (/= "" DEFAULT)
                           (strcat
                              PRMPT
                              " <"
                              DEFAULT
                              ">: "
                           )
                           (strcat PRMPT ": ")
                        )
                     )
             )
          )
            DEFAULT
         )
         (t TEMP)
      )
   )
   ;=======================================================
   ; Apply func to every entity in ss
   ;=======================================================
   (defun CBC_MAPSET (FUNC SS / N ENAME)
      (if SS
         (progn
            (setq FUNC (eval FUNC) N (sslength SS))
            (while (> N 0)
               (setq N (1- N) ENAME (ssname SS N))
               (FUNC ENAME)
            )
         )
      )
   )
   ;=======================================================
   ; Return an item n from an association list.
   ;=======================================================
   (defun CBC_ITEM (N L) (cdr (assoc N L)))
   ;=======================================================
   ; Return T if any bit of A matches B 
   ;=======================================================
   (defun CBC_BITSET (A B) (/= 0 (logand A B)))
   ;=======================================================
   ; Return a vertex/bulge list 
   ; for a polyline -- ((point . bulge)...) 
   ;=======================================================
   (defun CBC_VBLIST (PNAME / ENT ENAME CLOSED PLIST)
      (setq
         ENT    (entget PNAME)
         ENAME  PNAME
         CLOSED (CBC_BITSET 1 (CBC_ITEM 70 ENT))
      )
      (while (=
                "VERTEX"
                (CBC_ITEM
                   0
                   (setq
                      ENAME (entnext ENAME)
                      ENT   (entget ENAME)
                   )
                )
             )
         ;-----------------------------
         ; Ignore spline control points
         (if (not (CBC_BITSET 16 (CBC_ITEM 70 ENT)))
            (setq
               V     (cons
                        (CBC_ITEM 10 ENT)
                        (CBC_ITEM 42 ENT)
                     )
               PLIST (cons V PLIST)
            )
         )
      )
      (if CLOSED (setq PLIST (cons (last PLIST) PLIST)))
      (reverse
         PLIST
      )
   )
   ;=======================================================
   ; Perimeter of a polyline
   ;=======================================================
   (defun CBC_PERIMETER (PLIST / PERIM)
      (setq PERIM 0.0)
      (while PLIST
         (setq
            PERIM (+ (CBC_SEGLEN PLIST) PERIM)
            PLIST (cdr PLIST)
         )
      )
      PERIM
   )
   ;=======================================================
   ; Length of a polyline segment
   ;=======================================================
   (defun CBC_SEGLEN (PLIST / P0 1 B ATANB D/2 R)
      (setq P0 (caar PLIST) B (cdar PLIST) P1 (caadr PLIST))
      (cond
         ((null P1) 0)
         ((zerop B) (distance P0 P1))
         (t
            (setq
               ATANB (atan B)
               D/2   (* 0.5 (distance P0 P1))
               R     (/ D/2 (sin (* 2.0 ATANB)))
            )
            (abs (* R 4.0 ATANB))
         )
      )
   )
   ;=======================================================
   ; Length of entity
   ;=======================================================
   (defun LENGTHOF (ENAME / ENT ETYPE ANG)
      (setq ENT (entget ENAME) ETYPE (CBC_ITEM 0 ENT))
      (cond
         ((= ETYPE "LINE")
            (distance
               (CBC_ITEM 10 ENT)
               (CBC_ITEM 11 ENT)
            )
         )
         ((= ETYPE "CIRCLE") (* (CBC_ITEM 40 ENT) 2PI))
         ((= ETYPE "ARC")
            (*
               (CBC_ITEM 40 ENT)
               (if (>
                      (setq
                         ANG (-
                                (CBC_ITEM 51 ENT)
                                (CBC_ITEM 50 ENT)
                             )
                      )
                      0
                   )
                  ANG
                  (+ ANG 2PI)
               )
            )
         )
         ((= ETYPE "POLYLINE")
            (CBC_PERIMETER (CBC_VBLIST ENAME))
         )
      )
   )
   ;=======================================================
   ; One Takeoff.
   ;=======================================================
   (defun ONE_TAKEOFF (ENAME / ENT ETYPE)
      (setq TOTAL (+ (LENGTHOF ENAME) TOTAL))
      (princ
         (strcat
            "\rTotal Length = "
            (rtos TOTAL)
            "            "
         )
      )
   )
   ;=======================================================
   ; Main routine.
   ;=======================================================
   (setq OLD_ERROR *error* *error* ERROR)
   (setq
      LAYERS (CBC_GETSTRING
                nil
                "\nLayer(s) to accumulate"
                (getvar "clayer")
             )
      SS1    (ssget "x" (cons (cons 8 LAYERS) FILTER))
      TOTAL  0
   )
   (princ
      (strcat
         (if SS1 (itoa (sslength SS1)) "0")
         " found\n"
      )
   )
   (CBC_MAPSET 'ONE_TAKEOFF SS1)
   (setq *error* OLD_ERROR)
   (princ)
)
(princ
   (strcat
      "\nTAKEOFF.LSP -- (c) 1993, 1994 "
      "by Looking Glass Microproducts"
   )
)(princ)


