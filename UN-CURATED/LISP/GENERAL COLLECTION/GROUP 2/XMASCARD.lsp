;;;   ***   XMASCARD.LSP   ***
;;;   *    For 2004-2005     *
;;;   ************************

(defun C:XMASCARD ()

;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++;
;;                           SUBROUTINES 
;
;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++;

;;--------------------------------------------------------------------------;
;;                             DELAY 
;
;;--------------------------------------------------------------------------;
  (defun Delay (SEC)
    (setq SECS     (getvar "Date")                        ; get current time
          BASETIME (* 86400.0 (- SECS (fix SECS)))        ; convert to seconds
          SECONDS   BASETIME)                             ; set new time
    (while (not (< (+ BASETIME SEC) SECONDS))
      (setq SECS (getvar "Date")                          ; get new current time
            SECONDS (* 86400.0 (- SECS (fix SECS))))      ; convert to seconds
    )
  )

;;--------------------------------------------------------------------------;
;;                             LASTN 
;
;;--------------------------------------------------------------------------;
  (defun LastN (NUMB / COUNT SMAX)
    (setq SS3 (ssadd))                            ; create empty selection set
    (repeat NUMB                                  ; repeat input number of times
      (ssadd (entlast) SS3)                       ; add last entity to selection
      (entdel (entlast))                          ; temporary delete last entity
    )
    (setq COUNT  0                                ; initialize a counter
          SMAX  (sslength SS3))                   ; length of selection set
    (while (< COUNT SMAX)                         ; cycle thru selection set
      (entdel (ssname SS3 COUNT))                 ; undelete the delete
      (setq COUNT (1+ COUNT))                     ; increment counter
    )
    SS3                                           ; return the selection set
  )


  (setq EPSILON 0.02
        AFRACT (/ PI 5))

;;--------------------------------------------------------------------------;
;;                             FRACTAL 
;
;;--------------------------------------------------------------------------;
;; Function Fractal by others, wish I could remember who did it to give
;;      them the credit.

  (defun Fractal (P1 P2 / P3 A DISTS)
    (setq DISTS (distance P1 P2)
          A     (angle P1 P2))
    (command "._ucsicon" "OFF"
             "._zoom" "W" "-1.03,-1.1875" "1.03,2.15")
    (if (< DISTS EPSILON)
      (command "._line" P1 P2 nil)
      (progn
        (setq P3 (polar P1 (angle P1 P2) (* DISTS 0.5)))
        (command "._line" P1 P3 nil)
        (Fractal P3 (polar P3 (+ (angle P1 P2) AFRACT) (* DISTS 0.5 )))
        (Fractal P3 (polar P3 (- (angle P1 P2) AFRACT) (* DISTS 0.5 )))
        (Fractal P1 P3)
      )
    )
  )

  (setvar "Cmdecho" 0)
  (setvar "Blipmode" 0)

;;--------------------------------------------------------------------------;
;;                             DrawTree 
;
;;--------------------------------------------------------------------------;
  (defun DrawTree ()
    (command "._color" "3")
    (Fractal '(0 2) '(0 0))
    (command "._line" '(0 0) '(0 1) nil
             "._color" "bylayer")
  )

;;--------------------------------------------------------------------------;
;;                             MakeText 
;
;;--------------------------------------------------------------------------;
  (defun MakeText ()
    (setq W:SSX (ssadd))
    (command "._style" "SIMPLEX" "SIMPLEX" "" "" "" "" "" "")
    (entmake '((0 . "TEXT") (10 -3.37934 1.12871 0.0)(40 . 0.245209)
               (1 . "MERRY")))
    (setq W:SSX (ssadd (entlast) W:SSX))
    (entmake '((0 . "TEXT") (10 -3.37934 0.662811 0.0)(40 . 0.245209)
               (1 . "CHRISTMAS")))
    (setq W:SSX (ssadd (entlast) W:SSX))
    (entmake '((0 . "TEXT") (10 1.23658 1.13534 0.0)(40 . 0.245209)
               (1 . "HAPPY")))
    (setq W:SSX (ssadd (entlast) W:SSX))
    (entmake '((0 . "TEXT") (10 1.23658 0.669438 0.0)(40 . 0.245209)
               (1 . "NEW YEAR")))
    (setq W:SSX (ssadd (entlast) W:SSX))
    (command "._chprop" W:SSX "" "_C" "1" "")
    (entmake '((0 . "TEXT") (10 -0.969983 2.29321 0.0) (40 . 0.245209)
               (1 . "2005-")))
    (setq W:SSX2 (entlast))
    (command "._change" "_L" "" "" "" "SIMPLEX" "" "" ""
             "._chprop" (entlast) "" "_C" "2" "")

    (entmake '((0 . "TEXT") (10 0.221032 2.29321 0.0) (40 . 0.245209)
               (1 . "2006")))
    (setq W:SSX3 (entlast))
    (command "._change" "_L" "" "" "" "SIMPLEX" "" "" ""
             "._chprop" (entlast) "" "_C" "6" "")
  )

;;--------------------------------------------------------------------------;
;;                             MakeBalls 
;
;;--------------------------------------------------------------------------;
  (defun MakeBalls ()
    (setq W:SSX4 (ssadd))
    (command "._donut" "0" "0.032" '(-4.0 1.0 0.0) "")
    (repeat 15
      (command "._copy" (entlast) "" "@" "@")
    )
    (setq W:SSX4 (LastN 16))
    (setq X:CNTR 0)
    (repeat 16
      (set (read (strcat "BALL" (itoa (1+ X:CNTR))))(ssname W:SSX4 (1+ 
X:CNTR)))
      (setq X:CNTR (1+ X:CNTR))
    )

    (setq X:CNTR 0)
    (repeat 6                                             ; make color red
      (command "._chprop" (ssname W:SSX4 X:CNTR) "" "_C" "1" "")
      (setq X:CNTR (1+ X:CNTR))
    )

    (setq PTS  '(-4.0 1.0 0.0)
          PT1  '(0.4190 0.1906 0.0)
          DIST1 (/ (distance PTS PT1) 10.0)
          ANGL1 (angle PTS PT1))
    (setq PT2  '(0.0545  1.8671 0.0)
          DIST2 (/ (distance PTS PT2) 10.0)
          ANGL2 (angle PTS PT2))
    (setq PT3  '(0.0424  1.3664 0.0)
          DIST3 (/ (distance PTS PT3) 10.0)
          ANGL3 (angle PTS PT3))
    (setq PT4  '(0.1551  1.1030 0.0)
          DIST4 (/ (distance PTS PT4) 10.0)
          ANGL4 (angle PTS PT4))
    (setq PT5  '(0.2660  0.7045 0.0)
          DIST5 (/ (distance PTS PT5) 10.0)
          ANGL5 (angle PTS PT5))
    (setq PT6  '(0.1612  0.1896 0.0)
          DIST6 (/ (distance PTS PT6) 10.0)
          ANGL6 (angle PTS PT6))

    (repeat 4                                             ; make color magenta
      (command "._chprop" (ssname W:SSX4 X:CNTR) "" "_C" "6" "")
      (setq X:CNTR (1+ X:CNTR))
    )
    (setq PT7  '(0.1984  1.2364 0.0)
          DIST7 (/ (distance PTS PT7) 10.0)
          ANGL7 (angle PTS PT7))
    (setq PT8  '(0.1584  0.6073 0.0)
          DIST8 (/ (distance PTS PT8) 10.0)
          ANGL8 (angle PTS PT8))
    (setq PT9  '(0.3049  0.2800 0.0)
          DIST9 (/ (distance PTS PT9) 10.0)
          ANGL9 (angle PTS PT9))
    (setq PT10  '(0.4102  0.5149 0.0)
          DIST10 (/ (distance PTS PT10) 10.0)
          ANGL10 (angle PTS PT10))

    (repeat 4                                             ; make color cyan
      (command "._chprop" (ssname W:SSX4 X:CNTR) "" "_C" "4" "")
      (setq X:CNTR (1+ X:CNTR))
    )
    (setq PT11  '(0.1221  1.5899 0.0)
          DIST11 (/ (distance PTS PT11) 10.0)
          ANGL11 (angle PTS PT11))
    (setq PT12  '(0.3371  1.2399 0.0)
          DIST12 (/ (distance PTS PT12) 10.0)
          ANGL12 (angle PTS PT12))
    (setq PT13  '(0.3590  0.3872 0.0)
          DIST13 (/ (distance PTS PT13) 10.0)
          ANGL13 (angle PTS PT13))
    (setq PT14  '(0.6829  0.5182 0.0)
          DIST14 (/ (distance PTS PT14) 10.0)
          ANGL14 (angle PTS PT14))

    (repeat 2                                             ; make color white
      (command "._chprop" (ssname W:SSX4 X:CNTR) "" "_C" "7" "")
      (setq X:CNTR (1+ X:CNTR))
    )
    (setq PT16  '(0.0320  1.6280 0.0)
          DIST16 (/ (distance PTS PT16) 10.0)
          ANGL16 (angle PTS PT16))
    (command "._zoom" "_W" "-3.5494,-1.0558" "3.3490,3.5817")
  )

;;--------------------------------------------------------------------------;
;;                             MoveBalls 
;
;;--------------------------------------------------------------------------;
  (defun MoveBalls ()
    (command "._move" BALL1 "" PTS (polar PTS ANGL1 DIST1))
    (delay 0.25)

    (command "._move" BALL1 "" PTS (polar PTS ANGL1 DIST1)
             "._move" BALL2 "" PTS (polar PTS ANGL2 DIST2))
    (delay 0.25)

    (command "._move" BALL1 "" PTS (polar PTS ANGL1 DIST1)
             "._move" BALL2 "" PTS (polar PTS ANGL2 DIST2)
             "._move" BALL3 "" PTS (polar PTS ANGL3 DIST3))
    (delay 0.25)

    (command "._move" BALL1 "" PTS (polar PTS ANGL1 DIST1)
             "._move" BALL2 "" PTS (polar PTS ANGL2 DIST2)
             "._move" BALL3 "" PTS (polar PTS ANGL3 DIST3)
             "._move" BALL4 "" PTS (polar PTS ANGL4 DIST4))
    (delay 0.25)

    (command "._move" BALL1 "" PTS (polar PTS ANGL1 DIST1)
             "._move" BALL2 "" PTS (polar PTS ANGL2 DIST2)
             "._move" BALL3 "" PTS (polar PTS ANGL3 DIST3)
             "._move" BALL4 "" PTS (polar PTS ANGL4 DIST4)
             "._move" BALL5 "" PTS (polar PTS ANGL5 DIST5))
    (delay 0.25)

    (command "._move" BALL1 "" PTS (polar PTS ANGL1 DIST1)
             "._move" BALL2 "" PTS (polar PTS ANGL2 DIST2)
             "._move" BALL3 "" PTS (polar PTS ANGL3 DIST3)
             "._move" BALL4 "" PTS (polar PTS ANGL4 DIST4)
             "._move" BALL5 "" PTS (polar PTS ANGL5 DIST5)
             "._move" BALL6 "" PTS (polar PTS ANGL6 DIST6))
    (delay 0.25)

    (command "._move" BALL1 "" PTS (polar PTS ANGL1 DIST1)
             "._move" BALL2 "" PTS (polar PTS ANGL2 DIST2)
             "._move" BALL3 "" PTS (polar PTS ANGL3 DIST3)
             "._move" BALL4 "" PTS (polar PTS ANGL4 DIST4)
             "._move" BALL5 "" PTS (polar PTS ANGL5 DIST5)
             "._move" BALL6 "" PTS (polar PTS ANGL6 DIST6)
             "._move" BALL7 "" PTS (polar PTS ANGL7 DIST7))
    (delay 0.25)

    (command "._move" BALL1 "" PTS (polar PTS ANGL1 DIST1)
             "._move" BALL2 "" PTS (polar PTS ANGL2 DIST2)
             "._move" BALL3 "" PTS (polar PTS ANGL3 DIST3)
             "._move" BALL4 "" PTS (polar PTS ANGL4 DIST4)
             "._move" BALL5 "" PTS (polar PTS ANGL5 DIST5)
             "._move" BALL6 "" PTS (polar PTS ANGL6 DIST6)
             "._move" BALL7 "" PTS (polar PTS ANGL7 DIST7)
             "._move" BALL8 "" PTS (polar PTS ANGL8 DIST8))
    (delay 0.25)

    (command "._move" BALL1 "" PTS (polar PTS ANGL1 DIST1)
             "._move" BALL2 "" PTS (polar PTS ANGL2 DIST2)
             "._move" BALL3 "" PTS (polar PTS ANGL3 DIST3)
             "._move" BALL4 "" PTS (polar PTS ANGL4 DIST4)
             "._move" BALL5 "" PTS (polar PTS ANGL5 DIST5)
             "._move" BALL6 "" PTS (polar PTS ANGL6 DIST6)
             "._move" BALL7 "" PTS (polar PTS ANGL7 DIST7)
             "._move" BALL8 "" PTS (polar PTS ANGL8 DIST8)
             "._move" BALL9 "" PTS (polar PTS ANGL9 DIST9))
    (delay 0.25)

    (command "._move" BALL1 "" PTS (polar PTS ANGL1 DIST1)
             "._move" BALL2 "" PTS (polar PTS ANGL2 DIST2)
             "._move" BALL3 "" PTS (polar PTS ANGL3 DIST3)
             "._move" BALL4 "" PTS (polar PTS ANGL4 DIST4)
             "._move" BALL5 "" PTS (polar PTS ANGL5 DIST5)
             "._move" BALL6 "" PTS (polar PTS ANGL6 DIST6)
             "._move" BALL7 "" PTS (polar PTS ANGL7 DIST7)
             "._move" BALL8 "" PTS (polar PTS ANGL8 DIST8)
             "._move" BALL9 "" PTS (polar PTS ANGL9 DIST9)
             "._move" BALL10 "" PTS (polar PTS ANGL10 DIST10))
    (delay 0.25)

    (command "._move" BALL2 "" PTS (polar PTS ANGL2 DIST2)
             "._move" BALL3 "" PTS (polar PTS ANGL3 DIST3)
             "._move" BALL4 "" PTS (polar PTS ANGL4 DIST4)
             "._move" BALL5 "" PTS (polar PTS ANGL5 DIST5)
             "._move" BALL6 "" PTS (polar PTS ANGL6 DIST6)
             "._move" BALL7 "" PTS (polar PTS ANGL7 DIST7)
             "._move" BALL8 "" PTS (polar PTS ANGL8 DIST8)
             "._move" BALL9 "" PTS (polar PTS ANGL9 DIST9)
             "._move" BALL10 "" PTS (polar PTS ANGL10 DIST10)
             "._move" BALL11 "" PTS (polar PTS ANGL11 DIST11))
    (delay 0.25)

    (command "._move" BALL3 "" PTS (polar PTS ANGL3 DIST3)
             "._move" BALL4 "" PTS (polar PTS ANGL4 DIST4)
             "._move" BALL5 "" PTS (polar PTS ANGL5 DIST5)
             "._move" BALL6 "" PTS (polar PTS ANGL6 DIST6)
             "._move" BALL7 "" PTS (polar PTS ANGL7 DIST7)
             "._move" BALL8 "" PTS (polar PTS ANGL8 DIST8)
             "._move" BALL9 "" PTS (polar PTS ANGL9 DIST9)
             "._move" BALL10 "" PTS (polar PTS ANGL10 DIST10)
             "._move" BALL11 "" PTS (polar PTS ANGL11 DIST11)
             "._move" BALL12 "" PTS (polar PTS ANGL12 DIST12))
    (delay 0.25)

    (command "._move" BALL4 "" PTS (polar PTS ANGL4 DIST4)
             "._move" BALL5 "" PTS (polar PTS ANGL5 DIST5)
             "._move" BALL6 "" PTS (polar PTS ANGL6 DIST6)
             "._move" BALL7 "" PTS (polar PTS ANGL7 DIST7)
             "._move" BALL8 "" PTS (polar PTS ANGL8 DIST8)
             "._move" BALL9 "" PTS (polar PTS ANGL9 DIST9)
             "._move" BALL10 "" PTS (polar PTS ANGL10 DIST10)
             "._move" BALL11 "" PTS (polar PTS ANGL11 DIST11)
             "._move" BALL12 "" PTS (polar PTS ANGL12 DIST12)
             "._move" BALL13 "" PTS (polar PTS ANGL13 DIST13))
    (delay 0.25)

    (command "._move" BALL5 "" PTS (polar PTS ANGL5 DIST5)
             "._move" BALL6 "" PTS (polar PTS ANGL6 DIST6)
             "._move" BALL7 "" PTS (polar PTS ANGL7 DIST7)
             "._move" BALL8 "" PTS (polar PTS ANGL8 DIST8)
             "._move" BALL9 "" PTS (polar PTS ANGL9 DIST9)
             "._move" BALL10 "" PTS (polar PTS ANGL10 DIST10)
             "._move" BALL11 "" PTS (polar PTS ANGL11 DIST11)
             "._move" BALL12 "" PTS (polar PTS ANGL12 DIST12)
             "._move" BALL13 "" PTS (polar PTS ANGL13 DIST13)
             "._move" BALL14 "" PTS (polar PTS ANGL14 DIST14))
    (delay 0.25)

    (command "._move" BALL6 "" PTS (polar PTS ANGL6 DIST6)
             "._move" BALL7 "" PTS (polar PTS ANGL7 DIST7)
             "._move" BALL8 "" PTS (polar PTS ANGL8 DIST8)
             "._move" BALL9 "" PTS (polar PTS ANGL9 DIST9)
             "._move" BALL10 "" PTS (polar PTS ANGL10 DIST10)
             "._move" BALL11 "" PTS (polar PTS ANGL11 DIST11)
             "._move" BALL12 "" PTS (polar PTS ANGL12 DIST12)
             "._move" BALL13 "" PTS (polar PTS ANGL13 DIST13)
             "._move" BALL14 "" PTS (polar PTS ANGL14 DIST14))
    (delay 0.25)

    (command "._move" BALL7 "" PTS (polar PTS ANGL7 DIST7)
             "._move" BALL8 "" PTS (polar PTS ANGL8 DIST8)
             "._move" BALL9 "" PTS (polar PTS ANGL9 DIST9)
             "._move" BALL10 "" PTS (polar PTS ANGL10 DIST10)
             "._move" BALL11 "" PTS (polar PTS ANGL11 DIST11)
             "._move" BALL12 "" PTS (polar PTS ANGL12 DIST12)
             "._move" BALL13 "" PTS (polar PTS ANGL13 DIST13)
             "._move" BALL14 "" PTS (polar PTS ANGL14 DIST14))
    (delay 0.25)

    (command "._move" BALL8 "" PTS (polar PTS ANGL8 DIST8)
             "._move" BALL9 "" PTS (polar PTS ANGL9 DIST9)
             "._move" BALL10 "" PTS (polar PTS ANGL10 DIST10)
             "._move" BALL11 "" PTS (polar PTS ANGL11 DIST11)
             "._move" BALL12 "" PTS (polar PTS ANGL12 DIST12)
             "._move" BALL13 "" PTS (polar PTS ANGL13 DIST13)
             "._move" BALL14 "" PTS (polar PTS ANGL14 DIST14))
    (delay 0.25)

    (command "._move" BALL9 "" PTS (polar PTS ANGL9 DIST9)
             "._move" BALL10 "" PTS (polar PTS ANGL10 DIST10)
             "._move" BALL11 "" PTS (polar PTS ANGL11 DIST11)
             "._move" BALL12 "" PTS (polar PTS ANGL12 DIST12)
             "._move" BALL13 "" PTS (polar PTS ANGL13 DIST13)
             "._move" BALL14 "" PTS (polar PTS ANGL14 DIST14))
    (delay 0.25)

    (command "._move" BALL10 "" PTS (polar PTS ANGL10 DIST10)
             "._move" BALL11 "" PTS (polar PTS ANGL11 DIST11)
             "._move" BALL12 "" PTS (polar PTS ANGL12 DIST12)
             "._move" BALL13 "" PTS (polar PTS ANGL13 DIST13)
             "._move" BALL14 "" PTS (polar PTS ANGL14 DIST14))
    (delay 0.25)

    (command "._move" BALL11 "" PTS (polar PTS ANGL11 DIST11)
             "._move" BALL12 "" PTS (polar PTS ANGL12 DIST12)
             "._move" BALL13 "" PTS (polar PTS ANGL13 DIST13)
             "._move" BALL14 "" PTS (polar PTS ANGL14 DIST14))
    (delay 0.25)

    (command "._move" BALL12 "" PTS (polar PTS ANGL12 DIST12)
             "._move" BALL13 "" PTS (polar PTS ANGL13 DIST13)
             "._move" BALL14 "" PTS (polar PTS ANGL14 DIST14))
    (delay 0.25)

    (command "._move" BALL13 "" PTS (polar PTS ANGL13 DIST13)
             "._move" BALL14 "" PTS (polar PTS ANGL14 DIST14))
    (delay 0.25)

    (command "._move" BALL14 "" PTS (polar PTS ANGL14 DIST14))
    (delay 0.25)

    (command "._mirror" (LastN 16) "" "0,0" "0,1" "N")
    (setq LASTSSX (LastN 16))
    (setq W:CNTR 0)
  )

;;--------------------------------------------------------------------------;
;;                             ChangeText 
;
;;--------------------------------------------------------------------------;
  (defun ChangeText ()
;    (repeat 5
;      (setq EN (ssname W:SSX W:CNTR)
    (setq W:SSX (ssget "X" '((0 . "TEXT"))))
    (repeat (sslength W:SSX)
      (setq EN (ssname W:SSX W:CNTR)
            EL (entget EN)
            EL (subst (cons 7 "SIMPLEX") (assoc 7 EL) EL))
      (entmod EL)
      (setq W:CNTR (1+ W:CNTR))
    )

    (setq EL (entget W:SSX2)
          EL (subst (cons 7 "SIMPLEX") (assoc 7 EL) EL))
    (entmod EL)
  )

;;--------------------------------------------------------------------------;
;;                             FlashText 
;
;;--------------------------------------------------------------------------;
  (defun FlashText ()
    (repeat 5
      (command "._erase" W:SSX "")
      (Delay 0.25)
      (command "._U")
      (Delay 0.1)
      (command "._erase" W:SSX2 "")
      (Delay 0.25)
      (command "._U")
      (command "._erase" W:SSX3 "")
      (Delay 0.25)
      (command "._U")
    )

    (repeat 5
      (command "._erase" W:SSX4 "")
      (Delay 0.25)
      (command "._U")
      (command "._erase" LASTSSX "")
      (Delay 0.25)
      (command "._U")
    )
  )

;;--------------------------------------------------------------------------;
;;                             MakeTrain 
;
;;--------------------------------------------------------------------------;
  (defun MakeTrain ()
    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.81856 -0.333977 0.0)
               (11 -4.81856 -0.422624 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.63435 -0.333977 0.0)
               (11 -4.81856 -0.333977 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.63435 -0.422624 0.0)
               (11 -4.63435 -0.333977 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.81856 -0.422624 0.0)
               (11 -4.63435 -0.422624 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.4102 -0.387363 0.0)
               (11 -5.41705 -0.393333 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.40388 -0.383264 0.0)
               (11 -5.4102 -0.387363 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.39803 -0.380704 0.0)
               (11 -5.40388 -0.383264 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.39257 -0.379353 0.0)
               (11 -5.39803 -0.380704 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.38741 -0.378878 0.0)
               (11 -5.39257 -0.379353 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.3825 -0.378949 0.0)
               (11 -5.38741 -0.378878 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.37776 -0.379235 0.0)
               (11 -5.3825 -0.378949 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.3731 -0.379404 0.0)
               (11 -5.37776 -0.379235 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.36848 -0.379201 0.0)
               (11 -5.3731 -0.379404 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.36387 -0.378674 0.0)
               (11 -5.36848 -0.379201 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.35926 -0.37795 0.0)
               (11 -5.36387 -0.378674 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.35465 -0.377151 0.0)
               (11 -5.35926 -0.37795 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.35003 -0.376404 0.0)
               (11 -5.35465 -0.377151 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.34539 -0.375834 0.0)
               (11 -5.35003 -0.376404 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.34074 -0.375564 0.0)
               (11 -5.34539 -0.375834 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.33605 -0.37572 0.0)
               (11 -5.34074 -0.375564 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.33131 -0.376377 0.0)
               (11 -5.33605 -0.37572 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.32647 -0.377409 0.0)
               (11 -5.33131 -0.376377 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.32145 -0.378643 0.0)
               (11 -5.32647 -0.377409 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.3162 -0.379901 0.0)
               (11 -5.32145 -0.378643 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.31064 -0.381011 0.0)
               (11 -5.3162 -0.379901 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.3047 -0.381796 0.0)
               (11 -5.31064 -0.381011 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.29831 -0.382082 0.0)
               (11 -5.3047 -0.381796 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.29142 -0.381693 0.0)
               (11 -5.29831 -0.382082 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.28398 -0.380517 0.0)
               (11 -5.29142 -0.381693 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.27611 -0.378688 0.0)
               (11 -5.28398 -0.380517 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.26794 -0.376401 0.0)
               (11 -5.27611 -0.378688 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.25962 -0.373853 0.0)
               (11 -5.26794 -0.376401 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.25129 -0.37124 0.0)
               (11 -5.25962 -0.373853 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.24309 -0.368757 0.0)
               (11 -5.25129 -0.37124 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.23517 -0.366601 0.0)
               (11 -5.24309 -0.368757 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.22767 -0.364968 0.0)
               (11 -5.23517 -0.366601 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.22069 -0.363998 0.0)
               (11 -5.22767 -0.364968 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.21422 -0.36361 0.0)
               (11 -5.22069 -0.363998 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.2082 -0.363668 0.0)
               (11 -5.21422 -0.36361 0.0) (210
               0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.20258 -0.364035 0.0)
               (11 -5.2082 -0.363668 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.1973 -0.364574 0.0)
               (11 -5.20258 -0.364035 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.1923 -0.36515 0.0)
               (11 -5.1973 -0.364574 0.0) (210
               0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.18753 -0.365625 0.0)
               (11 -5.1923 -0.36515 0.0) (210
               0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.18294 -0.365864 0.0)
               (11 -5.18753 -0.365625 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.17848 -0.365769 0.0)
               (11 -5.18294 -0.365864 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.17411 -0.365403 0.0)
               (11 -5.17848 -0.365769 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.16982 -0.36487 0.0)
               (11 -5.17411 -0.365403 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.16559 -0.364271 0.0)
               (11 -5.16982 -0.36487 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.16137 -0.363709 0.0)
               (11 -5.16559 -0.364271 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.15717 -0.363288 0.0)
               (11 -5.16137 -0.363709 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.15294 -0.363109 0.0)
               (11 -5.15717 -0.363288 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.14868 -0.363275 0.0)
               (11 -5.15294 -0.363109 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.14434 -0.363852 0.0)
               (11 -5.14868 -0.363275 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.13992 -0.364753 0.0)
               (11 -5.14434 -0.363852 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.13537 -0.365855 0.0)
               (11 -5.13992 -0.364753 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.13067 -0.367033 0.0)
               (11 -5.13537 -0.365855 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.1258 -0.368165 0.0)
               (11 -5.13067 -0.367033 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.12072 -0.369127 0.0)
               (11 -5.1258 -0.368165 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.1154 -0.369795 0.0)
               (11 -5.12072 -0.369127 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.10983 -0.370045 0.0)
               (11 -5.1154 -0.369795 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.10398 -0.369795 0.0)
               (11 -5.10983 -0.370045 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.09792 -0.369127 0.0)
               (11 -5.10398 -0.369795 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.09171 -0.368165 0.0)
               (11 -5.09792 -0.369127 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.08544 -0.367033 0.0)
               (11 -5.09171 -0.368165 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.07917 -0.365855 0.0)
               (11 -5.08544 -0.367033 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.07297 -0.364753 0.0)
               (11 -5.07917 -0.365855 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.06693 -0.363852 0.0)
               (11 -5.07297 -0.364753 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.06112 -0.363275 0.0)
               (11 -5.06693 -0.363852 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.05558 -0.363113 0.0)
               (11 -5.06112 -0.363275 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.05028 -0.363323 0.0)
               (11 -5.05558 -0.363113 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.04517 -0.363827 0.0)
               (11 -5.05028 -0.363323 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.04017 -0.364551 0.0)
               (11 -5.04517 -0.363827 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.03525 -0.365417 0.0)
               (11 -5.04017 -0.364551 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.03033 -0.366348 0.0)
               (11 -5.03525 -0.365417 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.02536 -0.36727 0.0)
               (11 -5.03033 -0.366348 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.02028 -0.368104 0.0)
               (11 -5.02536 -0.36727 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.01501 -0.368835 0.0)
               (11 -5.02028 -0.368104 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.00939 -0.369688 0.0)
               (11 -5.01501 -0.368835 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.00324 -0.370948 0.0)
               (11 -5.00939 -0.369688 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.99637 -0.3729 0.0)
               (11 -5.00324 -0.370948 0.0) (210
               0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.98861 -0.37583 0.0)
               (11 -4.99637 -0.3729 0.0) (210
               0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.97978 -0.380021 0.0)
               (11 -4.98861 -0.37583 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.96968 -0.385761 0.0)
               (11 -4.97978 -0.380021 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.95814 -0.393333 0.0)
               (11 -4.96968 -0.385761 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "CIRCLE")
               (8 . "0")
               (10 -5.36761 -0.632364 0.0)
               (40 . 0.0338936)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.41705 -0.594459 0.0)
               (11 -4.95814 -0.594459 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.41705 -0.594459 0.0)
               (11 -5.41705 -0.393333 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.14248 -0.438766 0.0)
               (11 -4.1577 -0.438766 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.14248 -0.48498 0.0)
               (11 -4.14248 -0.438766 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.1577 -0.48498 0.0)
               (11 -4.14248 -0.48498 0.0) (210
               0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.2592 -0.308728 0.0)
               (11 -4.18443 -0.308728 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.24037 -0.373235 0.0)
               (11 -4.2592 -0.308728 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.20326 -0.373235 0.0)
               (11 -4.18443 -0.308728 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "CIRCLE")
               (8 . "0")
               (10 -4.68302 -0.615417 0.0)
               (40 . 0.0508404)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "CIRCLE")
               (8 . "0")
               (10 -4.55453 -0.615417 0.0)
               (40 . 0.0508404)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.1577 -0.628332 0.0)
               (11 -4.1577 -0.55051 0.0) (210
               0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.03736 -0.628332 0.0)
               (11 -4.1577 -0.628332 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.1577 -0.55051 0.0)
               (11 -4.03736 -0.628332 0.0) (210
               0.0 0.0 1.0)))

    (entmake '((0 . "CIRCLE")
               (8 . "0")
               (10 -4.29071 -0.632364 0.0)
               (40 . 0.0338936)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "CIRCLE")
               (8 . "0")
               (10 -4.21604 -0.632364 0.0)
               (40 . 0.0338936)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.85569 -0.533354 0.0)
               (11 -4.75976 -0.533354 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.85569 -0.461221 0.0)
               (11 -4.85569 -0.533354 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.75613 -0.55051 0.0)
               (11 -4.77502 -0.461221 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.77502 -0.461221 0.0)
               (11 -4.87271 -0.461221 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.1577 -0.55051 0.0)
               (11 -4.75613 -0.55051 0.0) (210
               0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.1577 -0.373235 0.0)
               (11 -4.1577 -0.55051 0.0) (210
               0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.55579 -0.373235 0.0)
               (11 -4.1577 -0.373235 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.58024 -0.307734 0.0)
               (11 -4.55579 -0.373235 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.87271 -0.307734 0.0)
               (11 -4.58024 -0.307734 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.87271 -0.461221 0.0)
               (11 -4.87271 -0.307734 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "CIRCLE")
               (8 . "0")
               (10 -5.00862 -0.632364 0.0)
               (40 . 0.0338936)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "CIRCLE")
               (8 . "0")
               (10 -5.08329 -0.632364 0.0)
               (40 . 0.0338936)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "CIRCLE")
               (8 . "0")
               (10 -5.29294 -0.632364 0.0)
               (40 . 0.0338936)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -4.95814 -0.594459 0.0)
               (11 -4.95814 -0.393333 0.0)
               (210 0.0 0.0 1.0)))

    (setq W:TRAIN (LastN 114))
    (command "._chprop" W:TRAIN "" "_C" "7" "")

;; add smoke
    (entmake '((0 . "ARC")
               (8 . "0")
               (10 -4.31574 -0.250341 0.0)
               (40 . 0.0405365)
               (210 0.0 0.0 1.0)
               (50 . 6.02146)
               (51 . 1.96613)))

    (entmake '((0 . "ARC")
               (8 . "0")
               (10 -4.76184 -0.0775557 0.0)
               (40 . 0.0405365)
               (210 0.0 0.0 1.0)
               (50 . 0.436408)
               (51 . 2.66426)))

    (entmake '((0 . "ARC")
               (8 . "0")
               (10 -4.90733 -0.0745786 0.0)
               (40 . 0.0405365)
               (210 0.0 0.0 1.0)
               (50 . 0.436408)
               (51 . 2.66426)))

    (entmake '((0 . "ARC")
               (8 . "0")
               (10 -4.83459 -0.0760672 0.0)
               (40 . 0.0405365)
               (210 0.0 0.0 1.0)
               (50 . 0.436408)
               (51 . 2.66426)))

    (entmake '((0 . "ARC")
               (8 . "0")
               (10 -4.48539 -0.122093 0.0)
               (40 . 0.0405365)
               (210 0.0 0.0 1.0)
               (50 . 7.6136e-005)
               (51 . 2.22793)))

    (entmake '((0 . "ARC")
               (8 . "0")
               (10 -4.55007 -0.0970401 0.0)
               (40 . 0.0405365)
               (210 0.0 0.0 1.0)
               (50 . 0.174609)
               (51 . 2.40246)))

    (entmake '((0 . "ARC")
               (8 . "0")
               (10 -4.42604 -0.157996 0.0)
               (40 . 0.0405365)
               (210 0.0 0.0 1.0)
               (50 . 6.10873)
               (51 . 2.0534)))

    (entmake '((0 . "ARC")
               (8 . "0")
               (10 -4.37051 -0.202442 0.0)
               (40 . 0.0405365)
               (210 0.0 0.0 1.0)
               (50 . 6.02146)
               (51 . 1.96613)))

    (entmake '((0 . "ARC")
               (8 . "0")
               (10 -4.6891 -0.0790443 0.0)
               (40 . 0.0405365)
               (210 0.0 0.0 1.0)
               (50 . 0.436408)
               (51 . 2.66426)))

    (entmake '((0 . "ARC")
               (8 . "0")
               (10 -4.61812 -0.0836001 0.0)
               (40 . 0.0405365)
               (210 0.0 0.0 1.0)
               (50 . 0.349142)
               (51 . 2.577)))

    (entmake '((0 . "ARC")
               (8 . "0")
               (10 -4.26097 -0.298239 0.0)
               (40 . 0.0405365)
               (210 0.0 0.0 1.0)
               (50 . 6.02146)
               (51 . 1.96613)))

    (setq W:SMOKE (LastN 11))

;; Make Gondola car with boxes
;; 1ST BOX
    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.94657 -0.481324 0.0)
               (11 -5.94657 -0.578014 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.78211 -0.481324 0.0)
               (11 -5.94657 -0.481324 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.78211 -0.578014 0.0)
               (11 -5.78211 -0.481324 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.94657 -0.578014 0.0)
               (11 -5.78211 -0.578014 0.0)
               (210 0.0 0.0 1.0)))

    (command "._wipeout" "-5.8083,-0.4802"              ; cover tree
                         "-5.7182,-0.4802"
                         "-5.8027,-0.3472"
                         "-5.7237,-0.3472" ""
             "._wipeout" "Frames" "OFF")
    (setq W:MASK (entlast))

    (command "._pline" "-5.86434,-0.358177"             ; make tree
               "-5.84606,-0.38871"
               "-5.85544,-0.386628"
               "-5.8412,-0.40571"
               "-5.84919,-0.405363"
               "-5.83253,-0.427569"
               "-5.84051,-0.427222"
               "-5.82801,-0.448039"
               "-5.83322,-0.448386"
               "-5.82663,-0.462264"
               "-5.86434,-0.462264" ""
             "._chprop" (entlast) "" "C" "3" "")

    (command "._mirror" (entlast) "" "-5.86434,-0.358177" "@0,1" "")
    (command "._line" "-5.86434,-0.462264" "-5.86434,-0.481324" ""
             "._chprop" (entlast) "" "C" "38" "")

    (command "._line" "-5.876266,-0.481324" "-5.876266,-0.578014" ""
             "._chprop" (entlast) "" "C" "2" ""
             "._line" "-5.852414,-0.481324" "-5.852414,-0.578014" ""
             "._chprop" (entlast) "" "C" "2" "")

    (command "._line" "-5.782110,-0.517743" "-5.946570,-0.517743" ""
             "._chprop" (entlast) "" "C" "1" ""
             "._line" "-5.782110,-0.541595" "-5.946570,-0.541595" ""
             "._chprop" (entlast) "" "C" "1" "")

    (command "._select" (Lastn 12) "R" W:MASK "")
    (setq W:BOXES1 (ssget "P"))
    (command "._move" W:BOXES1 "" "@" "@0.1,0")

    (command "._copy" W:BOXES1 "" "@" "@")
    (setq W:BOXES2 (Lastn 11))

    (command "._copy" W:BOXES1 "" "@" "@")
    (setq W:BOXES3 (Lastn 11))

    (command "._copy" W:BOXES1 "" "@" "@")
    (setq W:BOXES4 (Lastn 11))

;; GONDOLA
    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.97955 -0.517821 0.0)
               (11 -5.52064 -0.517821 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "CIRCLE")
               (8 . "0")
               (10 -5.85544 -0.632364 0.0)
               (40 . 0.0338936)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "CIRCLE")
               (8 . "0")
               (10 -5.64579 -0.632364 0.0)
               (40 . 0.0338936)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "CIRCLE")
               (8 . "0")
               (10 -5.57112 -0.632364 0.0)
               (40 . 0.0338936)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.97955 -0.594459 0.0)
               (11 -5.97955 -0.517821 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.97955 -0.594459 0.0)
               (11 -5.52064 -0.594459 0.0)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "CIRCLE")
               (8 . "0")
               (10 -5.93011 -0.632364 0.0)
               (40 . 0.0338936)
               (210 0.0 0.0 1.0)))

    (entmake '((0 . "LINE")
               (8 . "0")
               (10 -5.52064 -0.594459 0.0)
               (11 -5.52064 -0.517821 0.0)
               (210 0.0 0.0 1.0)))

    (setq W:GONDOLA (Lastn 8))

    (command "._chprop" W:SMOKE "" "_C" "7" "")
  )

;;--------------------------------------------------------------------------;
;;                             MoveTrain 
;
;;--------------------------------------------------------------------------;
  (defun MoveTrain ()
    ;; move all of train with boxes
    (repeat 36
      (command "._move" W:TRAIN W:SMOKE W:MASK W:BOXES1 W:BOXES2
                        W:BOXES3 W:BOXES4 W:GONDOLA ""
                        PTS
                        (polar PTS 0 (* 0.25 DIST14)))
      (delay 0.125)
    )

    ;; start DROPPING box1 and box2
    (repeat 5
      (command "._move" W:TRAIN W:SMOKE W:MASK W:GONDOLA
                        W:BOXES2 W:BOXES3 W:BOXES4 ""
                        PTS
                        (setq W:PT1 (polar PTS 0 (* 0.25 DIST14))))

      (command "._move" W:BOXES1 "" "@"
                        (strcat "@" (rtos (* 0.15 DIST14) 2 8) ",-0.01"))
      (delay 0.125)
    )

    (repeat 5
      (command "._move" W:TRAIN W:SMOKE W:MASK W:GONDOLA
                        W:BOXES3 W:BOXES4 ""
                        PTS
                        (setq W:PT1 (polar PTS 0 (* 0.25 DIST14))))

      (command "._move" W:BOXES1 W:BOXES2 "" "@"
                        (strcat "@" (rtos (* 0.15 DIST14) 2 8) ",-0.01"))
      (delay 0.125)
    )

    (repeat 5
      (command "._move" W:TRAIN W:SMOKE W:MASK W:GONDOLA
                        W:BOXES4 ""
                        PTS
                        (setq W:PT1 (polar PTS 0 (* 0.25 DIST14))))

      (command "._move" W:BOXES2 W:BOXES3 "" "@"
                        (strcat "@" (rtos (* 0.15 DIST14) 2 8) ",-0.01"))
      (delay 0.125)
    )

    (repeat 5
      (command "._move" W:TRAIN W:SMOKE W:MASK W:GONDOLA ""
                        PTS
                        (setq W:PT1 (polar PTS 0 (* 0.25 DIST14))))

      (command "._move" W:BOXES3 W:BOXES4"" "@"
                        (strcat "@" (rtos (* 0.15 DIST14) 2 8) ",-0.01"))
      (delay 0.125)
    )

    (repeat 5
      (command "._move" W:TRAIN W:SMOKE W:MASK W:GONDOLA ""
                        PTS
                        (setq W:PT1 (polar PTS 0 (* 0.25 DIST14))))

      (command "._move" W:BOXES4"" "@"
                        (strcat "@" (rtos (* 0.15 DIST14) 2 8) ",-0.01"))
      (delay 0.125)
    )

    (setq W:PT1 '(-1.65625 -0.4297 0.0)
          W:PT2 '(-1.65625 -0.4297 0.0)
          W:PT3 '(-1.65625 -0.4297 0.0)
          W:PT4 '(-1.65625 -0.4297 0.0))

    (repeat 40
      (command "._move" W:TRAIN W:SMOKE W:GONDOLA W:MASK ""
                        PTS
                        (polar PTS 0 (* 0.25 DIST14)))

      (delay 0.125)
    )
  )

;;==========================================================================;
;;                             MAIN ROUTINE 
;
;;==========================================================================;
  (command "._undo" "GROUP")
  (setq CMD (getvar "Cmdecho"))
  (setvar "Cmdecho" 0)
  (setq OLDSNAP (getvar "Osmode"))
  (command "._ucsicon" "OFF")
  (setvar "Osmode" 0)
  (DrawTree)
  (MakeText)
  (MakeBalls)
  (MoveBalls)
  (ChangeText)
  (FlashText)
  (MakeTrain)
  (MoveTrain)

  (setvar "Cmdecho" 1)
  (setvar "Osmode" OLDSNAP)
  (setvar "Blipmode" 1)
  (command "_undo" "END")
  (princ)
)
(princ "\nC:XMASCARD ")