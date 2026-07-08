;GEAR.LSP - This program generates involute curve profile.
;Version 1.0
;written by T. Kopinski 12-9-86 / 2-26-88
;User inputs are:
;            Number of teeth
;            Diametral pitch
;            Pressure angle
;            Number of points to model curve (determines accuracy)
; 
(defun c:gear ()
  (setq numt (getint "Number of teeth:<24> "))
  (if (= numt nil)
    (setq numt 24)
  )
  (setq diap (getreal "Diametral pitch:<4> "))
  (if (= diap nil)
    (setq diap 4.0)
  )
  (setq prsa (getreal "Pressure angle:<14.5> "))
  (if (= prsa nil)
    (setq prsa 14.5)
  )
  (setq pnts (getint "Number of points on curve:<10> "))
  (if (= pnts nil)
    (setq pnts 10)
  )
;start calculations
;
  (setq prsa (/ (* prsa pi) 180.))  ;convert pressure angle to rad.
  (setq pitd (/ numt diap)) ;calculate pitch diameter
  (setq outd (/ (+ numt 2) diap))  ;calculate outer diameter
  (setq basr (/ (* pitd (cos prsa)) 2)) ;compute base radius of gear
  (setq orad (/ outd 2))
  (setq z (- (expt orad 2.0) (Expt basr 2.0)))  ;three lines of
  (setq x (sqrt z))                             ;code to fake
  (setq paodd (atan (/ x basr)))                ;an arccosine
  (setq incr (/ paodd pnts))
  (setq p 0.0)
  (setq pitr (/ pitd 2))
  (setq pang (/ 360. (* Numt 4.0)))
  (setq pang (/ (* pang pi) 180.))
  (graphscr)
  (setq p2 (getpoint "Center of gear: ")) ;center of gear
  (setq y2 (cadr p2))
  (setq x2 (car p2))
  (setq y5 (+ y2 basr))
  (setq p5 (list x2 y5))
  (command "circle" p2 pitr)  ;place pitch circle
  (setq a1 (- (/ pi 2) 0.1))
  (setq a2 (+ (/ pi 2) 0.1))
  (setq pz1 (polar p2 a1 basr))
  (setq pz2 (polar p2 a2 orad))
  (command "zoom" "w" pz1 pz2)
  (setq s (ssadd))
  (setq le (entlast))
;
;begin a loop for placement of coordinate pairs
;the command pline will be used to generate a polyline whose
;vertices will be computed by a standard involute curve formula
;
  (setq test 0)
  (command "pline" p5)
  (setq p (+ incr p))
  (while (> pnts 0)
    (setq e1 (sin p))
    (setq e2 (cos p))
    (setq e (/ e1 e2))
    (setq j (- e p))
    (setq x1 (* (/ (sin j) (cos p)) basr))
    (setq y1 (* (/ (cos j) (cos p)) basr))
    (setq x3 (+ x2 x1))
    (setq y3 (+ y2 y1))
    (setq p3 (list x3 y3))
    (command p3)
    (setq p (+ incr p))
    (setq pnts (- pnts 1))
    (if (/= test 1)
      (progn
        (setq hyp (sqrt (+ (expt x1 2) (expt y1 2))))
        (if (> hyp pitr)
          (progn
            (setq pint p3)
            (setq test 1)
          )
        )
      )
    )
  )
  (command "")
  (initget "Y y N n")
  (setq ans (getkword "\nFinish the gear?:<Y> "))
  (if (/= ans "N")
    (progn
      (command "zoom" "e")
      (setq p11 (osnap pint "inter"))
      (setq ang (angle p2 p11))
      (setq angi (- ang pang))
      (setq p12 (polar p2 angi 1.0))
      (command "mirror" p5 "" p2 p12 "")
      (setq beta (angle p2 p3))
      (setq ang2 (- (* angi 2) beta))
      (setq p15 (polar p2 ang2 orad))
      (command "arc" p15 "c" p2 p3)
      (setq adj1 (- angi (/ pi 2)))
      (setq adj2 (- (/ pi 2) (* pang 4)))
      (setq p17 (polar p2 (+ angi adj1) basr))
      (setq p16 (polar p2 adj2 basr))
      (command "arc" p16 "c" p2 p17)
      (while (setq le (entnext le))
        (ssadd le s)
      )
      (command "array" s "" "p" p2 numt "" "")
      (command "zoom" "e")
    )
  )
  (Prompt "\nDone!")
  (princ)
)
     
