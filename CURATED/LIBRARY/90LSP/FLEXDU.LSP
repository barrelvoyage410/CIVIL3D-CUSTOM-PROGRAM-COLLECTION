; draw flex ducts as zig-zag line 10/02/89 REV 5 (C) 1989 Jim Weisbin
; draw arc (supply 3 points) or straight (2 points and return)
; diameter=width of zig-zag saved as system variable userr4
; rib spacing saved as system variable userr3
; pline width saved as system variable userr5
; rib spacing is 3 inches on center
; flex ducts are usually between 1 and 6 ft long - outside of
; these limits you may get very strange looking results!
;
(DEFUN C:FLEXDUCT (/ d1 d3 w ribsp p1 p2 p3 p4 p5 count s1 s2 a1 a2 a90
       a270 a360 a3 a4 mid1 mid2 sang eang temp a1 a2 x w radius c1)
 (setvar "cmdecho" 0)
 (graphscr)
 (setq d1 (getvar "userr4")
       d1 (if (or (not (numberp d1)) (zerop d1)) 6.0 d1)
       w (if (numberp (setq w (getvar "userr5"))) w 0.0)
       ribsp (getvar "userr3")
       ribsp (if (or (not (numberp ribsp)) (zerop ribsp)) 3.0 ribsp)
 )
 ; (initget 1)
 (if
 (setq p1 (getpoint "\nStart point: "))
 (progn
 (initget 1)
 (setq p2 (getpoint p1 "\nPoint on arc: "))
 (while (equal p1 p2)
   (initget 1)
   (setq p2 (getpoint p1 "\nSame point not allowed - Point on arc: "))
 ) ; end while
 (grdraw p1 p2 -1)
 (setq p3 (getpoint p2 "\nEnd point/<straight line>: "))
 (if (null p3) (setq p3 p2))
 (grdraw p2 p3 -1)
 (initget 6)
 (setq d3 (getdist p3 (strcat "\nHalf diameter/<" (rtos d1 2 2) ">: "))
      d1 (if (null d3) d1 d3) 
      x (getdist p3 (strcat "\nRib spacing/<" (rtos ribsp 2 2) ">: "))
      ribsp (if (null x) ribsp x)      
      mid1 (list (/ (+ (car p1) (car p2)) 2) (/ (+ (cadr p1) (cadr p2)) 2))
      mid2 (list (/ (+ (car p2) (car p3)) 2) (/ (+ (cadr p2) (cadr p3)) 2))
      a1 (angle p1 p2)
      a2 (angle p2 p3)
      ; check if angle 1 is 180 degrees from angle 2
      ; if yes, then this is a straight duct:
      p3 (if (= (angle p1 p2) ( angle p3 p2)) p2 p3)
      x (getdist (strcat "\nPline width <" (rtos w 2 2) ">  "))
      w (if (null x) w x)
      a90 (/ pi 2.0)
      a270 (* pi 1.5)
      a360 (* pi 2.0)
 )
 (prompt "Diameter = ")
 (princ (* d1 2.0))
 (princ " Rib spacing = ")
 (princ ribsp)
 ; update user variables:
 (setvar "userr4" d1)
 (setvar "userr5" w)
 (setvar "userr3" ribsp)
 ;
 ; start the pline:
 ;
 (command "pline" p1 "w" w w)
 ; if a1=a2 or p2=p3 then this is a straight line, else draw curved
 (if (or (equal a1 a2) (equal p2 p3)) 
   (progn 
     (if (< (distance p1 p3) (distance p1 p2))
         (setq p3 p2)
     ) ; end if
           ; ensure that count is rounded up to an even number:
     (setq count (* 2 (fix (+ (/ (distance p1 p3) ribsp 2) 0.5)))
           ; divide distance by count to get each segment:
           seg (/ (distance p1 p3) count)
           ; get starting point:
           temp (polar p1 (- a1 pi) (/ seg 2.0))
     )
     (repeat (/ count 2)
        (command (polar (setq temp (polar temp a1 seg))
                 (+ a1 a90) d1))
        (command (polar (setq temp (polar temp a1 seg))
                 (- a1 a90) d1))
     ) end repeat      
   ) ; end progn (if)
   (progn
     (setq p4 (polar mid1 (- a1 a90) d1))
     (setq p5 (polar mid2 (- a2 a90) d1))
     (setq c1 (inters mid1 p4 mid2 p5 nil)
           radius (distance c1 p1)
           sang (angle c1 p1)
           eang (angle c1 p3)
           a3 (abs (- (angle p2 p1) (angle p2 p3)))
           a3 (if (>= a3 pi) (- a360 a3) a3)
           a4 (- (angle p1 p2) (angle p2 p3))
           a1 (abs (- sang eang))
           a2 (if (> a1 pi) (- a360 a1) a1)
           a2 (if (< a3 a90) (- a360 a2) a2)
           count (* 2 (fix (+ (/ (* radius a2) ribsp 2) 0.5)))
           s1 (/ a2 count)
           s2 (if (> sang eang) (- 0.0 s1) s1)
           s2 (if (< a3 a90) (- 0.0 s2) s2)
           s2 (if (> a1 pi) (- 0.0 s2) s2)
           s2 (if (or (= a4 a90) (= a4 (- 0.0 a270))) (- 0.0 s1) s2)
           s2 (if (or (= a4 a270) (= a4 (- 0.0 a90))) s1 s2)
           temp (- sang (/ s2 2.0))
      )
      (repeat (/ count 2)
         (command (polar c1 (setq temp (+ temp s2)) (+ radius d1)))
         (command (polar c1 (setq temp (+ temp s2)) (- radius d1)))
       ) ; end repeat
     ) ; end progn (else)
   ) ; end if
   ; now finish the polyline:
   (command p3 "")
   ; clean up the drawing:
   (grdraw p1 p2 -1)
   (grdraw p2 p3 -1)
   (redraw (entlast) 1)
 ) ; end main progn
 ) ; end main if
(princ)
) ; end function
; run the function after first load:
(C:FLEXDUCT)
