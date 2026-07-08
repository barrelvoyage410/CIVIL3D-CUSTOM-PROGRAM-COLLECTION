; AL.LSP
; Written by Glen Nevill
; Bechtel Corp.
; This program creates leaders with squiggly lines.
; arrow edited by LJL - 3/21/1992
; --------------------------------------------------

(defun c:al ()
        (setq segs (getvar "splinesegs"))
        (setvar "splinesegs" 8)
        (if (not scl)(setq scl (getreal "\nDrawing Scale: ")))
        (setq p1(getpoint"\nArrow location: "))
        (setq p2(getpoint"\nText location: "))
        (setq a1(angle p1 p2))
        (setq a2(+ a1 (* pi 0.5)))
        (setq a3(+ a1 (* pi 1.5)))
        (setq d1(/ (distance p1 p2) 5))
        (setq p3(polar (polar p1 a1 (* 4 d1)) a2 d1))
        (setq p4(polar (polar p1 a1 d1) a3 d1))
        (setq p5(polar (polar p1 a1 (* 4.5 d1)) a2 (* 0.25 d1)))
        (command "pline" p1 "w" "0" (rtos (* 0.04 scl))
                      (polar p1 (angle p1 p3) (* 0.18 scl)) "")
        (command "pline" p1 "w" "0" "0" p3 p4 p5 p2 "")
        (command "pedit" "L" "S" "X")
;        (command "dtext")
        (setvar "splinesegs" segs)
)

