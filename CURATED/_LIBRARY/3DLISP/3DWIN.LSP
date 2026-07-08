;  This was written to put "windows" (rectangular holes) in walls of 3D houses
;  Type "3dwin", then respond to prompt by picking a 3DFACE
;  Then respond to further prompts by picking the opposite corners of
;  the rectangular window.
;
;  The "wall" will be replaced with four new 3DFACEs, with appropriate
;  invisible edges, to form an apparent solid wall with an opening.
;
;  When picking the window corners, do not go beyond the edges of the
;  original 3DFACE.
;
(write-line "3DWIN  Copyright 1991 James White, Whiteware, St. Louis MO")
;  Messages to Compuserve 72060,1152 or write 8544 Bryan, St. Louis 63117.
;
;  Use for home, hobby and trial use is licensed.  If used professionally,
;  please register for $10 (single machine) or ask about site license or
;  trade similar 3D Lisp utilities.

(defun c:3dwin ()
  (setvar "cmdecho" 0)
; pick a 3dface
  (setq count 0)
  (while (not (= count 1))
    (if (not (= count 1))
        (setq ss (entsel))); if
    (if (= "3DFACE" (cdr (assoc 0 (entget (car ss)))))
      (setq count 1)
      (prompt "Not a 3DFace")); if
  ); while
  (redraw (car ss) 3)
; get its corners
  (setq d1 (cdr (assoc 10 (entget (car ss))))
        d2 (cdr (assoc 11 (entget (car ss))))
        d3 (cdr (assoc 12 (entget (car ss))))
        d4 (cdr (assoc 13 (entget (car ss))))
        invis (cdr (assoc 70 (entget (car ss)))))
; save current ucs and set new one on 3dface
  (command "ucs" "save" "temucs" "y")
  (command "ucs" "3" d1 d2 d3)
; pick window corners
  (setq p1 (getpoint "Select window corner...")
        p2 (getpoint "Select opposite window corner..." p1))
; get 4 corners of window
  (setq f1 p1
        f2 (list (car p2) (cadr p1) (caddr p1))
        f3 p2
        f4 (list (car p1) (cadr p2) (caddr p2)))
; translate window corners to wcs
  (setq f1 (trans f1 1 0)
        f2 (trans f2 1 0)
        f3 (trans f3 1 0)
        f4 (trans f4 1 0))
; orient window corners to wall corners
  (if (> (distance d1 f1) (distance d1 f2))
      (progn (setq p1 f1) (setq f1 f2) (setq f2 p1)))
  (if (> (distance d1 f1) (distance d1 f3))
      (progn (setq p1 f1) (setq f1 f3) (setq f3 p1)))
  (if (> (distance d1 f1) (distance d1 f4))
      (progn (setq p1 f1) (setq f1 f4) (setq f4 p1)))
  (if (> (distance d2 f2) (distance d2 f3))
      (progn (setq p1 f2) (setq f2 f3) (setq f3 p1)))
  (if (> (distance d2 f2) (distance d2 f4))
      (progn (setq p1 f2) (setq f2 f4) (setq f4 p1)))
  (if (> (distance d3 f3) (distance d3 f4))
      (progn (setq p1 f3) (setq f3 f4) (setq f4 p1)))
; set wcs for correct 3dface and make new faces
  (command "ucs" "w")
  (if (= 1 (logand 1 invis)) (command "3dface" "i" d1 "i" d2 f2 "i" f1 "")
                             (command "3dface" d1 "i" d2 f2 "i" f1 ""))
  (if (= 2 (logand 2 invis)) (command "3dface" "i" d2 "i" d3 f3 "i" f2 "")
                             (command "3dface" d2 "i" d3 f3 "i" f2 ""))
  (if (= 4 (logand 4 invis)) (command "3dface" "i" d3 "i" d4 f4 "i" f3 "")
                             (command "3dface" d3 "i" d4 f4 "i" f3 ""))
  (if (= 8 (logand 8 invis)) (command "3dface" "i" d4 "i" d1 f1 "i" f4 "")
                             (command "3dface" d4 "i" d1 f1 "i" f4 ""))
; delete the old wall and clean up
  (command "erase" ss "")
  (command "redraw")
  (command "ucs" "r" "temucs")
  (setvar "cmdecho" 1)
); defun
(prompt "\n        Copyright 1991 James H. White")
(prompt "\n      type '3dwin' to  perforate 3dface")
(princ)