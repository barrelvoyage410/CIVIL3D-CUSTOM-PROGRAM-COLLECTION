;;POLYPT.LSP (c) 1997 Tee Square Graphics
;;
;; POLYPT writes an ASCII file, POINTS.LST containing a list of all of
;;  the vertices of a polyline or LWPolyline selected by the user.
;;
;; FIL2POLY reads the file POINTS.LST and creates a new polyline using
;;  the list of points therein as vertices.
;;
;; NOTE: These simple routines are intended as coding samples ONLY, and
;;  do NOT contain error traps or other safety/security enhancements
;;  that should be included in full-blown "applications"; no guarantees
;;  are made or implied as to suitability to purpose or data security.
;;  This code is supplied "as-is" and all risks associated with its use
;;  are borne by the user.
;;
(defun C:POLYPT (/ ent outf pt1)
  (setq en0 (entsel "\nSelect a Polyline: ")
        en1 (car en0)
        ent (entget en1)
        what (cdr (assoc 0 ent)))
  (cond
    ((= what "LWPOLYLINE")
      (setq outf (open "points.lst" "w"))
      (while (setq pt1 (assoc 10 ent))
        (setq ent (cdr (member pt1 ent)))
        (princ (cdr pt1) outf)
        (princ "\n" outf))
      (close outf)
      (alert (strcat "Your list of points is in\n" (findfile "points.lst"))))
    ((= what "POLYLINE")
      (setq outf (open "points.lst" "w"))
      (while (setq ent (entnext en1))
        (setq en1 ent
              ent (entget ent)
              what (cdr (assoc 0 ent)))
        (if (= what "VERTEX")
          (progn
            (setq pt1 (cdr (assoc 10 ent)))
            (princ pt1 outf)
            (princ "\n" outf))))
      (close outf)
      (alert (strcat "Your list of points is in\n" (findfile "points.lst"))))
    (T (alert "No Polyline or LWPolyline selected.")))
  (princ)            
)
(defun C:FIL2POLY (/ )
  (setq inf (open (findfile "points.lst") "r"))
  (command "_.pline")
  (while (setq pt1 (read-line inf))
    (command (read pt1)))
  (command "")
  (close inf)
  (princ)
)
;;
;; The REVPOLY command is included to create a polyline from the POINTS.LST
;; file in reverse order to the original (LW)polyline. While these routines
;; may be used to reverse the direction of a polyline, a much more robust
;; routine, called REV.LSP is available from the Tee Square Graphics Free
;; Stuff page at http://www.turvill.com/t2
;;
(defun C:REVPOLY (/ inf pt plist)
  (setq inf (open (findfile "points.lst") "r"))
  (while (setq pt (read-line inf))
    (setq plist (append (list (read pt)) plist)))
  (close inf)
  (command "_.pline")
  (apply 'command plist)
  (command "")
  (princ)
)
