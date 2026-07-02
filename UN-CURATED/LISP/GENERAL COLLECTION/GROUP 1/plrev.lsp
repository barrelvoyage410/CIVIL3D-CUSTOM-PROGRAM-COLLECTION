;|

PLREV.LSP (c) 1999-2004 Tee Square Graphics

Version 2.03a - 11/15/2004

Used to reverse the direction in which Lines, Polylines, LWPolylines
  and other objects are drawn. Useful for correcting the "direction"
  of specialized complex linetypes.
____

Revision History:

Version 2.03a: problem with non-American English installations fixed
  (with thanks to Jürgen Palme); name changed from REV to PLREV to
  resolve conflict with shortcut for REVolve command. 11/15/2004

Version 2.03: problem with ByLayer linetype fixed. 9/18/2001

Version 2.02: revised to preserve layer property of selected object.
  8/17/2001

Version 2.01: error checking added; bug related to PLINEGEN system
  variable fixed. 3/27/2001

Version 2.0 updated to include Arcs and Circles. These will be
  converted to either Polylines or LWPolylines, depending on the
  current setting of the PLINETYPE variable. Circles and Arcs over
  180 may sometimes behave oddly; for best results, select these
  items by clicking on a point near the "quad" points, at 0, 90,
  180 or 270 degrees. If an Arc or Circle disappears during a REV
  operation, just enter a U (undo) command to bring back the object
  and try again by picking a different point on the object. 3/24/2001

Please report bugs and other difficulties, along with a detailed
  description of the steps leading up to the problem, via email to
  cadman@turvill.com.

|;

(defun C:PLREV (/ olderr cmde blip ltsc cclr snap pwid pgenen1 nam ent p obj ltp
                clr lts wid flgs first final next spl cur vert a clos zoomit clyr lyr)
  (setq olderr *error*)
  (defun *error* (x)
    (setvar "cmdecho" cmde)
    (setvar "blipmode" blip)
    (setvar "osmode" snap)
    (setvar "celtscale" ltsc)
    (setvar "cecolor" cclr)
    (setvar "plinewid" pwid)
    (setvar "plinegen" pgen)
    (setq *error* olderr)
    (princ)
  ) ;; end of *error* function
  (setq cmde (getvar "cmdecho")
        blip (getvar "blipmode")
        ltsc (getvar "celtscale")
        cclr (getvar "cecolor")
        snap (getvar "osmode")
        pwid (getvar "plinewid")
        clyr (getvar "clayer")
        pgen (getvar "plinegen"))
  (setvar "cmdecho" 0)
  (setvar "blipmode" 0)
  (setvar "osmode" 0)
  (setvar "plinewid" 0)
  (setvar "plinegen" 1)
  (command "_.undo" "_be")
  (while (null (setq en1 (entsel "\nPick an object to reverse: "))))
  (setq nam (car en1)
        ent (entget nam)
        p (cadr en1)
        obj (cdr (assoc 0 ent)))
  (cond
    ((= obj "CIRCLE")
      (setq ctr (cdr (assoc 10 ent))
            dia (* 2.0 (cdr (assoc 40 ent)))
            a (angle p ctr))
      (command "_.break" p (polar p (/ pi 4) 0.001)
               "_.pedit" p "_y" "_c" "_x")
      (carc))
    ((= obj "ARC")
      (command "_.break" p "@"
               "_.pedit" p "_y" "_j" nam (entlast) "" "_x")
      (carc))
    (T nil))
  (setq ltp (cdr (assoc 6 ent))
        lyr (cdr (assoc 8 ent))
        clr (cdr (assoc 62 ent))
        lts (cdr (assoc 48 ent))
        wid (cdr (assoc 40 ent))
        flgs (cdr (assoc 70 ent)))
  (if (not ltp)(setq ltp "bylayer"))
  (cond
    ((= obj "LINE")
      (setq first (assoc 10 ent)
            final (assoc 11 ent)
            ent (subst (cons 10 (cdr final)) first ent)
            ent (subst (cons 11 (cdr first)) final ent))
      (entmod ent))
    ((= obj "LWPOLYLINE")
      (setq final (cdr (assoc 10 (setq ent (reverse ent))))
            next (cdr (assoc 10 (cdr (member (assoc 10 ent) ent)))))
      (prev))
    ((= obj "POLYLINE")
      (setq spl (= (logand flgs 4) 4)
            cur (= (logand flgs 2) 2)
            vert (entnext nam))
      (if cur
        (command "_.pedit" p "_s" ""))
      (while (= (cdr (assoc 0 (entget (setq vert (entnext vert))))) "VERTEX")
        (setq next final
              final (cdr (assoc 10 (entget vert)))))
      (prev))
    (T (alert "Not a REVersible object.")))
  (command "_.undo" "_e")
  (setvar "cmdecho" cmde)
  (setvar "blipmode" blip)
  (setvar "osmode" snap)
  (setvar "celtscale" ltsc)
  (setvar "cecolor" cclr)
  (setvar "plinewid" pwid)
  (setvar "plinegen" pgen)
  (setvar "clayer" clyr)
  (setq *error* olderr)
  (princ)
)
(defun carc ()
  (setq ent (entget (entlast))
        nam (cdr (assoc -1 ent))
        obj (cdr (assoc 0 ent)))
)
(defun prev ()
  (setq a (angle next final)
        clos (= (logand flgs 1) 1))
  (if clos (command "_.pedit" nam "_o" ""))
  (setq zoomit (null (ssget "_c" final final)))
  (if zoomit (command "_.zoom" "_c" final ""))
  (if clr (command "_.color" clr))
  (if lts (setvar "celtscale" lts))
  (setvar "clayer" lyr)
  (command "_.pline" (polar final a 0.0001) final ""
           "_.chprop" (entlast) "" "_lt" ltp ""
           "_.pedit" (entlast) "_j" nam "" ""
           "_.break" final (polar final a 0.001))
  (if cur (command "_.pedit" (entlast) "_f" ""))
  (if spl (command "_.pedit" (entlast) "_s" ""))
  (if clos (command "_.pedit" (entlast) "_c" ""))
  (if wid (command "_.pedit" (entlast) "_w" wid ""))
  (if zoomit (command "_.zoom" "_p"))
)

(princ)
