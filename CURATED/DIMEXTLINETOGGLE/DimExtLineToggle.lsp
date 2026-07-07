;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;[this works in WORLD coordinates, but sometimes toggles the wrong extension line in other UCS's]
;;;  DimExtLineToggle.LSP
;;;  Dimension's Extension-line Toggle [command name: DET]
;;;  To toggle the on/off state [suppression] of the nearer Extension Line of a selected
;;;    dimension, without needing to know which end is #1 and which is #2.
;;;  Select anywhere on Dimension (extension line when visible, or dimension line, or text).
;;;  Kent Cooper, January 2009
;
(defun C:DET (/ *error* cmde sel data dtyp pickpt picknear def1 def2 def3 def4 vert rad a1a
  a1b a2a a2b ain alist ainpos angA angB ang1 ang2 ar1 ar2 ctr dimdir seend dXdata toggle)
;
  (defun *error* (errmsg)
    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
      (princ (strcat "\nError: " errmsg))
    ); end if
    (command "_.undo" "_end")
    (setvar 'cmdecho cmde)
  ); end defun - *error*
;
  (setq cmde (getvar 'cmdecho))
  (command "_.undo" "_begin")
  (while
    (not
      (and
        (setq
          sel (entsel "\nSelect Dimension on or near Extension Line to Toggle: ")
          data (if sel (entget (car sel)))
          dtyp (if data (cdr (assoc 70 data)))
        ); end setq
        (= (cdr (assoc 0 data)) "DIMENSION")
        (= (cdr (assoc 70 (tblsearch "layer" (cdr (assoc 8 data))))) 0); Unlocked Layer
        (not (member dtyp '(38 102 163 164))); not ordinate or diameter or radius
      ); end and
    ); end not
    (prompt "\nNothing selected/not a Dimension/has no Extension Lines/on Locked Layer -- ")
  ); end while
  (setq
    pickpt (cadr sel)
    picknear (osnap (cadr sel) "nea")
  ); end setq
  (cond ; type of dimension
    ((= dtyp 34); 2-line angular dimension [lines or polyline segments]
      (setq
        def1 (cdr (assoc 10 data)); end of 1st line
        def2 (cdr (assoc 13 data)); start of 2nd line
        def3 (cdr (assoc 14 data)); end of 2nd line
        def4 (cdr (assoc 15 data)); start of 1st line
        vert (inters def1 def4 def2 def3 nil); vertex or [apparent] intersection
        rad (distance vert (cdr (assoc 16 data))); radius of dimension-line arc
        a1a (angle def4 def1); original direction of 1st line
        a1b (angle def1 def4); opposite direction of 1st line
        a2a (angle def2 def3); original direction of 2nd line
        a2b (angle def3 def2); opposite direction of 2nd line
        ain (angle vert (cdr (assoc 16 data))); angle guaranteed to be INternal
        alist (vl-sort (list a1a a1b a2a a2b ain) '<); list of angles in ascending order
        ainpos (vl-position ain alist); where inside direction is, in ordered list
      ); end setq
      (cond ; save angA = direction at clockwise end of measured angle; angB at counterclockwise end
        ((= ainpos 0); if inside direction is smallest angle
          (setq angA (nth 4 alist) angB (nth 1 alist))
        )
        ((= ainpos 4); if inside direction is largest angle
          (setq angA (nth 3 alist) angB (nth 0 alist))
        )
        (T (setq angA (nth (1- ainpos) alist) angB (nth (1+ ainpos) alist))); otherwise, the ones before and after
      ); end cond
      (if ; determine which direction goes with which extension line
        (or (= angA a1a) (= angA a1b)); clockwise-end direction goes with extension line 1
        (setq ang1 angA ang2 angB)
        (setq ang1 angB ang2 angA)
      ); end if - which extension line is which
      (setq ; arrow point locations where dimension arc meets extension lines
        ar1 (polar vert ang1 rad)
        ar2 (polar vert ang2 rad)
      ); end setq
    ); end first condition - 2-line angular settings
    ((= dtyp 37); 3-point angular [arc or circle]
      (setq
        ctr (cdr (assoc 15 data)); center of arc or circle
        rad (distance ctr (cdr (assoc 10 data))); radius of dimension-line arc
        ar1 (polar ctr (angle ctr (cdr (assoc 13 data))) rad)
        ar2 (polar ctr (angle ctr (cdr (assoc 14 data))) rad)
      ); end setq
    ); end second condition - 3-point angular settings
    (T ; otherwise - linear, incl. aligned & rotated
      (setq
        def1 (cdr (assoc 13 data)); definition point 1
        def2 (cdr (assoc 14 data)); definition point 2
        ar2 (cdr (assoc 10 data)); arrow-point/end of dimension line at extension line 2
        dimdir ; direction of dimension line
          (if (= dtyp 33); aligned dimension always has (50 . 0.0), so
            (angle def1 def2); then - direction between definition points
            (cdr (assoc 50 data)); else - direction of dimension line
          ); end if & dimdir
        ar1; not stored for linear - must calculate
          (inters
            ar2
            (polar ar2 dimdir 1); in direction of dimension line
            def1
            (polar def1 (angle def2 ar2) 1); in direction of extension lines
            nil
          ); end inters & ar1
      ); end setq
    ); end third condition - linear settings
  ); end cond
  (setq
    seend ; identify appropriate extension line's suppression variable
      (if (and picknear (/= dtyp 37)); if picked on extension line or dimension line/arc [not text], and not 3-point angular
        (cond
          ((equal (angle def1 picknear) (angle picknear ar1) 0.001) "DIMSE1"); if picked on extension line of a sufficiently obliqued linear,
          ((equal (angle def2 picknear) (angle picknear ar2) 0.001) "DIMSE2"); or certain extreme conditions of a 2-line angular, dimension
          (T (if (< (distance picknear ar1) (distance picknear ar2)) "DIMSE1" "DIMSE2")); otherwise, nearest arrowpoint location
        ); end cond
        (if (< (distance pickpt ar1) (distance pickpt ar2)) "DIMSE1" "DIMSE2"); else - picked on text, or 3-point angular; nearest arrowpoint location
      ); end if & seend
    dXdata (cadr (assoc -3 (entget (car sel) '("ACAD")))); extended data; if none, neither extension line is suppressed
    toggle
      (if (= seend "DIMSE1")
        (- 1 (if (member '(1070 . 75) dXdata) (cdadr (member '(1070 . 75) dXdata)) 0)); find current DIMSE1 value and toggle 1/0
        (- 1 (if (member '(1070 . 76) dXdata) (cdadr (member '(1070 . 76) dXdata)) 0)); same for DIMSE2
      ); end if & toggle
  ); end setq
  (command
    "_.dimoverride" seend toggle "" (car sel) ""
    "_.undo" "_end"
  ); end command
  (setvar 'cmdecho cmde)
  (princ)
); end defun - DET
(prompt "\nType DET for Dimension Extension-line Toggle.")
