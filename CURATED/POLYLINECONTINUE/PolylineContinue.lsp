;;  PolylineContinue.lsp [command name: PLC];;;;; WORKS, BUT working on issues:
;;;;; Not always finding correct end!
;;;;; Possible no-Type 2D from Pedit -- a fluke? or account for it?
;;;;; Make it complete instead of quit if Esc used in Pline-drawing portion?
;;;;; [non-zz-prefixed version works as far as it goes]
;;;;; CURRENTLY USING (command-s) -- NOT VIABLE IN 2004
;;;;; Use JOIN? -- NOT VIABLE IN 2004

;;  To add more to an existing Polyline, or an existing Line or Arc converted
;;    into one, continuing from its end nearer the point at which it is selected,
;;    whether that is the starting or ending point of the selected object.
;;  [Abbreviations LW, 2D & 3D stand for LightWeight Polyline, 2D "heavy"
;;    Polyline (incl. fit-/spline-curved by Pediting LW), and 3D Polyline.]
;;  If selected object is on locked Layer, allows drawing more, but does not
;;    join to selected object.  Otherwise:
;;    a. For 3D, redraws entirely and leaves User to continue [only way to
;;      ensure result in one object, since 3D can't always be joined]. ;;;;; CAN BE w/ JOIN in newer versions IF planar and sometimes otherwise, NOT always
;;    b. For others, draws more from end, then PEDIT-Joins to selected object.
;;  For all but 3D, establishes direction at appropriate end, so that possible
;;    initial arc segment has correct tangent direction, and for LW/2D starts
;;    with width at that end.  [Arc option & width not available with 3D.]
;;  For Lines/Arcs, complies with Peditaccept System Variable as to whether
;;    to ask User for permission to convert to Polyline in order to add to it.
;;  For Lines not parallel to the current UCS, finds an orientation in which the
;;    Line lies in the current plane, from among the infinite possibilities.
;;  Close option in LW/2D [including converted Line/Arc] takes to other end
;;    of selected object, rather than to start of added portion.  If either or both
;;    end segments are arcs or have non-zero width(s), may not always close in
;;    same way as if new part were originally drawn as part of selected object;
;;    depends on which end is being added to, whether and which end
;;    segments are arcs, possibly other things.
;;  For 3D, since it redraws, Close option is available on initiation if original
;;    has more than one segment.  For Line/Arc/LW/2D, Close option is not ;;;;; make it possible?
;;    available until another point has been specified, because it will be in a
;;    new Pline command; if only closing is wanted, use ordinary Pedit, or
;;    via Properties palette.
;;  [Concept from SWPOLY from autocadtips.wordpress.com site, 11/3/11,
;;    but it sometimes needs to ask for starting point, applies selected Pline's
;;    starting width even if you're adding to the other end, doesn't account for
;;    different coordinate systems or locked Layers, would accept selection of
;;    a 3D Pline and then draw a LW/2D Pline and try to join it to the 3D, etc.
;;    Thoroughly re-written from scratch with improvements/enhancements.]
;;
;;  Kent Cooper, last edited 30 April 2013;;;;; update when done

(defun C:PLC ; = PolyLine Continuation
  (/ *error*);;;;; *PLCreset *ed *matchucs cmde svnames svvals esel ent edata etype latest
 ;;;;; obj locked curv curvtyp atStart newstart 3d ldata layc coords pts pt nodraw)
  (setq cmde nil svvals nil svnames nil esel nil ent nil edata nil etype nil latest nil obj nil locked nil
    curv nil curvtyp nil atStart nil newstart nil 3d nil ldata nil layc nil coords nil pts nil pt nil nodraw nil
    testpt nil);;;;; for testing

  (defun *error* (errmsg)
    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
      (prompt (strcat "\nError: " errmsg))
    ); if
    (*PLCreset)
;;;;;    (command "_.u");;;;; not? in regular Pline/3DPoly, Esc completes rather than cancels, but
;;;;; here, Esc would call *error* and undo -- employ (grread)/errno instead???  (vl-catch-all-apply?)
  ); defun -- *error*

  (defun *PLCreset ()
    (mapcar 'setvar svnames svvals)
    (command-s "_.ucs" "_restore" "PLCtemp");;;;; NOT VIABLE IN 2004
    (command-s "_.ucs" "_delete" "PLCtemp");;;;; NOT VIABLE IN 2004
    (command-s "_.undo" "_end");;;;; NOT VIABLE IN 2004 [can be vla- instead....]
    (setvar 'cmdecho cmde)
    (princ)
  ); defun -- *PLCreset

  (defun *ed (dxf); = Entity Data value of DXF code number [selected object]
    (cdr (assoc dxf edata))
  ); defun -- *ed

  (defun *matchucs (/ extr)
    (setq extr (*ed 210))
    (cond ; set UCS to match object only under certain circumstances
      ( (= etype "Line")
        (if
          (not ; unequal Z components at ends, in current CS; change UCS
            (equal
              (caddr (trans (*ed 10) 0 1))
              (caddr (trans (*ed 11) 0 1))
              1e-8
            ); equal
          ); not
          (if (equal (last (assoc 10 edata)) (last (assoc 11 edata)) 1e-8); then [outer]
            ; in plane parallel to WCS
            (command "_.ucs" "_world"); then [inner]
            (command ; else -- change UCS for origin at start, Line along X axis
              ; [omits "_new" initial UCS option throughout -- add if needed by version]
              "_.ucs" "_zaxis" (*ed 10) (*ed 11)
              "_.ucs" "_z" (angtos (/ pi -2)); work with current angle settings
              "_.ucs" "_y" (angtos (/ pi -2))
            ); command
          ); if -- parallel to WCS or not
        ); if -- change UCS or not
      ); Line condition
      ( (not 3d); Arc or LW/2D
        (if (equal extr '(0 0 1) 1e-8); parallel to WCS is enough
          (command "_.ucs" "_world"); then [don't align with ent regardless -- can rotate CS]
          (command "_.ucs" "_object" ent); else -- UCS match object
        ); if -- parallel to WCS or not
      ); Arc/LW/2D condition
    ); cond [no fall-back; do nothing if 3D]
  ); defun -- *matchucs

  (vl-load-com)
  (setq cmde (getvar 'cmdecho))
;;;;;  (setvar 'cmdecho 0);;;;; for testing
  (command
    "_.undo" "_begin" ;;;;; [can be vla- instead....]
    "_.ucs" "_save" "PLCtemp"
      ; UCS can be changed more than once, so UCS P may not go all the way back
  ); command
  (setq
    svnames ; = System Variable NAMES
      '(peditaccept ucsfollow osmode blipmode plinewid clayer
        cecolor celtype celtscale celweight thickness)
    svvals (mapcar 'getvar svnames); = System Variable VALueS
  ); setq
  ; system variables needing in-routine setting are at start of above list, because:
  (mapcar 'setvar svnames '(1 0 0)); sets only as far as this list goes [others in
    ; list may be used at non-default settings, depending on entity selected]
  (setvar 'plinetype 2); assumes that if not 2, no desire to save and set back

  (while
    (not
      (and
        (setq esel (entsel "\nSelect Polyline [or Line/Arc] near end to Continue from: "))
          ; not via (ssget) with filtering, because need selection point
        (setq ; separated from previous (setq) to run only if something selected
          ent (car esel)
          edata (entget ent)
          elay (*ed 8)
          locked (= (logand 4 (cdr (assoc 70 (tblsearch "layer" elay)))) 4)
            ; 0 [nil] = unlocked, 4 = locked [not last, so (setq) won't return nil if not locked]
          etype (substr (cdr (assoc 100 (reverse edata))) 5); without "AcDb" prefix
            ; instead of (*ed 0), to distinguish different Polyline types
        ); setq
        (wcmatch etype "*Polyline,Line,Arc"); Pline * prefix = nothing [LW], "2d" or "3d"
        (not (vlax-curve-isClosed ent))
      ); and
    ); not
    (prompt "\nNothing selected, not a Polyline [or convertible to one], or closed --")
  ); while

  (if (wcmatch etype "Line,Arc")
    (progn ; then
      (if (= (car svvals) 0); ask only if peditaccept was 0 at command call
        (progn ; then
          (initget "Yes No")
          (if
            (/= ; User types N, No or Enter for N default
              (getkword
                (strcat "\nConvert " etype " to Polyline to add to it [Y/N]? <N>: ")
              ); getkword
              "Yes"
            ); /=
            (quit); then -- runs *error* to reset system variables
          ); if -- [no else; don't quit if User types Y/Yes]
        ); progn -- then
      ); if -- peditaccept = 0
      (*matchucs);;;;; do PEDIT conversion before this, to simplify UCS match?
      (if (not locked)
        (progn ; then
          (command "_.pedit" ent ""); convert to LW
          (setq ; replace Line/Arc & its data with new converted Pline & its data
            ent (entlast)
            edata (entget ent)
            etype (substr (cdr (assoc 100 (reverse edata))) 5)
          ); setq
        ); progn
      ); if
    ); progn -- then
  ); if -- Line/Arc;;;;; does it for off-CS Line/Arc, followed by 2D/3D point nil error.

  (setq
    latest (entlast); for reference at end, whether something was drawn to join
    obj (vlax-ename->vla-object ent) ; for 3D coords, 2D/3D curve's Type property [read & apply]
      ; [not used with LWPoly/Line/Arc];;;;; but gets this far with Line/Arc in off-CS
    curv ; = fit- or spline-CURVed [not about LW/2D arc segments]; for 2D, lost when joined
      (and
        (= (substr etype 2 1) "d"); 2D/3D only [LW has no Type property, not Line/Arc]
        (> (setq curvtyp (vla-get-Type obj)) 0); = CURVature TYPe:  0 = none,
          ; curve fit = 1 [2D only], quadratic = 2 [2D] or 1 [3D], cubic = 3 [2D] or 2 [3D]
;|
Sometimes LW PEDIT Fit/Spline leaves 2D without "Type", which causes error --
In Properties Box, Still indicates Polyline [as if LW], but no Geometry/Misc entries show values or can be accessed.
Neither Regen nor (entmod) fixes it; closing and getting back in does; Breaking does, but makes it just arc-segmented 2D.
Does it still have these 70/75 values in edata? YES!
None: (70 . 128) (75 . 0) [would be (70 . 0) if linetype generation off] -- use (logand)
Fit: (70 . 130) (75 . 0)
Cubic: (70 . 132) (75 . 6)
Quadratic: (70 . 132) (75 . 5)
70	Polyline flag (bit-coded); default is 0:
	1 = This is a closed polyline (or a polygon mesh closed in the M direction). N/A
	2 = Curve-fit vertices have been added.<----------
	4 = Spline-fit vertices have been added.<----------
	8 = This is a 3D polyline. N/A
	16 = This is a 3D polygon mesh. N/A
	32 = The polygon mesh is closed in the N direction. N/A
	64 = The polyline is a polyface mesh. N/A
	128 = The linetype pattern is generated continuously around the vertices of this polyline. N/A
75	Curves and smooth surface type (optional; default = 0); integer codes, not bit-coded:
	0 = No smooth surface fitted<----------
	5 = Quadratic B-spline surface<----------
	6 = Cubic B-spline surface<----------
	8 = Bezier surface N/A
|;
      ); and & curv
    atStart ; T or nil ;;;;; can be wrong end for: Arc even in WCS [sometimes atStart wherever picked], 3D in off-CS
      (< ; closer to start than to end [exactly at midpoint rounds "up" to end]
        (vlax-curve-getDistAtPoint
          ent

;|----------------------
;;;;; point argument:
;;;;; This combination at least gets point ON off-CS Line/Arc when run in WCS plan view [not in other view],
;;;;;   so -get-ToProjection is working,
;;;;;   but can put newstart at start no matter where picked [not getting distance right]
          (vlax-curve-getClosestPointToProjection;;;;; put (trans) on this? probably not [returns in WCS]
            ent
            (trans (cadr esel) 1 0)
;;;;;            (getvar 'viewdir);;;;; need (trans)? use 2 options [Display Coordinate System]?
            (trans (getvar 'viewdir) 1 0 T);;;;; test
          ); -getClosest
|; ;----------------------

(setq test1 ;;;;; testing -- NOT setting this with off-CS Line/Arc

; problem when not viewing in plan direction of current CS, and/or just in non-parallel CS....
          (vlax-curve-getClosestPointToProjection
;;;;; 'curve-obj' argument:
            ent
;;;;; 'givenPnt' argument [must be in WCS]:
(setq test2
            (trans (cadr esel) 1 0);;;;; in case this is the problem, trying other approaches instead....
              ;;;;; [(cadr esel) in terms of current CS]
); setq [test2] ;;;;;
;;;;;            (cadr esel);;;;; non-(trans) didn't help
;;;;;            (trans (cadr esel) 0 1);;;;; reversing (trans) didn't help
;;;;; 'normal' argument [must be in WCS]:
            (getvar 'viewdir);;;;; without (trans) -- TRY THIS
;;;;;            (trans (getvar 'viewdir) 1 0 T); T didn't help
;;;;;            (trans (getvar 'viewdir) 1 0); 'viewdir is in UCS;;;;; testing.... need (trans) here?

          ); ...-getClosest...

;;;;; so try Osnap instead???
;;;;;          (trans (osnap (cadr esel) "_nea") 1 0); ;;;;; seems to work for Pline, NOT for Line/Arc in off-CS
;;;;;          (osnap (cadr esel) "_nea")
); setq [test1] ;;;;;



        ); -getDist
        (/ (vlax-curve-getDistAtParam ent (vlax-curve-getEndParam ent)) 2); half overall length
      ); < & atStart
    newstart (if atStart (vlax-curve-getStartPoint ent) (vlax-curve-getEndPoint ent));;;;; do AFTER (*matchucs)? ;;;;; in WCS
;;;;; NOT SETTING newstart with off-CS Line/Arc, but UCS is changed [and then changed back by *error*],
;;;;; and Line/Arc was turned into LW, so subsequent try works
    3d (= etype "3dPolyline")
  ); setq

  (if locked ;;;;; need to set to unlocked Layer??? can draw on locked one..... BUT can't apply curvature type
    (progn ; then
      (while ; look for Layer that's on, thawed, not in an Xref, not selected object's
        (and
          (setq ldata (tblnext "layer" (not ldata))); still Layer(s) left in table
            ; nil if it gets to end of table without finding qualifying Layer;
            ; assumes it will find some qualifying Layer [if not, will result in error]
          (or
            (=
              (setq layc (cdr (assoc 2 ldata))); = LAYer to [possibly] make Current
              elay ; selected object's Layer
            ); =
            (minusp (cdr (assoc 62 ldata))); off [could be made current, but...]
            (= (logand (cdr (assoc 70 ldata)) 1) 1); frozen [can't be made current]
            (wcmatch layc "*|*"); Xref-dependent [can't be made current]
          ); or
        ); and
      ); while
      (setvar 'clayer layc); once found [usually 0], set it current
      (alert
        (strcat
          "\nLayer " elay " is locked.  Additional Polyline will be drawn"
          "\non Layer " layc " and changed to Layer " elay " when done,"
          (if curv "\nand will have selected Polyline's curvature type applied," "")
          "\nbut will not be joined to selected object."
        ); strcat
      ); alert
    ); progn -- then
    (progn ; else -- not locked
      (setvar 'clayer elay); selected object's Layer
      (if curv
        (alert ; then
          (strcat
            (if (and (= (substr etype 1 2) "2d") (= curvtyp 1)) "Fit" "Spline")
            " curvature will be temporarily\nremoved, and restored when done."
          ); strcat
        ); alert
      ); if -- curv
    ); progn
  ); if -- locked or not

  ; Set all current-entity properties except Layer to match selected object:
  (setvar 'cecolor (if (assoc 62 edata) (itoa (*ed 62)) "BYLAYER"));;;;; instead, let joining do it, or Matchprop after drawing for locked Layer?
    ; cecolor doesn't accept integer -- requires text string, even of integer value
  (setvar 'celtype (cond ((*ed 6)) ("BYLAYER")))
  (setvar 'celtscale (cond ((*ed 48)) (1.0)))
  (setvar 'celweight (cond ((*ed 370)) (-1)))
  (setvar 'thickness (cond ((*ed 39)) (0)))

  (setvar 'osmode 0)
  (setvar 'blipmode 0)

  (if 3d
    (if locked ; then [3d]
      (command "_.3dPoly" (trans newstart 0 1)); then [locked] -- new additional
      (progn ; else -- redraw over it, to make one [can't join]; don't need to decurve
        (setq coords (vlax-get obj 'Coordinates))
        (repeat (/ (length coords) 3)
          (setq
            pts (cons (list (car coords) (cadr coords) (caddr coords)) pts); builds list reversed
            coords (cdddr coords); take off first point's worth
          ); setq
        ); repeat
        (if (not atStart) (setq pts (reverse pts)))
        (entdel ent)
        (command "_.3dPoly")
        (while (setq pt (car pts)) (command (trans pt 0 1)) (setq pts (cdr pts)))
          ; leaves command running at end, unlike (apply 'command pts)
      ); progn -- else [unlocked]
    ); if -- locked or not; also end then [3D]
    (progn; else -- LW [including converted Line/Arc] or 2D
      (*matchucs)
      (if (not locked)
        (cond
          (curv ; curve-fit/splined
            (command "_.pedit" ent "_decurve" ""); retains entity name, handle
              ; doing it this way w/ 2D makes it LWPoly [putting 'Type = 0 leaves it 2D]
          ); curve-fit/splined condition
          ( (= etype "2dPolyline"); not curve-fit/splined, but heavy
            (command "_.convertpoly" "_light" ent ""); retains entity name, handle
              ; don't use decurve [straightens arc segments]
          ); not-curv 2D condition
        ); cond [no fall-back; do nothing for LW]
      ); if -- not locked
      (setq edata (entget ent))
        ; replace 2D data with LW data [no effect unless converted from not-locked 2D];;;;; so put it up there instead?
      (setvar 'plinewid ; for all LW/converted 2D
        (if atStart
          (if (and locked (= etype "2dPolyline")); outer then -- width at beginning
            (cdr (assoc 40 (entget (entnext ent)))); inner then -- first vertex sub-entity
            (*ed 40); inner else [LW]
          ); if -- outer then; beginning width
          (if (and locked (= etype "2dPolyline")); outer else -- width at end
            ; both then & else get from next-to-last vertex, because last vertex's "ending width"
            ; can sometimes differ from actual width at end
            (progn; inner then -- ending width of next-to-last vertex sub-entity
              (setq ver ent)
              (while
                (= (cdr (assoc 0 (entget (entnext (setq ver (entnext ver)))))) "VERTEX")
                (setq wid (cdr (assoc 41 (entget ver))))
              ); while
              wid
            ); progn
            (cdadr (reverse (vl-remove-if-not '(lambda (x) (= (car x) 41)) edata))); inner else [LW]
          ); if -- outer else; ending width
        ); if -- atStart or not
      ); setvar
      (command
        ; to set direction for possible initial Arc option to be tangent
        ; [sets LastAngle System Variable -- Read-Only, so can't use (setvar)]
        "_.line"
        (trans newstart 0 1)
        (trans ;;;;; SOMETIMES numberp nil error adding to off-CS LW at arc-segment end, ;;;;; SEEMS to be fixed....
;;;;; leaving in Line command, and Esc ends that but doesn't call *error*, so UCS isn't reset.
          (mapcar '+
            newstart
            (mapcar
              '(lambda (x) (* x (if atStart -1 1))); reverse tangent direction if at start
;|
;;;;; if at arc-segment end at least, can sometimes get nil from inner function:
              (vlax-curve-getFirstDeriv ent (fix (vlax-curve-getParamAtPoint ent newstart)))
                ; originally had without (fix), but sometimes in non-parallel UCS, though inner
                ; function returned real number ending in .0, outer function sometimes returned
                ; nil, resulting in "Error: bad argument type: 2D/3D point: nil" and "_.ucs" in
                ; *PLCreset being taken for next point in Line, leaving in Line command, etc.
|;
              (vlax-curve-getFirstDeriv
                ent
                (if atStart (vlax-curve-getStartParam ent) (vlax-curve-getEndParam ent))
                  ; originally based on newstart, but sometimes missed in non-parallel CS
              ); -getFirstDeriv ;;;;; testing -- seems to work
            ); mapcar
          ); mapcar '+
          0 1
        ); trans
        "" ; complete Line command
        "_.erase" "_last" "" ;;;;; possible to be off-screen and need (entdel (entlast)) instead?
      ); command
      (setvar 'cmdecho cmde)
      (command "_.pline" (trans newstart 0 1));;;;; BUILD IN CLOSE option for first pick?
    ); progn -- else [LW/2D]
  ); if -- 3D vs. other

  (setvar 'osmode (caddr svvals)); back to User setting at initiation
  (setvar 'blipmode (cadddr svvals)); back to User setting at initiation
  (while (> (getvar "cmdactive") 0) (command pause)); DRAW CONTINUATION
;;;;; Esc while drawing ends entire routine instead of ending just Polyline as in regular Pline command --
;;;;;   check errno? vl-catch-all-apply?

;;;;;  (setvar 'cmdecho 0);;;;; commented out for testing
  (if (eq (entlast) latest); didn't draw anything to add
    (setq nodraw T); then -- nothing drawn [see end]
    (progn ; else -- drew something
      (cond ; "fix" closing and/or match layer and/or join as applicable
        (locked
          (if (vlax-curve-isClosed (entlast))
            ; User used Close [went to start of added part, not of selected object]
            (progn ; then
              (command ; move "closing" to other end of selected object
                "_.pedit" "_last" "_open"
                "_edit" (repeat (1- (*ed 90)) ""); go to end
                "_insert"
              ); command
              (if atStart
                (command "_none" (trans (vlax-curve-getEndPoint ent) 0 1)); then
                (command "_none" (trans (vlax-curve-getStartPoint ent) 0 1)); else
              ); if
              (command "_exit" "")
            ); progn -- then
          ); if -- locked/closed [no else -- do nothing if locked/not closed]
          (command "_.chprop" "_last" "" "_layer" elay ""); whether closed or not
        ); locked condition
        ( (not 3d); not locked, LW/2D only
          (if (vlax-curve-isClosed (entlast))
            ; User Close [to start of added part; not joinable to selected object]
            (command ; then -- join and close to other end of selected object instead
              "_.pedit" "_last" "_open" ""
              "_.pedit" ent "_join" "_last" "" "_close" ""
            ); command -- then
            (command "_.pedit" ent "_join" "_last" "" ""); else -- not closed
          ); if
        ); not locked LW/2D condition
      ); cond -- close/layer/join
      (if curv ; re-impose curve-fit/splined condition if applicable
        (progn
          (if (not 3d) (command "_.convertpoly" "_heavy" ent "")); back from LW
          (vla-put-Type
            (if (or locked 3d)
              (vlax-ename->vla-object (entlast)); then -- newly-drawn object
              obj ; else -- not-locked LW/2D = original object with new joined
            ); if
            curvtyp
          ); vla-put-Type
        ); progn
      ); if -- curv
    ); progn -- else [something drawn]
  ); if -- drew something or not

  (*PLCreset) ; includes Undo End, for purposes of the following:
  (if nodraw (command "_.u"))
    ; return Line/Arc/2D to original type from LW conversion
  (princ)
); defun -- PLC

(prompt "\nType PLC for PolyLine Continuation.")