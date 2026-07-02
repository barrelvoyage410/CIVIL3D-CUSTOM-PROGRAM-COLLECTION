;;  DivideMeasurePlus.lsp [command names: DIV+ and MEA+]
;;  Kent Cooper, last edited December 2011
;;  "Super-Divide" and "Super-Measure" commands, with the following improvements on AutoCAD's
;;  standard Divide and Measure ["D&M"] commands [in Acad2004 under which this was written]:
;;  In both commands:
;;    1.  User is asked to pick again if they miss or select an inappropriate object type [D&M quit];
;;    2.  Options to place Lines of User-specified length perpendicular to path, or ad-hoc User Selection
;;         set [D&M do only Points or Blocks];
;;    3.  User can pick as many paths as desired to divide/measure in the same way [D&M do only one]
;;         (which is why these ask for options PRIOR to path-object selection [D&M ask after]);
;;    4.  Choice of Layer for Points/Blocks/Lines (Current, or Same as path object, or any other specified
;;         Layer) [D&M use only current Layer], and ask again if User enters non-existent Layer;
;;    5.  Under Point option:  match extrusion direction to selected object if applicable, and if not, to
;;         current Coordinate System [D&M are inconsistent];
;;    6.  Under Block/Selection options:  choice of Rotation (ANY constant angle, or Aligned with object,
;;         or any angle Relative to object) [D&M allow only 0 or Aligned]*
;;    7.  Under Block option:
;;       a.  Can find drawings in Support File Search Paths [D&M require Block already in drawing], and
;;            will ask again if Block name is neither already in drawing nor in one of those paths;
;;       b.  Choice of ANY Block scale [D&M use only 1], with option for Graphic symbols scaled to
;;            drawing using Dimscale System Variable;
;;       c.  Handle different coordinate systems reliably [D&M misplace blocks if neither path object's nor
;;            current Coordinate System are parallel to WCS];
;;       d.  If option has been used in other command, but not in current one, offers other command's
;;            default Block name;
;;    8.  Under Selection option:  can re-use prior Selection set from either command [if still present] in
;;         either one, without re-selecting, independent of regular 'Previous' selection option;
;;    9.  Remember all choices (separately for each command) and offer them as defaults during later use
;;         in the same editing session of the same drawing [D&M retain no defaults].
;;  In DIV+ only:
;;  10.  Option to divide into a number of segments (as with Divide), OR into smallest required number
;;         of equal segments of no longer than a specified Maximum length, however many it takes [Divide
;;         does explicit number of segments only];
;;  11.  Option to inset endmost Points/Blocks/Lines/Selections from ends of path, by User-specified
;;         distance [applies other options to remaining length between inset ends].
;;  12.  Option to place Points/Blocks/Lines/Selections at Midpoints of divisions, not at division points.
;;  13.  If not doing the above, choice of whether to add Points/Blocks/Lines/Selections at ENDS of open
;;         -ended object [Divide won't], and if so, can "divide" into 1 segment [Divide requires 2 or more].
;;  In MEA+ only:
;;  14.  Choice of whether to CENTER Points/Blocks/Lines/Selections within length of object, with excess
;;         length split equally between ends [Measure only spaces from an end, with all excess at other end];
;;  15.  If not doing the above, choice of whether to add Point/Block/Selection at START of object
;;         [Measure won't].
;
;;  *Note on rotation with User-Selection-set option:  zero-degree direction relative to selected objects at
;;    their selected location will be angle that follows chosen rotation/alignment option in placements, as
;;    though User Selection was for a Block definition, but without pre-defined/named Block.
;
;;  Variable names surrounded by _underscores_ [many such variable names do not appear in the code,
;;    but are created with (set) and (dmvar), and their values read with (dmeval)] are global and retained
;;    for defaults; those without are local.  Variable and subroutine names beginning with 'div' are used
;;    only in DIV+; those beginning with 'mea' only in MEA+; those beginning with 'dm' are, or are used
;;    in, shared subroutines; those without such prefixes may be used in either or both commands.
;
(vl-load-com)
;
(defun dmerror ()
  (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
    (princ (strcat "\nError: " errmsg))
  ); if
  (command)
  (if ucschanged (command "_.ucs" "_prev"))
    ;; ^ don't go back unless routine reached UCS change but didn't change it back
  (command "_.undo" "_end")
  (dmreset)
); defun
;
(defun dmvar (opt); VARiable name for command OPTion
  (read (strcat "_" (substr dmwhich 1 3) opt "_"))
); defun
;
(defun dmeval (opt); variable EVALuation for command OPTion
  (eval (dmvar opt))
); defun
;
(defun dmcommon ; ------------------------------------------ COMMON to both DIV+ & MEA+
  (/ laytemp blktemp divprior meaprior scltemp selopt selbase rottemp)
  (setq
    osm (getvar 'osmode)
    cmde (getvar 'cmdecho)
    blips (getvar 'blipmode)
  ); setq
  (setvar 'cmdecho 0)
  (setq clay (getvar 'clayer))
;
  (initget "Blocks Points Lines Selection")
  (set (dmvar "type")
    (cond
      ( ; User input
        (getkword
          (strcat
            "\n"
            dmwhich
            " with Points/Blocks/Lines/Selection ? <"
            (if (dmeval "type") (substr (dmeval "type") 1 1) "P"); Points default on first use
            ">: "
          ); strcat
        ); getkword
      ); User input [other than Enter] condition
      ((dmeval "type")); Enter - prior use
      ("Points"); Enter - first use
    ); cond
  ); set
;
  (cond ; settings for different type options
    ((= (dmeval "type") "Blocks")
      (while
        (cond
          ((not blktemp)); none yet [first time through (while) loop]
          ((and (= blktemp "") (not (or _Meablk_ _Divblk_)) (= (getvar 'insname) "")))
            ; User hit Enter, but no this-command or other-command or Insert defaults
          ( ; availability check
            (and
              (/= blktemp ""); User typed something, but
              (not (tblsearch "block" blktemp)); no such Block in drawing
              (not (findfile (strcat blktemp ".dwg"))); no such drawing in Search paths
            ); and
          ); condition
        ); cond
        (setq blktemp
          (getstring
            (strcat
              (if (and blktemp (/= blktemp "")) "\nNo such Block or Drawing available." "")
              "\nBlock to insert to "
              dmwhich
              " path"
              (cond
                ((dmeval "blk") (strcat " <" (dmeval "blk") ">")); prior Block in this command, if any
                ((if (= dmwhich "Divide"); default Block in other command, if any
                    (if _Meablk_ (strcat " <" _Meablk_ ">"))
                    (if _Divblk_ (strcat " <" _Divblk_ ">"))
                  ); if
                ); condition
                ((/= (getvar 'insname) "") (strcat " <" (getvar 'insname) ">")); Insert's default, if any
                (""); no default on first use if no this-command, other-command or Insert defaults
              ); cond
              ": "
            ); strcat
          ); getstring and blktemp
        ); setq
      ); while
      (set (dmvar "blk")
        (cond
          ((/= blktemp "") blktemp); User typed something
          ((dmeval "blk")); default for this command, if any
          ((if (= dmwhich "Divide") _Meablk_ _Divblk_)); default Block from other command, if any
          ((getvar 'insname)); Enter on first use with Insert default
        ); cond
      ); set
      (initget 134 "Graphic")
      (setq scltemp
        (getkword ; [returns nil on Enter]
          (strcat
            "\nScale for Blocks, or Graphic for symbol scaled to drawing <"
            (cond
              ((= (dmeval "scl") (getvar 'dimscale)) (strcat (rtos (dmeval "scl") 2 4) "= Graphic scale"))
              ((dmeval "scl") (rtos (dmeval "scl") 2 4))
              ("1"); default on first use
            ); cond
            ">: "
          ); strcat
        ); getkword and scltemp
      ); setq
      (set (dmvar "scl")
        (cond
          ((= scltemp "Graphic") (getvar 'dimscale)); User chose Graphic; get drawing scale
          ((and scltemp (/= (atof scltemp) 0)) (atof scltemp)); User typed numerical string; convert to number
          ((dmeval "scl")); Enter on prior use
          (1); Enter on first use
        ); cond and scale
      ); set
    ); Blocks condition
    ((= (dmeval "type") "Lines")
      (initget (if (dmeval "lin") 6 7)); no Enter on first use
      (set (dmvar "lin")
        (cond
          ( ; User input
            (getdist
              (strcat
                "\nEnter length of marking Lines"
                (if (dmeval "lin") (strcat " <" (rtos (dmeval "lin")) ">") ""); default if present
                ": "
              ); end strcat
            ); end getdist and lintemp
          ); User input [other than Enter] condition
          ((dmeval "lin")); Enter - prior use
        ); cond
      ); set
    ); Lines condition
    ((= (dmeval "type") "Selection")
      (setq
        divprior (and _Divset_ (entget (ssname _Divset_ 0)))
          ; if 1st item in prior DIV+ selection is gone, will return nil
        meaprior (and _Measet_ (entget (ssname _Measet_ 0)))
      ); setq
      (if (or divprior meaprior); prior selection(s)
        (progn ; then
          (initget (strcat (if divprior "PDiv " "") (if meaprior "PMea " "") "New"))
          (setq selopt
            (getkword
              (strcat
                "\nSelection set ["
                (if divprior "PDiv for Prior Div+ set/" "")
                (if meaprior "PMea for Prior Mea+ set/" "")
                "New]"
                (if (dmeval "set") (strcat " <P" (substr dmwhich 1 3) ">") " <New>")
                  ; if prior set for this command, offer it; New for first-time default
                ": "
              ); strcat
            ); getkword
          ); setq
          (set (dmvar "selopt")
            (cond
              (selopt); User typed something
              ((dmeval "set") (strcat "P" (substr dmwhich 1 3))); Enter - prior use
              ("New"); Enter - first use
            ); cond
          ); set
        ); progn - then
        (set (dmvar "selopt") "New"); else [no prior selection(s)]
      ); if
      (cond
        ((= (dmeval "selopt") "New")
          (prompt (strcat "\nFor Selection set with which to " dmwhich ","))
          (set (dmvar "set") (ssget))
        ); 1st condition - New
        ((= (dmeval "selopt") "PDiv") (set (dmvar "set") _Divset_)); 2nd condition - Div+ prior
        ((= (dmeval "selopt") "PMea") (set (dmvar "set") _Measet_)); 3rd condition - Mea+ prior
      ); cond
      (while (not selbase)
        (setq selbase (getpoint "\nBase point in relation to Selection: "))
      ); while
      (command "_.copybase" selbase (dmeval "set") "")
        ;;;;; save point as global variable? on re-use, what if set is still in dwg, but moved?
    ); Selection condition
    ((progn (setvar 'pdmode 35) (setvar 'pdsize -3))); none-of-the-above - Points - make visible
  ); cond - settings for different 'type' options
;
  (if (wcmatch (dmeval "type") "Blocks,Selection")
    (progn ; then - Rotation options
      (initget 32 "Aligned Relative"); dashed rubber-band if picked on-screen
      (setq rottemp
        (getangle ; [returns nil on Enter]
          (strcat
            "\n"
            (dmeval "type")
            " rotation, or Aligned with path or Relative angle to path [angle/A/R] <"
            (cond
              ((numberp (dmeval "rot")) (angtos (dmeval "rot"))); default is a number
              ((dmeval "rot") (substr (dmeval "rot") 1 1)) ; "A" or "R" from prior use
              ("A"); default on first use
            ); cond
            ">: "
          ); strcat
        ); getangle and rottemp
      ); setq
      (set (dmvar "rot")
        (cond
          ((numberp rottemp) rottemp); User number
          (rottemp); User A or R
          ((dmeval "rot")); Enter - prior use
          ("Aligned"); Enter - first use
        ); cond & rotation
      ); set
      (if (= (dmeval "rot") "Relative")
        (progn
          (initget 36)
            ; no negative, dashed rubber-band if picked on-screen
          (set (dmvar "rel")
            (cond
              ( ; User input
                (getangle
                  (strcat
                    "\nAngle of Blocks Relative to path direction <"
                    (if (dmeval "rel") (angtos (dmeval "rel")) "0")
                      ; 0 default on first use; designate units/precision if desired
                    ">: "
                  ); strcat
                ); getangle
              ); User input [other than Enter] condition
              ((dmeval "rel")); Enter - prior use
              (0); Enter - first use
            ); cond
          ); set
        ); progn
      ); if - Relative
    ); progn
  ); if - Blocks/Selection types
;
  (if (/= (dmeval "type") "Selection"); ---------- LAYER for Points/Blocks/Lines options
    (progn
      (initget 128 "Current Same"); allow Enter or non-keyword input
      (while
        (and
          (setq laytemp
            (getkword
              (strcat
                "\nLayer for "
                (dmeval "type")
                ", or Current, or Same as selected path <"
                (cond ((dmeval "lay")) ("Current")); current-Layer first-use default
                ">: "
              ); strcat
            ); getkword and laytemp
          ); setq
          (not (wcmatch laytemp "Current,Same")); User input but not C or S,
          (not (tblsearch "layer" laytemp)); and Layer is not in the drawing
        ); and
        (prompt "\nLayer does not exist in this drawing--"); ask them to try again
          ;;;;; option to make it?
        (initget 128 "Current Same")
      ); while
      (set (dmvar "lay")
        (cond
          (laytemp); User input [including C or S]
          ((dmeval "lay")); Enter - prior use
          ("Current"); Enter - first use
        ); cond
      ); set
      (if (not (wcmatch (dmeval "lay") "Current,Same"))
        ; if it's a Layer name that does exist, not current nor the object's,
        (command "_.layer" "_thaw" (dmeval "lay") ""); then - ensure it's Thawed; set current later
      ); if
    ); progn
  ); if - Layer [options other than Selection]
); defun - dmcommon
;
(defun dmpath (/ pathsel pathtype pathextr) ; --------------------- SELECT Path Object
  ;; shared path-object-selection, length calculation and UCS determination/setting
  (if
    (and
      (not ; T when (while) below is satisfied
        (while
          (not
            (and
              (not ; T when (while) below is satisfied
                (while
                  (and
                    (not (setq pathsel (entsel (strcat "\nSelect object to " dmwhich ": "))))
                    (= (getvar 'errno) 7)
                  ); and
                  (prompt "\nNothing selected -- try again.")
                ); while
              ); not
              (if pathsel
                (wcmatch ; then
                  (cdr (assoc 0 (entget (car pathsel))))
                  "LINE,ARC,CIRCLE,ELLIPSE,*POLYLINE,SPLINE"
                ); wcmatch
                T ; else - Enter/space for (entsel) above
              ); if
            ); and
          ); not
          (prompt "\nInvalid object type.")
        ); while
      ); not
      pathsel ; something selected - lets Enter/space end routine
    ); and
    (progn ; then
      (command "_.undo" "_begin")
      (setq
        path (car pathsel)
        pathdata (entget path)
        pathtype (cdr (assoc 0 pathdata))
        pathtype
          (if (= pathtype "POLYLINE")
            (substr (cdr (assoc 100 (cdr (member (assoc 100 pathdata) pathdata)))) 5); then
              ;; ^ = entity type from second (assoc 100) without "AcDb" prefix;  uses this because (assoc 0)
              ;; value is the same for 2D heavy & 3D Polylines; can set UCS to match former, but not latter
            pathtype ; else - leave alone
          ); if and pathtype
        pathlength
          (vlax-curve-getDistAtParam path (vlax-curve-getEndParam path))
        pathextr (cdr (assoc 210 pathdata))
      ); setq
      (if (= dmwhich "Divide") (setq pathlength (- pathlength (* _divinset_ 2))))
        ; effective length to be Divided, between end-insets
      (if ; set UCS to match object only under certain circumstances
        (or ; look at entity types other than 3D Polylines and 3D Splines
          (and
            (= pathtype "LINE")
            (not ; unequal Z components at ends, in current CS
              (equal
                (caddr (trans (cdr (assoc 10 pathdata)) 0 1))
                (caddr (trans (cdr (assoc 11 pathdata)) 0 1))
                1e-12
              ); equal
            ); not
          ); and - Line UCS check
          (and
            (wcmatch pathtype "ARC,CIRCLE,ELLIPSE,LWPOLYLINE,2dPolyline")
            (not (equal (trans pathextr 0 1) '(0 0 1) 1e-12)); extrusion direction not = current CS
          ); and - A/C/E/LWP/2dP UCS check
          (and
            (= pathtype "SPLINE")
            (if pathextr (not (equal (trans pathextr 0 1) '(0 0 1) 1e-12)))
              ;; ^ planar [2D] Splines have 210 value; non-planar [3D] do not
          ); and - Spline UCS check
        ); or - change UCS
        (progn ; then
          (if (equal pathextr '(0 0 1) 1e-12)
            (command "_.ucs" "_world")
            (command "_.ucs" "_new" "_object" path); set UCS to match object
          ); if
          (setq ucschanged T) ; marker for *error* to reset UCS if routine doesn't get to it
        ); progn - then
      ); if - UCS match object
    ); progn - then
  ); if
  pathsel ; returns something if valid selection, nil on Esc/Enter/space
); defun - dmpath
;
(defun dmdir () ; ----------------------- local DIRection [radians] of path at dmpt location
  (angle ; then - calculate local direction
    '(0 0 0)
    (trans
      (vlax-curve-getFirstDeriv
        path
        (vlax-curve-getParamAtPoint path (trans dmpt 1 0))
      ); getFirstDeriv
      0 1 T ; world to current CS, as displacement
    ); trans
  ); angle
); defun - dmdir
;
(defun dmrot () ; --------------------------------------------------- ROTation of Blocks/Selection
  (cond
    ((= (dmeval "rot") "Aligned") (dmdir)); local direction
    ((= (dmeval "rot") "Relative") (+ (dmdir) (dmeval "rel"))); local direction + relative angle
    ((dmeval "rot")) ; otherwise - specified constant angle
  ); cond - rotation
); defun - dmrot
;
(defun dmputblk () ; ----------------------------------- insert Block on path at dmpt location
  (command
    "_.insert" (dmeval "blk")
    "_scale" (dmeval "scl")
    dmpt
    (angtos (dmrot))
  ); command
); defun - dmputblk
;
(defun dmputlin () ; -------------------------------------- place Line on path at dmpt location
  (command
    "_.line"
    (polar dmpt (+ (dmdir) (/ pi 2)) (/ (dmeval "lin") 2))
    (polar dmpt (- (dmdir) (/ pi 2)) (/ (dmeval "lin") 2))
    "" ; line
  ); command
); defun - dmputlin
;
(defun dmputset () ; ------------------------------- place Selection on path at dmpt location
  (command
    "_.pasteclip" "_rotate"
    (angtos (dmrot))
    dmpt
  ); command
); defun - dmputset
;
(defun dmputall (/ incr) ; -------- PUT IN Points/Blocks/Lines/Selection along path
  (if
    (and
      (/= (dmeval "type") "Selection"); set Layer only if Points/Blocks/Lines
      (/= (dmeval "lay") "Current"); set Layer only if Current not selected
    ); and
    (command "_.layer" "_set" ; [this instead of (setvar 'clayer) will turn it on if it's off]
      (if (= (dmeval "lay") "Same"); if Layer is Same,
        (cdr (assoc 8 pathdata)); then - get layer of object
        (dmeval "lay") ; else - use specified Layer
      ); if
      ""
    ); command - Layer
  ); if - not Current Layer
;
  (setvar 'osmode 0)
  (setvar 'blipmode 0)
;
  (setq incr 0)
  (repeat
    (if (and (= dmwhich "Divide") (= _divmid_ "Midpoints"))
      segments ; then
      (1- segments); else
        ; [if not DIV+/Midpoints, and 1 segment or path shorter than Max: 0 times]
    ); if
    (setq
      incr (1+ incr)
      dmpt
        (trans
          (vlax-curve-getPointAtDist
            path
            (if (= dmwhich "Divide")
              (+ ; then [DIV+]
                (* incr (/ pathlength segments))
                (if (= _divmid_ "Midpoints") (- (/ pathlength segments 2)) _divinset_)
              ); +
              (+ (* incr _measpc_) meainset); else [MEA+]
            ); if
          ); getPointAtDist
          0 1
        ); trans and dmpt
    ); setq
    (dmput1)
  ); repeat
); defun - dmputall
;
(defun dmput1 ()
  (cond
    ((= (dmeval "type") "Points") (command "_.point" dmpt))
    ((= (dmeval "type") "Blocks") (dmputblk))
    ((= (dmeval "type") "Lines") (dmputlin))
    ((dmputset)); none-of-the-above - Selection
  ); cond
); defun - dmput1
;
(defun dmreset ()
  (setvar 'clayer clay)
  (setvar 'osmode osm)
  (setvar 'blipmode blips)
  (setvar 'cmdecho cmde)
  (princ)
); defun - dmreset
;
;
(defun C:DIV+
  (/ *error* dmwhich osm cmde blips clay path
  pathdata pathlength ucschanged segments dmpt)
;
  (defun *error* (errmsg) (dmerror))
  (setq dmwhich "Divide")
  (dmcommon)
;
  (initget 6 "Maximum"); no zero, no negative
  (setq _divseg_
    (cond
      ; once set, _divseg_ is either integer or "Maximum" [first-use default]
      ( ; User input
        (getint
          (strcat
            "\nEnter number of Segments, or M for Maximum spacing <"
            (if (and _divseg_ (numberp _divseg_)); if default is a number,
              (itoa _divseg_); then - text equivalent
              "M" ; else - no default yet, or it's "Maximum"
            ); if
            ">: "
          ); strcat
        ); getint
      ); User input [other than Enter] condition
      (_divseg_); Enter - prior use
      ("Maximum"); Enter - first use
    ); cond
  ); setq
  (if (= _divseg_ "Maximum")
    (progn
      (initget (if _divmax_ 38 39))
        ; no Enter on first use, no 0, no negative, dashed rubber-band if picked on-screen
      (setq _divmax_
        (cond
          ( ; User input
            (getdist
              (strcat
                "\nMaximum spacing of "
                _divtype_
                (if _divmax_ (strcat " <" (rtos _divmax_) ">") ""); no default on first use
                ": "
              ); strcat
            ); getdist
          ); User input [other than Enter] condition
          (_divmax_); Enter - prior use
        ); cond
      ); setq
    ); progn
  ); if
;
  (initget "Midpoints Standard")
  (setq _divmid_
    (cond
      ( ; User input
        (getkword
          (strcat
            "\nPlace "
            (dmeval "type")
            " at division points (Standard) or at Midpoints of divisions [S/M]? <"
            (if _divmid_ (substr _divmid_ 1 1) "S")
            ">: "
          ); strcat
        ); getkword
      ); User input [other than Enter] condition
      (_divmid_); Enter - prior use
      ("Standard"); Enter - first use
    ); cond
  ); setq
;
  (initget 4); no negative
  (if (= _divmid_ "Standard")
    (setq _divinset_ ; then
      (cond
        ( ; User input
          (getdist
            (strcat
              "\nInset from both ends of path to endmost "
              _divtype_
              " <"
              (if _divinset_ (rtos _divinset_) "0"); default if present, otherwise 0
              ">: "
            ); strcat
          ); getdist
        ); User input [other than Enter] condition
        (_divinset_); Enter - prior use
        (0); Enter - first use
      ); cond and _divinset_
    ); setq
    (setq _divinset_ 0); else
  ); if
;
  (if (and (= _divmid_ "Standard") (zerop _divinset_))
    (progn
      (initget "Yes No")
      (setq _divends_
        (cond
          ( ; User input
            (getkword
              (strcat
                "\nPlace "
                _divtype_
                " at Ends of unclosed path [Yes/No]? <"
                (cond (_divends_) ("Yes")); Y default on first use
                ">: "
              ); strcat
            ); getkword
          ); User input [other than Enter] condition
          (_divends_); Enter - prior use
          ("Yes"); Enter - first use
        ); cond & _divends_
      ); setq
    ); progn
  ); if
;
  (if (and (zerop _divinset_) (= _divends_ "No") (= _divseg_ 1) (= _divmid_ "Standard"))
    (progn
      (alert
        (strcat
          "You have asked for 1 segment without "
          _divtype_
          " at the Ends.\nNo "
          _divtype_
          " will be placed."
        ); strcat
      ); alert
      (dmreset)
      (command "_.undo" "_end")
      (exit)
    ); progn
  ); if
;
  (while
    (dmpath) ; ----------------------------------------------------------------------- SELECT Path Object
    (if
      (or
        (and
          (= _divseg_ "Maximum")
          (<= pathlength _divmax_); effective path no longer than maximum spacing,
          (= _divends_ "No"); and User didn't choose to put at ends,
          (= _divmid_ "Standard"); and not putting at division Midpoints
        ); and
        (and
          (< pathlength 0); full path shorter than end-insets
          (= _divmid_ "Standard"); and not putting at division Midpoints
        ); and
      ); or
      (alert ; tell them
        (strcat
          "Path is not long enough;\nNo "
          _divtype_
          " will be placed."
        ); strcat
      ); alert
    ); if
;
    (setq segments
      (cond
        ((< pathlength 0) 1); end-inset > half length; 0 repeats
        ((numberp _divseg_) _divseg_); number with long-enough path
        (T ; Max with long-enough path
          (if (zerop (rem pathlength _divmax_))
            (fix (/ pathlength _divmax_))
            (1+ (fix (/ pathlength _divmax_)))
          ); if
        ); T condition
      ); cond and segments
    ); setq
;
    (dmputall) ; --------------------------------------- PUT IN Points/Blocks/Lines/Selections
;
    (cond ; at end(s)
      ((and (zerop _divinset_) (vlax-curve-isClosed path) (= _divmid_ "Standard"))
        ; add one at start/end of closed path to emulate Divide, if no end-inset
        (setq dmpt (trans (vlax-curve-getStartPoint path) 0 1))
        (dmput1)
      ); closed-path condition
      ((or ; placements at ends of open path if:
          (and (zerop _divinset_) (= _divends_ "Yes") (= _divmid_ "Standard"))
            ; requested and no end-inset
          (and (/= _divinset_ 0) (>= pathlength 0))
            ; effective ends with end-inset, if path is long enough
        ); or
        (setq dmpt
          (if (= _divinset_ 0)
            (trans (vlax-curve-getStartPoint path) 0 1); then
              ; line above because if 0 inset, can sometimes miss starting end using line below
                ;;;;; [unsolved quirk: can miss starting end, rarely, after putting in along path -- 0 inset,
                ;;;;; Yes at ends, e.g. on Line in different CS.  Error: bad argument type: numberp: nil]
            (trans (vlax-curve-getPointAtDist path _divinset_) 0 1)); else
          ); if
        (dmput1)
        (setq dmpt
          (if (= _divinset_ 0)
            (trans (vlax-curve-getEndPoint path) 0 1); then
              ; line above because if 0 inset, can sometimes miss trailing end using line below
            (trans (vlax-curve-getPointAtDist path (+ pathlength _divinset_)) 0 1); else
          ); if
        ); setq & dmpt
        (dmput1)
      ); open-path condition
    ); cond - at end(s)
;
    (if ucschanged (command "_.ucs" "_prev"))
    (setq ucschanged nil); eliminate UCS reset in *error* since routine did it already
    (command "_.undo" "_end")
  ); while
  (dmreset)
); defun - DIV+
;
;
(defun C:MEA+
  (/ *error* dmwhich osm cmde blips clay blktemp scltemp path
  pathdata pathlength ucschanged segments meainset dmpt)
;
  (defun *error* (errmsg) (dmerror))
  (setq dmwhich "Measure")
  (dmcommon)
;
  (initget (if _measpc_ 6 7)); no zero, no negative, no Enter on first use
  (setq _measpc_
    (cond
      ( ; User input
        (getdist
          (strcat
            "\nEnter length of segment"
            (if _measpc_ (strcat " <" (rtos _measpc_) ">") ""); default if present
            ": "
          ); strcat
        ); getdist
      ); User input [other than Enter] condition
      (_measpc_); Enter - prior use
    ); cond
  ); setq
;
  (initget "Center Standard")
  (setq _meactr_
    (cond
      ( ; User input
        (getkword
          (strcat
            "\nStandard measure alignment or Center on path length [S/C]? <"
            (cond
              (_meactr_ (substr _meactr_ 1 1)); default from prior use
              ("Standard"); default on first use
            ); cond
            ">: "
          ); strcat
        ); getkword
      ); User input [other than Enter] condition
      (_meactr_); Enter - prior use
      ("Standard"); Enter - first use
    ); cond
  ); setq
;
  (if (= _meactr_ "Standard")
    (progn ; then
      (initget "Yes No")
      (setq _meastart_
        (cond
          ( ; User input
            (getkword
              (strcat
                "\nPlace "
                (if (= _meatype_ "Selection") _meatype_ (substr _meatype_ 1 (1- (strlen _meatype_))))
                " at starting end(s) of path(s) [Yes/No]? <"
                (cond
                  (_meastart_ (substr _meastart_ 1 1)); default from prior use
                  ("Y"); default on first use
                ); cond
                ">: "
              ); strcat
            ); getkword
          ); User input [other than Enter] condition
          (_meastart_); Enter - prior use
          ("Yes"); Enter - first use
        ); cond
      ); setq
    ); progn
    (setq _meastart_ "Yes"); else - always include starting one when centered
  ); if
;
  (while
    (dmpath) ; --------------------------------------------------------------------- SELECT Path Object
    (if
      (and
        (< pathlength _measpc_); path is shorter than spacing,
        (or
          (= _meactr_ "Center"); under Center spacing
          (= _meastart_ "No"); or User didn't choose to put at start,
        ); or
      ); and
      (progn
        (alert ; tell them
          (strcat
            "Path is shorter than segment length;\nNo "
            _meatype_
            " will be placed."
          ); strcat
        ); alert
      ); progn
    ); if
    (setq
      segments (1+ (fix (/ pathlength _measpc_)))
      meainset (if (= _meactr_ "Center") (/ (rem pathlength _measpc_) 2) 0)
    ); setq
;
    (dmputall) ; ------------------------------------ PUT IN Points/Blocks/Lines/Selection
;
    (if ; at start
      (or
        (and (= _meactr_ "Standard") (= _meastart_ "Yes")); Standard if starting end requested
        (and (= _meactr_ "Center") (>= pathlength _measpc_)); Center only if long enough
      ); or
      (progn ; then
        (setq dmpt (trans (vlax-curve-getPointAtDist path meainset) 0 1))
        (dmput1)
          ;;;;; [unsolved quirk: can miss starting end, rarely, after putting in along path --  Standard
          ;;;;; position, Yes at start, e.g. on Line in different CS.  Error: bad argument type: numberp: nil]
      ); progn
    ); if - at start
;
    (if ucschanged (command "_.ucs" "_prev"))
    (setq ucschanged nil); eliminate UCS reset in *error* since routine did it already
    (command "_.undo" "_end")
  ); while
  (dmreset)
); defun - MEA+
;
(prompt "Type DIV+ to DividePlus objects, MEA+ to MeasurePlus objects.")
