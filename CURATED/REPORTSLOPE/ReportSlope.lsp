;|
ReportSlope.LSP [Command name: RS]
To Report the Slope [rise-over-run] of a selected straight object or segment,
  giving its relative angle off of the Horizontal, regardless of which direction
  it slopes or in what direction it was drawn.  [See instructions in code below
  if absolute angle is desired; NOTE that for other than vlax-curve-applicable
  types, and for Xlines/Rays when in Blocks, same object will return opposite
  slopes from different pick point locations.]
Reports in Decimal Degrees, Radians, Percentage, X in 12, and 1 in X values,
  to both an Alert box and the Command: line for later reference.
Use to determine whether a resultant slope is too steep for an accessible
  ramp, or shallow enough to be a "sidewalk"-type slope and not a ramp, or
  "flat" enough for a landing or parking space loading area, or steep enough
  to drain, the roof slope with which a section or elevation was drawn, etc.
If segment with Z differential is selected, reports slope in X-Y plane; ignores
  Z component.
[Have not yet found a way to determine whether the selected point on a
  Region or 3D Solid boundary came from a Spline or Splined Polyline; if
  picked there, routine will not object, but will report an inappropriate result.
  (Both remain in the filter list because reporting on straight segments is still
  valid, and circle/arc/ellipse-source segments will correctly be denied.)]
Kent Cooper, last edited 9 February 2017
|;
(defun C:RS
  (/ *error* esel edata etype pickpt subedata subetype path ang 1stDeriv angadj angrad angdeg pcnt in12 1in info)
  (defun *error* (errmsg)
    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
      (princ (strcat "\nError: " errmsg))
    ); if
    (setvar 'aperture aper)
  ); defun
  (setq aper (getvar 'aperture))
  (setvar 'aperture (getvar 'pickbox))
  (while
    (not
      (and
        (setq esel (entsel "\nSelect object to report its Slope: "))
        (setq
          edata (entget (car esel))
          etype (cdr (assoc 0 edata))
          pickpt (osnap (cadr esel) "_nea"); for (vlax-curve) later; also prohibits things like text elements of Dimensions
        ); setq
        (wcmatch etype "LINE,*POLYLINE,@LINE,RAY,INSERT,HATCH,DIMENSION,LEADER,*SOLID,3DFACE,WIPEOUT,TRACE,REGION,IMAGE,VIEWPORT,TOLERANCE")
        (not (osnap pickpt "_cen"))
          ; if Polyline/Block/Region/3DSolid/angular Dimension, not on arc segment or circle/arc element
        (cond
          ((= etype "INSERT")
            (and ; then, use nested object -- same checks as above, except:
                ; no center Osnap check [earlier check covers it]
                ; no Insert or heavy Polyline object types [never returned by (nentselp)]
                ; add Vertex type for heavy Polylines
              (setq
                subedata (entget (car (nentselp pickpt)))
                subetype (cdr (assoc 0 subedata))
              ); setq
              (wcmatch subetype "LINE,LWPOLYLINE,VERTEX,@LINE,RAY,HATCH,DIMENSION,LEADER,*SOLID,3DFACE,WIPEOUT,TRACE,REGION,IMAGE,VIEWPORT,TOLERANCE")
              (if (= subetype "LEADER") (= (cdr (assoc 72 subedata)) 0) T); STraight, not Splined
              (if (= subetype "VERTEX") (= (boole 1 8 (cdr (assoc 70 subedata))) 0) T); not Splined 2DPolyline
            ); and
          ); Insert condition
          ((= etype "LEADER") (= (cdr (assoc 72 edata)) 0)); STraight, not Splined
          ((= etype "POLYLINE") (= (boole 1 4 (cdr (assoc 70 edata))) 0)); not Splined 2DPolyline
          (T) ; all other object types
        ); cond
      ); and
    ); not
    (setvar 'aperture aper)
    (prompt "\nNothing selected, slope varies, or cannot Report Slope on that object --")
  ); while
  (if (wcmatch etype "LINE,*POLYLINE,XLINE,RAY"); vlax-curve-applicable types
    (setq ; then
      path (car esel)
      ang
        (angle; then
          '(0 0 0)
          (vlax-curve-getFirstDeriv
            path
            (vlax-curve-getParamAtPoint path pickpt)
          ); 1st deriv
        ); angle
    ); setq
    (setq; else [other types]
      ang
        (angle ; [will return 0 if Ray picked AT end or Xline picked AT origin/midpoint]
          (osnap pickpt (if (member "RAY" (list etype subetype)) "_nea" "_mid")); account for Ray in Block [no midpoint]
          (osnap pickpt (if (member "XLINE" (list etype subetype)) "_nea" "_end")); account for Xline in Block [no endpoint]
            ; curiosity: Xlines/Rays in Blocks extend normally on-screen, but cannot be selected
            ; or snapped to beyond some limit slightly outside extent of other finite Block elements.
        ); angle
    ); setq & ang
  ); if
  (setq
    angadj; angle adjusted--relative to Horizontal, regardless of direction
      ; remove this variable for absolute angle, and change occurrences of 'angadj' below to 'ang'
      (cond
        ((<= ang (/ pi 2)) ang); no more than 90 degrees
        ((<= ang pi) (- pi ang)); no more than 180 degrees
        ((< ang (* pi 1.5)) (- ang pi)); under 270 degrees
        (T (- (* pi 2) ang)); 270 degrees or more
      ); cond & angadj
    angrad (rtos angadj 2 4); in Radians
    angdeg (angtos angadj 1 6); in Degrees/minutes/seconds
    pcnt; as a Percentage
      (cond
        ((equal angadj (/ pi 2) 1e-8) "[Infinite]"); use (or) and include (* pi 1.5) for absolute angle
        (T (rtos (* 100 (/ (sin angadj) (cos angadj))) 2 4))
      ); cond - Percent
    in12; whatever rise in 12 units run
      (cond
        ((equal angadj (/ pi 2) 1e-8) "[Infinite]"); use (or) and include (* pi 1.5) for absolute angle
        (T (rtos (* 12 (/ (sin angadj) (cos angadj))) 2 4))
      ); cond - X in 12
    1in; 1 unit rise in whatever run
      (cond
        ((equal angadj 0 1e-8) "[Infinite]"); use (or) and include pi for absolute angle
        (T (rtos (/ 1 (/ (sin angadj) (cos angadj))) 2 4))
      ); cond - 1 in X
  ); setq
  (setq info
    (strcat
      "Slope of Object off of Horizontal is:\n" ; remove "off of Horizontal" for absolute angle
      angdeg " Degrees,\n"
      angrad " Radians,\n"
      pcnt " Percent,\n"
      in12 " in 12, or\n1 in "
      1in
      "."
    ); strcat
  ); setq
  (alert info)
  (prompt (strcat "\n\n" info "\n"))
  (setvar 'aperture aper)
  (princ)
); defun

(vl-load-com)
(prompt "\nType RS to Report Slope of object.")
