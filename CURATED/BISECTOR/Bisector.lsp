;;;  Bisector.LSP [Command name: BI]
;;;  To Bisect either:
;;;    1.  the angle between two straight non-parallel drawing entities or sub-entities;
;;;    2.  the distance between two straight parallel drawing entities or sub-entities.
;;;  Draws an Xline bisecting the angle or distance, in the case of angles in the
;;;    direction between picked locations, on the current Layer.
;;;  Kent Cooper, last edited 2 April 2015

(defun C:BI (/ *error* var ev getobj pick1 ent1 pick2 ent2 appint ang1 ang2 cmde aper)

  (defun *error* (errmsg)
    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
      (princ (strcat "\nError: " errmsg))
    ); if
    (setvar 'aperture aper)
    (setvar 'cmdecho cmde)
  ); defun - *error*

  (defun var (typ); build variable name with number
    (read (strcat typ num))
  ); defun - var

  (defun ev (typ); evaluate what's in variable name with number
    (eval (read (strcat typ num)))
  ); defun - ev

  (defun getobj (num / esel edata etype subedata subetype path)
    (while
      (not
        (and
          (setq
            esel (entsel (strcat "\nSelect linear object #" num " for Angle/Spacing Bisector: "))
            edata (if esel (entget (car esel)))
            etype (if esel (cdr (assoc 0 edata)))
          ); setq
          (set
            (var "pick"); = pick1 or pick2
            (osnap (cadr esel) "nea"); for (vlax-curve) later; also prohibits things like text elements of Dimensions
          ); set
          (wcmatch etype "LINE,*POLYLINE,@LINE,RAY,INSERT,HATCH,DIMENSION,LEADER,*SOLID,3DFACE,WIPEOUT,TRACE,REGION,IMAGE,VIEWPORT,TOLERANCE")
          (not (osnap (ev "pick") "cen"))
            ; if Polyline/Block/Region/3DSolid/angular Dimension, not on arc/ellipse segment or circle/arc/ellipse element
          (cond
            ((= etype "INSERT")
              (and ; then, use nested object -- same checks as above, except:
                  ; no center Osnap check [earlier check covers it]
                  ; no Insert or heavy Polyline object types [never returned by (nentselp)]
                  ; add Vertex type for heavy Polylines
                (setq
                  subedata (entget (car (nentselp (ev "pick"))))
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
      (prompt "\nNothing selected, or not a straight object with linearity --")
    ); while
    (set (var "ent") (car esel)); = ent1 or ent2 [only ent1 used, to highlight]
    (if (wcmatch etype "LINE,*POLYLINE,XLINE,RAY"); vlax-curve-applicable types
      (progn ; then
        (setq path (car esel))
        (set
          (var "ang"); = ang1 or ang2
          (angle; then
            '(0 0 0)
            (vlax-curve-getFirstDeriv
              path
              (vlax-curve-getParamAtPoint path (ev "pick"))
            ); 1st deriv
          ); angle
        ); set
      ); progn - then
      (set ; else [other types]
        (var "ang"); = ang1 or ang2
        (angle ; [will return 0 if Ray picked AT end or Xline picked AT origin/midpoint]
          (osnap (ev "pick") (if (= subetype "RAY") "nea" "mid")); account for Ray in Block [no midpoint]
          (osnap (ev "pick") (if (= subetype "XLINE") "nea" "end")); account for Xline in Block [no endpoint]
            ; curiosity: Xlines/Rays in Blocks extend normally on-screen, but cannot be selected
            ; or snapped to beyond some limit slightly outside other finite Block elements.
        ); angle
      ); set - else
    ); if
  ); defun - getobj

  (setq
    cmde (getvar 'cmdecho)
    aper (getvar 'aperture)
  ); setq
  (setvar 'cmdecho 0)
  (setvar 'aperture (getvar 'pickbox)); prevents Osnapping to inappropriate nearby things
  (getobj "1")
  (redraw ent1 3); highlight
  (getobj "2")
  (redraw ent1 4); un-highlight
  (setvar 'aperture aper)
  (if (or (equal ang1 ang2 1e-8) (equal (abs (- ang1 ang2)) pi 1e-8))
    (progn ; then
      (prompt "\nObjects are parallel; drawing Xline halfway between.")
      (command
        "_.xline"
        "_ang" (* (/ ang1 pi) 180)
        "_none" (mapcar '/ (mapcar '+ pick1 pick2) '(2 2 2))
        ""
      ); command
    ); progn
    (progn ; else - not parallel
      (setq
        appint (inters pick1 (polar pick1 ang1 1) pick2 (polar pick2 ang2 1) nil)
        ang1 (angle appint pick1); both outward [may reverse either or both]
        ang2 (angle appint pick2)
      ); setq
      (command
        "_.xline"
        "_ang" (* (/ (+ ang1 ang2) 2 pi) 180)
        "_none" appint
        ""
      ); command
    ); progn
  ); if
  (setvar 'cmdecho cmde)
  (princ)
); defun

(vl-load-com)
(prompt "\nType BI to draw the Bisector of an angle or parallel-object spacing.")
(princ)