;;; AutoBlockLayouts.lsp
;;; Builds saved views and paper-space layouts from selected marker blocks.
;;;
;;; Command: ABLOCKLAYOUTS
;;; Command: ABLOCKLAYOUTSALL
;;;
;;; What each selected block controls:
;;; - Block bounding box = model-space view limits.
;;; - SHEET attribute value = layout suffix, for example .02.
;;; - Layout prefix comes from the loaded _GetDWGPrefix function if available,
;;;   otherwise this routine extracts it from the DWG name.
;;; - Dynamic block property or scale note text such as 1/2" = 1'-0" = viewport XP scale.
;;;
;;; Recommended setup:
;;; - Keep one clean template layout in the drawing with the title block and one viewport.
;;; - Put one marker block around each model-space view area.
;;; - Put the sheet suffix in the block attribute named SHEET, such as .01, .02, .03.
;;; - Put the viewport scale in a dynamic block property such as Visibility1,
;;;   or as text inside/near the marker block.

(setq *ABLMarkerBlockName* "LayoutGEN2")
(setq *ABLMarkerBlockNames* '("LayoutGEN2" "Layout" "Layout:")) ; Marker blocks scanned by ABLOCKLAYOUTSALL.
(setq *ABLWarnedCannoScales* nil)

(vl-load-com)

(defun abl:doc ()
  (vla-get-ActiveDocument (vlax-get-acad-object))
)

(defun abl:layouts ()
  (vla-get-Layouts (abl:doc))
)

(defun abl:views ()
  (vla-get-Views (abl:doc))
)

(defun abl:str-trim (s)
  (if s
    (vl-string-trim " \t\r\n\"" s)
    ""
  )
)

(defun abl:variant-point (v)
  (cond
    ((= (type v) 'VARIANT) (vlax-safearray->list (vlax-variant-value v)))
    ((= (type v) 'SAFEARRAY) (vlax-safearray->list v))
    (t v)
  )
)

(defun abl:bbox (obj / minpt maxpt)
  (vla-GetBoundingBox obj 'minpt 'maxpt)
  (list (vlax-safearray->list minpt)
        (vlax-safearray->list maxpt))
)

(defun abl:obj-closed-p (obj / val)
  (setq val (vl-catch-all-apply 'vla-get-Closed (list obj)))
  (and (not (vl-catch-all-error-p val))
       (= val :vlax-true))
)

(defun abl:obj-cyan-p (obj / col)
  ;; AutoCAD ACI 4 = cyan. ByLayer objects can still be candidates,
  ;; but cyan objects are preferred when finding the view border.
  (setq col (vl-catch-all-apply 'vla-get-Color (list obj)))
  (and (not (vl-catch-all-error-p col))
       (= col 4))
)

(defun abl:bbox-area (bounds / minpt maxpt)
  (setq minpt (car bounds)
        maxpt (cadr bounds))
  (* (abs (- (car maxpt) (car minpt)))
     (abs (- (cadr maxpt) (cadr minpt))))
)

(defun abl:transform-point (pt ins sx sy rot / x y xr yr)
  (setq x  (* (car pt) sx)
        y  (* (cadr pt) sy)
        xr (- (* x (cos rot)) (* y (sin rot)))
        yr (+ (* x (sin rot)) (* y (cos rot))))
  (list (+ (car ins) xr)
        (+ (cadr ins) yr)
        0.0)
)

(defun abl:transform-bounds (bounds blk / minpt maxpt ins sx sy rot pts xs ys)
  (setq minpt (car bounds)
        maxpt (cadr bounds)
        ins   (abl:variant-point (vlax-get blk 'InsertionPoint))
        sx    (vla-get-XScaleFactor blk)
        sy    (vla-get-YScaleFactor blk)
        rot   (vla-get-Rotation blk)
        pts   (list
                (list (car minpt) (cadr minpt) 0.0)
                (list (car maxpt) (cadr minpt) 0.0)
                (list (car maxpt) (cadr maxpt) 0.0)
                (list (car minpt) (cadr maxpt) 0.0)))
  (setq pts (mapcar '(lambda (p) (abl:transform-point p ins sx sy rot)) pts)
        xs  (mapcar 'car pts)
        ys  (mapcar 'cadr pts))
  (list (list (apply 'min xs) (apply 'min ys) 0.0)
        (list (apply 'max xs) (apply 'max ys) 0.0))
)

(defun abl:block-definition (blk / name res)
  ;; Dynamic blocks often use an anonymous block name for the active geometry.
  (setq name (vla-get-Name blk))
  (setq res (vl-catch-all-apply 'vla-Item (list (vla-get-Blocks (abl:doc)) name)))
  (if (vl-catch-all-error-p res) nil res)
)

(defun abl:best-border-from-objects (objs / obj objname bounds candidates cyan noncyan sorted chosen)
  (setq cyan '()
        noncyan '())
  (foreach obj objs
    (setq objname (vla-get-ObjectName obj))
    (if (and (member objname '("AcDbPolyline" "AcDb2dPolyline"))
             (abl:obj-closed-p obj))
      (progn
        (setq bounds (abl:bbox obj))
        (if (> (abl:bbox-area bounds) 0.0001)
          (if (abl:obj-cyan-p obj)
            (setq cyan (cons bounds cyan))
            (setq noncyan (cons bounds noncyan))
          )
        )
      )
    )
  )
  (setq candidates
        (if cyan
          cyan
          ;; If no cyan border exists, use the smaller closed rectangle,
          ;; since the largest is commonly the outer sheet/control border.
          (cdr (vl-sort noncyan '(lambda (a b) (> (abl:bbox-area a) (abl:bbox-area b)))))
        ))
  (if candidates
    (progn
      (setq sorted
            (vl-sort
              candidates
              '(lambda (a b) (> (abl:bbox-area a) (abl:bbox-area b)))
            )
      )
      (setq chosen (car sorted))
      chosen
    )
  )
)


(defun abl:large-layout-marker-p (blk)
  ;; The larger marker block uses its OUTSIDE rectangle as the viewport extents.
  ;; LayoutGEN2 still uses the cyan/internal view border behavior.
  (abl:member-ci-p (abl:block-name blk) '("Layout" "Layout:"))
)

(defun abl:largest-border-from-objects (objs / obj objname bounds candidates sorted)
  ;; For the large Layout/Layout: block, always use the largest closed polyline
  ;; in the evaluated block geometry. This prevents interior cyan/detail boxes
  ;; from being mistaken as the viewport window.
  (setq candidates '())
  (foreach obj objs
    (setq objname (vla-get-ObjectName obj))
    (if (and (member objname '("AcDbPolyline" "AcDb2dPolyline"))
             (abl:obj-closed-p obj))
      (progn
        (setq bounds (abl:bbox obj))
        (if (> (abl:bbox-area bounds) 0.0001)
          (setq candidates (cons bounds candidates))
        )
      )
    )
  )
  (if candidates
    (progn
      (setq sorted
        (vl-sort candidates '(lambda (a b) (> (abl:bbox-area a) (abl:bbox-area b))))
      )
      (car sorted)
    )
  )
)

(defun abl:exploded->list (exploded)
  (cond
    ((= (type exploded) 'VARIANT)
     (vlax-safearray->list (vlax-variant-value exploded)))
    ((= (type exploded) 'SAFEARRAY)
     (vlax-safearray->list exploded))
    ((listp exploded)
     exploded)
    (t nil)
  )
)

(defun abl:view-border-bounds-from-explode (blk / exploded objs result)
  ;; Explode evaluates dynamic block visibility/stretch for this specific insert.
  ;; Delete the temporary exploded objects immediately after measuring them.
  (setq exploded (vl-catch-all-apply 'vlax-invoke (list blk 'Explode)))
  (if (not (vl-catch-all-error-p exploded))
    (progn
      (setq objs (abl:exploded->list exploded))
      (setq result (abl:best-border-from-objects objs))
      (foreach obj objs
        (vl-catch-all-apply 'vla-Delete (list obj))
      )
      result
    )
  )
)


(defun abl:view-border-bounds-large-from-explode (blk / exploded objs result)
  ;; Large Layout/Layout: markers: use evaluated exploded geometry so dynamic
  ;; visibility/stretch states are respected, then select the largest closed rectangle.
  (setq exploded (vl-catch-all-apply 'vlax-invoke (list blk 'Explode)))
  (if (not (vl-catch-all-error-p exploded))
    (progn
      (setq objs (abl:exploded->list exploded))
      (setq result (abl:largest-border-from-objects objs))
      (foreach obj objs
        (vl-catch-all-apply 'vla-Delete (list obj))
      )
      result
    )
  )
)

(defun abl:view-border-bounds-large-from-definition (blk / def obj objs chosen)
  (setq def (abl:block-definition blk)
        objs '())
  (if def
    (progn
      (vlax-for obj def
        (setq objs (cons obj objs))
      )
      (setq chosen (abl:largest-border-from-objects objs))
      (if chosen
        (abl:transform-bounds chosen blk)
      )
    )
  )
)

(defun abl:view-border-bounds-from-definition (blk / def obj objs chosen)
  (setq def (abl:block-definition blk)
        objs '())
  (if def
    (progn
      (vlax-for obj def
        (setq objs (cons obj objs))
      )
      (setq chosen (abl:best-border-from-objects objs))
      (if chosen
          (abl:transform-bounds chosen blk)
      )
    )
  )
)

(defun abl:view-border-bounds (blk / b)
  (if (abl:large-layout-marker-p blk)
    (progn
      ;; For large layout markers, the outside rectangle is the view window.
      ;; Try exploded/evaluated geometry first; fall back to block definition.
      (setq b (abl:view-border-bounds-large-from-explode blk))
      (if b b (abl:view-border-bounds-large-from-definition blk))
    )
    (progn
      ;; For LayoutGEN2, keep the existing cyan/internal border behavior.
      (setq b (abl:view-border-bounds-from-explode blk))
      (if b b (abl:view-border-bounds-from-definition blk))
    )
  )
)

(defun abl:center2d (minpt maxpt)
  (list (/ (+ (car minpt)  (car maxpt))  2.0)
        (/ (+ (cadr minpt) (cadr maxpt)) 2.0))
)

(defun abl:point2d-variant (pt / arr)
  (setq arr (vlax-make-safearray vlax-vbDouble '(0 . 1)))
  (vlax-safearray-put-element arr 0 (car pt))
  (vlax-safearray-put-element arr 1 (cadr pt))
  (vlax-make-variant arr)
)

(defun abl:point3d-variant (pt / arr)
  (setq arr (vlax-make-safearray vlax-vbDouble '(0 . 2)))
  (vlax-safearray-put-element arr 0 (car pt))
  (vlax-safearray-put-element arr 1 (cadr pt))
  (vlax-safearray-put-element arr 2 (if (caddr pt) (caddr pt) 0.0))
  (vlax-make-variant arr)
)

(defun abl:width (minpt maxpt)
  (abs (- (car maxpt) (car minpt)))
)

(defun abl:height (minpt maxpt)
  (abs (- (cadr maxpt) (cadr minpt)))
)

(defun abl:pad-bounds (bounds factor / minpt maxpt dx dy)
  (setq minpt (car bounds)
        maxpt (cadr bounds)
        dx    (* (abl:width minpt maxpt) factor)
        dy    (* (abl:height minpt maxpt) factor))
  (list
    (list (- (car minpt) dx) (- (cadr minpt) dy) 0.0)
    (list (+ (car maxpt) dx) (+ (cadr maxpt) dy) 0.0)
  )
)

(defun abl:dist2d (p q)
  (distance (list (car p) (cadr p) 0.0)
            (list (car q) (cadr q) 0.0))
)

(defun abl:layout-exists-p (name / res)
  (setq res (vl-catch-all-apply 'vla-Item (list (abl:layouts) name)))
  (not (vl-catch-all-error-p res))
)

(defun abl:first-paper-layout (/ found lay)
  (vlax-for lay (abl:layouts)
    (if (and (null found) (/= (strcase (vla-get-Name lay)) "MODEL"))
      (setq found (vla-get-Name lay))
    )
  )
  found
)

(defun abl:paper-layout-count (/ count lay nm)
  (setq count 0)
  (vlax-for lay (abl:layouts)
    (setq nm (strcase (vla-get-Name lay)))
    (if (/= nm "MODEL")
      (setq count (1+ count))
    )
  )
  count
)

(defun abl:first-paper-layout-not-named (badname / found lay nm)
  (setq badname (strcase (abl:str-trim badname)))
  (vlax-for lay (abl:layouts)
    (setq nm (strcase (vla-get-Name lay)))
    (if (and (null found)
             (/= nm "MODEL")
             (/= nm badname))
      (setq found (vla-get-Name lay))
    )
  )
  found
)

(defun abl:delete-layout-named (layoutname / target fallback result)
  ;; Deletes a leftover template/layout tab by exact name, case-insensitive.
  ;; Used to remove TEMP automatically after generated sheet layouts are made.
  (setq layoutname (abl:str-trim layoutname))
  (if (and (/= layoutname "") (abl:layout-exists-p layoutname))
    (progn
      (cond
        ((= (strcase layoutname) "MODEL") nil)
        ((<= (abl:paper-layout-count) 1)
         (prompt (strcat "
TEMP cleanup skipped: cannot delete the only paper-space layout " layoutname ".")))
        (t
         (if (= (strcase (getvar 'CTAB)) (strcase layoutname))
           (progn
             (setq fallback (abl:first-paper-layout-not-named layoutname))
             (if fallback
               (setvar 'CTAB fallback)
               (setvar 'CTAB "Model")
             )
           )
         )
         (setq target (vla-Item (abl:layouts) layoutname))
         (setq result (vl-catch-all-apply 'vla-Delete (list target)))
         (if (vl-catch-all-error-p result)
           (prompt (strcat "
TEMP cleanup warning: could not delete layout " layoutname "."))
           (prompt (strcat "
Deleted leftover layout " layoutname "."))
         )
        )
      )
    )
  )
)

(defun abl:member-str-ci-p (value values / v found)
  ;; Case-insensitive string-list membership.
  (setq value (strcase (abl:str-trim value)))
  (foreach v values
    (if (= value (strcase (abl:str-trim v)))
      (setq found T)
    )
  )
  found
)

(defun abl:prefix-layout-p (layoutname prefix)
  ;; True for generated sheet layouts only, for example 13101.01, 13101.02, etc.
  ;; This prevents cleanup from touching unrelated layouts.
  (and layoutname
       prefix
       (wcmatch (strcase layoutname) (strcase (strcat prefix ".*"))))
)

(defun abl:all-digits-p (s / i ch ok)
  (setq s  (abl:str-trim s)
        i  1
        ok (/= s ""))
  (while (and ok (<= i (strlen s)))
    (setq ch (substr s i 1))
    (if (not (wcmatch ch "#"))
      (setq ok nil)
    )
    (setq i (1+ i))
  )
  ok
)

(defun abl:has-digit-p (s / i ch found)
  (setq s (abl:str-trim s)
        i 1)
  (while (and (null found) (<= i (strlen s)))
    (setq ch (substr s i 1))
    (if (wcmatch ch "#")
      (setq found T)
    )
    (setq i (1+ i))
  )
  found
)

(defun abl:generated-sheet-layout-p (layoutname / dot prefix suffix)
  ;; Generated sheet tabs are named like 10.01, 123.02, or 13101A.03.
  (setq layoutname (abl:str-trim layoutname)
        dot        (vl-string-search "." layoutname))
  (if dot
    (progn
      (setq prefix (substr layoutname 1 dot)
            suffix (substr layoutname (+ dot 2)))
      (and (/= (strcase layoutname) "MODEL")
           (abl:has-digit-p prefix)
           (>= (strlen suffix) 2)
           (abl:all-digits-p suffix))
    )
  )
)

(defun abl:delete-layout-safe (layoutname reason / target fallback result)
  ;; Deletes one paper-space layout safely. Never deletes Model or the last paper layout.
  (setq layoutname (abl:str-trim layoutname))
  (if (and (/= layoutname "")
           (/= (strcase layoutname) "MODEL")
           (abl:layout-exists-p layoutname))
    (cond
      ((<= (abl:paper-layout-count) 1)
       (prompt (strcat "\nCleanup skipped: cannot delete the only paper-space layout " layoutname ".")))
      (t
       (if (= (strcase (getvar 'CTAB)) (strcase layoutname))
         (progn
           (setq fallback (abl:first-paper-layout-not-named layoutname))
           (if fallback
             (setvar 'CTAB fallback)
             (setvar 'CTAB "Model")
           )
         )
       )
       (setq target (vla-Item (abl:layouts) layoutname))
       (setq result (vl-catch-all-apply 'vla-Delete (list target)))
       (if (vl-catch-all-error-p result)
         (prompt (strcat "\nCleanup warning: could not delete layout " layoutname "."))
         T
       )
      )
    )
  )
)

(defun abl:delete-layouts-without-current-views (prefix current-layouts / lay nm kill count)
  ;; After a run, only layouts created by this run should remain.
  ;; Example: if the current run creates 123.01-.03, this removes old
  ;; generated sheet tabs such as 10.01, 11.02, 19.03, or stale 123.04.
  (setq kill '())
  (vlax-for lay (abl:layouts)
    (setq nm (vla-get-Name lay))
    (if (and (abl:generated-sheet-layout-p nm)
             (not (abl:member-str-ci-p nm current-layouts)))
      (setq kill (cons nm kill))
    )
  )
  (setq count 0)
  (foreach nm kill
    (if (abl:delete-layout-safe nm "no corresponding marker/view in current run")
      (setq count (1+ count))
    )
  )
  count
)

(defun abl:delete-generated-layouts-before-run (prefix template / lay nm kill count)
  ;; Start each auto run from a clean generated-sheet set, but keep the layout
  ;; being used as the copy source.
  (setq template (strcase (abl:str-trim template))
        kill     '())
  (vlax-for lay (abl:layouts)
    (setq nm (vla-get-Name lay))
    (if (and (abl:generated-sheet-layout-p nm)
             (/= (strcase nm) template))
      (setq kill (cons nm kill))
    )
  )
  (setq count 0)
  (foreach nm kill
    (if (abl:delete-layout-safe nm "cleared before auto layout rebuild")
      (setq count (1+ count))
    )
  )
  count
)

(defun abl:layout-sort-key (name / dot suffix)
  (setq dot (vl-string-search "." name))
  (if dot
    (progn
      (setq suffix (substr name (+ dot 2)))
      (atoi suffix)
    )
    999999
  )
)

(defun abl:sort-layout-tabs (prefix / layouts lst idx pair nm)
  (setq layouts (abl:layouts)
        lst     '()
        idx     1)
  (vlax-for lay layouts
    (setq nm (vla-get-Name lay))
    (if (and (/= (strcase nm) "MODEL")
             (or (null prefix)
                 (wcmatch nm (strcat prefix ".*"))))
      (setq lst (cons (cons (vla-get-Name lay) lay) lst))
    )
  )
  (setq lst
        (vl-sort
          lst
          '(lambda (a b)
             (< (abl:layout-sort-key (car a))
                (abl:layout-sort-key (car b)))
           )
        )
  )
  (foreach pair lst
    (vla-put-TabOrder (cdr pair) idx)
    (setq idx (1+ idx))
  )
)

(defun abl:default-template-layout (/ found)
  (if (abl:layout-exists-p "temp")
    "temp"
    (abl:first-paper-layout)
  )
)

(defun abl:view-exists-p (name / res)
  (setq res (vl-catch-all-apply 'vla-Item (list (abl:views) name)))
  (not (vl-catch-all-error-p res))
)

(defun abl:safe-layout-name (name / bad i ch out)
  (setq name (abl:str-trim name)
        bad  "\\/:*?\"<>|"
        i    1
        out  "")
  (while (<= i (strlen name))
    (setq ch (substr name i 1))
    (if (vl-string-search ch bad)
      (setq out (strcat out "-"))
      (setq out (strcat out ch))
    )
    (setq i (1+ i))
  )
  (if (= out "") nil out)
)

(defun abl:dwg-prefix-fallback (/ name idx base)
  (setq name (vl-filename-base (getvar "DWGNAME"))
        idx  1
        base "")
  (while (and (<= idx (strlen name))
              (not (wcmatch (substr name idx 1) "[0-9]")))
    (setq idx (1+ idx))
  )
  (while (and (<= idx (strlen name))
              (wcmatch (substr name idx 1) "[0-9]"))
    (setq base (strcat base (substr name idx 1)))
    (setq idx (1+ idx))
  )
  (if (and (<= idx (strlen name))
           (wcmatch (substr name idx 1) "[A-Za-z]"))
    (setq base (strcat base (substr name idx 1)))
  )
  (if (/= base "") (strcase base))
)

(defun abl:get-layout-prefix (/)
  ;; Self-contained prefix finder.
  ;; Do NOT call external _GetDWGPrefix here; some workstations may not have that helper loaded,
  ;; which causes: Error: bad function: _GETDWGPREFIX.
  (abl:dwg-prefix-fallback)
)

(defun abl:normalize-sheet-suffix (suffix / s)
  (setq s (abl:str-trim suffix))
  (cond
    ((= s "") nil)
    ((= (substr s 1 1) ".") s)
    (t (strcat "." s))
  )
)

(defun abl:unique-name (base existsfn / idx name)
  (setq base (abl:safe-layout-name base))
  (if base
    (progn
      (setq name base
            idx  1)
      (while (apply existsfn (list name))
        (setq idx  (1+ idx)
              name (strcat base "-" (itoa idx)))
      )
      name
    )
  )
)

(defun abl:get-attributes (blk / attrs out att txt tag pos)
  (setq out '())
  (if (= :vlax-true (vla-get-HasAttributes blk))
    (progn
      (setq attrs (vlax-invoke blk 'GetAttributes))
      (foreach att attrs
        (setq txt (abl:str-trim (vla-get-TextString att))
              tag (strcase (abl:str-trim (vla-get-TagString att)))
              pos (abl:variant-point (vlax-get att 'InsertionPoint)))
        (if (/= txt "")
          (setq out (cons (list tag txt pos) out))
        )
      )
    )
  )
  out
)

(defun abl:get-dynamic-props (blk / props out prop name val)
  (setq out '())
  (setq props (vl-catch-all-apply 'vlax-invoke (list blk 'GetDynamicBlockProperties)))
  (if (not (vl-catch-all-error-p props))
    (foreach prop props
      (setq name (strcase (abl:str-trim (vlax-get prop 'PropertyName)))
            val  (vlax-get prop 'Value))
      (if (= (type val) 'VARIANT)
        (setq val (vlax-variant-value val))
      )
      (if val
        (setq out (cons (list name (abl:str-trim (vl-princ-to-string val))) out))
      )
    )
  )
  out
)

(defun abl:block-name (blk / name)
  (setq name (vl-catch-all-apply 'vla-get-EffectiveName (list blk)))
  (if (vl-catch-all-error-p name)
    (vla-get-Name blk)
    name
  )
)

(defun abl:member-ci-p (value values / v found)
  (setq value (strcase (abl:str-trim value)))
  (foreach v values
    (if (= value (strcase (abl:str-trim v)))
      (setq found T)
    )
  )
  found
)

(defun abl:marker-block-p (blk)
  (abl:member-ci-p (abl:block-name blk) *ABLMarkerBlockNames*)
)

(defun abl:ss-all-marker-blocks (/ ss idx ent blk out)
  (setq ss  (ssget "_X" '((0 . "INSERT")))
        idx 0
        out (ssadd))
  (if ss
    (while (< idx (sslength ss))
      (setq ent (ssname ss idx)
            blk (vlax-ename->vla-object ent))
      (if (abl:marker-block-p blk)
        (setq out (ssadd ent out))
      )
      (setq idx (1+ idx))
    )
  )
  (if (> (sslength out) 0) out nil)
)

(defun abl:attr-by-tag (attrs tag / found)
  (setq tag (strcase (abl:str-trim tag)))
  (foreach a attrs
    (if (= (car a) tag)
      (setq found (cadr a))
    )
  )
  found
)

(defun abl:bottom-left-attr (attrs minpt / best bestdist d)
  (foreach a attrs
    (setq d (abl:dist2d (caddr a) minpt))
    (if (or (null bestdist) (< d bestdist))
      (setq best     a
            bestdist d)
    )
  )
  (if best (cadr best))
)

(defun abl:read-all-text-inside (minpt maxpt / ss idx ent data txt out)
  (setq out '())
  (setq ss (ssget "_C"
                  (list (car minpt) (cadr minpt) 0.0)
                  (list (car maxpt) (cadr maxpt) 0.0)
                  '((0 . "TEXT,MTEXT"))))
  (if ss
    (progn
      (setq idx 0)
      (while (< idx (sslength ss))
        (setq ent  (ssname ss idx)
              data (entget ent)
              txt  (cdr (assoc 1 data)))
        (if txt
          (setq out (cons (abl:str-trim txt) out))
        )
        (setq idx (1+ idx))
      )
    )
  )
  out
)

(defun abl:scale-candidate-p (s)
  (and s
       (vl-string-search "=" s)
       (or (vl-string-search "'" s)
           (vl-string-search "XP" (strcase s))))
)

(defun abl:first-scale-text (attrs props texts / found p a txt n v)
  ;; Prefer dynamic block visibility/scale properties over random model text
  ;; inside the view window. This avoids 1-1/2" markers being overridden by
  ;; unrelated 1/2" notes inside the view area.
  (foreach p props
    (setq n (car p) v (cadr p))
    (if (and (null found)
             (abl:scale-candidate-p v)
             (or (vl-string-search "VISIBILITY" n)
                 (vl-string-search "SCALE" n)))
      (setq found v)
    )
  )
  (foreach a attrs
    (if (and (null found) (abl:scale-candidate-p (cadr a)))
      (setq found (cadr a))
    )
  )
  (foreach p props
    (if (and (null found) (abl:scale-candidate-p (cadr p)))
      (setq found (cadr p))
    )
  )
  (foreach txt texts
    (if (and (null found) (abl:scale-candidate-p txt))
      (setq found txt)
    )
  )
  found
)

(defun abl:strip-spaces (s / i ch out)
  (setq i 1 out "")
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (if (not (member ch '(" " "\t" "\r" "\n")))
      (setq out (strcat out ch))
    )
    (setq i (1+ i))
  )
  out
)

(defun abl:parse-number (s / dash slash whole frac)
  (setq s (abl:str-trim s))
  (setq dash  (vl-string-search "-" s)
        slash (vl-string-search "/" s))
  (cond
    ((and dash slash (< dash slash))
     (setq whole (atof (substr s 1 dash))
           frac  (substr s (+ dash 2)))
     (+ whole (abl:parse-number frac))
    )
    ((and slash (> slash 0))
     (/ (atof (substr s 1 slash))
        (atof (substr s (+ slash 2)))))
    (t (atof s))
  )
)

(defun abl:paper-inches-from-left-scale (left / q)
  ;; Left side can be architectural feet/inches too.
  ;; Example: 1'-0" = 1'-0" must resolve to 12 paper inches, not 1 paper inch.
  ;; Earlier logic only looked for the double quote and parsed the leading number,
  ;; which could make full scale read like 1" = 1'-0".
  (if (vl-string-search "'" left)
    (abl:model-inches-from-right-scale left)
    (progn
      (setq q (vl-string-search "\"" left))
      (if q
        (abl:parse-number (substr left 1 q))
        (abl:parse-number left)
      )
    )
  )
)

(defun abl:model-inches-from-right-scale (right / tick dash feet inches rest q)
  (setq feet 0.0 inches 0.0)
  (setq tick (vl-string-search "'" right))
  (if tick
    (progn
      (setq feet (atof (substr right 1 tick)))
      (setq rest (substr right (+ tick 2)))
      (setq dash (vl-string-search "-" rest))
      (if dash
        (setq rest (substr rest (+ dash 2)))
      )
      (setq q (vl-string-search "\"" rest))
      (if q
        (setq inches (abl:parse-number (substr rest 1 q)))
      )
      (+ (* feet 12.0) inches)
    )
    (progn
      (setq q (vl-string-search "\"" right))
      (if q
        (abl:parse-number (substr right 1 q))
        (atof right)
      )
    )
  )
)

(defun abl:scale-text-to-xp (txt / s eq left right paper model)
  (setq s  (abl:strip-spaces txt)
        eq (vl-string-search "=" s))
  (cond
    ((vl-string-search "XP" (strcase s))
     (atof s))
    (eq
     (setq left  (substr s 1 eq)
           right (substr s (+ eq 2))
           paper (abl:paper-inches-from-left-scale left)
           model (abl:model-inches-from-right-scale right))
     (if (and paper model (/= model 0.0))
       (/ paper model)
     ))
  )
)

(defun abl:known-scale (scale-text / s compact)
  ;; Normalize common viewport scale note text.
  ;; Important: check architectural foot marks BEFORE inch-only matches.
  ;; Otherwise 1'-0" = 1'-0" can be misread as 1" = 1'-0".
  (setq s (abl:str-trim scale-text))
  (setq compact (abl:strip-spaces s))
  (cond
    ((or (wcmatch compact "*1'-0\"=1'-0\"*")
         (wcmatch compact "*1'=1'*")
         (wcmatch compact "*1'-0=1'-0*"))
     "1'-0\" = 1'-0\"")
    ((wcmatch s "*1/128*") "1/128\" = 1'-0\"")
    ((wcmatch s "*1/64*")  "1/64\" = 1'-0\"")
    ((wcmatch s "*1/32*")  "1/32\" = 1'-0\"")
    ((wcmatch s "*1/16*")  "1/16\" = 1'-0\"")
    ((wcmatch s "*3/32*")  "3/32\" = 1'-0\"")
    ((wcmatch s "*1/8*")   "1/8\" = 1'-0\"")
    ((wcmatch s "*3/16*")  "3/16\" = 1'-0\"")
    ((wcmatch s "*1/4*")   "1/4\" = 1'-0\"")
    ((wcmatch s "*3/8*")   "3/8\" = 1'-0\"")
    ;; Check 1-1/2 before 1/2. Otherwise AutoCAD reads 1-1/2" as 1/2".
    ((or (wcmatch compact "*1-1/2\"=1'-0\"*")
         (wcmatch compact "*11/2\"=1'-0\"*")
         (wcmatch compact "*1.5\"=1'-0\"*")
         (wcmatch s "*1-1/2*"))
     "1-1/2\" = 1'-0\"")
    ((wcmatch s "*1/2*")   "1/2\" = 1'-0\"")
    ((wcmatch s "*3/4*")   "3/4\" = 1'-0\"")
    ((wcmatch s "*3\"*")   "3\" = 1'-0\"")
    ((wcmatch s "*6\"*")   "6\" = 1'-0\"")
    ((wcmatch s "*1\"*")   "1\" = 1'-0\"")
    (t s)
  )
)

(defun abl:set-cannoscale (scale-text / oldscale result)
  ;; Sets the active annotation scale only when a valid scale name is supplied.
  ;; Returns the previous CANNOSCALE so callers can restore it.
  (setq oldscale (getvar 'CANNOSCALE))
  (setq scale-text (abl:known-scale scale-text))
  (if (and scale-text (/= (abl:str-trim scale-text) ""))
    (progn
      (setq result (vl-catch-all-apply 'setvar (list 'CANNOSCALE scale-text)))
      (if (and (vl-catch-all-error-p result)
               (not (abl:member-str-ci-p scale-text *ABLWarnedCannoScales*)))
        (progn
          (setq *ABLWarnedCannoScales* (cons scale-text *ABLWarnedCannoScales*))
          (prompt (strcat "
Warning: could not set CANNOSCALE to " scale-text "."))
        )
      )
    )
  )
  oldscale
)

(defun abl:restore-var (var value)
  ;; Safe restore for sysvars touched by this routine.
  (if value
    (vl-catch-all-apply 'setvar (list var value))
  )
)

(defun abl:add-view (name minpt maxpt / view)
  (if (abl:view-exists-p name)
    (vla-Delete (vla-Item (abl:views) name))
  )
  (setq view (vla-Add (abl:views) name))
  (vla-put-Center view (abl:point2d-variant (abl:center2d minpt maxpt)))
  (vla-put-Width view (abl:width minpt maxpt))
  (vla-put-Height view (abl:height minpt maxpt))
  view
)

(defun abl:generated-view-p (viewname / prefix sheet)
  (setq viewname (abl:str-trim viewname)
        prefix   "VIEW-")
  (if (and (>= (strlen viewname) (strlen prefix))
           (= (strcase (substr viewname 1 (strlen prefix))) prefix))
    (progn
      (setq sheet (substr viewname (1+ (strlen prefix))))
      (abl:generated-sheet-layout-p sheet)
    )
  )
)

(defun abl:delete-stale-generated-views (current-views / view nm kill count result)
  (setq kill '())
  (vlax-for view (abl:views)
    (setq nm (vla-get-Name view))
    (if (and (abl:generated-view-p nm)
             (not (abl:member-str-ci-p nm current-views)))
      (setq kill (cons nm kill))
    )
  )
  (setq count 0)
  (foreach nm kill
    (setq result (vl-catch-all-apply 'vla-Delete (list (vla-Item (abl:views) nm))))
    (if (vl-catch-all-error-p result)
      (prompt (strcat "\nCleanup warning: could not delete saved view " nm "."))
      (setq count (1+ count))
    )
  )
  count
)

(defun abl:copy-layout (template newname / before after)
  ;; Use AutoCAD's layout-copy command so title blocks, page setup, viewports,
  ;; and paper-space state match the proven L2 workflow.
  (setq before (abl:layout-exists-p newname))
  (if before
    nil
    (progn
      (command-s "_.-LAYOUT" "_Copy" template newname)
      (setq after (abl:layout-exists-p newname))
      (if after
        (vla-Item (abl:layouts) newname)
        nil
      )
    )
  )
)

(defun abl:largest-viewport-in-layout (layoutname / lay blk best bestarea area)
  (setq lay (vla-Item (abl:layouts) layoutname)
        blk (vla-get-Block lay))
  (vlax-for obj blk
    (if (= (vla-get-ObjectName obj) "AcDbViewport")
      (progn
        (setq area (* (vla-get-Width obj) (vla-get-Height obj)))
        (if (or (null bestarea) (> area bestarea))
          (setq best     obj
                bestarea area)
        )
      )
    )
  )
  best
)

(defun abl:set-layout-viewports-locked (layoutname locked / lay blk obj val count result)
  (setq lay (vla-Item (abl:layouts) layoutname)
        blk (vla-get-Block lay)
        val (if locked :vlax-true :vlax-false)
        count 0)
  (vlax-for obj blk
    (if (vlax-property-available-p obj 'DisplayLocked)
      (progn
        (setq result (vl-catch-all-apply 'vla-put-DisplayLocked (list obj val)))
        (if (not (vl-catch-all-error-p result))
          (setq count (1+ count))
        )
        (vl-catch-all-apply 'vla-Update (list obj))
      )
    )
  )
  count
)

(defun abl:set-viewport-view (layoutname minpt maxpt xp scale-text / doc lay vp cen viewheight viewratio borderratio targetwidth targetheight)
  (setq doc (abl:doc)
        lay (vla-Item (abl:layouts) layoutname)
        vp  (abl:largest-viewport-in-layout layoutname))
  (if vp
    (progn
      (vla-put-ActiveLayout doc lay)
      (abl:set-cannoscale scale-text)
      (vl-catch-all-apply 'vla-put-ViewportOn (list vp :vlax-true))
      (vl-catch-all-apply 'vla-put-DisplayLocked (list vp :vlax-false))
      (setq cen          (append (abl:center2d minpt maxpt) (list 0.0))
            viewratio    (/ (vla-get-Width vp) (vla-get-Height vp))
            targetwidth  (abl:width minpt maxpt)
            targetheight (abl:height minpt maxpt)
            borderratio  (/ targetwidth targetheight)
            viewheight   (if (> borderratio viewratio)
                           (/ targetwidth viewratio)
                           targetheight))
      (vl-catch-all-apply 'vlax-put-property (list vp 'Target (abl:point3d-variant cen)))
      (vl-catch-all-apply 'vlax-put-property (list vp 'Direction (abl:point3d-variant '(0.0 0.0 1.0))))
      (vl-catch-all-apply 'vlax-put-property (list vp 'ViewHeight viewheight))
      (vla-put-CustomScale vp xp)
      ;; Keep the viewport's annotation context aligned with its plotted scale.
      (abl:set-cannoscale scale-text)
      (abl:set-layout-viewports-locked layoutname T)
    )
    nil
  )
)

(defun abl:restore-view-command (layoutname viewname xp scale-text / ok locked)
  ;; Same general behavior as L2/Auto Layout Creator: switch to copied layout,
  ;; enter the template viewport, restore the named view, then apply XP scale.
  (setq ok T)
  (command-s "_.LAYOUT" "_Set" layoutname)
  (command-s "_.REGENALL")
  (abl:set-layout-viewports-locked layoutname nil)
  (setq ok (not (vl-catch-all-error-p (vl-catch-all-apply 'command-s (list "_.MSPACE")))))
  (if (= 1 (getvar 'CVPORT))
    (setq ok nil)
  )
  (if ok
    (progn
      ;; Set annotation scale while inside the viewport, restore the view,
      ;; then set it again after the XP zoom. This prevents AutoCAD from
      ;; leaving the viewport at a stale annotation scale such as 1" = 1'-0".
      (abl:set-cannoscale scale-text)
      (command-s "_.-VIEW" "_Restore" viewname)
      (command-s "_.ZOOM" "_Scale" (strcat (rtos xp 2 8) "XP"))
      (abl:set-cannoscale scale-text)
      (command-s "_.PSPACE")
      (setq locked (abl:set-layout-viewports-locked layoutname T))
      locked
    )
    (progn
      (abl:set-layout-viewports-locked layoutname T)
      nil
    )
  )
)

(defun abl:sort-blocks-left-to-right (items)
  (vl-sort
    items
    '(lambda (a b)
       (< (car (cadr a)) (car (cadr b)))
     )
  )
)


(defun abl:sheet-suffix-from-number (n / s)
  ;; 1 -> .01, 2 -> .02, 10 -> .10
  (setq s (itoa n))
  (while (< (strlen s) 2)
    (setq s (strcat "0" s))
  )
  (strcat "." s)
)

(defun abl:set-attr-by-tag (blk tag val / att found)
  ;; Updates matching attribute tag on a block insert. Returns T if found.
  (setq found nil
        tag   (strcase (abl:str-trim tag)))
  (if (= :vlax-true (vla-get-HasAttributes blk))
    (foreach att (vlax-invoke blk 'GetAttributes)
      (if (= (strcase (abl:str-trim (vla-get-TagString att))) tag)
        (progn
          (vla-put-TextString att val)
          (setq found T)
        )
      )
    )
  )
  (if found (vla-update blk))
  found
)

(defun abl:effective-layout-name-for-index (prefix n)
  (abl:safe-layout-name (strcat prefix (abl:sheet-suffix-from-number n)))
)


(defun abl:build-layouts-from-selection (ss / *error* doc oldcmdecho oldregenmode oldctab oldcannoscale oldannoautoscale template defaulttemplate prefix tag defaultscale idx ent blk bounds minpt maxpt attrs props texts scale-text xp viewname items made updated skipped item lname suffix current-layouts current-views deleted-layouts deleted-views locked-viewports lockres)
  (defun *error* (msg)
    (abl:restore-var 'CTAB oldctab)
    (abl:restore-var 'CANNOSCALE oldcannoscale)
    (abl:restore-var 'ANNOAUTOSCALE oldannoautoscale)
    (abl:restore-var 'REGENMODE oldregenmode)
    (abl:restore-var 'CMDECHO oldcmdecho)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*")))
      (prompt (strcat "\nError: " msg))
    )
    (princ)
  )

  (setq doc        (abl:doc)
        oldctab    (getvar 'CTAB)
        oldcmdecho (getvar 'CMDECHO)
        oldregenmode (getvar 'REGENMODE)
        oldcannoscale (getvar 'CANNOSCALE)
        oldannoautoscale (getvar 'ANNOAUTOSCALE)
        made       0
        updated    0
        skipped    0
        current-layouts '()
        current-views '()
        deleted-layouts 0
        deleted-views 0
        locked-viewports 0)
  (setq *ABLWarnedCannoScales* nil)

  (setvar 'CMDECHO 0)
  (setvar 'REGENMODE 0)
  ;; Prevent AutoCAD from automatically adding/changing annotative scales
  ;; while the routine is creating/restoring viewports.
  (setvar 'ANNOAUTOSCALE 0)

  (setq defaulttemplate (abl:first-paper-layout))
  (if (null defaulttemplate)
    (setq defaulttemplate oldctab)
  )
  (setq template defaulttemplate)

  (if (or (= (strcase template) "MODEL")
          (not (abl:layout-exists-p template)))
    (progn
      (prompt (strcat "\nTemplate layout must be an existing paper-space layout: " template))
      (abl:restore-var 'ANNOAUTOSCALE oldannoautoscale)
      (abl:restore-var 'CANNOSCALE oldcannoscale)
      (abl:restore-var 'REGENMODE oldregenmode)
      (abl:restore-var 'CMDECHO oldcmdecho)
      (princ)
      (exit)
    )
  )

  (setq prefix (abl:get-layout-prefix))
  (if (null prefix)
    (setq prefix (getstring T "\nLayout prefix was not found. Enter prefix: "))
  )
  (if (= (abl:str-trim prefix) "")
    (progn
      (prompt "\nNo layout prefix found. No changes made.")
      (abl:restore-var 'ANNOAUTOSCALE oldannoautoscale)
      (abl:restore-var 'CANNOSCALE oldcannoscale)
      (abl:restore-var 'REGENMODE oldregenmode)
      (abl:restore-var 'CMDECHO oldcmdecho)
      (princ)
      (exit)
    )
  )

  (setq tag "SHEET")
  (setq defaultscale "1/2\" = 1'-0\"")

  (prompt (strcat "\nTemplate layout: " template))
  (prompt (strcat "\nLayout prefix: " prefix))
  (prompt "\nAuto-numbering SHEET attributes from .01 every run. No prompt.")
  (prompt (strcat "\nFallback scale: " defaultscale))

  (if ss
    (progn
      ;; First pass: collect valid marker blocks and view/scale data only.
      ;; SHEET attribute is intentionally ignored here, because the routine
      ;; renames it after left-to-right sorting so numbering always starts .01.
      (setq idx 0
            items '())
      (while (< idx (sslength ss))
        (setq ent    (ssname ss idx)
              blk    (vlax-ename->vla-object ent)
              bounds (abl:view-border-bounds blk))
        (if (null bounds)
          (progn
            (setq bounds (abl:bbox blk))
            (prompt "\nWarning: cyan/outer closed view border not found; using full block extents.")
          )
        )
        (setq minpt  (car bounds)
              maxpt  (cadr bounds)
              attrs  (abl:get-attributes blk)
              props  (abl:get-dynamic-props blk)
              texts  (abl:read-all-text-inside minpt maxpt)
              scale-text (abl:first-scale-text attrs props texts))
        (if (null scale-text)
          (setq scale-text defaultscale)
        )
        (setq scale-text (abl:known-scale scale-text))
        (setq xp (abl:scale-text-to-xp scale-text))
        (if (and xp (> xp 0.0))
          (setq items (cons (list blk minpt maxpt scale-text xp) items))
          (progn
            (setq skipped (1+ skipped))
            (prompt "\nSkipped one block: readable viewport scale not found.")
          )
        )
        (setq idx (1+ idx))
      )

      ;; Second pass: sort markers and assign sheet/layout names .01, .02, .03...
      (if items
        (setq deleted-layouts (+ deleted-layouts (abl:delete-generated-layouts-before-run prefix template)))
      )
      (setq items (abl:sort-blocks-left-to-right items))
      (setq idx 1)
      (foreach item items
        (setq blk        (car item)
              minpt      (cadr item)
              maxpt      (caddr item)
              scale-text (nth 3 item)
              xp         (nth 4 item)
              suffix     (abl:sheet-suffix-from-number idx)
              lname      (abl:effective-layout-name-for-index prefix idx)
              viewname   (strcat "VIEW-" lname))
        (if lname
          (setq current-layouts (cons lname current-layouts))
        )
        (if viewname
          (setq current-views (cons viewname current-views))
        )

        ;; Change SHEET attribute FIRST, then create/update layout/view from that name.
        (if (not (abl:set-attr-by-tag blk tag suffix))
          (prompt (strcat "\nWarning: block " (abl:block-name blk) " has no SHEET attribute to update."))
        )

        (if (and lname viewname)
          (progn
            (abl:add-view viewname minpt maxpt)
            (if (abl:layout-exists-p lname)
              (progn
                ;; Existing layout: do not skip. Refresh the viewport/view so reruns stay current.
                (setq lockres (abl:restore-view-command lname viewname xp scale-text))
                (if (not lockres)
                  (prompt (strcat "\nWarning: could not refresh view on existing layout " lname "."))
                  (setq locked-viewports (+ locked-viewports lockres))
                )
                (setq updated (1+ updated))
              )
              (progn
                (if (abl:copy-layout template lname)
                  (progn
                    (setq lockres (abl:restore-view-command lname viewname xp scale-text))
                    (if (not lockres)
                      (prompt (strcat "\nWarning: could not restore view on layout " lname "."))
                      (setq locked-viewports (+ locked-viewports lockres))
                    )
                    (setq made (1+ made))
                  )
                  (progn
                    (setq skipped (1+ skipped))
                    (prompt (strcat "\nWarning: layout copy failed for " lname "."))
                  )
                )
              )
            )
          )
          (setq skipped (1+ skipped))
        )
        (setq idx (1+ idx))
      )

      (if (and oldctab (abl:layout-exists-p oldctab))
        (setvar 'CTAB oldctab)
        (setvar 'CTAB (or (abl:first-paper-layout) "Model"))
      )
      (abl:sort-layout-tabs prefix)
      ;; Remove previously generated layouts for this DWG prefix that no longer
      ;; have a corresponding marker/view from the current run.
      (setq deleted-layouts (+ deleted-layouts (abl:delete-layouts-without-current-views prefix current-layouts)))
      (setq deleted-views (abl:delete-stale-generated-views current-views))
      ;; Auto-remove the leftover template tab named TEMP after sheets are built.
      (abl:delete-layout-named "TEMP")
      (prompt (strcat "
Done. Created " (itoa made)
" layout(s). Updated " (itoa updated)
" existing layout(s). Deleted " (itoa deleted-layouts)
" old layout(s). Deleted " (itoa deleted-views)
" old saved view(s). Locked " (itoa locked-viewports)
" viewport(s). Skipped " (itoa skipped) "."))
    )
    (prompt "\nNothing selected.")
  )

  (if (and oldctab (abl:layout-exists-p oldctab))
    (abl:restore-var 'CTAB oldctab)
    (if (abl:first-paper-layout)
      (abl:restore-var 'CTAB (abl:first-paper-layout))
    )
  )
  (abl:restore-var 'CANNOSCALE oldcannoscale)
  (abl:restore-var 'ANNOAUTOSCALE oldannoautoscale)
  (abl:restore-var 'REGENMODE oldregenmode)
  (abl:restore-var 'CMDECHO oldcmdecho)
  (princ)
)

(defun c:ABLOCKLAYOUTS (/ ss)
  (prompt "\nSelect the layout marker blocks to process.")
  (setq ss (ssget '((0 . "INSERT"))))
  (abl:build-layouts-from-selection ss)
)

(defun c:ABLOCKLAYOUTSALL (/ ss)
  (prompt (strcat "\nFinding all marker blocks in drawing: " (vl-princ-to-string *ABLMarkerBlockNames*) "."))
  (setq ss (abl:ss-all-marker-blocks))
  (if ss
    (progn
      (prompt (strcat "\nFound " (itoa (sslength ss)) " marker block(s)."))
      (abl:build-layouts-from-selection ss)
    )
    (prompt (strcat "\nNo marker blocks found. Checked: " (vl-princ-to-string *ABLMarkerBlockNames*) "."))
  )
  (princ)
)

(princ "\nLoaded AutoBlockLayouts_Combined_SheetSync.lsp - auto SHEET .01 numbering + stale layout cleanup + TEMP cleanup + annotation-scale safe. Run ABLOCKLAYOUTS or ABLOCKLAYOUTSALL.")
(princ)
