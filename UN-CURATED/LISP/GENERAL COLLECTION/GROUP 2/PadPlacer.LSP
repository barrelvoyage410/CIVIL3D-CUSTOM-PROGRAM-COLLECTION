;;;-----------------------------------------------------------------------;;;
;;;                   Original lisp provided by Mark Evinger              ;;;
;;;                                                                       ;;;
;;;-----------------------------------------------------------------------;;;
;;;                             Copyright 2007                            ;;;
;;;-----------------------------------------------------------------------;;;
;;;                                                                       ;;;
;;;                                                                       ;;;
;;;                                                                       ;;;
;;; PadPlacer.lsp - sets building pad blocks on lots between side yard    ;;;
;;; lines and at a set distance from the building setback line.  Uses 3   ;;;
;;; picks to get Left, Right and Setback line.                            ;;;
;;;                                                                       ;;;
;;;                                                                       ;;;
;;;-----------------------------------------------------------------------;;;

;;------------------------------------------------
;; Listing 3:  Application Specific Entmake Function
;;------------------------------------------------
(defun DEF_PadPlacer_BLOCK ()
  (setq Pad1 (list (- 0.0 (/ PadWidth 2)) 0 0))
  (setq Pad2 (list (- 0.0 (/ PadWidth 2)) PadDepth 0))
  (setq Pad3 (list (/ PadWidth 2) PadDepth 0))
  (setq Pad4 (list (/ PadWidth 2) 0 0))
  (setq Pad5 (inters Pad1 Pad3 Pad2 Pad4 nil))
;;;   (entmake
;;;     '((0 . "BLOCK") (8 . "0") (70 . 2)
;;;       (2 . "BLOCKNAME") (10 0.0 0.0 0.0)))
   (entmake (list (cons 0 "BLOCK") (cons 8 "0") (cons 70 2) (cons 2 PadPlacer_BlockName) (list '10 '0.0 '0.0 '0.0)));create block name/insert point
   (entmake (list (cons 0 "LINE") (cons 8 "0") (cons 10 Pad1) (cons 11 Pad2))) ; line Pad1>Pad2
   (entmake (list (cons 0 "LINE") (cons 8 "0") (cons 10 Pad2) (cons 11 Pad3))) ; line Pad2>Pad3
   (entmake (list (cons 0 "LINE") (cons 8 "0") (cons 10 Pad3) (cons 11 Pad4))) ; line Pad3>Pad4
   (entmake (list (cons 0 "LINE") (cons 8 "0") (cons 10 Pad4) (cons 11 Pad1))) ; line Pad4>Pad1
   (entmake (list (cons 0 "LINE") (cons 8 "0") (cons 10 Pad1) (cons 11 Pad3))) ; line Pad1>Pad3 diagonal
   (entmake (list (cons 0 "LINE") (cons 8 "0") (cons 10 Pad2) (cons 11 Pad4))) ; line Pad2>Pad4 diagonal
   (entmake (list (cons 0 "LINE") (cons 8 "0") (cons 10 Pad5) (cons 11 (list 0.0 0.0 0.0)))) ; line Pad5>0,0,0
   (entmake '((0 . "ENDBLK") (8 . "0")));close out the block
);end defun


;;By TonyT from the adesk newsgroups
; (vector-side <origin> <direction> <point> ) 
  ; 
  ; Returns an integer code indicating position of <point> 
  ; in relation to the directed vector whose endpoints are 
  ; <origin> and <direction>. 
  ; 
  ;   Result  Meaning 
  ;   --------------------------------------------------------- 
  ;    -1     Point is to the right of vector. 
  ;     0     Point is on (colinear with) vector 
  ;     1     Point is to the left of vector. 


(defun vector-side (v1 v2 p / r) 
     (setq r (- (* (- (car v2) 
                      (car v1) 
                   ) 
                   (- (cadr p) 
                      (cadr v1) 
                   ) 
                ) 
                (* (- (cadr v2) 
                      (cadr v1) 
                   ) 
                   (- (car p) 
                      (car v1) 
                   ) 
                ) 
             ) 
     ) 
     (cond ((fzerop r) 0) 
           (t (fix (/ (abs r) r))) 
     ) 
) 


(defun fzerop (n) 
     (equal n 0.0 *fuzz*) 
) 


(if (not *fuzz*) 
     (setq *fuzz* 1e-10) 
) 

;;;Are Points CoLinear
;;;ARG -> 3 Points and Fuzz
;;;RET -> T nil
(defun is_pt_colinear (p1 p2 p3 fz)
  (or (equal (distance p1 p3)
             (+ (distance p1 p2)
                (distance p2 p3))
             fz)
      (equal (distance p1 p2)
             (+ (distance p1 p3)
                (distance p3 p2))
             fz)
      (equal (distance p2 p3)
             (+ (distance p1 p2)
                (distance p1 p3))
             fz)))


;; Math by John Uhden
;; Argument: the list returned by entsel, nentsel or nentselp.
;; Returns: the radius point of an arc in a lightweight or
;;          heavy polyline in UCS coordinates.
;; Example: (setq pt (PlineRadPt (entsel "Select arc in pline: ")))
(defun
      PlineRadPt
                (elst / obj typ pt param bulge p1 p2 arcpt crdpt chord arcang rad
                 radpt)
  (and
    (setq obj (vlax-ename->vla-object (car elst)))
    (setq typ (vlax-get obj 'ObjectName))
    (or
      (= typ "AcDbPolyline") ;lightweight pline
      (= typ "AcDb2dPolyline") ;heavy pline
      )
    (setq pt (vlax-curve-getClosestPointTo obj (trans (cadr elst) 1 0)))
    (setq param (fix (vlax-curve-getParamAtPoint obj pt)))
    (setq bulge (vlax-invoke obj 'GetBulge param))
    (not (zerop bulge))
    (setq p1 (vlax-curve-getPointAtParam obj param))
    (setq p2 (vlax-curve-getPointAtParam obj (1+ param)))
    (setq arcpt (vlax-curve-getPointAtParam obj (+ param 0.5)))
    (setq crdpt (mapcar '* '(0.5 0.5 0.5) (mapcar '+ p1 p2)))
    (setq chord (distance p1 p2))
    (setq arcang (* 4 (atan (abs bulge))))
    (setq rad (abs (/ chord 2 (sin (/ arcang 2)))))
    (setq radpt (trans (polar arcpt (angle arcpt crdpt) rad) 0 1))
    )
  radpt
  ) ;end

 ;---------------------------------------------------------------------------------------------------------------------------
 ;by John Uhden 
 ;example: (SegmentPts (entsel)) or (SegmentPts (nentselp pt)) 
 ;returns a list of two 3D points 
 ;for lines and arcs, the start and end points 
 ;for plines, the first and second verticies of the segment picked 

 ;---------------------------------------------------------------------------------------------------------------------------
(defun
      SegmentPts
                (ent / e pnt vobj Name param1 param2 p1 p2 SegPts)
  (vl-load-com)
  (and
    (setq e (car ent))
    (= (type e) 'ENAME)
    (setq pnt (cadr ent))
    (listp pnt)
    (not (atom (cdr pnt)))
    (vl-every (function (lambda (x) (= (type x) 'REAL))) pnt)
    (setq vobj (vlax-ename->vla-object (car ent)))
    (setq pnt (trans (cadr ent) 1 0))
    (setq pnt (vlax-curve-getClosestPointTo vobj pnt))
    (setq Name (vla-get-ObjectName vobj))
    (cond
      ((vl-position Name '("AcDbArc" "AcDbLine"))
       (setq p1 (vlax-curve-getStartPoint vobj))
       (setq p2 (vlax-curve-getEndPoint vobj))
       )
      ((wcmatch (strcase Name) "*POLYLINE")
       (setq param1 (vlax-curve-getParamAtPoint vobj pnt))
       (setq param1 (fix param1))
       (setq param2 (1+ param1))
       (if (equal param1 (vlax-curve-getStartParam vobj) 1e-10)
         (setq p1 (vlax-curve-getStartPoint vobj))
         (setq p1 (vlax-curve-getPointAtParam vobj param1))
         )
       (if (equal param2 (vlax-curve-getEndParam vobj) 1e-10)
         (setq p2 (vlax-curve-getEndPoint vobj))
         (setq p2 (vlax-curve-getPointAtParam vobj param2))
         )
       ) ;pline cond 
      (T
       (prompt (strcat "\nHaven't figured out a(n) " Name " yet.")))
      ) ;conditions 
    p1
    p2
    (setq SegPts (list p1 p2))
    ) ;and 
  SegPts
  ) ;end 
 ;---------------------------------------------------------------------------------------------------------------------------
 ;---------------------------------------------------------------------------
 ; save specified mode settings
 ;---------------------------------------------------------------------------
  (defun MODESAVE (A_A)
    (setq MLST '())
    (repeat (length A_A)
      (setq
        MLST (append MLST (list (list (car A_A) (getvar (car A_A)))))
      ) ;_ end setq
      (setq A_A (cdr A_A))
    ) ;_ end repeat
  ) ;_ end defun
 ;---------------------------------------------------------------------------
 ; restore saved mode settings
 ;---------------------------------------------------------------------------
  (defun MODER ()
    (repeat (length MLST)
      (setvar (caar MLST) (cadar MLST))
      (setq MLST (cdr MLST))
    ) ;_ end repeat
  ) ;_ end defun
 ;---------------------------------------------------------------------------
 ;error handler
 ;---------------------------------------------------------------------------
  (defun PadPlacer_ERR (S) ;If an error (such as CTRL-C) occurs
    (if (/= S "Function cancelled") ;while this command is active...
      (princ (strcat "\nError: " S))
    ) ;_ end if
    (setq *ERROR* OLD_ERR) ;Restore old *error* handler
    (MODER) ;return modes to old values
    (princ)
  ) ;_ end defun
 ;---------------------------------------------------------------------------


(defun c:PadPlacer ()
(setq CURRENT_LAYER (getvar "CLAYER"))
(setq CGL_OSMODE (getvar "OSMODE"))
(setq OLD_ERR *ERROR* ; use special error handling function
      *ERROR* PadPlacer_ERR
      ) ;_ end setq

(MODESAVE '("BLIPMODE" "CMDECHO" "COORDS" "DIMZIN" "GRIPS" "OSMODE" "AUNITS" "CELTYPE" "PICKBOX")
          ) ;save modes
(setvar "BLIPMODE" 0) ;turn off blips
(setvar "CMDECHO" 0) ;turn off command echo
(setvar "COORDS" 2) ;sets coords to show distance & angle
(setvar "DIMZIN" 0) ;turn off dimzin (controls leading/trailing 0's)
(setvar "GRIPS" 0) ;turn off grips
(setvar "OSMODE" 0) ;turn off osnaps
(setvar "AUNITS" 0) ;set angular units to degrees only
(setvar "CELTYPE" "BYLAYER") ; set CurrentEntity Linetype to bylayer
(setvar "PICKBOX" 10) ;set pickbox bigger for easier use

(if (null iPadW); set up pad width
  (setq iPadW 45)
  );end if
(initget (+ 4))
(setq PadWidth (getreal (strcat "\n Please enter the pad Width in feet: << "(rtos iPadW 2 2)" >> ")))
(if (not (null PadWidth))
  (setq  iPadW PadWidth)
  (setq PadWidth iPadw)
  ) ;_ end of if

(if (null iPadD); set up pad depth
  (setq iPadD 55)
  );end if
(initget (+ 4))
(setq PadDepth (getreal (strcat "\n Please enter the pad Depth in feet: << "(rtos iPadD 2 2)" >> ")))
(if (not (null PadDepth))
  (setq iPadD PadDepth)
  (setq PadDepth iPadD)
  ) ;_ end of if

(setq PadPlacer_BlockName (strcat "Pad_" (rtos PadWidth 2 2) "x" (rtos PadDepth 2 2)))
(if (null (tblsearch "BLOCK" PadPlacer_BlockName))
  (DEF_PadPlacer_BLOCK)
  (princ (strcat "\t\tBlock " (chr 34) PadPlacer_BlockName (chr 34) " already exists in the drawing."))
  );returns new pad block of PAD_WidthXDepth

(print)
(if (null iOffset); set up offset from setback line
  (setq iOffset 0.5)
  );end if
(initget (+ 4))
(setq SBLINE-OFFSET
       (getreal (strcat "\n Enter desired distance BEHIND the setback line: <<"(rtos iOffset 2 2)">> ") ;_ end of strcat
         ) ;_ end of getreal
      ) ;_ end of setq
(if (not (null SBLINE-OFFSET))
  (setq iOffset SBLINE-OFFSET)
  (setq SBLINE-OFFSET iOffset)
  ) ;_ end of if

  (while
    (setq SY1 (entsel "\n Select LEFT side yard line.") ;_ end of entsel
      ) ;_ end of setq
     (while
       (and (/= (cdr (assoc 0 (entget (car sy1)))) "LINE")
            (/= (cdr (assoc 0 (entget (car sy1)))) "ARC")
            (/= (cdr (assoc 0 (entget (car sy1)))) "POLYLINE")
            (/= (cdr (assoc 0 (entget (car sy1)))) "LWPOLYLINE")
            ) ;_ end of and

        (prompt
          "\n Selection was not a LINE or ARC or POLYLINE! Try again!!"
          ) ;_ end of prompt
        (print)
        (setq SY1
               (entsel
                 "\n Select LEFT side yard line."
                 ) ;_ end of entsel
              ) ;_ end of setq
        ) ;_ end of while
     (setq sy1BCLST (entget (car SY1)))
     (setq sy1ENTTYP (cdr (assoc 0 Sy1BCLST)))
     (setq sy1PICKPT (cadr SY1))
    (princ)


(setq SY2 (entsel "\n Select RIGHT side yard line.") ;_ end of entsel
      ) ;_ end of setq
     (while
       (and (/= (cdr (assoc 0 (entget (car SY2)))) "LINE")
            (/= (cdr (assoc 0 (entget (car SY2)))) "ARC")
            (/= (cdr (assoc 0 (entget (car SY2)))) "POLYLINE")
            (/= (cdr (assoc 0 (entget (car SY2)))) "LWPOLYLINE")
            ) ;_ end of and

        (prompt
          "\n Selection was not a LINE or ARC or POLYLINE! Try again!!"
          ) ;_ end of prompt
        (print)
        (setq SY2
               (entsel
                 "\n Select RIGHT side yard line."
                 ) ;_ end of entsel
              ) ;_ end of setq
        ) ;_ end of while
     (setq sy2BCLST (entget (car SY2)))
     (setq sy2ENTTYP (cdr (assoc 0 Sy2BCLST)))
     (setq sy2PICKPT (cadr SY2))
    (princ)

;; should have SY1 and SY2

        (setq SY1PT1 (car (SegmentPts SY1))) ;segment start point
        (setq SY1PT2 (cadr (SegmentPts SY1))) ;segment end point

        (setq SY2PT1 (car (SegmentPts SY2))) ;segment start point
        (setq SY2PT2 (cadr (SegmentPts SY2))) ;segment end point


    
     
        ;(setq Sy1PT1 (cdr (assoc 10 SY1BCLST)))
        ;(setq Sy1PT2 (cdr (assoc 11 SY1BCLST)))
;;;        (if (< (distance PICKPT sy1PT1) (distance PICKPT sy1PT2))
;;;          (setq sy1INSPT PT1)
;;;          (setq sy1INSPT PT2)
;;;          ) ;_ end of if

        ;(setq Sy2PT1 (cdr (assoc 10 SY2BCLST)))
        ;(setq Sy2PT2 (cdr (assoc 11 SY2BCLST)))
;;;        (if (< (distance PICKPT sy2PT1) (distance PICKPT sy2PT2))
;;;          (setq sy2INSPT PT1)
;;;          (setq sy2INSPT PT2)
;;;          ) ;_ end of if




(setq SBLINE (entsel "\n Select building setback line: "))
    (if (= SBLINE nil)
      (progn
      (princ "\n Please pick the setback line!")
      (setq SBLINE (entsel "\n Select building setback line: "))
      );end progn
      (progn
    (setq SBLINEPT1 (car (SegmentPts SBLINE)))
    (setq SBLINEPT2 (car (SegmentPts SBLINE)))

    (setq BCLST (entget (car SBLINE)))
    (setq ENTTYP (cdr (assoc 0 BCLST)))
    (setq PICKPT (cadr SBLINE))
   
    (COND
      ((= ENTTYP "LINE")
        (setq SBLINEPT1 (cdr (assoc 10 BCLST)))
        (setq SBLINEPT2 (cdr (assoc 11 BCLST)))       
       )
      
       ((= ENTTYP "ARC")
        (setq SBLINE-CENPT (cdr (assoc 10 BCLST)))
        (setq SBLINEPT1 (polar SBLINE-CENPT (cdr (assoc 50 BCLST)) (cdr (assoc 40 BCLST))))
        (setq SBLINEPT2 (polar SBLINE-CENPT (cdr (assoc 51 BCLST)) (cdr (assoc 40 BCLST))))
       ) ;end arc
       
        ((= T (wcmatch (strcase ENTTYP) "*POLYLINE"))
        (princ "\n Polyline or Lightweight Polyline Selected!!")        
        (setq pl-StartPoint (car (SegmentPts SBLINE))) ;segment start point
        (setq pl-EndPoint (cadr (SegmentPts SBLINE))) ;segment end point
        (setq pl-Pickpt (vlax-curve-getClosestPointTo (vlax-ename->vla-object (car SBLINE)) PICKPT)) ;returns the a point actually on the pline
        (cond ((= T (is_pt_colinear pl-StartPoint pl-EndPoint pl-PICKPT 1e-10)) ;are the 3 points all in a line?
               (princ "\n This is a polyline LINE segment.")
               (setq SBLINEPT1 pl-StartPoint)
               (setq SBLINEPT2 pl-EndPoint)
               ;(setq PT3 (polar SIDEPT (+ (angle PT1 PT2) (* 0.5 pi)) 0.1000))
               ;(setq PT4 (inters PT1 PT2 SIDEPT PT3 nil))
               ) ;end line polyline segment


              ((= nil (is_pt_colinear pl-StartPoint pl-EndPoint pl-PICKPT 1e-10)); points are non-collinear, must be arc
               (princ "\n This is a polyline ARC segment.")
               (setq SBLINEPT1 pl-StartPoint)
               (setq SBLINEPT2 pl-EndPoint)	       
               (setq SBLINE-CENPT (PlineRadPt SBLINE))
               (print)
               ) ;end arc polyline segment

              ) ; end cond points collinear

        ) ;end LWpolyline
       ) ;_ end of cond LINE ARC *POLYLINE
    );end progn
      );end if
    
;; now have SY1PT1, SY1PT2, SY2PT1, SY2PT2, SBLINEPT1, SBLINEPT2, SBLINT-CENPT
;; get to work on pad placement...
    
;;calc intersection points
(cond
  ((= ENTTYP "LINE")
   (setq P1 (inters SY1PT1 SY1PT2 SBLINEPT1 SBLINEPT2 nil ))
   (setq P2 (inters SY2PT1 SY2PT2 SBLINEPT1 SBLINEPT2 nil ))
   (setq P3 (polar P1 (angle p1 p2) (/ (distance p1 p2) 2)))
   (setq P4 (polar P3 (+ (angle P1 P2) (* 0.5 pi)) SBLINE-OFFSET))
   (setq PadRot (+ (angle P1 P2) (* 0.0 pi)))
   (setq INSDIR (angtos PadRot 0 4))
   (command "insert" PadPlacer_BlockName P4 "1" "" INSDIR)
   ); line
  ((= ENTTYP "ARC")
   (setq SY1-obj (vlax-ename->vla-object (car SY1)))
   (setq SY2-obj (vlax-ename->vla-object (car SY2)))
   (setq SBLINE-OBJ (vlax-ename->vla-object (car SBLINE)))
   (setq P1 (vlax-invoke SY1-obj 'intersectwith SBLINE-OBJ acExtendOtherEntity))
   (setq P2 (vlax-invoke SY2-obj 'intersectwith SBLINE-OBJ acExtendOtherEntity))
   ;(setq P1 (inters SY1PT1 SY1PT2 SBLINEPT1 SBLINEPT2 nil ))
   ;(setq P2 (inters SY2PT1 SY2PT2 SBLINEPT1 SBLINEPT2 nil ))
   (setq P3 (polar P1 (angle p1 p2) (/ (distance p1 p2) 2)))
   (if (= 1 (vector-side P1 P2 SBLINE-CENPT))
     (setq P4 (polar SBLINE-CENPT (angle SBLINE-CENPT P3) (- (distance SBLINE-CENPT SBLINEPT1) SBLINE-OFFSET)))
     (setq P4 (polar SBLINE-CENPT (angle SBLINE-CENPT P3) (+ (distance SBLINE-CENPT SBLINEPT1) SBLINE-OFFSET)))
     )
   ;(setq P4 (polar SBLINE-CENPT (angle SBLINE-CENPT P3) (+ (distance SBLINE-CENPT SBLINEPT1) SBLINE-OFFSET)))
   (setq PadRot (angle P1 P2))
   (setq INSDIR (angtos PadRot 0 4))
   (command "insert" PadPlacer_BlockName P4 "1" "" INSDIR)
   );arc
  ((= T (wcmatch (strcase ENTTYP) "*POLYLINE"))
   (if (= T (is_pt_colinear pl-StartPoint pl-EndPoint pl-PICKPT 1e-10))
     (progn
   (setq P1 (inters SY1PT1 SY1PT2 SBLINEPT1 SBLINEPT2 nil ))
   (setq P2 (inters SY2PT1 SY2PT2 SBLINEPT1 SBLINEPT2 nil ))
   (setq P3 (polar P1 (angle p1 p2) (/ (distance p1 p2) 2)))
   (setq P4 (polar P3 (+ (angle P1 P2) (* 0.5 pi)) SBLINE-OFFSET))
   (setq PadRot (+ (angle P1 P2) (* 0.0 pi)))
   (setq INSDIR (angtos PadRot 0 4))
   (command "insert" PadPlacer_BlockName P4 "1" "" INSDIR)       
       ); end progn pline LINE segment
   (progn
    (setq SY1-obj (vlax-ename->vla-object (car SY1)))
    (setq SY2-obj (vlax-ename->vla-object (car SY2)))
    (setq SBLINE-OBJ (vlax-ename->vla-object (car SBLINE)))
    (setq P1 (vlax-invoke SY1-obj 'intersectwith SBLINE-OBJ acExtendOtherEntity))
    (if (not P1)
      (setq P1 (inters sy1pt1 sy1pt2 sblinept1 sblinept2 nil))
      );end if
    (if (< 5 (length P1))
      (setq P1 (list (car p1) (Cadr p1) (caddr p1)))); use the first intersection point only if p1 is a list of 2 points    
    (setq P2 (vlax-invoke SY2-obj 'intersectwith SBLINE-OBJ acExtendOtherEntity))
    (if (not P2)
      (setq P2 (inters sy2pt1 sy2pt2 sblinept1 sblinept2 nil))
      );end if
    (if (< 5 (length P2))
      (setq P2 (list (car p2) (Cadr p2) (caddr p2)))); use the first intersection point only if p2 is a list of 2 points
    ;(setq P1 (inters SY1PT1 SY1PT2 SBLINEPT1 SBLINEPT2 nil ))
    ;(setq P2 (inters SY2PT1 SY2PT2 SBLINEPT1 SBLINEPT2 nil ))
    (setq P3 (polar P1 (angle p1 p2) (/ (distance p1 p2) 2)))
    (if (= T (is_pt_colinear pl-StartPoint pl-EndPoint pl-PICKPT 1e-10))
      (setq P4 (polar P3 (+ (angle P1 P2) (* 0.5 pi)) SBLINE-OFFSET))
      (progn
       (if (= 1 (vector-side P1 P2 SBLINE-CENPT))
        (setq P4 (polar SBLINE-CENPT (angle SBLINE-CENPT P3) (- (distance SBLINE-CENPT SBLINEPT1) SBLINE-OFFSET)))
        (setq P4 (polar SBLINE-CENPT (angle SBLINE-CENPT P3) (+ (distance SBLINE-CENPT SBLINEPT1) SBLINE-OFFSET)))
       );end if
      );end progn
    );end if
    (setq PadRot (angle P1 P2))
    (setq INSDIR (angtos PadRot 0 4))
    (command "insert" PadPlacer_BlockName P4 "1" "" INSDIR)
    );end pline curve progn
     );end if LINE/ARC

   );pline
    );end cond

    (setq enttype nil)
    (setq sy1-obj nil)
    (setq sy2-obj nil)
    (setq sbline-obj nil)
     ) ;_ end of while

  (moder)
  (princ)
  );end padplacer