 ; BCTIC.lsp  (TM) copyright September 1992 by Julian Laskey 
 ;---------------------------------------------------------------------------------------------------------------------------
 ;;; Customized for Cowhey Gudmundson Leder
 ;;; -->revised July 20, 1993 by Mark Evinger<--
 ;
 ;;; revised 12/8/1999 8:35:07 AM to change layer color of 1P_BC from 7 to 114.
 ;;; This change allows for the back of curb to show up more clearly onscreen.
 ;
 ;;; revised 9/29/2006 6:24 PM to allow for polyline selections for C3D2007 rollout.  ME
 ;;; revised 03-20-2007 to check for serial number            ;;;ME
 ;---------------------------------------------------------------------------------------------------------------------------
 ; This program repetitively draws a "tick" line, typically to mark back of curb P.C.'s, at right angles or radially from the
 ; nearest end point of a selected line or arc entity to the user-selected side of the entity. The length of the tick is user
 ; definable in drawing units but is offerred at 0.04" as default. Layer "1P_BC" is created (color white) if not existing and
 ; is used for new tick marks. (changed initial tick length to 2 units....ME)
 ; Program revamped to allow lines/arcs/polylines as selected geometry.
 ;---------------------------------------------------------------------------------------------------------------------------

 
;============================================

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

(defun
      C:BCTIC
             (/       bm      ce      os      clyr    olderr  msg     TICFT
              TICLEN  BCENT   BCLST   ENTTYP  PICKPT  PT1     PT2     INSPT
              SIDEPT  PT3     PT4     TICEND  CENPT PL-ENDPOINT PL-PICKPT PL-STARTPOINT)
  (if (cgl_check_serial)
    (progn
 ;---------------------------------------------------------------------------
 ;                                       DEFINE STANDARD FUNCTIONS
 ;---------------------------------------------------------------------------
  (defun
        SAVE
            ()
    (setq bm      (getvar "BLIPMODE")
          ce      (getvar "CMDECHO")
          os      (getvar "OSMODE")
          clyr    (getvar "CLAYER")
          olderr  *error*
          *error* newerr
          ) ;_ end of setq
    ) ;_ end of defun
 ;---------------------------------------------------------------------------
  (defun
        RESET
             ()
    (setvar "BLIPMODE" bm)
    (setvar "CMDECHO" ce)
    (setvar "OSMODE" os)
    (command "layer" "s" clyr "")
    (setq *error* olderr)
    ) ;_ end of defun
 ;---------------------------------------------------------------------------
  (defun
        NEWERR
              (msg)
    (if (/= msg "Function Cancelled")
      (princ (strcat "\nError: " msg))
      ) ;_ end of if
    (RESET)
    (print)
    ) ;_ end of defun
 ;---------------------------------------------------------------------------
 ;                                       MAIN ROUTINE
 ;---------------------------------------------------------------------------
  (SAVE)
  (setvar "CMDECHO" 0)
  (setvar "BLIPMODE" 0)
  (setvar "OSMODE" 0)
  (if (= (tblsearch "layer" "1P_bc") nil)
    (command "._layer" "n" "1P_bc" "c" "114" "1P_bc" "")
    ) ;_ end of if
  (command "._layer" "s" "1P_bc" "")
 ; (setq TICFT (* (getvar "LTSCALE") 0.04)) <--this was initially too short, so I changed it..ME
  (setq TICFT 2.0)
  (print)
  (initget (+ 2 4))
  (setq TICLEN
         (getreal
           (strcat
             "\n Enter desired tick length in drawing units: <<"
             (rtos TICFT 2 1)
             ">>"
             ) ;_ end of strcat
           ) ;_ end of getreal
        ) ;_ end of setq
  (if (= TICLEN nil)
    (setq TICLEN TICFT)
    ) ;_ end of if
  (print)
  (while
    (setq
      BCENT
       (entsel
         "\n Select LINE or ARC or POLYLINE near end to be marked with tick: "
         ) ;_ end of entsel
      ) ;_ end of setq
     (while
       (and (/= (cdr (assoc 0 (entget (car BCENT)))) "LINE")
            (/= (cdr (assoc 0 (entget (car BCENT)))) "ARC")
            (/= (cdr (assoc 0 (entget (car BCENT)))) "POLYLINE")
            (/= (cdr (assoc 0 (entget (car BCENT)))) "LWPOLYLINE")
            ) ;_ end of and

        (prompt
          "\n Selection was not a LINE or ARC or POLYLINE! Try again!!"
          ) ;_ end of prompt
        (print)
        (setq BCENT
               (entsel
                 "\n Select LINE or ARC or POLYLINE near end to be marked with tick: "
                 ) ;_ end of entsel
              ) ;_ end of setq
        ) ;_ end of while
     (setq BCLST (entget (car BCENT)))
     (setq ENTTYP (cdr (assoc 0 BCLST)))
     (setq PICKPT (cadr BCENT))

     (cond
       ((= ENTTYP "LINE")
        (setq PT1 (cdr (assoc 10 BCLST)))
        (setq PT2 (cdr (assoc 11 BCLST)))
        (if (< (distance PICKPT PT1) (distance PICKPT PT2))
          (setq INSPT PT1)
          (setq INSPT PT2)
          ) ;_ end of if
        (print)
        (initget 1)
        (setq SIDEPT
               (getpoint
                 INSPT
                 (strcat "\n Indicate side of " ENTTYP " for tick mark: ")
                 ) ;_ end of getpoint
              ) ;_ end of setq
        (setq PT3 (polar SIDEPT (+ (angle PT1 PT2) (* 0.5 pi)) 0.1000))
 ; the value of 0.1000 was originally .0001, which apparently was too
 ; short for the new version 12 display-list handlers. The original
 ; program would crash out with an *error: bad arguement type*
 ; Increasing the offset value for pt3 seems to have cured the problem.
 ; Actually, any value up to the perpendicular distance to the PT1 PT2
 ; will work to determine which side of the line to draw the tick.
        (setq PT4 (inters PT1 PT2 SIDEPT PT3 nil))
        (setq TICEND (polar INSPT (angle PT4 SIDEPT) TICLEN))
        ) ;end line


       ((= ENTTYP "ARC")
        (setq CENPT (cdr (assoc 10 BCLST)))
        (setq PT1 (polar CENPT (cdr (assoc 50 BCLST)) (cdr (assoc 40 BCLST))))
        (setq PT2 (polar CENPT (cdr (assoc 51 BCLST)) (cdr (assoc 40 BCLST))))
        (if (< (distance PICKPT PT1) (distance PICKPT PT2))
          (setq INSPT PT1)
          (setq INSPT PT2)
          ) ;_ end of if
        (print)
        (initget 1)
        (setq SIDEPT
               (getpoint
                 INSPT
                 (strcat "\n Indicate side of " ENTTYP " for tick mark: ")
                 ) ;_ end of getpoint
              ) ;_ end of setq
        (if (> (distance CENPT SIDEPT) (distance CENPT INSPT))
          (setq TICEND (polar INSPT (angle CENPT INSPT) TICLEN))
          (setq TICEND (polar INSPT (angle INSPT CENPT) TICLEN))
          ) ;_ end of if
        ) ;end arc

       
        ((= T (wcmatch (strcase ENTTYP) "*POLYLINE"))
       ;((= ENTTYP "POLYLINE")
        (princ "\n Polyline or Lightweight Polyline Selected!!")
        ;;) ;end polyline
       ;((= ENTTYP "LWPOLYLINE")
        ;(princ "\n Lightweight Polyline Selected!!")
 ;(SegmentPts BCENT)
 ;(setq cgl_vobj (vlax-ename->vla-object (car BCENT))) 
 ;(setq StartPointCurve (vlax-curve-getStartPoint cgl_vobj))
 ;(setq EndPointCurve (vlax-curve-getEndPoint cgl_vobj))
        (setq pl-StartPoint (car (SegmentPts BCENT))) ;segment start point
        (setq pl-EndPoint (cadr (SegmentPts BCENT))) ;segment end point
 ;(setq pl-Pickpt PICKPT)
        (setq pl-Pickpt (vlax-curve-getClosestPointTo (vlax-ename->vla-object (car BCENT)) PICKPT)) ;returns the a point actually on the pline
        (if (< (distance PICKPT pl-StartPoint);test for which side of line/arc the pick point is closest to...
               (distance PICKPT pl-EndPoint))
          (setq INSPT pl-StartPoint)
          (setq INSPT pl-EndPoint)
          ) ;_ end of if

        (cond ((= T (is_pt_colinear pl-StartPoint pl-EndPoint pl-PICKPT 1e-10)) ;are the 3 points all in a line?
               (princ "\n This is a polyline LINE segment.")
               (setq PT1 pl-StartPoint)
               (setq PT2 pl-EndPoint)
               (if (< (distance PICKPT PT1) (distance PICKPT PT2))
                 (setq INSPT PT1)
                 (setq INSPT PT2)
                 ) ;_ end of if
               (print)
               (initget 1)
               (setq SIDEPT
                      (getpoint
                        INSPT
                        (strcat "\n Indicate side of " ENTTYP " for tick mark: ")
                        ) ;_ end of getpoint
                     ) ;_ end of setq
               (setq PT3 (polar SIDEPT (+ (angle PT1 PT2) (* 0.5 pi)) 0.1000))
               (setq PT4 (inters PT1 PT2 SIDEPT PT3 nil))
               (setq TICEND (polar INSPT (angle PT4 SIDEPT) TICLEN))
               ) ;end line polyline segment


              ((= nil (is_pt_colinear pl-StartPoint pl-EndPoint pl-PICKPT 1e-10)); points are non-collinear, must be arc
               (princ "\n This is a polyline ARC segment.")
               (setq CENPT (PlineRadPt BCENT))
               (print)
               (initget 1)
               (setq SIDEPT
                      (getpoint
                        INSPT
                        (strcat "\n Indicate side of " ENTTYP " for tick mark: ")) ;_ end of getpoint
                     ) ;_ end of setq

               (if (> (distance CENPT SIDEPT) (distance CENPT INSPT))
                 (setq TICEND (polar INSPT (angle CENPT INSPT) TICLEN))
                 (setq TICEND (polar INSPT (angle INSPT CENPT) TICLEN))
                 ) ;_ end of if

               ) ;end arc polyline segment

              ) ; end cond points collinear

        ) ;end LWpolyline
       ) ;_ end of cond LINE ARC *POLYLINE
     (if (and INSPT TICEND)
       (command "line" INSPT TICEND "")
       (princ "\n Nothing to do...")
       ) ;end if
     (print)
     ) ;_ end of while
 ;---------------------------------------------------------------------------
  (RESET)
  (princ)
  (prinC)
    );end checker progn
    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
    );end if
    (princ)

 ;---------------------------------------------------------------------------
  ) ;_ end of defun
