;; revised 03-20-2007 to check for serial number  ME
;; revised 11/13/2007 10:46 AM to change MTEXT as well.  ME
;; revised 11/21/2007 10:34 AM to allow for polylines.  ME

;-------------------------------------------------------------------------
(defun rtd (LINE_ANG)
     (/ (* 180 LINE_ANG) pi)
); end defun rads_to_degrees
;-------------------------------------------------------------------------

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
  ) ;end SegmentPts
;---------------------------------------------------------------------------------------------------------------------------

(defun GET_TEXT ()
     (setq TEXT_PICK (entsel "\nSelect text to rotate :"))
     (while (= TEXT_PICK nil)
          (prompt "\nNothing selected!  Please try again...")
          (setq TEXT_PICK (entsel "\nSelect text to rotate :"))
     );end while
     (setq TEXT_NAME (car TEXT_PICK))
     (setq TXT_LIST (entget TEXT_NAME))
     (setq TXT_TYPE (cdr (assoc 0 TXT_LIST)))
     (while (not (wcmatch TXT_TYPE "*TEXT"));match TEXT or MTEXT
          (prompt (strcat "\nEntity picked was " TXT_TYPE " not TEXT. "))
          (setq TEXT_PICK (entsel "\nPlease select again: "))
          (while (= TEXT_PICK nil)
               (prompt "\nNothing selected DOOFUS!!")
               (setq TEXT_PICK (entsel "\nSelect TEXT to rotate :"))
          );end while
          ;(while (= TEXT_PICK nil)
          ;     (prompt "\nGEEZ!! Do you know how to use that thing?????")
          ;     (setq TEXT_PICK (entsel "\nSelect TEXT to rotate :"))
          ;);end while
          (setq TEXT_NAME (car TEXT_PICK))
          (setq TXT_LIST (entget TEXT_NAME))
          (setq TXT_TYPE (cdr (assoc 0 TXT_LIST)))
     );end while
     (progn
          (setq TEXT_NAME (car TEXT_PICK))
          (setq TXT_LIST (entget TEXT_NAME))
          (setq TXT_TYPE (cdr (assoc 0 TXT_LIST)))
     );end progn
     (setq TEXT_NAME (car TEXT_PICK))
     (setq TXT_LIST (entget TEXT_NAME))
);end get_text
;-------------------------------------------------------------------------
(defun GET_LINE ()
     (setq LINE_PICK (entsel "\nSelect line or polyline segment for text alignment :"))
     (while (= LINE_PICK nil)
          (prompt "\nNothing selected!  Please try again...")
          (setq LINE_PICK (entsel "\nSelect line for text alignment :"))
     );end while
     (setq LINE_NAME (car LINE_PICK))
     (setq ENT_LIST (entget LINE_NAME))
     (setq ENT_TYPE (cdr (assoc 0 ENT_LIST)))
     (while (not (wcmatch (strcase ENT_TYPE) "*LINE"))
          (prompt (strcat "\nEntity picked was a " ENT_TYPE " not line or polyline. "))
          (setq LINE_PICK (entsel "\nPlease select again: "))
          (while (= LINE_PICK nil)
               (prompt "\nNothing selected DOOFUS!!")
               (setq LINE_PICK (entsel "\nSelect LINE for text alignment :"))
          );end while
          (setq LINE_NAME (car LINE_PICK))
          (setq ENT_LIST (entget LINE_NAME))
          (setq ENT_TYPE (cdr (assoc 0 ENT_LIST)))
     );end while
     ;(setq SP (cdr (assoc 10 ENT_LIST)))
     ;(setq EP (cdr (assoc 11 ENT_LIST)))
     (setq SP (car (SegmentPts LINE_PICK)));segment start point
     (setq EP (cadr (SegmentPts LINE_PICK)));segment end point
  
     (setq LINE_ANG (angle SP EP))
     (setq TEMP_ANG LINE_ANG)
     (cond
          ((<= TEMP_ANG (/ pi 2))
               (setq LINE_ANG TEMP_ANG)
          ); end TEMP_ANG < 90
          ((and (<= TEMP_ANG pi) (> TEMP_ANG (/ pi 2)))
               (setq LINE_ANG (+ TEMP_ANG pi))
          ); end 180 < TEMP_ANG < 90
          ((and (<= TEMP_ANG (* 1.5 pi)) (> TEMP_ANG pi))
               (setq LINE_ANG (- TEMP_ANG pi))
          ); end 270 < LINE_ANG < 180
          ((and (<= TEMP_ANG (* 2 pi)) (> TEMP_ANG (* 1.5 pi)))
               (setq LINE_ANG TEMP_ANG)
          ); end 270 < LINE_ANG < 360
     ); end angle condition
);end get_line
;-------------------------------------------------------------------------
(defun TXT_ROT ()
     (setq en (car TEXT_PICK))
     (setq ed (entget en))
     (setq ed
          (subst (cons 50 LINE_ANG)
               (assoc 50 ed)
               ed
          );end subst
     );end setq
     (princ)
     (entmod ed)
     (princ)
); end defun txt_rot
;-------------------------------------------------------------------------
(defun c:TXTANG ()
  (if (cgl_check_serial)
    (progn
     (get_text)
     (get_line)
     (txt_rot)
    );end checker progn
    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
    );end if
    (princ)     
)
