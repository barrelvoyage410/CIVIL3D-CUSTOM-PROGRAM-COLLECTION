; This program will draw a table of points in a drawing.
; Upon running the command the user picks an insertion point and then
; a selection set of Civil Point Objects.
;
;
; Known issues:
; Does not draw table at correct scale when selecting points inside a 
; paperspace viewport. I will try to fix this when I get time.
;
;
; THIS PRORAM MAY BE DISTRIBUTED AS PUBLIC DOMAIN. END USERS ARE FREE
; TO MODIFY PROGRAM TO SUIT THEIR NEEDS.
; 
; written by JIM CRANE (Guph3all@hotmail.com). I wrote this to see if 
; it could be done.
;
;




(defun text (pt lbl / th size)
(setq th (cdr (assoc 40 (tblsearch "style" (getvar "textstyle")))))
(if (= 0 th) 
    (setq th (getvar "textsize") size 1)
 )
 
(if size
  (command "text" pt th "0" lbl )
  (command "text" pt "0"    lbl )
 )
(princ)
);defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    C:PTABLE                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;tableofpoints
(DEFUN c:PTable ( / pt sset ename elist desc ptn pte ptl olddimzin strlist pts
                    widlin widpts widptn widpte line strlist p1 p2 p3 p4 th lspace size)  

 (setq th (cdr (assoc 40 (tblsearch "style" (getvar "textstyle")))))
 (if (= "L" (strcase(substr (getvar "textstyle") 1 1)))
   (setq lspace 1.71429); leroy
   (setq lspace 1.61905); roman
  )

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;           TWEAKS           ;; 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ; this is where the field widths are declared

 (SETQ pnum 1); nil=no pt numbers
 (SETQ desc 1); nil=no desc
 (setq widpts 5.0 )
 (setq widptn 15.0)
 (setq widpte 25.0)
 (setq widptz  33.0)
 (setq widdesc 39.0)
 (setq coords (getvar "luprec"))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

 (setq olddimzin (getvar "dimzin"))
 (setvar "dimzin" 0)
 (if (= 0 (getvar "tilemode"))(command "pspace"));if

 (setq pt (getpoint "\nPick table point: ")); top left of table

 (if (= 0 (getvar "tilemode")) (command "mspace"))
 (setq sset (SSGET '((0 . "AECC_POINT")))); get points

 (if (= 0 (getvar "tilemode"))
   (progn
   (command "pspace")
   (setvar "angbase" 0)
   (setvar "snapang" 0)
   );progn
   );if


 (if (not sset)
  (setq sset (SSGET "X" '((0 . "AECC_POINT")))); get points
 )

 ; draw the top line
 ;(setq p1 (polar pt (+ 3.2986722 (getvar "angbase")) (* th 1.9783)) p3 p1)
 (setq p1 pt p3 p1)
 (setq p2 (polar p1 (+ 0.00 (getvar "angbase")) (* widdesc th)) p4 p2)
 (command "line" p1 p2 "")

 (while (> (sslength sset) 0)
  (progn
   (setq ename (ssname sset (- (sslength sset) 1))
         sset  (ssdel ename sset)
         elist (entget ename)
          pts (itoa (cdr (assoc 90 elist)))
          ptl (cdr (assoc 11 elist))
          ptn (rtos (cadr ptl) 2 coords)
          pte (rtos (car ptl) 2 coords)
          ptz (rtos (caddr ptl) 2 coords)
         desc (cdr (assoc 303 elist))
   );setq

   (setq p1 (polar p1 (+ 4.71238898 (getvar "angbase")) (* th lspace)) )
   (setq p2 (polar p1 (+ 0.00 (getvar "angbase")) (* widdesc th)))
   (command "line" p1 p2 "")

   (setq pt10 (polar p1 (+ 0.394792 (getvar "angbase")) (* 0.928574 th)))
   (if pnum (text pt10  pts)(text pt10  desc))
   (setq pt12 (polar pt10 (+ 0.00 (getvar "angbase")) (* widpts th)))
   (text pt12 ptn)
   (setq pt13 (polar pt10 (+ 0.00 (getvar "angbase")) (* widptn th)))
   (text pt13 pte)
   (setq pt14 (polar pt10 (+ 0.00 (getvar "angbase")) (* widpte th)))
   (text pt14 ptz)
   (setq pt15 (polar pt10 (+ 0.00 (getvar "angbase")) (* widptz th)))
   (if pnum (text pt15  desc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;'    This section added so that STATION & OFFSET data may be included          ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Since I work on airports and the alignments are straight I came up 
; with a simple subroutine that 'works' and 'gets the job done'. I have
; not even attempted to extract alignment data from Land Desktop/Civil 3D.
; If someone out there is smarter than me I hope they consider working on 
; adding alignment functionality.
;
 (if (not(null c:stp))
    (progn
     (setq pt16 (polar pt15 (+ 0.00 (getvar "angbase")) (* 6.0 th)))
     (text pt16 (stpsta ptl))
     (setq pt17 (polar pt16 (+ 0.00 (getvar "angbase")) (* 10.0 th)))
     (text pt17 (stpoff ptl))
    );progn
 );if
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   );progn
 );while

 ;draw sides
 (command "line" p1 p3 "")
 (command "line" p2 p4 "")

 (setvar "dimzin" olddimzin)
(princ)
);defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                       DXF Parts of an AECC_POINT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(
; (-1 . <Entity name: 7c6ea630>)
; (0 . "AECC_POINT")
; (330 . <Entity name: 7c712cf8>)
; (5 . "6026")
; (100 . "AcDbEntity")
; (67 . 0)
; (410 . "Model")
; (8 . "C-NODE-EOP")
; (100 . "AecDbEntity")
; (102 . "{AEC_SUBOBJECT")
; (300 . "AeccImpPoint")
; (100 . "AecImpObj")
; (3 . "")
; (100 . "AecImpEnt")
; (171 . 0)
; (100 . "AecImpGeo")
; (10 441700.0 211268.0 0.0);        <---label justification point
; (15 1.0 0.0 0.0)
; (16 0.0 1.0 0.0)
; (210 0.0 0.0 1.0)
; (360 . <Entity name: 0>)
; (100 . "AeccImpPoint")
; (90 . 7039)
; (11 441677.0 211242.0 0.0);        <---(11 east north elev)
; (302 . "40")              ;        <---(302 description)
; (301 . "")
; (300 . "")
; (280 . 0) 
; (303 . "40")              ;        <---(303 description)
; (304 . "")
; (305 . "")
; (10 441677.0 211242.0 0.0)
; (285 . 0)
; (286 . 1)
; (170 . 3)
; (141 . 5.0)
; (306 . "L100")
; (140 . 10.0)
; (282 . 0)
; (283 . 0)
; (284 . 1)
; (171 . 2)
; (172 . 1)
; (173 . 2)
; (287 . 0)
; (142 . 0.370658)
; (288 . 1)
; (102 . "AEC_SUBOBJECT}")
; (102 . "{AEC_NULLOBJECT}")
; (100 . "AecDbGeo")
; (100 . "AeccDbPoint")
;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (SSGET "X" '((0 . "AECC_POINT"))); get points
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
