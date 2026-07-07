;;;---------------------------------------------------------------------;;;
;;;  RevCloud.lsp							;;;
;;;  Created by Jonathan Norton						;;;
;;;  jonathann3891@gmail.com						;;;
;;;									;;;
;;;  Copyright © 2020                                                   ;;;
;;;                                                                     ;;;
;;;  FUNCTION:                                                          ;;;
;;;  Revision Cloud Utility						;;;
;;;									;;;
;;;  COMMAND: RVC							;;;
;;;  									;;;
;;;  PLATFORMS:                                                         ;;;
;;;  Tested on 2020					                ;;;
;;;                                                                     ;;;
;;;  THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED       ;;;
;;;  WARRANTY. ALL IMPLIED WARRANTIES OF FITNESS FOR ANY PARTICULAR     ;;;
;;;  PURPOSE AND OF MERCHANTABILITY ARE HEREBY DISCLAIMED.              ;;;
;;;									;;;
;;;---------------------------------------------------------------------;;;

(defun c:RVC (/ ans)
   (initget "Polyline Freehand Triangle")
   (if (null (setq ans (getkword (strcat "\nSelect revision cloud option: [Polyline/Freehand/Triangle] <Polyline> :"))))
	     (setq ans "Polyline"))
   (cond
      ((= ans "Polyline") (rvc_Polyline))
      ((= ans "Freehand") (rvc_Freehand))
      ((= ans "Triangle") (rvc_Triangle))
      )
	  )

; ---------------------------------------------------------------------------------------------------------
;Polyine to Revision Cloud

(defun rvc_Polyline (/ *error* doc en 0layer 0delobj 0echo _entselpoly ArcLen ds)

  ;-----
  (defun *error* (errmsg)
    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break,end"))
      (princ (strcat "\nError: " errmsg)))
    (if 0layer (setvar 'clayer 0layer))
    (if 0delobj (setvar 'delobj 0delobj))
    (if 0echo (setvar 'cmdecho 0echo))
    (vla-endundomark doc)
    (princ))

  ;-----
  (defun _entselpoly (msg / ensel)
    (setvar 'errno 0)
    (while (not	(cond ((setq ensel (entsel (strcat "\n" msg)))
		       (or (= "LWPOLYLINE" (cdr (assoc 0 (entget (car ensel)))))
			   (prompt "\nObject selected is NOT a polyline!")))
		      ((= 52 (getvar 'errno))) ; Exit
		      (T
		       (prompt "\nSelect Polyline: Nothing selected")))))
    ensel
    )

; ---------------------------------------------------------------------------------------------------------
  
  (vla-startundomark (setq doc (vla-get-activedocument (vlax-get-acad-object))))
  
  (setq 0layer  (getvar 'clayer)
	0delobj (getvar 'delobj)
	0echo   (getvar 'cmdecho)
	0AttReq (getvar 'AttReq))
  (setvar 'delobj 1)
  (setvar 'cmdecho 0)
  (setvar 'AttReq 0);Supress attribute editor
  (command "layer" "m" "CLOUD" "c" "241" "" "")
  
  ;;If dimscale is 0 set variable to 1
  (setq ds (getvar "DIMSCALE"))
  (or (/= 0 (setq arclen (* 0.25 ds)))
      (setq ArcLen 1.))

  ;;Set Revision Number
  (setq en (car (_entselpoly "\nSelect Polyline: ")))
  (command "._revcloud" "a" ArcLen ArcLen "o" en "")
  (setq RevNum ((lambda ( input ) (if (eq "" input) RevNum input))
		 (getstring (strcat "\nEnter Revision Number: <" (setq RevNum (cond ( RevNum ) ( "0B" ))) "> : "))))

  (command "insert" "Z:/ECI Custom CAD/_AutoCAD/2020/Shared/Blocks/REV" "_Scale" ds "_Rotate" 0 pause)
  (setpropertyvalue (entlast) "REV" RevNum);Edit Attribute

  (setvar 'clayer 0layer)
  (setvar 'AttReq 0AttReq)
  (*error* "end")
  )

; ---------------------------------------------------------------------------------------------------------
;Freehand Revision Cloud

(defun rvc_Freehand ( / 0layer 0osmode last_pnt start_pnt next_pnt arc_dist inc_angle)

  (setq 0osmode (getvar "osmode")
	0layer (getvar "clayer"))
  
  (setvar 'osmode 0)
  (setvar 'cmdecho 0)
  (setvar 'orthomode 0)
  (command "layer" "m" "cloud" "c" "241" "" "")

  (setq last_pnt (getpoint "\nPick cloud start point: " )
	start_pnt last_pnt
	arc_dist (* 0.25 (getvar "dimscale" )) ;; adjust arc size here!
	inc_angle 110)                         ;; adjust arc angle here!

  (prompt "\nGuide crosshairs along cloud path..." )
  (command "pline" last_pnt "w" "0" "0" "a" "a" inc_angle )
  (while last_pnt
    (setq next_pnt (cadr (grread 1 )))
    (if (> (distance last_pnt next_pnt ) arc_dist )
      (progn
	(command next_pnt "a" inc_angle )
	(setq last_pnt next_pnt ))
      )
    (if (> (distance last_pnt next_pnt ) (distance start_pnt next_pnt ))
      (progn
	(command start_pnt "cl" )
	(setq last_pnt nil ))
      )
    )
  (rvc_triangle);Run RVC_Triangle
  (setvar 'osmode 0osmode)
  (setvar 'clayer 0layer)
  )

; ---------------------------------------------------------------------------------------------------------
;Insert Revision Triangle

(defun RVC_Triangle (/ 0layer ds)

  (setq 0layer (getvar 'clayer)
	0AttReq (getvar 'AttReq))

  (setvar 'AttReq 0);Supress attribute editor
  (command "layer" "m" "cloud" "c" "241" "" "")

  ;;If dimscale is 0 set variable to 1
  (setq ds (getvar "DIMSCALE"))
  
  (or (/= 0 (setq arclen (* 0.25 ds)))
      )
      
  (setq RevNum ((lambda ( input ) (if (eq "" input) RevNum input))
		 (getstring (strcat "\nEnter Revision Number: <" (setq RevNum (cond ( RevNum ) ( "0B" ))) "> : "))))
  
  (command "insert" "Z:/ECI Custom CAD/_AutoCAD/2020/Shared/Blocks/REV" "_Scale" ds "_Rotate" 0 pause)
  (setpropertyvalue (entlast) "REV" RevNum);Edit Attribute

  (setvar 'clayer 0layer)
  (setvar 'AttReq 0AttReq)
  )
(princ)