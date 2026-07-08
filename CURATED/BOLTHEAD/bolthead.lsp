;
;    Written by: Art Whitton
;    Date: October 10, 1996
;
;    Description:
;         This program will draw a bolthead (hexagon and circle) using a pline after the 
;         user has supplied the insertion point and nominal shank diameter of the bolt.
;
;    Required Functions: none
;        
;   Variable Listing:
;
; Name     Scope     Type      Description
;
; pt0      local     list      insertion point, center (user)
; shank    local     real      nominal shank diameter
; distance local     real      distance from center to corner
; angle    local     real      angle to vertex (re-used)
; pt1      local     list      first vertex (then re-used)
; 
;
;****************  PROGRAM SET-UP  *******************************************

(defun c:bolthead (/ pt0 pt1 angle distance shank); DEFINE 'BOLTHEAD' FUNCTION

   (setq oldcmd (getvar "cmdecho"))               ; CANCEL SCREEN ECHO 
   (setvar "cmdecho" 0)

;****************  INPUT FROM USER  ******************************************
   
   (setq pt0 (getpoint "\nEnter the insertion point: "))          
   (setq shank (getdist "\nEnter the nominal shank diameter: "))

;***************  CALCULATION OF DISTANCE, ANGLE AND PT1  ********************

   (setq distance ( / ( * 0.75 shank)( cos (/ pi 6))))
   (setq angle (- (* 2 pi)( / pi 6)))

   (setq pt1 (polar pt0 (/ pi 6) distance ))         ; SET FIRST POINT
  
;***************  BEGIN DRAWING BOLTHEAD  ************************************

   (command "pline")                                 ; ESTABLISH COMMAND 
	 (repeat 6                                       ; BEGIN LOOP
	    (setq angle (+ angle (/ pi 3)))              ; ADD TO ANGLE
	    (setq pt1 (polar pt0 angle distance))        ; SET VERTEX
	    (command pt1)                                ; DRAW TO VERTEX 
	 )                                               : END LOOP
    (command "c")                                    ; CLOSE PLINE
    (command "circle" pt0 (* 0.75 shank))            ; DRAW CIRCLE

    (setvar "cmdecho" oldcmd)                        ; RESTORE CMDECHO VALUE
    (princ)
)
   (princ "\nLoaded... type <bolthead> to run.")     ; INFO AT LOADING ONLY
   (princ)
