;Label finish grade contour by LPS 2008
;modified by RWI for JSD 2012

(defun DXF (ELEMENT ENTITY /) (cdr (assoc ELEMENT ENTITY)))(defun c:lc ()
(vl-load-com)
(defun rtd (a) (/ (* a 180.0) pi))

(defun dxf(code elist)
(cdr (assoc code elist))
)
(setq th (getvar "textsize")
         ts (getvar "textstyle")
         cl (getvar "clayer")
         os (getvar "osmode")
)

(setq ent (entsel "Pick contour to be labeled: ")
           p1 (vlax-curve-getclosestpointto (car ent) (cadr ent) )
           ed (entget (car ent))
);setq

(if (= (caddr p1) 0.0)
    (alert "Contour needs an elevation!!!")
      (progn

  (if (= (DXF 0 ED) "LWPOLYLINE")
	(SETQ txt1 (rtos (DXF 38 ED)2 0))
	(SETQ txt1 (rtos (CADDR (DXF 10 ED))2 0))
  )

(setvar "osmode" 512)
	(setq a1 (rtd(getangle "Specify angle of label: " p1)))

  	(setvar "clayer" "J-PGT")

	(command "text" "s" "SIMPLEX" "M" P1 (* 0.08(GETVAR "DIMSCALE")) a1 txt1)

  	(setvar "textsize" th)
	(setvar	"textstyle" ts)
	(setvar	"clayer" cl)
              (setvar "osmode" os)
	
	(PRINC)	
);progn
);if
)