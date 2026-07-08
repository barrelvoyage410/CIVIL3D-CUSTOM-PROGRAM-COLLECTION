; HELIX.LSP  8/89  Tony Tanzillo.
;
; Draws 3D helical springs and coils of varying width.
;
; Generic front end, no validation of user input is done, and
; and no error handler is included.
;

  (defun C:HELIX ( / cp od wi sa pt rt)
    (cond
       (  (and (setq cp (getpoint "\nCenter point: "))
	       (setq ra (getdist cp "\nOutside radius: "))
	       (setq wi (getdist "\nCoil width: "))
	       (setq sa (getangle cp "\nStarting angle: "))
	       (setq pt (getdist "\nPitch: "))
	       (setq rt (getreal "\nNumber of rotations: ")))
	  (princ "\nGenerating helix...")
	  (helix cp (* 2.0 ra) wi sa pt rt)))
    (princ)
  )

; Returns delta Z offset of a 2D or 3D point in the current UCS

  (defun delta_z (pt z)
    (list (car pt)
	  (cadr pt)
	  (cond (  (caddr pt) (+ (caddr pt) z)) (t z))))


; (helix <cen pt> <outer dia> <coil width> <start ang> <pitch> <rotations> )
;
; Back end for C:HELIX
;
;   Note: SURFTAB1 controls resolution (number of segments/rotation)

(defun helix (	 cp od wi sang pitch rot /
		 orad irad segs s_ang s_pitch bp iang w bm)

  (setq segs	(getvar "surftab1")	      ; SURFTAB1 controls resolution
	s_ang	(/ (* 2.0 pi) segs)	      ; included angle of segment
	s_pitch (/ pitch segs)		      ; segment pitch (z offset)
	iang	(- sang s_ang)		      ; segment angle
	bp	(delta_z cp (- s_pitch))      ; segment pitch
	orad	(/ od 2.0)		      ; outer radius
	irad	(- orad wi))		      ; inner radius

  (setvar "cmdecho" 0)
  (setq bm (getvar "blipmode"))
  (setvar "blipmode" 0)
  (cond (  (setq w (> wi 0.0))		      ; if helix width > 0,
	   (command "3DMESH"		      ; then start a 3DMESH
	      (fix (1+ (* segs rot))) 2))     ; M = 1 + segements * rotations
					      ; N = 2 edges of the flat coil
	(t (command "3DPOLY"))) 	      ; otherwise, start 3D polyline
  (repeat (fix (1+ (* segs rot)))	      ; and go for it
    (command
      (polar
	 (setq bp (delta_z bp s_pitch))       ; increment pitch
	 (setq iang (+ iang s_ang))	      ; increment angle
	 orad ))			      ; outer radius
    (cond (w (command (polar bp iang irad)))) ; if helix has width, then
  )					      ; specify inner edge.
  (cond ((not w) (command ""))) 	      ; Stop if drawing a 3DPLINE.
  (setvar "blipmode" bm)
)


; ---------------------------------eof helix.lsp--------------------------
