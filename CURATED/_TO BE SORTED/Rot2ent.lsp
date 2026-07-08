;=========================================================================
; ROT2ENT.lsp   Rotate to Entity  (Command function for AutoCAD) 
; (c) Copyright 2001 TimeSavers for CAD
; from: www.timesaversforcad.com
;-----------------------------------------------------------------------------
; Description:
;
; Rotates objects relative to a specific angle or angle of an entity
; 1. select objects to rotate
; 2. select Rotation Baseline (straight segment on object(s) being rotated,
;      which will take on new angle)
; 3. select line segment whose angle you want Baseline to match
; 4. pick Base Point for rotation
; 5. then drag ortho rotation for final position
;==========================================================================
(defun c:rot2ent (/ l:ang r:cang *r2:err* c_e s_a ss ne ed e1 ab am ra bp)

  ;-SELECT LINE, RETURN ANGLE
  (defun l:ang (pmt / e et ed p1 p2 aptr)
    (cond ((not pmt)(setq pmt "\nSelect line: " ne nil))
          ((= (type pmt) 'LIST)(setq ne pmt)) ;called with nentsel list
          ((setq ne nil))
    )
    (if (or ne
            (while (progn (initget "A  ") ;keyword "A" available
                     (setq ne (nentsel pmt))
                     (cond ((not ne)(princ "0 found"))
                           ((listp ne)
                             (setq e (car ne)
                                   ed (entget e)
                                   et (cdr (assoc 0 ed))
                             )
                             (and (/= et "LINE")(/= et "LEADER")
                                  (/= et "LWPOLYLINE")(/= et "VERTEX")
                                  (princ "Invalid object, Try Again ")
                             )
                           )
                     )
                   )
            ) 
            (/= ne "")
        )
      (cond ((= ne "A") "A")
            (t (setq aptr (getvar "aperture"))
               (setvar "aperture" (getvar "pickbox"))
               (setq p1 (if (= et "XLINE")'(0 0 0)(osnap (cadr ne) "nea"))
                     p2 (if (= et "XLINE")(cdr (assoc 11 ed))(osnap (cadr ne) "endp"))
               )
               (setvar "aperture" aptr)
               (angle p1 p2)
            )
      )
    )
  )
  ;-CORRECT ANGLE 
  (defun r:cang (ang rot /)
    (cond ((>= ang rot)(- ang pi))
          ((< ang 0.0)(+ ang pi))
          (ang)
    )
  )
  ;-ERROR HANDLING
  (defun *r2:err* (m)
    (or (member m '("Function cancelled" "quit / exit abort" "console break"))
        (prompt (strcat "\n< " m " >\n"))
    )
    (if s_a (setvar "snapang" s_a))
    (if aptr (setvar "aperture" aptr))
    (setvar "cmdecho" c_e)
    (setq *error* *e*)(princ)
  )

  ;-GET VALUES
  (setq *e* (cond (*e*)(*error*))
        *error* *r2:err*
        c_e (getvar "cmdecho")
  )

  ;-MAIN FUNCTION
  (if (setq ss (ssget))
    (progn
      (setvar "cmdecho" 0)
      (command "rotate" ss "")
      (if (and (setq bp (getpoint "\nBase point: "))
               (setq ab (l:ang "\nSelect rotation baseline: "))
               (setq am (l:ang "\nAngle/<match line>:"))
               (or (/= am "A")(setq am (getangle "\nNew angle: ")))
          )
        (progn
          (setq ra (- am ab)
                s_a (getvar "snapang")
          )
          (setvar "orthomode" 1)
          (prompt "\nOrtho position: ")
          (setvar "snapang" 0.0)
          (command bp (/ (* ra 180.0) pi) "rotate" ss "" bp pause)
          (setvar "snapang" s_a)(setq s_a nil)
        )
        (command)
      )
    )
  )

  ;-RETURN VARIABLES
  (setvar "cmdecho" c_e)
  (setq *error* *e*)
  (princ)
)
;-END FILE
