 ; Annotate a Contour Line
; BY: Tom Beauford - 7/11/2005
; Tombu@LeonCountyFL.gov
; LEON COUNTY PUBLIC WORKS SURVEY & R/W SECTION
; (load "ContourAnnotate.lsp") ContourAnnotate 
;=============================================================
(defun c:ContourAnnotate (/ mspace om osz e ent pt tblname Rot str mtextobj ent el)
  (setq mspace (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object)))
            om (getvar "osmode")
            osz (getvar "osnapz")
  )
  (setvar "osnapz" 0)
  (setvar "osmode" 512)
  (setq pt (getpoint"\nSelect point on Contour Line: ")
            e (nentselp pt) 
            ent (car e)
            pt (cadr e)
            tblname (car(ade_odgettables ent))
  )
  (setvar "osmode" 0)
  (setq Rot (getangle pt "\nPick or Enter angle..." )
            str (rtos (caddr pt) 2 0))
 
  (ade_errsetlevel 2) 
  (if(ade_odgettables ent)
    (cond
      ((ade_odgetfield ent (car(ade_odgettables ent)) "ELEV" 0)
      (setq str (itoa(ade_odgetfield ent (car(ade_odgettables ent)) "ELEV" 0))))
      ((ade_odgetfield ent (ade_odgettables ent) "ELEVATION" 0)
      (setq str (itoa(ade_odgetfield ent (ade_odgettables ent) "ELEVATION" 0))))
    )
   )
  (ade_errsetlevel 0) 
 
  (princ "\nElev = ")
  (princ str)
 
  (setq mtextobj (vla-addMText mspace (vlax-3d-point pt) 0.0 str))
  (vla-put-attachmentPoint mtextobj acAttachmentPointMiddleCenter)
  (vla-put-insertionPoint mtextobj (vlax-3d-point pt))
  (vla-put-Rotation mtextobj Rot)
  (vla-put-Color mtextobj 7)
  (vla-put-backgroundfill mtextobj :vlax-true); mask on with background color
  (setq el (entget (vlax-vla-object->ename mtextobj)))
  (setq el (subst (cons 45 1.15) (assoc 45 el) el)) ;Set MaskSize 1.15 × TextSize
  (entmod el)
  (setvar "osmode" om)
  (setvar "osnapz" osz) 
  (princ)
)