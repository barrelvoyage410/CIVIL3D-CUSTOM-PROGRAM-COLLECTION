                              MVSF.LSP
                Michael K. Weaver   Wed 06-23-1993 
;                   Compuserve Id. # 71461,1775
;  Routines to find the scale factor of a model view relative to paper space.
;Included routines:
;       c:mvsf  Condition 1: Tilemode = 0, in paper space.
;			The operator is prompted to select a viewport
;			entity.  The model view scale factor of the
;			selected viewport is displayed.
;		Condition 2: Tilemode = 0, in model space.  The model
;			view scale factor for the current viewport is
;			displayed.
;		Condition 3: Tilemode = 1.  Error message.
;	
;       mvsf    Calling mvsf with a viewport entity's entity name as the
;               argument will return the model view scale factor of the
;               subject viewport.

;       c:avsf  (Attribute Viewport Scale Factor) will stuff the scale factor
;               as displayed by c:mvsf above into an existing text or attribute
;               entity.
;Revision History:
;	 Fri 07-16-1993 Michael K. Weaver	Added code to get scale
;			of the current viewport if in model space with
;			tilemode turned off.
;

(defun c:avsf(;         Stuff mview sf into existing text or attribute entity
  /;                    no formal arguments
; attr;                 attribute entity name and selection point
; ent;                  viewport entity name and selection point
; avsf;                 viewport scale factor
; elist;                attribute entity list
 );                     end of local variables
 (if (and
   (setq attr (nentsel "\nAttribute for scale: "))
   (= "ATTRIB" (cdr (assoc 0 (entget (car attr)))))
   (setq ent (entsel "\nViewport entity of detail: "))
   (setq avsf (mvsf (car ent)))
  )
  (setq
   avsf (if (= (getvar "lunits") 4)
    (strcat (rtos (* 12.0 avsf)) " = 1'-0\"")
    (strcat (rtos mvscale) " = 1")
   );                            end if
   elist (entget (car attr))
   elist (subst (cons 1 avsf) (assoc 1 elist) elist)
   elist (entmod elist)
   elist (entupd elist)
  );                            end setq
  (princ "\nInvalid selection. ")
 );                             end if
 (princ)
);                              end c:avsf

(defun c:mvsf(;         Model View Scale factor
  /;                    no formal arguments
  ent;                  entity name and pick-point
  elist;                entity data list
  mvscale;              model view scale factor
 )
 (if (= 0 (getvar "tilemode"))
  (progn
   (cond
    ((and;				paper space is active (condition 1)
      (= 1 (getvar "cvport"))
      (setq ent (entsel))
      (setq
       ent (car ent)
       elist (entget ent)
      )
      (= "VIEWPORT" (cdr (assoc 0 elist)))
     );                            end and
     T
    )
    ((setq ss1 (ssget "X" (list '(0 . "VIEWPORT") (cons 69  (getvar "cvport")))));in model space (condition 2)
     (setq ent (ssname ss1 0))
    )
    (T (exit))
   );				end cond
   (setq mvscale (mvsf ent))
   (if (= (getvar "lunits") 4)
    (princ (strcat
      "\nModel view scale factor: "
      (rtos (* 12.0 mvscale))
      " = 1'-0\""
     )
    )
    (princ (strcat
      "\nModel view scale factor: "
      (rtos mvscale)
      " = 1"
     );				end strcat
    );				end princ
   );                            end if lunits = 4
  );                              end progn tilemode /= 1
  (princ "\nTilemode must be set to 0 for this routine. ")
 );                               end if tilemode = 0
 (princ)
)


(defun mvsf(;           Model View Scale Factor subroutine
  ent;                          viewport entity name
  /;                            no formal arguments
  vpxdata;                      viewport extended entity data
  ms_height;                    model view height (in model space)
  ps_height;                    paper space viewport height
  mvsf;                         Model View Scale Factor
  entxdata;                     get extended entity data
 )
;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ SUBROUTINES ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
 (defun entxdata(; get entity data with extended data.  By Tony Tanzillo
   ent;                          entity name
   appid;                        name of the registered application
   /;                    end of formal arguments
   d;                            extended entity data
  );                     end of local variable list
  (if (and
    (setq d (entget ent (list appid)));    entity list retrieved
    (setq d (assoc -3 d));                 extended entity data exists
    (cdadr d);                             extended entity data retrieved
   );                                      end of condition
   (cdadr d)
  );                                       end if
 );                                        end entxdata
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ  START MAIN ROUTINE ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
 (setq
  vpxdata (entxdata ent "acad")          ;Get extended entity data
  ms_height (cdr (nth 6 vpxdata))          ;MS view height.
  ps_height (cdr (assoc 41 (entget ent)));PS viewport height
 )
 (/ ps_height ms_height)
)

(progn
 (princ "\nC:MVSF By Michael Weaver Compuserve Id. # 71461,1775 ")
 (princ)
)
