;*****************************************************************************
;*****************************************************************************
;*****************************************************************************
;
;    Program: awhitton.lsp
;
;    Written by: Art Whitton
;    Finish Date: November 21, 1996
;    Revision History: 
;         November 27, 1996
;    
;    Location: I:\cdcm3375\Lab6\awhitton.lsp  (Room SW-3 1920 @ BCIT)
;
;    Description:  This program will reset the current layer to the 
;                  layer of a user picked entity.   
;
;    Required Functions:
;         none
;
;    Variable Listing:
;
; Name      Scope     Type      Description
;
; ent_name  global    string    unique identifier name of the entity
; ent_list  global    list      entity's definition data list
; l_name    global    string    layer name pulled from the entity
;
;*****************************************************************************
;

(defun c:sl ()
	(setq ent_name (car (entsel "\nPick an entity to reset current layer: ")))
	(setq ent_list (entget ent_name))       ; retrieves the entity's data
	(setq l_name (cdr (assoc 8 ent_list)))  ; pulls out the layer name
	(setvar "clayer" l_name)		; sets the system variable
	(printc)
)  
   (princ"\nloaded...type <sl> to run.")
   (princ)
