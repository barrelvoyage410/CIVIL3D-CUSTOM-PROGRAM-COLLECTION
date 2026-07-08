
;
;    Written by: Art Whitton
;    Date: November 27, 1996
;
;    Description:  This program will allow the user to select an existing text
;                  from the screen and then select text using a selection set
;                  the change this text to the same height and style of the 
;                  original source text.
;
;    Required Functions:  None
;         
;    Variable Listing:
;
; Name       Scope     Type      Description
; ent_name   Local     e data    the original pick's name and location
; sample     Local     e name    the source text's entity name
; samp_list  Local     data      the source text's entity data
; hite       Local     real      the source text's height
; stile      Local     string    the source text's style
; sel_set    Local     sel/set   the selection set for the target text entities
; set_size   Local     real      the size of the selection set (all entities)
; index      Local     real      an index for looping through the sel/set
; counter    Local     real      a counter to update the screen entity number
; old_hite   Local     real      the height of the target text
; old_style  Local     string    the style of the target text
; new_pair1  Local     dotted pr new dotted pair for the target text's height
; new_pair2  Local     dotted pr new dotted pair for the target text's style
; new_list1  Local     data      new data including new height
; new_list2  Local     data      new data including new height and style
;
;***********************************************************************
;
;*************************  BEGIN THE PROGRAM  *************************
;
(defun c:ct ();/ sample samp_list hite stile sel_set set_size index counter old_hite old_style new_pair1 new_pair2 new_list1 new_list2)
   (setq ent_name (entsel "\nPick a text entity as your source : "))
   (while (or (= ent_name nil) (/= (cdr (assoc 0 (entget (car ent_name)))) "TEXT"))
      (setq ent_name (entsel "\n Pick a TEXT entity as your source : "))
   )
   (setq sample (car ent_name))
   (setq samp_list (entget sample))                ;GET THE DATA LIST OF SAMPLE TEXT
   (setq hite (cdr (assoc 40 samp_list)))          ;PULL OUT THE SAMPLE HEIGHT
   (setq stile (cdr (assoc 7 samp_list)))          ;PULL OUT THE SAMPLE STYLE 
   (princ "\nThis entity has a height of ")        ;DISPLAY THE SAMPLE HEIGHT / STYLE
   (princ hite)
   (princ " and is the style: ")
   (princ stile)
   (princ ".")
;
;************************  GET SELECTION SET  **************************
;
   (setq sel_set (ssget))                          ;GET THE SELECTION SET
   (setq set_size (sslength sel_set))              ;GET THE SIZE OF THE SEL/SET
   (setq index 0)
   (setq counter (1+ index ))                      ;SET COUNTER FOR COMMAND LINE DISPLAY  
   (repeat set_size                                ;BEGIN LOOP 
      (princ " Processing entity #")               ;UPDATE COMMAND LINE DISPLAY 
         (princ counter) (princ "\r")
      (setq ent_list (entget (ssname sel_set index)))  ;GET DATA FROM ENTITY
      (setq index (1+ index))                    
      (setq counter (1+ counter))
      (setq ent_type (assoc 0 ent_list))               ;GET ENTITY TYPE FROM DATA
      (if (= "TEXT" (cdr ent_type))                    ;IF TYPE = "TEXT" BEGIN LOOP
         (progn
          (setq old_hite (assoc 40 ent_list))          ;GET OLD HEIGHT
          (setq old_style (assoc 7 ent_list))          ;GET OLD STYLE
          (setq new_pair1 (cons (car old_hite) hite))  ;CONSTRUCT NEW DOT.PR FOR HEIGHT
          (setq new_pair2 (cons (car old_style) stile));CONSTRUCT NEW DOR.PR FOR STYLE 
          (setq new_list1 (subst new_pair1 old_hite ent_list)) ;SUB-IN ENTITY DATA (HEIGHT)
          (setq new_list2 (subst new_pair2 old_style new_list1)) ;SUB-IN ENTITY DATA (STYLE)
          (entmod new_list2)                            ;UPDATE DATABASE W/ NEW ENTITY LIST  
          )
      )
   )
   (princ)
)
   (princ "Loaded...type <CT> to run.")
   (princ)









