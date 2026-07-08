;
; ****************** RELEASE 13 AND EARLIER ONLY!!!! *************
;
;    Written by: Art Whitton 
;    Description:  This program will enable the user to pick on a 2D polyline
;                       and have the cumulative distances displayed above each
;                       vertex of the polyline. (command : pdist )
;                       This program uses the setup and go_away functions to 
;                       clean up the program.  The *error* function is temporarily
;                       replaced with a smoother function to exit.
;                       Does not work with lightweight polylines.
;
;    Required Functions:  setup       : collects existing system variables
;                                      guts         : the main functioning part of the program
;                                      go_away : a clean exit from the program restores sysvars
;                                      newerror : creates a new error message function         
;
;    Variable Listing: all variables are contained in the separate functions
;
;******************************************************************************
;
;******************************************************************************
;**************************  GUTS OF THE PROGRAM  *****************
;******************************************************************************
;    Description:  This function is the main part of the program
;
;    Required Functions:  none
;
;    Variable Listing:
;    
;  name		scope	type	desctription
;
;  curr_style	local	real	current text style
;  ent_list		local	list	listing of entity properties (reused)
;  ent_name	local	name	entity name (reused)
;  seg_leng	local	real	length of pline segment (added to)
;  style_hite	local	real	text height 
;  style_list	local	list	list of current textstyle properties
;  txt_len		local 	real	length of pline to print to graphics screen
;  txt_pt		local	list	point for text placement (re-used)
;  upby		local	real	1/2 of style_hite for offset of text from vertex
;  vert1		local	real	current first vertex (re-used)
;  vert2		local	real 	current second vertex (re-used)
;
(defun guts(/ curr_style ent_list ent_name seg_leng style_hite style_list txt_len txt_pt upby vert1 vert2)
   ;
   ;*****************************  ENTITY SELECTION SECTION  *************************************
   ;
   (setq seg_leng 0)
   (setq ent_name (entsel "\nPick a 2D polyline entity as your source : "))
   (while (or (= ent_name nil) (/= (cdr (assoc 0 (entget (car ent_name)))) "POLYLINE"))
      (setq ent_name (entsel "\n Pick a 2D POLYLINE entity as your source : "))
   )
   ;
   ;****************************  GET FIRST VERTEX, TEXT HEIGHT AND PRINT TEXT   ****************************
   ;   
   (setq ent_name (entnext (car ent_name)))
   (setq ent_list (entget ent_name))
   (setq vert1 (cdr (assoc 10 ent_list)))
   
   (setq txt_len (rtos seg_leng))
   
   (setq curr_style (getvar "TEXTSTYLE"))
   (setq style_list (tblsearch "STYLE" curr_style))
   (setq style_hite (cdr (assoc 40 style_list)))
   (if (= style_hite 0)
      (progn
         (princ "\nText height has not been defined.")
         (initget (+ 1 2 4))
         (setq style_hite (getdist "\nPlease enter a text height: "))
      )
   )
   (setq upby (/ style_hite 2))
   (setq txt_pt (list (car vert1) (+ (cadr vert1) upby)))
   (command "text" "j" "bc" txt_pt style_hite "" txt_len)
   ;
   ;***********************  LOOP THROUGH VERTICES UNTIL "SEQEND" IS FOUND  **********
   ;
   (while (/=  (cdr (assoc 0 (entget ent_name))) "SEQEND")
      (setq ent_list (entget ent_name))
      (setq vert2 (cdr (assoc 10 ent_list)))
      (setq seg_leng (+ seg_leng (distance vert1 vert2)))
      (setq txt_len (rtos seg_leng))
      (setq txt_pt (list (car vert2) (+ (cadr vert2) upby)))
      (command "_.text" "j" "bc" txt_pt style_hite "" txt_len)
      (setq vert1 vert2)
      (setq ent_name (entnext ent_name))
   )
)
;******************************************************************************
;*************************  SETUP FUNCTION  ******************************
;******************************************************************************
;    Description:  This function stores the sysvars
;
;    Required Functions:  newerror
;
;    Variable Listing:
;
;  errno		global	real	temp for sysvar ERRORNO  
;  old_blips	global	real	storage for sysvar BLIPMODE
;  old_echo	global	real	storage for sysvar CMDECHO
;  old_osnap	global	real	storage for sysvar OSMODE
;  olderr		global	function	storage for *error* function
;********************************************************************************
(defun setup()
   (graphscr)
   (setq olderr *error*)
   (setq *error* newerror)
   (command "_.undo" "begin")
   (setq old_echo (getvar "cmdecho"))
   (setvar "cmdecho" 0)
   (setq old_blips (getvar "blipmode"))
   (setvar "blipmode" 0)
   (setq old_osnap (getvar "osmode"))
   (setvar "osmode" 0)
)
;*********************************************************************************
;*************************  SMOOTH EXIT FUNCTION  **********************
;*********************************************************************************
;    Description:  This function restores the sysvars
;
;    Required Functions: none
;
;    Variable Listing:
;
;  old_blips	global	real	storage for sysvar BLIPMODE
;  old_echo	global	real	storage for sysvar CMDECHO
;  old_osnap	global	real	storage for sysvar OSMODE
;  olderr		global	function	storage for *error* function

(defun go_away()
   (command "_.undo" "end")
   (setvar "cmdecho" old_echo)
   (setvar "blipmode" old_blips)
   (setvar "osmode" old_osnap)
   (setq *error* olderr)
   (setq old_echo nil old_osnap nil old_blips nil olderr nil)
)
;***************************************************************************************
;
;***************************************************************************************
;*************************  ERROR REPLACEMENT FUNCTION  ****************
;***************************************************************************************
;
;    Description:  This function replaces *error* with a defined smoother error
;
;    Required Functions: go_away
;
;    Variable Listing:
;
(defun newerror (msg)
   (command)
   (command)
   (if (and (/= msg "Function cancelled") (/= msg "console break"))
      (progn
         (princ "\n Program error - ")
         (princ msg)
         (setq errno (getvar "ERRNO"))
         (princ (strcat "\nError # " (itoa errno)))
      )
   )
   (go_away)
)
;*********************************************************************************
;*************************  BEGIN THE PROGRAM  *************************
;*********************************************************************************

(defun c:pdist (/ curr_style ent_list ent_name seg_leng style_hite style_list txt_len txt_pt upby vert1 vert2)
   (setup)
   (guts)
   (go_away)
   (princ)
)
(princ "Loaded...type <pdist> to run.")
(princ)








