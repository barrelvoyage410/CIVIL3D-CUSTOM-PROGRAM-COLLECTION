;;This program copies a selected object to be extruded and places
;;it onto a layer called profile and then extrudes the original so the 
;;user always has a backup of the extrusion in wireframe.

(defun c:ext ( / hgt tapeang object copylist copylist1 oldlayer newlayer )

 (setup)
   (setq object nil)
   (while (= object nil)
      (setq object (car (entsel "\nSelect object to be extruded.")))
   ) ;end while  
  (setq hgt (getreal "\nEnter height of extrusion "))
  (setq tapeang (getreal "\nEnter extrusion taper angle  "))
   
   (command "copy" object "" "0,0,0"  "0,0,0" )

   (setq copylist (entget (entlast)))

   (setq newlayer (cons 8 "PROFILE"))
   (setq oldlayer (assoc 8 copylist))
   (setq copylist1 (subst newlayer oldlayer copylist))
   (entmod copylist1)

   
	(command "extrude" object "" hgt tapeang )
	
	
  (shutdown)
  (princ)

);end of defun

(princ "\n Loaded - type ext to run")
(princ)		;load clean

;end of program.























;;*************************************
;Auxillary functions 
;;*************************************

;;*************************************
;Error handeler "err"
;;*************************************

(defun err (msg)
   (command) ;cancel running commands
   (command) 
   (if (and(/= msg "Function cancelled")(/= "console break"))
     (princ (strcat "\nError: " msg))
   )
   (shutdown) ;run shutdown function
)                        

;;*************************************
;Setvar function "setup"
;;*************************************
(defun setup ()
   (command "_.undo" "begin")
   (setq oldCmdEcho (getvar "cmdecho"))
   (setvar "cmdecho" 0)
  (setq oldErr  *error*
        *error* err
        blip    (getvar "blipmode"))
   (setvar "blipmode" 0)
   (princ)
)
;;*************************************
;Reset setvar function "shutdown"
;;*************************************

(defun shutdown ()		; close down 
   (setvar "cmdecho" oldCmdEcho)
   (setq *error* oldErr)
   (setvar "blipmode" blip)
   (command "_.undo" "end")
   (setq oldErr nil
         blip nil
         oldCmdEcho nil) ;free memory used by globals
   (princ) 
)










 







