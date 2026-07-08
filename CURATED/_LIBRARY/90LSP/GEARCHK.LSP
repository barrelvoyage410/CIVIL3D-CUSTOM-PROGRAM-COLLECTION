;*************************************************************************
;
;			    GEARCHK.LSP
;
;	      (c) Copyright 1988, All rights reserved.
;	      Frontier Automation Inc.	 October 1988
;
;		     Written by Pete Giannis
;		      Arlington Heights, IL
;
;*************************************************************************


(defun C:GEARCHK(/ scenes ctr slide name mvi gear center_g theta_g teeth_g
					       pinion center_p theta_p teeth_p )

   (setq name (getstring "\nEnter the name of the file: " ) )
   (setq scenes (getint "\nEnter number of scenes: " ) )

   (setq mvi (open (strcat name ".mvi") "w") )

   (prompt "Select the first gear: ")
   (setq gear (ssget))
   (setq teeth_g (getreal "\nEnter number of teeth: " ) )
   (setq center_g (getpoint "Enter the center of the gear: ") )
   (setq theta_g (/ (/ 360.00 teeth_g) (float scenes) ) )

   (prompt "Select the mating gear: ")
   (setq pinion (ssget))
   (setq teeth_p (getreal "\nEnter number of teeth: " ) )
   (setq center_p (getpoint "Enter the center of the gear: ") )
   (setq theta_p (/ (/ -360.00 teeth_p) (float scenes) ) )

   (setq ctr 1)

   (while (<= ctr scenes)
	(setq slide (strcat name (itoa ctr) ) )
        (command "mslide" slide)
	(write-line slide mvi)
        (command "rotate" gear "" center_g theta_g)
        (command "rotate" pinion "" center_p theta_p)

	(setq ctr (1+ ctr) )
   )

   (close mvi)
)
