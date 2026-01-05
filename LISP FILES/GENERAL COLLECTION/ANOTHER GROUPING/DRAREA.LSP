;;; By Mark Evinger            CIS ID 73162,1676                   02-26-1999;;;
;;; Copyright © 1999-2001 Cowhey Gudmundson Leder, Ltd.                      ;;;
;;;                                                                          ;;;
;;;                    Mark.Evinger@cgl-ltd.com                              ;;;
;;;                     http://www.cgl-ltd.com                               ;;;
;;;                                                                          ;;;
;;; Inserts CGL standard Drainage Area block, prompting for                  ;;;
;;; Area Number, Area in Acres, and Time of Concentration in minutes.        ;;;
;;;--------------------------------------------------------------------------;;;
;;; revised 03/19/99 10:47:19 AM to include A= and Tc= in front of area and  ;;;
;;; time of concentration callouts.                                          ;;;
;;;                                                                          ;;;
;;; revised 1:54 PM 4/9/2001 to calculate area from a closed polyline in dwg ;;;
;;;                                                                          ;;;
;;;                                                                          ;;;
;;;                                                                          ;;;
;;; revised 03-20-2007 to check for serial number                            ;;;
;;========================================================================
;;     error handler
;;========================================================================
(defun DRAREA_ERR (S) ;If an error (such as CTRL-C) occurs
  (if (/= S "Function cancelled") ;while this command is active...
    (princ (strcat "\nError: " S))
  ) ;_ end if
  (setq *ERROR* OLD_ERR) ;Restore old *error* handler
  (RETURN)
  (princ)
) ; end defun DRAREA_ERR
 ;-------------------------------------------------------------------------


 ;-------------------------------------------------------------------------
(defun DRAREA (/		SCAL		 TEMP_SCAL
	       CGL_SS		CGL_ACRES	 CGL_SQFEET
	       INSP		DR_AREA_NUMBER	 DR_AREA_SIZE
	       DR_AREA_TIME_INTEGER		 DR_AREA_TIME_START
	       DR_AREA_TIME
	      )

 ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  (setq	OLD_ERR	*ERROR* ; Use special error handling function.
	*ERROR*	DRAREA_ERR
  ) ;_ end setq
 ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

 ;---------------------------------------------------------------------------
 ;  get drawing scale -  this is designed to use Softdesk var userr2 for
 ;  horizontal scale

 ;test for USERR2 not equal to LTSCALE, get user input if not equal
  (setq SCAL (getvar "USERR2"))
  (setq TEMP_SCAL (getvar "LTSCALE"))
  (cond
    ((/= SCAL TEMP_SCAL)
     (setq SCAL	(getreal (strcat "\nEnter HORIZONTAL scale: >>"
				 (rtos (getvar "ltscale") 2 1)
				 "<< "
			 ) ;_ end strcat
		) ;_ end getreal
     ) ;_ end setq
     (if (null SCAL)
       (setq SCAL (getvar "LTSCALE"))
     ) ;end if
     (setvar "USERR2" SCAL)
    ) ;end cond HSCAL /= LTSCAL
  ) ;end cond

  (if (or (= (getvar "USERR2") NIL) (= (getvar "USERR2") 0))
    (progn
      (initget (+ 1 2 4))
      (setvar
	"USERR2"
	(getreal "\n Drawing scale not found. Enter current scale: "
	) ;_ end getreal
      ) ;_ end setvar
    ) ;_ end progn
  ) ;_ end if
  (setq SCAL (getvar "userr2"))
 ;---------------------------------------------------------------------------
  (setq DR_AREA_TIME_START 15); set initial time of concentration
  (while (setq CGL_SS (entsel "\n Select closed polyline drainage area:")); if there is a pick, do something
    (progn
      (command "AREA" "o" CGL_SS)
    )
    (setq CGL_ACRES (/ (setq CGL_SQFEET (/ (getvar "AREA") 1)) 43560.0)) ;convert square feet to acres
    (princ)
    (setq INSP (getpoint "\n Select DRAREA block insertion point: "))
    (setq DR_AREA_NUMBER
	   (strcat "No. "
		   (getstring t "\n Enter Drainage Area Number: ")
	   ) ;_ end strcat
    ) ;_ end setq

    (setq DR_AREA_SIZE
	   (strcat "A="
		   (rtos CGL_ACRES 2 2)
		   " ac."
	   ) ;_ end strcat
    ) ;_ end setq

 ;(initget 5)
    (setq DR_AREA_TIME_INTEGER
	   (getint (strcat "\n Enter the Time of Concentration in minutes: <" (itoa DR_AREA_TIME_START) "> "))
    );end setq
    (if	(= DR_AREA_TIME_INTEGER NIL)
      (setq DR_AREA_TIME_INTEGER DR_AREA_TIME_START)
      (setq DR_AREA_TIME_INTEGER DR_AREA_TIME_INTEGER
	    DR_AREA_TIME_START   DR_AREA_TIME_INTEGER
      );end setq
    ) ;end if

    (setq DR_AREA_TIME
	   (strcat "Tc=" (itoa DR_AREA_TIME_INTEGER) " min.") ;_ end strcat
    ) ;_ end setq

    (command "._INSERT"
	     "DRAREA"
	     INSP
	     (rtos SCAL 2 2)
	     ""
	     "0"
	     DR_AREA_NUMBER
	     DR_AREA_SIZE
	     DR_AREA_TIME
    ) ;_ end command
  ) ;_ end while
  (princ)

) ; end defun drarea
(princ)
(defun C:DRAREA ()
  (if (cgl_check_serial)
    (progn
  (vmon)
  (DRAREA)
    );end checker progn
    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
    );end if
    (princ)
  )
(princ "\n\tc:DRAREA loaded. Start command with DRAREA.")
(princ)