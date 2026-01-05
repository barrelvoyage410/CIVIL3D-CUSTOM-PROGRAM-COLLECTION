;;;-----------------------------------------------------------------------;;;
;;;                   Original lisp provided by Julian Laskey             ;;;
;;;                         Modified by Mark Evinger                      ;;;
;;;-----------------------------------------------------------------------;;;
;;;revised 11:10:37 Tuesday, 01-14-1997 to include all existing structures;;;
;;;and added () around ALL invert descriptions                            ;;;
;;;  For more information, contact acad_guru@compuserve.com               ;;;
;;;                                                                       ;;;
;;; revised 07/24/98 4:35:22 PM to check for USERR2 /= LTSCALE for R14    ;;;v1.22
;;;                                                                       ;;;
;;; revised 08/26/98 4:00:14 PM to keep the order on PRofile Storm mh's   ;;;v1.24
;;; consistent.  If there is an open lid call to the manhole, it will     ;;;
;;; always appear as such:        STM MH No. 123                          ;;;
;;;                               TYPE A (48" DIA)                        ;;;
;;;                               WITH OPEN LID                           ;;;
;;;                               RIM= XXX.XX                             ;;;
;;;                               INV= XXX.XX (DESC)                      ;;;
;;;                               INV= XXX.XX (DESC)                      ;;;
;;;                               INV= XXX.XX (DESC)                      ;;;
;;;                                                                       ;;;
;;; Any manhole with open lid call can only have 3 inverts max.  Method   ;;;
;;; for getting "WITH OPEN LID" is to give "ol" as answer to 2nd, 3rd or  ;;;
;;; 4th invert elevation.                                                 ;;;
;;;                                                                       ;;;
;;; revised 03/19/99 11:14:00 AM to include the option of W/GRATING on    ;;;v1.25
;;; the Flared End Section callout.  Also added Concrete Headwall option  ;;;
;;;                                                                       ;;;
;;; revised 5/13/99 1:45:50 PM to account for ATTDIA system variable.     ;;;v1.26
;;; program will check ATTDIA and turn it off (set to 0) then return the  ;;;
;;; variable to original state if cancelled or at end of routine.         ;;;
;;;                                                                       ;;;
;;;                                                                       ;;;
;;; revised 5/6/05 1:14 PM to shrink existing callouts to 0.75x of size,  ;;;v1.28
;;; making them plot at L60 size instead of L80.  For LDT2005 rollout.    ;;;
;;;                                                                       ;;;
;;; revised 3/9/06 4:14 PM to att ATTREQ=1 to the Modesave list, forcing  ;;;
;;; Autocad to actually use the attributes as filled out by the user.     ;;;v1.30
;;;                                                                       ;;;
;;; revised 03-20-2007 to check for serial number                         ;;;v1.32
(defun C:PPTXT ()
;  (if (cgl_check_serial)
;    (progn
  (setq	VERSION	"1.32"
	REDRAW-COMMAND
	 ".REDRAWALL"
  ) ;_ end setq
  (princ
    (strcat
      "\r PPTXT version "
      VERSION
      " -- copyright © Cowhey Gudmundson Leder, Ltd. 1993-2007. "
    ) ;_ end strcat
  ) ;_ end princ
;;; This bit is designed more for a dual monitor system, as the version will
;;; scroll off a three line display....

 ;---------------------------------------------------------------------------
 ; save specified mode settings
 ;---------------------------------------------------------------------------
  (defun MODESAVE (A_A)
    (setq MLST '())
    (repeat (length A_A)
      (setq
	MLST (append MLST (list (list (car A_A) (getvar (car A_A)))))
      ) ;_ end setq
      (setq A_A (cdr A_A))
    ) ;_ end repeat
  ) ;_ end defun
 ;---------------------------------------------------------------------------
 ; restore saved mode settings
 ;---------------------------------------------------------------------------
  (defun MODER ()
    (repeat (length MLST)
      (setvar (caar MLST) (cadar MLST))
      (setq MLST (cdr MLST))
    ) ;_ end repeat
  ) ;_ end defun
 ;---------------------------------------------------------------------------
 ;error handler
 ;---------------------------------------------------------------------------
  (defun PPTXT_ERR (S) ;If an error (such as CTRL-C) occurs while this command is active...
    (if	(/= S "Function cancelled")
      (princ (strcat "\nError: " S))
    ) ;_ end if
    (setq *ERROR* OLD_ERR) ;Restore old *error* handler
    (MODER) ;return modes to old values
    (princ)
  ) ;_ end defun
 ;---------------------------------------------------------------------------;
  (defun QTYPSIZ ()
    (if	(not (null TYP))
      (progn
	(setq TYPX (getstring t (strcat "\n   TYPE: >> " TYP " << ")))
	(if (/= TYPX "")
	  (setq TYP TYPX)
	) ;_ end if
      ) ;_ end progn
      (setq TYP (getstring t "\n   TYPE: "))
    ) ;_ end if
    (if	(not (null SIZ))
      (progn
	(setq SIZX
	       (getstring t
			  (strcat "\n   DIA in inches: >> " SIZ " << ")
	       ) ;_ end getstring
	) ;_ end setq
	(if (/= SIZX "")
	  (setq SIZ SIZX)
	) ;_ end if
      ) ;_ end progn
      (setq SIZ (getstring t "\n   DIA in inches: "))
    ) ;_ end if
  ) ;_ end defun
 ;---------------------------------------------------------------------------;
  (defun QSTAOFF ()
    (setq STA (getstring t "\n   Station: "))
    (if	(not (null OFST))
      (progn
	(setq OFSTX
	       (getstring t
			  (strcat "\n   Offset dist: >> " OFST " << ")
	       ) ;_ end getstring
	) ;_ end setq
	(if (/= OFSTX "")
	  (setq OFST OFSTX)
	) ;_ end if
      ) ;_ end progn
      (setq OFST (getstring t "\n   Offset dist: "))
    ) ;_ end if
    (if	(not (null LR))
      (progn
	(setq LRX (getstring t (strcat "\n   L or R: >> " LR " << ")))
	(if (/= LRX "")
	  (setq LR LRX)
	) ;_ end if
      ) ;_ end progn
      (setq LR (getstring t "\n   L or R: "))
    ) ;_ end if
  ) ;_ end defun
 ;---------------------------------------------------------------------------;
  (defun QRIMINV ()
    (setq DESC ""
	  DESC2	""
	  DESC3	""
	  DESC4	""
	  DESC_! ""
    ) ;_ end setq
    (setq RIM (getstring t "\n   RIM elevation: "))
    (setq INV (getstring t "\n   INV elevation: "))
    (setq DESC_! (getstring t "\n   INV description (optional): "))
;;;    (cond
;;;      ((= DESC_! "OL")
;;;       (setq DESC_! "WITH OPEN LID")
;;;      ) ; end OL
;;;      ((= DESC_! "CL")
;;;       (setq DESC_! "WITH CLOSED LID")
;;;      ) ; end CL
;;;    ) ; end cond
    (if	(= DESC_! "")
      (setq DESC DESC_!
	    DESC_! ""
      ) ;_ end setq
      (setq DESC   (strcat " (" DESC_! ")")
	    DESC_! ""
      ) ;_ end setq
    ) ;end if
    (setq INV2 ""
	  INV3 ""
	  INV4 ""
    ) ;_ end setq
    (setq INV2 (getstring t "\n   2nd INV elevation (optional): "))
    (if	(/= INV2 "")
      (progn
	(setq DESC_!
	       (getstring t
			  "\n   description for 2nd INV (optional): "
	       ) ;_ end getstring
	) ;_ end setq
	(if (= DESC_! "")
	  (setq	DESC2  DESC_!
		DESC_! ""
	  ) ;_ end setq
	  (setq	DESC2  (strcat " (" DESC_! ")")
		DESC_! ""
	  ) ;_ end setq
	) ;end if
	(setq INV3 (getstring t "\n   3rd INV elevation (optional): "))
	(if (/= INV3 "")
	  (progn
	    (setq DESC_! (getstring
			   t
			   "\n   description for 3rd INV (optional): "
			 ) ;_ end getstring
	    ) ;_ end setq
	    (if	(= DESC_! "")
	      (setq DESC3  DESC_!
		    DESC_! ""
	      ) ;_ end setq
	      (setq DESC3  (strcat " (" DESC_! ")")
		    DESC_! ""
	      ) ;_ end setq
	    ) ;end if
	    (setq
	      INV4 (getstring t "\n   4th INV elevation (optional): ")
	    ) ;_ end setq
	    (if	(/= INV4 "")
	      (progn
		(setq
		  DESC_! (getstring
			   t
			   "\n   description for 4th INV (optional): "
			 ) ;_ end getstring
		) ;_ end setq
		(if (= DESC_! "")
		  (setq	DESC4  DESC_!
			DESC_! ""
		  ) ;_ end setq
		  (setq	DESC4  (strcat " (" DESC_! ")")
			DESC_! ""
		  ) ;_ end setq
		) ;end progn
	      ) ;end if
	    ) ;end inv4 optional if
	  ) ;_ end progn
	) ;_ end if
      ) ;_ end progn
    ) ;_ end if
  ) ;end qriminv
 ;---------------------------------------------------------------------------;
  (defun PL_PR_EX () ;decides is it PLan, PRofile, or EXisting text.
    (print)
    (if	(null OPT2)
      (setq OPT2 "PL")
    ) ;_ end if
    (initget "PL PR EX")
    (setq
      ANS2 (getkword
	     (strcat "\n   (PL)an ,(PR)ofile or (EX)isting text?  >> "
		     OPT2
		     " << "
	     ) ;_ end strcat
	   ) ;_ end getkword
    ) ;_ end setq
    (if	(not (null ANS2))
      (setq OPT2 ANS2)
    ) ;_ end if
    (print)
  ) ;end defun PL_PR_EX
 ;---------------------------------------------------------------------------;
  (setq TYP "")
  (setq OFST "")
  (setq STA "")
  (setq RIM "")
  (setq INV "")
  (setq DESC "")
  (setq FESIZ "")

  (setvar "CMDECHO" 0)
 ;---------------------------------------------------------------------------;
  ;;The scale routine is coded for SOFTDESK products (dca, cogo, design, etc.)
  ;;If the var is not set, it will ask for it to be set
 ;---------------------------------------------------------------------------;

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
  (setq EX_SCAL (* 0.75 SCAL))
 ;---------------------------------------------------------------------------;
 ;           Begin Main Routine                                              ;
 ;---------------------------------------------------------------------------;
  (print)
  (setq CURRENT_LAYER (getvar "CLAYER"))
  (setq CGL_OSMODE (getvar "OSMODE"))

  (setq	OLD_ERR	*ERROR* ; use special error handling function
	*ERROR*	PPTXT_ERR
  ) ;_ end setq

  (MODESAVE '("BLIPMODE"   "CMDECHO"	"COORDS"     "DIMZIN" "ATTREQ"
	      "GRIPS"	   "OSMODE"	"AUNITS"     "ATTDIA"
	     )
  ) ;save modes
  (setvar "BLIPMODE" 0) ;turn off blips
  (setvar "CMDECHO" 0) ;turn off command echo
  (setvar "COORDS" 2) ;sets coords to show distance & angle
  (setvar "DIMZIN" 0) ;turn off dimzin (controls leading/trailing 0's)
  (setvar "GRIPS" 0) ;turn off grips
  (setvar "OSMODE" 0) ;turn off osnaps
  (setvar "AUNITS" 0) ;set angular units to degrees only
  (setvar "ATTDIA" 0) ;turn off attribute dialog boxes for data entry
  (setvar "ATTREQ" 1) ;request attributes be filled in when inserting blocks - added 03/09/2006 4:14 PM

 ;---------------------------------------------------------------------------;
  (setq OPT "X")
  (while (not (null OPT))
    (prompt
      "\n Indicate structure type for descriptive text or e(X)it: "
    ) ;_ end prompt
    (initget "SA ST CB IN CL FE VV FH LP CH X")
    (setq
      ANS
       (getkword
	 (strcat
	   "\n(ST)orm mh (CB)asin (IN)let (CL)eanout (FE)s (CH)eadwall"
	   " \n  (SA)n mh (VV)ault (FH)ydrant (LP)ole:>>"
	   OPT
	   "<< "
	 ) ;_ end strcat
       ) ;_ end getkword
    ) ;_ end setq
    (if	(not (null ANS))
      (setq OPT ANS)
    ) ;_ end if
 ;---------------------------------------------------------------------------;
    (cond
      ((= OPT "X")
       (setq OPT NIL)
      )
 ;---------------------------------------------------------------------------;
      ((= OPT "SA")
       (PL_PR_EX)
       (cond
	 ((= OPT2 "PL")
	  (setq NUM (getstring t "\n   SAN MH No.: "))
	  (QSTAOFF)
	  (setq ATT1 (strcat "SAN MH No. " NUM))
	  (setq
	    ATT2 (strcat "STA. " STA ", " OFST "'" (strcase LR) "T.")
	  ) ;_ end setq
	  (initget 1)
	  (setq INSP (getpoint "\n Select text insertion point: "))
	  (command "._INSERT"
		   "STMPLN"
		   INSP
		   (rtos SCAL 2 2)
		   ""
		   "0"
		   ATT1
		   ATT2
	  ) ;_ end command
	  (setq	ATT1 ""
		ATT2 ""
	  ) ;_ end setq
	 ) ;opt2 PL
	 ((= OPT2 "PR")
	  (setq NUM (getstring t "\n   SAN MH No.: "))
	  (QTYPSIZ)
	  (QRIMINV)
	  (setq ATT1 (strcat "SAN MH No. " NUM))
	  (setq ATT2 (strcat "TYPE " TYP " (" SIZ (chr 34) "DIA)"))
	  (setq ATT3 (strcat "RIM= " RIM))
	  (setq ATT4 (strcat "INV= " INV DESC))
	  (if (/= INV2 "")
	    (setq ATT5 (strcat "INV= " INV2 DESC2))
	    (setq ATT5 "")
	  ) ;_ end if
	  (if (/= INV3 "")
	    (setq ATT6 (strcat "INV= " INV3 DESC3))
	    (setq ATT6 "")
	  ) ;_ end if
	  (if (/= INV4 "")
	    (setq ATT7 (strcat "INV= " INV4 DESC4))
	    (setq ATT7 "")
	  ) ;_ end if
	  (initget 1)
	  (setq INSP (getpoint "\n Select text insertion point: "))
	  (command "._INSERT"
		   "SANPRF"
		   INSP
		   (rtos SCAL 2 2)
		   ""
		   "0"
		   ATT1
		   ATT2
		   ATT3
		   ATT4
		   ATT5
		   ATT6
		   ATT7
	  ) ;_ end command
	  (setq	ATT1 ""
		ATT2 ""
		ATT3 ""
		ATT4 ""
		ATT5 ""
		ATT6 ""
		ATT7 ""
	  ) ;_ end setq
	 ) ; opt2 PR
	 ((= OPT2 "EX")
	  (QRIMINV)
	  (setq ATT1 "EX. SAN MH")
	  (setq ATT2 "")
	  (setq ATT3 (strcat "RIM= " RIM))
	  (setq ATT4 (strcat "INV= " INV DESC))
	  (if (/= INV2 "")
	    (setq ATT5 (strcat "INV= " INV2 DESC2))
	    (setq ATT5 "")
	  ) ;_ end if
	  (if (/= INV3 "")
	    (setq ATT6 (strcat "INV= " INV3 DESC3))
	    (setq ATT6 "")
	  ) ;_ end if
	  (if (/= INV4 "")
	    (setq ATT7 (strcat "INV= " INV4 DESC4))
	    (setq ATT7 "")
	  ) ;_ end if
	  (initget 1)
	  (setq INSP (getpoint "\n Select text insertion point: "))
	  (command "._INSERT"
		   "SANPRF"
		   INSP
		   (rtos EX_SCAL 2 2)
		   ""
		   "0"
		   ATT1
		   ATT3
		   ATT4
		   ATT5
		   ATT6
		   ATT7
		   ATT2
	  ) ;_ end command
	  (setq	ATT1 ""
		ATT2 ""
		ATT3 ""
		ATT4 ""
		ATT5 ""
		ATT6 ""
		ATT7 ""
	  ) ;_ end setq
	 ) ; opt2 EX
       ) ;_ end cond
      ) ; opt SA
 ;---------------------------------------------------------------------------;
      ((= OPT "ST")
       (PL_PR_EX)
       (cond
	 ((= OPT2 "PL")
	  (setq NUM (getstring t "\n   STM MH No.: "))
	  (QSTAOFF)
	  (setq ATT1 (strcat "STORM MH No. " NUM))
	  (setq
	    ATT2 (strcat "STA. " STA ", " OFST "'" (strcase LR) "T.")
	  ) ;_ end setq
	  (initget 1)
	  (setq INSP (getpoint "\n Select text insertion point: "))
	  (command "._INSERT"
		   "STMPLN"
		   INSP
		   (rtos SCAL 2 2)
		   ""
		   "0"
		   ATT1
		   ATT2
	  ) ;_ end command
	  (setq	ATT1 ""
		ATT2 ""
	  ) ;_ end setq
	 ) ;opt2 PL
	 ((= OPT2 "PR")
	  (setq NUM (getstring t "\n   STM MH No.: "))
	  (QTYPSIZ)
	  (QRIMINV)
	  (setq ATT1 (strcat "STORM MH No. " NUM))
	  (setq ATT2 (strcat "TYPE " TYP " (" SIZ (chr 34) "DIA)"))
	  (setq ATT3 (strcat "RIM= " RIM))
	  (setq ATT4 (strcat "INV= " INV DESC))
	  (if (= (strcase INV2) "OL")
	    (setq ATT5 "WITH OPEN LID")
	    (progn
	      (if (/= INV2 "")
		(setq ATT5 (strcat "INV= " INV2 DESC2))
		(setq ATT5 "")
	      ) ; end if
	    ) ;end progn
	  ) ; end if INV2
	  (if (= (strcase INV3) "OL")
	    (setq ATT6 "WITH OPEN LID")
	    (progn
	      (if (/= INV3 "")
		(setq ATT6 (strcat "INV= " INV3 DESC3))
		(setq ATT6 "")
	      ) ;end if
	    ) ; end progn
	  ) ; end if INV3
	  (if (= (strcase INV4) "OL")
	    (setq ATT7 "WITH OPEN LID")
	    (progn
	      (if (/= INV4 "")
		(setq ATT7 (strcat "INV= " INV4 DESC4))
		(setq ATT7 "")
	      ) ;end if
	    ) ; end progn
	  ) ; end if INV4

;;; TEST FOR EACH INV? ATTRIBUTE TO SEE IF IT IS EXPLICT "WITH OPEN LID"
;;; IF SO, REORDER THE ATTRIBUTES TO THE RIGHT DISPLAY ORDER.
	  (cond
	    ((= ATT5 "WITH OPEN LID")
	     (initget 1)
	     (setq INSP (getpoint "\n Select text insertion point: "))
	     (command "._INSERT"
		      "STMPRF"
		      INSP
		      (rtos SCAL 2 2)
		      ""
		      "0"
		      ATT1
		      ATT2
		      ATT5
		      ATT3
		      ATT4
		      ATT6
		      ATT7
	     ) ;_ end command
	     (setq ATT1	""
		   ATT2	""
		   ATT3	""
		   ATT4	""
		   ATT5	""
		   ATT6	""
		   ATT7	""
	     ) ;_ end setq
	    ) ; inv2=open lid

	    ((= ATT6 "WITH OPEN LID")
	     (initget 1)
	     (setq INSP (getpoint "\n Select text insertion point: "))
	     (command "._INSERT"
		      "STMPRF"
		      INSP
		      (rtos SCAL 2 2)
		      ""
		      "0"
		      ATT1
		      ATT2
		      ATT6
		      ATT3
		      ATT4
		      ATT5
		      ATT7
	     ) ;_ end command
	     (setq ATT1	""
		   ATT2	""
		   ATT3	""
		   ATT4	""
		   ATT5	""
		   ATT6	""
		   ATT7	""
	     ) ;_ end setq
	    ) ; inv3=open lid

	    ((= ATT7 "WITH OPEN LID")
	     (initget 1)
	     (setq INSP (getpoint "\n Select text insertion point: "))
	     (command "._INSERT"
		      "STMPRF"
		      INSP
		      (rtos SCAL 2 2)
		      ""
		      "0"
		      ATT1
		      ATT2
		      ATT7
		      ATT3
		      ATT4
		      ATT5
		      ATT6
	     ) ;_ end command
	     (setq ATT1	""
		   ATT2	""
		   ATT3	""
		   ATT4	""
		   ATT5	""
		   ATT6	""
		   ATT7	""
	     ) ;_ end setq
	    ) ;inv4 = open lid
	    (t
	     (initget 1)
	     (setq INSP (getpoint "\n Select text insertion point: "))
	     (command
	       "._INSERT"
	       "STMPRF"
	       INSP
	       (rtos SCAL 2 2)
	       ""
	       "0"
	       ATT1
	       ATT2
	       ATT3
	       ATT4
	       ATT5
	       ATT6
	       ATT7
	     ) ;_ end command
	     (setq ATT1	""
		   ATT2	""
		   ATT3	""
		   ATT4	""
		   ATT5	""
		   ATT6	""
		   ATT7	""
	     ) ;_ end setq
	    ) ;end if not OL
	  ) ;_ end cond
	 ) ; opt2 PR
	 ((= OPT2 "EX")
	  (QRIMINV)
	  (setq ATT1 "EX. STORM MH")
	  (setq ATT2 "")
	  (setq ATT3 (strcat "RIM= " RIM))
	  (setq ATT4 (strcat "INV= " INV DESC))
	  (if (/= INV2 "")
	    (setq ATT5 (strcat "INV= " INV2 DESC2))
	    (setq ATT5 "")
	  ) ;_ end if
	  (if (/= INV3 "")
	    (setq ATT6 (strcat "INV= " INV3 DESC3))
	    (setq ATT6 "")
	  ) ;_ end if
	  (if (/= INV4 "")
	    (setq ATT7 (strcat "INV= " INV4 DESC4))
	    (setq ATT7 "")
	  ) ;_ end if
	  (initget 1)
	  (setq INSP (getpoint "\n Select text insertion point: "))
	  (command "._INSERT"
		   "SANPRF"
		   INSP
		   (rtos EX_SCAL 2 2)
		   ""
		   "0"
		   ATT1
		   ATT3
		   ATT4
		   ATT5
		   ATT6
		   ATT7
		   ATT2
	  ) ;_ end command
	  (setq	ATT1 ""
		ATT2 ""
		ATT3 ""
		ATT4 ""
		ATT5 ""
		ATT6 ""
		ATT7 ""
	  ) ;_ end setq
	 ) ; opt2 EX
       ) ;cond opt2
      ) ;opt ST
 ;---------------------------------------------------------------------------;
      ((= OPT "CB")
       (PL_PR_EX)
       (cond
	 ((= OPT2 "PL")
	  (setq NUM (getstring t "\n   CATCH BASIN No.: "))
	  (setq OFST NIL)
	  (QSTAOFF)
	  (QTYPSIZ)
	  (QRIMINV)
	  (setq ATT1 (strcat "STORM C.B. No. " NUM))
	  (if (/= OFST "")
	    (setq ATT2
		   (strcat "STA. " STA ", " OFST "'" (strcase LR) "T.")
	    ) ;_ end setq
	    (setq ATT2 (strcat "STA. " STA ", " (strcase LR) "T."))
	  ) ;_ end if
	  (setq ATT3 (strcat "TYPE " TYP " (" SIZ (chr 34) "DIA)"))
	  (setq ATT4 (strcat "RIM= " RIM))
	  (setq ATT5 (strcat "INV= " INV DESC))
	  (if (/= INV2 "")
	    (setq ATT6 (strcat "INV= " INV2 DESC2))
	    (setq ATT6 "")
	  ) ;_ end if
	  (if (/= INV3 "")
	    (setq ATT7 (strcat "INV= " INV3 DESC3))
	    (setq ATT7 "")
	  ) ;_ end if
	  (if (/= INV4 "")
	    (setq ATT8 (strcat "INV= " INV4 DESC4))
	    (setq ATT8 "")
	  ) ;_ end if
	  (initget 1)
	  (setq INSP (getpoint "\n Select text insertion point: "))
	  (command "._INSERT"
		   "CBTXT"
		   INSP
		   (rtos SCAL 2 2)
		   ""
		   "0"
		   ATT1
		   ATT2
		   ATT3
		   ATT4
		   ATT5
		   ATT6
		   ATT7
		   ATT8
	  ) ;_ end command
	  (setq	ATT1 ""
		ATT2 ""
		ATT3 ""
		ATT4 ""
		ATT5 ""
		ATT6 ""
		ATT7 ""
		ATT8 ""
	  ) ;_ end setq
	 ) ;opt CB PLAN
	 ((= OPT2 "PR")
	  (print)
	  (setq NUM (getstring t "\n  CATCH BASIN No.: "))
	  (setq OFST NIL)
	  (QTYPSIZ)
	  (QRIMINV)
	  (setq ATT1 (strcat "STORM C.B. No. " NUM))
	  (setq ATT2 (strcat "TYPE " TYP " (" SIZ (chr 34) "DIA)"))
	  (setq ATT3 (strcat "RIM= " RIM))
	  (setq ATT4 (strcat "INV= " INV DESC))
	  (if (/= INV2 "")
	    (setq ATT5 (strcat "INV= " INV2 DESC2))
	    (setq ATT5 "")
	  ) ;_ end if
	  (if (/= INV3 "")
	    (setq ATT6 (strcat "INV= " INV3 DESC3))
	    (setq ATT6 "")
	  ) ;_ end if
	  (if (/= INV4 "")
	    (setq ATT7 (strcat "INV= " INV4 DESC4))
	    (setq ATT7 "")
	  ) ;_ end if
	  (setq ATT8 "")
	  (initget 1)
	  (setq INSP (getpoint "\n Select text insertion point: "))
	  (command "._INSERT"
		   "CBTXT"
		   INSP
		   (rtos SCAL 2 2)
		   ""
		   "0"
		   ATT1
		   ATT2
		   ATT3
		   ATT4
		   ATT5
		   ATT6
		   ATT7
		   ATT8
	  ) ;_ end command
	  (setq	ATT1 ""
		ATT2 ""
		ATT3 ""
		ATT4 ""
		ATT5 ""
		ATT6 ""
		ATT7 ""
		ATT8 ""
	  ) ;_ end setq
	 ) ;opt CB PROFILE
	 ((= OPT2 "EX")
	  (QRIMINV)
	  (setq ATT1 "EX. STORM C.B.")
	  (setq ATT2 "")
	  (setq ATT3 (strcat "RIM= " RIM))
	  (setq ATT4 (strcat "INV= " INV DESC))
	  (if (/= INV2 "")
	    (setq ATT5 (strcat "INV= " INV2 DESC2))
	    (setq ATT5 "")
	  ) ;_ end if
	  (if (/= INV3 "")
	    (setq ATT6 (strcat "INV= " INV3 DESC3))
	    (setq ATT6 "")
	  ) ;_ end if
	  (if (/= INV4 "")
	    (setq ATT7 (strcat "INV= " INV4 DESC4))
	    (setq ATT7 "")
	  ) ;_ end if
	  (initget 1)
	  (setq INSP (getpoint "\n Select text insertion point: "))
	  (command "._INSERT"
		   "SANPRF"
		   INSP
		   (rtos EX_SCAL 2 2)
		   ""
		   "0"
		   ATT1
		   ATT3
		   ATT4
		   ATT5
		   ATT6
		   ATT7
		   ATT2
	  ) ;_ end command
	  (setq	ATT1 ""
		ATT2 ""
		ATT3 ""
		ATT4 ""
		ATT5 ""
		ATT6 ""
		ATT7 ""
	  ) ;_ end setq
	 ) ; opt2 EX
       ) ; cond opt2
      ) ; opt CB
 ;---------------------------------------------------------------------------;
      ((= OPT "IN")
       (PL_PR_EX)
       (cond
	 ((= OPT2 "PL")
	  (setq NUM (getstring t "\n   INLET No.: "))
	  (setq OFST NIL)
	  (QSTAOFF)
	  (QTYPSIZ)
	  (QRIMINV)
	  (setq ATT1 (strcat "STORM INLET No. " NUM))
	  (if (/= OFST "")
	    (setq ATT2
		   (strcat "STA. " STA ", " OFST "'" (strcase LR) "T.")
	    ) ;_ end setq
	    (setq ATT2 (strcat "STA. " STA ", " (strcase LR) "T."))
	  ) ;_ end if
	  (setq ATT3 (strcat "TYPE " TYP " (" SIZ (chr 34) "DIA)"))
	  (setq ATT4 (strcat "RIM= " RIM))
	  (setq ATT5 (strcat "INV= " INV DESC))
	  (if (/= INV2 "")
	    (setq ATT6 (strcat "INV= " INV2 DESC2))
	    (setq ATT6 "")
	  ) ;_ end if
	  (if (/= INV3 "")
	    (setq ATT7 (strcat "INV= " INV3 DESC3))
	    (setq ATT7 "")
	  ) ;_ end if
	  (if (/= INV4 "")
	    (setq ATT8 (strcat "INV= " INV4 DESC4))
	    (setq ATT8 "")
	  ) ;_ end if
	  (initget 1)
	  (setq INSP (getpoint "\n Select text insertion point: "))
	  (command "._INSERT"
		   "INLTXT"
		   INSP
		   (rtos SCAL 2 2)
		   ""
		   "0"
		   ATT1
		   ATT2
		   ATT3
		   ATT4
		   ATT5
		   ATT6
		   ATT7
		   ATT8
	  ) ;_ end command
	  (setq	ATT1 ""
		ATT2 ""
		ATT3 ""
		ATT4 ""
		ATT5 ""
		ATT6 ""
		ATT7 ""
		ATT8 ""
	  ) ;_ end setq
	 ) ;opt IN PLAN
	 ((= OPT2 "PR")
	  (print)
	  (setq NUM (getstring t "\n  INLET No.: "))
	  (setq OFST NIL)
	  (QTYPSIZ)
	  (QRIMINV)
	  (setq ATT1 (strcat "STORM INLET No. " NUM))
	  (setq ATT2 (strcat "TYPE " TYP " (" SIZ (chr 34) "DIA)"))
	  (setq ATT3 (strcat "RIM= " RIM))
	  (setq ATT4 (strcat "INV= " INV DESC))
	  (if (/= INV2 "")
	    (setq ATT5 (strcat "INV= " INV2 DESC2))
	    (setq ATT5 "")
	  ) ;_ end if
	  (if (/= INV3 "")
	    (setq ATT6 (strcat "INV= " INV3 DESC3))
	    (setq ATT6 "")
	  ) ;_ end if
	  (if (/= INV4 "")
	    (setq ATT7 (strcat "INV= " INV4 DESC4))
	    (setq ATT7 "")
	  ) ;_ end if
	  (setq ATT8 "")
	  (initget 1)
	  (setq INSP (getpoint "\n Select text insertion point: "))
	  (command "._INSERT"
		   "INLTXT"
		   INSP
		   (rtos SCAL 2 2)
		   ""
		   "0"
		   ATT1
		   ATT2
		   ATT3
		   ATT4
		   ATT5
		   ATT6
		   ATT7
		   ATT8
	  ) ;_ end command
	  (setq	ATT1 ""
		ATT2 ""
		ATT3 ""
		ATT4 ""
		ATT5 ""
		ATT6 ""
		ATT7 ""
		ATT8 ""
	  ) ;_ end setq
	 ) ;opt IN PROFILE
	 ((= OPT2 "EX")
	  (QRIMINV)
	  (setq ATT1 "EX. STORM INLET")
	  (setq ATT2 "")
	  (setq ATT3 (strcat "RIM= " RIM))
	  (setq ATT4 (strcat "INV= " INV DESC))
	  (if (/= INV2 "")
	    (setq ATT5 (strcat "INV= " INV2 DESC2))
	    (setq ATT5 "")
	  ) ;_ end if
	  (if (/= INV3 "")
	    (setq ATT6 (strcat "INV= " INV3 DESC3))
	    (setq ATT6 "")
	  ) ;_ end if
	  (if (/= INV4 "")
	    (setq ATT7 (strcat "INV= " INV4 DESC4))
	    (setq ATT7 "")
	  ) ;_ end if
	  (initget 1)
	  (setq INSP (getpoint "\n Select text insertion point: "))
	  (command "._INSERT"
		   "SANPRF"
		   INSP
		   (rtos EX_SCAL 2 2)
		   ""
		   "0"
		   ATT1
		   ATT3
		   ATT4
		   ATT5
		   ATT6
		   ATT7
		   ATT2
	  ) ;_ end command
	  (setq	ATT1 ""
		ATT2 ""
		ATT3 ""
		ATT4 ""
		ATT5 ""
		ATT6 ""
		ATT7 ""
	  ) ;_ end setq
	 ) ; opt2 EX
       ) ; cond opt2
      ) ; opt IN
 ;---------------------------------------------------------------------------;
      ((= OPT "CL")
       (PL_PR_EX)
       (cond
	 ((= OPT2 "PL")
	  (setq NUM (getstring t "\n   CLEANOUT No.: "))
	  (setq OFST NIL)
	  (QSTAOFF)
	  (QRIMINV)
	  (setq ATT1 (strcat "STORM CLEANOUT No. " NUM))
	  (if (/= OFST "")
	    (setq ATT2
		   (strcat "STA. " STA ", " OFST "'" (strcase LR) "T.")
	    ) ;_ end setq
	    (setq ATT2 (strcat "STA. " STA ", " (strcase LR) "T."))
	  ) ;_ end if
	  (setq ATT3 (strcat "RIM= " RIM))
	  (setq ATT4 (strcat "INV= " INV DESC))
	  (if (/= INV2 "")
	    (setq ATT5 (strcat "INV= " INV2 DESC2))
	    (setq ATT5 "")
	  ) ;_ end if
	  (if (/= INV3 "")
	    (setq ATT6 (strcat "INV= " INV3 DESC3))
	    (setq ATT6 "")
	  ) ;_ end if
	  (if (/= INV4 "")
	    (setq ATT7 (strcat "INV= " INV4 DESC4))
	    (setq ATT7 "")
	  ) ;_ end if
	  (initget 1)
	  (setq INSP (getpoint "\n Select text insertion point: "))
	  (command "._INSERT"
		   "INLTXT"
		   INSP
		   (rtos SCAL 2 2)
		   ""
		   "0"
		   ATT1
		   ATT2
		   ATT3
		   ATT4
		   ATT5
		   ATT6
		   ATT7
		   ATT8
	  ) ;_ end command
	  (setq	ATT1 ""
		ATT2 ""
		ATT3 ""
		ATT4 ""
		ATT5 ""
		ATT6 ""
		ATT7 ""
		ATT8 ""
	  ) ;_ end setq
	 ) ;opt IN PLAN
	 ((= OPT2 "PR")
	  (print)
	  (setq NUM (getstring t "\n  CLEANOUT No.: "))
	  (setq OFST NIL)
	  (QRIMINV)
	  (setq ATT1 (strcat "STORM CLEANOUT No. " NUM))
	  (setq ATT2 (strcat "RIM= " RIM))
	  (setq ATT3 (strcat "INV= " INV DESC))
	  (if (/= INV2 "")
	    (setq ATT4 (strcat "INV= " INV2 DESC2))
	    (setq ATT4 "")
	  ) ;_ end if
	  (if (/= INV3 "")
	    (setq ATT5 (strcat "INV= " INV3 DESC3))
	    (setq ATT5 "")
	  ) ;_ end if
	  (if (/= INV4 "")
	    (setq ATT6 (strcat "INV= " INV4 DESC4))
	    (setq ATT6 "")
	  ) ;_ end if
	  (setq ATT7 "")
	  (setq ATT8 "")
	  (initget 1)
	  (setq INSP (getpoint "\n Select text insertion point: "))
	  (command "._INSERT"
		   "INLTXT"
		   INSP
		   (rtos SCAL 2 2)
		   ""
		   "0"
		   ATT1
		   ATT2
		   ATT3
		   ATT4
		   ATT5
		   ATT6
		   ATT7
		   ATT8
	  ) ;_ end command
	  (setq	ATT1 ""
		ATT2 ""
		ATT3 ""
		ATT4 ""
		ATT5 ""
		ATT6 ""
		ATT7 ""
		ATT8 ""
	  ) ;_ end setq
	 ) ;opt IN PROFILE
	 ((= OPT2 "EX")
	  (QRIMINV)
	  (setq ATT1 "EX. STORM CLEANOUT")
	  (setq ATT2 "")
	  (setq ATT3 (strcat "RIM= " RIM))
	  (setq ATT4 (strcat "INV= " INV DESC))
	  (if (/= INV2 "")
	    (setq ATT5 (strcat "INV= " INV2 DESC2))
	    (setq ATT5 "")
	  ) ;_ end if
	  (if (/= INV3 "")
	    (setq ATT6 (strcat "INV= " INV3 DESC3))
	    (setq ATT6 "")
	  ) ;_ end if
	  (if (/= INV4 "")
	    (setq ATT7 (strcat "INV= " INV4 DESC4))
	    (setq ATT7 "")
	  ) ;_ end if
	  (initget 1)
	  (setq INSP (getpoint "\n Select text insertion point: "))
	  (command "._INSERT"
		   "SANPRF"
		   INSP
		   (rtos EX_SCAL 2 2)
		   ""
		   "0"
		   ATT1
		   ATT3
		   ATT4
		   ATT5
		   ATT6
		   ATT7
		   ATT2
	  ) ;_ end command
	  (setq	ATT1 ""
		ATT2 ""
		ATT3 ""
		ATT4 ""
		ATT5 ""
		ATT6 ""
		ATT7 ""
	  ) ;_ end setq
	 ) ; opt2 EX
       ) ; cond opt2
      ) ; opt CL
 ;---------------------------------------------------------------------------;
      ((= OPT "FE")
       (PL_PR_EX)
       (cond
	 ((or (= OPT2 "PL") (= OPT2 "PR"))
	  (print)
	  (setq FESIZ (getstring t "\n   F.E.S. size: "))
	  (setq NUM (getstring t "\n   FES No.: "))
	  (setq INV (getstring t "\n   INV elevation: "))
	  (initget "Y N")
	  (setq	WITH_GRATE
		 (getkword "\n With Grate? (Y)es or (N)o: <Y> ")
	  )
	  (if (or (= WITH_GRATE "N") (= WITH_GRATE "n"))
	    (setq ATT3 "")
	    (setq ATT3 "WITH GRATING")
	  ) ;end if
	  (if (/= FESIZ "")
	    (setq ATT1 (strcat FESIZ (chr 34) " STORM F.E.S. No. " NUM))
	    (setq ATT1 (strcat "STORM F.E.S. No. " NUM))
	  ) ; end if
	  (setq ATT2 (strcat "INV= " INV))
 ;(setq ATT3 "")
	  (initget 1)
	  (setq INSP (getpoint "\n Select text insertion point: "))
	  (if (= ATT3 "")
	    (command "._INSERT"
		     "FESTXT"
		     INSP
		     (rtos SCAL 2 2)
		     ""
		     "0"
		     ATT1
		     ATT2
		     ATT3
	    ) ;_ end command (THEN)
	    (command "._INSERT"
		     "FESTXT"
		     INSP
		     (rtos SCAL 2 2)
		     ""
		     "0"
		     ATT1
		     ATT3
		     ATT2
	    ) ;_ end command (ELSE)
	  ) ;_ end if
	  (setq	ATT1 ""
		ATT2 ""
		ATT3 ""
	  ) ;_ end setq
	 ) ;end PL or PR
	 ((= OPT2 "EX")
	  (setq FESIZ (getstring t "\n   Existing F.E.S. size: "))
	  (setq INV (getstring t "\n   Existing F.E.S. elevation: "))
	  (if (/= FESIZ "")
	    (setq ATT1 (strcat FESIZ (chr 34) " EX. STORM F.E.S."))
	    (setq ATT1 "EX. STORM F.E.S.")
	  ) ; end if
	  (setq ATT2 (strcat "INV= " INV))
	  (setq ATT3 "")
	  (initget 1)
	  (setq INSP (getpoint "\n Select text insertion point: "))
	  (command "._insert"
		   "FESTXT"
		   INSP
		   (rtos EX_SCAL 2 2)
		   ""
		   "0"
		   ATT1
		   ATT2
		   ATT3
	  ) ;_ end command
	 ) ;end EX
       ) ;end cond
      ) ;opt FE
 ;---------------------------------------------------------------------------;
      ((= OPT "CH")
       (setq ATT1 "")
       (setq ATT2 "")
       (setq ATT3 "")
       (setq ATT4 "")
       (setq ATT5 "")
       (setq ATT6 "")
       (setq ATT7 "")
       (setq ATT8 "")
       (PL_PR_EX)
       (cond
	 ((or (= OPT2 "PL") (= OPT2 "PR"))
	  (print)
	  (setq
	    FESIZ (getstring t "\n   Concrete Headwall pipe size: ")
	  )
	  (setq NUM (getstring t "\n   Concrete Headwall No.: "))
	  (setq T_WALL (getstring t "\n   Top of WALL elevation: "))
	  (setq INV (getstring t "\n   Pipe INV elevation: "))
	  (initget "Y N")
	  (setq	WITH_GRATE
		 (getkword "\n   With Grate? (Y)es or (N)o: <Y> ")
	  )
	  (if (or (= WITH_GRATE "N") (= WITH_GRATE "n"))
	    (setq W_GRATE "")
	    (setq W_GRATE "WITH GRATING")
	  ) ;end if

	  (if (/= NUM "")
	    (setq NUM (strcat "No. " NUM " "))
	    (setq NUM NUM)
	  ) ;_ end if

	  (setq ATT1 (strcat "STORM CONCRETE HEADWALL"))

	  (if (/= FESIZ "")
	    (setq ATT2 (strcat NUM "WITH " FESIZ (chr 34) " PIPE"))
	    (setq ATT2 NUM)
	  ) ;_ end if

	  (setq ATT3 (strcat "T/WALL= " T_WALL))
	  (setq ATT4 (strcat "INV= " INV))
	  (setq ATT5 W_GRATE)

	  (initget 1)
	  (setq INSP (getpoint "\n Select text insertion point: "))
	  (command "._INSERT"
		   "CHDWLTXT"
		   INSP
		   (rtos SCAL 2 2)
		   ""
		   "0"
		   ATT1
		   ATT2
		   ATT3
		   ATT4
		   ATT5
		   ATT6
		   ATT7
		   ATT8
	  ) ;_ end command

	  (setq	ATT1 ""
		ATT2 ""
		ATT3 ""
		ATT4 ""
		ATT5 ""
		ATT6 ""
		ATT7 ""
		ATT8 ""
	  ) ;_ end setq
	 ) ;end PL or PR
	 ((= OPT2 "EX")
	  (setq
	    FESIZ (getstring
		    t
		    "\n   Existing CONCRETE HEADWALL PIPE size: "
		  )
	  ) ;_ end setq
	  (setq	INV
		 (getstring t
			    "\n   Existing CONCRETE HEADWALL INV elevation: "
		 )
	  ) ;_ end setq
	  (setq ATT1 "EX. STORM CONCRETE HEADWALL")
	  (if (/= FESIZ "")
	    (setq ATT2 (strcat "WITH " FESIZ (chr 34) " PIPE")
		  ATT3 (strcat "INV= " INV)
		  ATT4 ""
		  ATT5 ""
		  ATT6 ""
		  ATT7 ""
		  ATT8 ""
	    ) ;_ end setq
	    (setq ATT2 (strcat "INV= " INV)
		  ATT3 ""
		  ATT4 ""
		  ATT5 ""
		  ATT6 ""
		  ATT7 ""
		  ATT8 ""
	    ) ;_ end setq
	  ) ; end if


	  (initget 1)
	  (setq INSP (getpoint "\n Select text insertion point: "))
	  (command
	    "._insert"
	    "CHDWLTXT"
	    INSP
	    (rtos EX_SCAL 2 2)
	    ""
	    "0"
	    ATT1
	    ATT2
	    ATT3
	    ATT4
	    ATT5
	    ATT6
	    ATT7
	    ATT8
	  ) ;_ end command
	 ) ;end EX
       ) ;end cond
       (setq ATT1 ""
	     ATT2 ""
	     ATT3 ""
	     ATT4 ""
	     ATT5 ""
	     ATT6 ""
	     ATT7 ""
	     ATT8 ""
       ) ;_ end setq
      ) ;opt Concrete Headwall
 ;---------------------------------------------------------------------------;
      ((= OPT "VV")
       (PL_PR_EX)
       (cond
	 ((or (= OPT2 "PL") (= OPT2 "PR"))
	  (print)
	  (setq NUM (getstring t "\n   VALVE VAULT No.: "))
	  (setq SIZ (getstring t "\n   VALVE SIZE in inches: "))
	  (setq VSIZ (getstring t "\n   VAULT SIZE in inches: "))
	  (setq OFST NIL)
	  (QSTAOFF)
	  (setq RIM (getstring t "\n   RIM elevation: "))
	  (if (/= SIZ "")
	    (if	(/= VSIZ "")
	      (setq ATT1 (strcat SIZ
				 (chr 34)
				 " VALVE & "
				 VSIZ
				 (chr 34)
				 " VAULT No. "
				 NUM
			 ) ;_ end strcat
	      ) ;_ end setq
	      (setq
		ATT1 (strcat SIZ (chr 34) " VALVE & VAULT No. " NUM)
	      ) ;_ end setq
	    ) ; end if VSIZ
	    (setq ATT1 (strcat "VALVE VAULT No. " NUM))
	  ) ; end if SIZ
	  (if (/= OFST "")
	    (setq ATT2
		   (strcat "STA. " STA ", " OFST "'" (strcase LR) "T.")
	    ) ;_ end setq
	    (setq ATT2 (strcat "STA. " STA ", " (strcase LR) "T."))
	  ) ;_ end if
	  (setq ATT3 (strcat "RIM= " RIM))
	  (initget 1)
	  (setq INSP (getpoint "\n Select text insertion point: "))
	  (command "._INSERT"
		   "VVTXT"
		   INSP
		   (rtos SCAL 2 2)
		   ""
		   "0"
		   ATT1
		   ATT2
		   ATT3
	  ) ;_ end command
	  (setq	ATT1 ""
		ATT2 ""
		ATT3 ""
		OFST ""
	  ) ;_ end setq
	 ) ;end cond PL PR
	 ((= OPT2 "EX")
	  (setq SIZ (getstring t "\n  Existing VALVE SIZE in inches: "))
	  (setq
	    VSIZ (getstring t "\n  Existing VAULT SIZE in inches: ")
	  ) ;_ end setq
	  (setq RIM (getstring t "\n   Existing RIM elevation: "))
	  (if (/= SIZ "")
	    (if	(/= VSIZ "")
	      (setq ATT1 (strcat "EX. "
				 SIZ
				 (chr 34)
				 " VALVE & "
				 VSIZ
				 (chr 34)
				 " VAULT"
			 ) ;_ end strcat
	      ) ;_ end setq
	      (setq ATT1 (strcat "EX. " SIZ (chr 34) " VALVE & VAULT"))
	    ) ; end if VSIZ
	    (setq ATT1 "EX. VALVE VAULT")
	  ) ; end if SIZ
	  (setq ATT2 (strcat "RIM= " RIM))
	  (setq ATT3 "")
	  (initget 1)
	  (setq INSP (getpoint "\n Select text insertion point: "))
	  (command "._INSERT"
		   "VVTXT"
		   INSP
		   (rtos EX_SCAL 2 2)
		   ""
		   "0"
		   ATT1
		   ATT2
		   ATT3
	  ) ;_ end command
	  (setq	ATT1 ""
		ATT2 ""
		ATT3 ""
	  ) ;_ end setq
	 ) ;end cond EX
       ) ;end cond
      ) ;opt VV
 ;---------------------------------------------------------------------------;
      ((= OPT "FH")
       (PL_PR_EX)
       (cond
	 ((or (= OPT2 "PL") (= OPT2 "PR"))
	  (print)
	  (setq NUM (getstring t "\n   FIRE HYDRANT No.: "))
	  (setq OFST NIL)
	  (QSTAOFF)
	  (setq RING (getstring t "\n   RING elevation: "))
	  (setq ATT1 (strcat "FIRE HYDRANT No. " NUM))
	  (if (/= OFST "")
	    (setq ATT2
		   (strcat "STA. " STA ", " OFST "'" (strcase LR) "T.")
	    ) ;_ end setq
	    (setq ATT2 (strcat "STA. " STA ", " (strcase LR) "T."))
	  ) ;_ end if
	  (setq ATT3 (strcat "RING EL.= " RING))
	  (initget 1)
	  (setq INSP (getpoint "\n Select text insertion point: "))
	  (command "._INSERT"
		   "FHTXT"
		   INSP
		   (rtos SCAL 2 2)
		   ""
		   "0"
		   ATT1
		   ATT2
		   ATT3
	  ) ;_ end command
	  (setq	ATT1 ""
		ATT2 ""
		ATT3 ""
		OFST ""
	  ) ;_ end setq
	 ) ;end PL or PR
	 ((= OPT2 "EX")
	  (setq RING (getstring t "\n  Existing RING elevation: "))
	  (setq ATT1 "EX. FIRE HYDRANT")
	  (setq ATT2 (strcat "RING EL.= " RING))
	  (setq ATT3 "")
	  (initget 1)
	  (setq INSP (getpoint "\n Select text insertion point: "))
	  (command "._INSERT"
		   "FHTXT"
		   INSP
		   (rtos EX_SCAL 2 2)
		   ""
		   "0"
		   ATT1
		   ATT2
		   ATT3
	  ) ;_ end command
	  (setq	ATT1 ""
		ATT2 ""
		ATT3 ""
		OFST ""
	  ) ;_ end setq
	 ) ;end EX
       ) ;end cond
      ) ;opt FH
 ;---------------------------------------------------------------------------;
      ((= OPT "LP")
       (PL_PR_EX)
       (cond
	 ((or (= OPT2 "PL") (= OPT2 "PR"))
	  (print)
	  (setq NUM NIL)
	  (setq
	    NUM	(getstring t "\n   Street light number (optional): ")
	  ) ;_ end setq
	  (setq WATT NIL)
	  (setq	WATT
		 (getstring t "\n   Street light wattage (optional): ")
	  ) ;_ end setq
	  (setq STA (getstring t "\n   STATION: "))
	  (initget 1)
	  (if (/= NUM "")
	    (if	(/= WATT "")
	      (progn
		(setq ATT1 (strcat WATT "W STREET LIGHT"))
		(setq ATT2 (strcat "No. " NUM ", STA. " STA))
	      ) ;end progn - yes num, yes watt
	      (progn
		(setq ATT1 (strcat "STREET LIGHT No. " NUM))
		(setq ATT2 (strcat "STA. " STA))
	      ) ; end else - yes num, no watt
	    ) ; end if
	    (if	(/= WATT "")
	      (progn
		(setq ATT1 (strcat WATT "W STREET LIGHT"))
		(setq ATT2 (strcat "STA. " STA))
	      ) ; end - no num, yes watt
	      (progn
		(setq ATT1 "STREET LIGHT")
		(setq ATT2 (strcat "STA. " STA))
	      ) ; end - no num, no watt
	    ) ; end if
	  ) ; end if
	  (setq INSP (getpoint "\n Select text insertion point: "))
	  (command "._INSERT"
		   "STLTTXT"
		   INSP
		   (rtos SCAL 2 2)
		   ""
		   "0"
		   ATT1
		   ATT2
	  ) ;_ end command
	  (setq	ATT1 ""
		ATT2 ""
	  ) ;_ end setq
	 ) ; end PL or PR
	 ((= OPT2 "EX")
	  (setq ATT1 "EX. STREET LIGHT")
	  (setq ATT2 "")
	  (setq INSP (getpoint "\n Select text insertion point: "))
	  (command "._INSERT"
		   "STLTTXT"
		   INSP
		   (rtos EX_SCAL 2 2)
		   ""
		   "0"
		   ATT1
		   ATT2
	  ) ;_ end command
	  (setq	ATT1 ""
		ATT2 ""
	  ) ;_ end setq
	 ) ;end EX
       ) ;end cond
      ) ;opt LP
 ;---------------------------------------------------------------------------;
    ) ;cond - main program
  ) ;while opt
  (MODER)
  (print)
  (print)
;    );end checker progn
;    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
;    );end if
;    (princ)  
) ;end defun c:pptxt