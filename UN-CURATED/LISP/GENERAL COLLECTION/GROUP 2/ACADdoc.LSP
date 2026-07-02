 ;revised 3/26/1998 for release 14 compatibility  ME
 ;revised 7/14/1998 for addditional R14 compatibility ME
 ;revised 08/17/98 10:18:11 AM for MaxHatch evironment variable
 ;revised 10/14/1998 11:18:00 AM for new TextOut.lsp
 ;revised 02/26/1999 12:00:00 PM for new DRAREA.lsp
 ;revised 07-24-2000 03:46 PM for new NEW_TF.lsp
 ;revised 08-02-2001 04:00 PM for DigiAir.lsp to import image files
 ;revised 09-11-2001 for R15 (Land Desktop 3) install...
 ;revised 09-18-2003 for the cgl-saveplot feature - AcadDoc.lsp will delete the c:\temp\cgl-saveplot.cgl file each time it is loaded
 ;revised 03-04-2004 to add the MoveGradLbl lisp routine.
 ;revised 3:07 PM 4/6/2005 for LDT2005  ME
 ;revised 3/9/06 4:26 PM to turn ATTREQ on for all dwgs  ME
 ;revised 9/27/06 11:11 AM to autoload STAOFF.lsp and TOTLEN.lsp  ME
 ;revised 11:49 AM 1/22/2007 to autoload CHGNUM.lsp  ME
 ;revised 11:42 AM 7/20/2007 to set LAYOUTREGENCTL to 1  ME
 ;revised 2:22 PM 1/7/2008 to autoload the ACRE.LSP  ME
 ;revised 9:52 AM 3/19/2008 to autoload 4.lsp
 ;*************************************************************************
 ; Cowhey applications!!

(if (getenv "MaxHatch")
  (setenv "MaxHatch" "100000000") ;allows sidewalk hatching
  (setenv "MaxHatch" "100000000") ;allows sidewalk hatching
) ;_ end if

;the defun-q call is for R2002 support
(defun-q CGL_STARTUP ()
  (load "plot_1")
  (C:PLOT_1) ; redefine plot command with our own.
  (load "dlf.lsp")
  (C:dlf) ; delete all layer filters at dwg open.
  (load "cgl_check_serial") ;keeps cgl lisp running ONLY on cgl machines
) ;end defun CGL_startup
(setq S::STARTUP (append S::STARTUP CGL_STARTUP))

(setvar "ucsicon" 1) (princ);sets the UCSICON to on, NO origin

(load "LayToolR12.lsp") ; for r12-style compatibility in POP0 menu for R14

;(load "lx2ls.fas"); load LayerExpress 2 LayerState to convert old .lx info
(princ "\nLoading LX2LS")
(load "LX2LS")(if (not (setq $LX2LS$ (lslist))) (c:LX2LS))
(setvar "visretain" 1) (princ); turn ON visretain in every dwg
(setvar "ATTREQ" 1) (princ) ;turn on attribute request in every dwg
(setvar "layoutregenctl" 1) (princ) ;cache one layout at a time

(vl-file-delete "c:/temp/cgl-saveplot.cgl") (princ) ;erase the saveplot file to make sure plots are NOT saved by default

(autoload "1" '("1"))
(autoload "2" '("2"))
(autoload "3" '("3"))
(autoload "4" '("4"))
(autoload "ADDTEXT" '("ADDTEXT"))
(autoload "ADD_ONE" '("ADD_ONE"))
(autoload "ARROW" '("ARROW"))
(autoload "ARRW" '("ARRW"))
(autoload "ASCTEXT" '("ASCTEXT"))
(autoload "ATT2TXT" '("ATT2TXT"))
(autoload "AXROT" '("AXROT"))
(autoload "BCTIC" '("BCTIC"))
(autoload "BRKFST" '("BRKFST"))
(autoload "calc" '("calc"))
(autoload "CGL_MEDIT" '("CGL_MEDIT"))
(autoload "CHBLOCK" '("CHBLOCK"))
(autoload "CHELEV" '("CHELEV"))
;for Scott K  - CHGNUM, the changenumber lisp 11:45 AM 1/22/2007
(autoload "CHGNUM" '("CHGNUM"))
(autoload "ch_case" '("ch_case"))
(autoload "CH_CIR" '("CH_CIR"))
(autoload "DigiAir" '("DigiAir"))
(autoload "DRAREA" '("DRAREA"))
(autoload "DRNTILE" '("DRNTILE"))
(autoload "ELEVCHG" '("ELEVCHG"))
(autoload "ESAN" '("ESAN"))
(autoload "FORCMAIN" '("FORCMAIN"))
(autoload "GLUE" '("GLUE"))
(autoload "GRADLBL" '("GRADLBL"))
(autoload "JUST" '("JUST"))
(autoload "LineTypeLister" '("LineTypeLister"))
(autoload "MHPRF" '("MHPRF"))
(autoload "MID" '("MID"))
(autoload "MVSF" '("MVSF"))
(autoload "NEW_TF" '("NEW_TF"))
(autoload "NEZ" '("NEZ"))
(autoload "PERPLINE" '("PERPLINE"))
(autoload "PIPE" '("PIPE"))
(autoload "PIPEREM" '("PIPEREM"))
(autoload "PIPEVIEW" '("PIPEVIEW"))
(autoload "PIPE_END" '("PIPE_END"))
(autoload "PLUD" '("PLUD"))
(autoload "PntFrz" '("PntFrz"))
(autoload "pntfrzme" '("pntfrzme"))
(autoload "POLY2TO3" '("POLY2TO3"))
(autoload "POLY3TO2" '("POLY3TO2"))
(autoload "PPTXT" '("PPTXT"))
(autoload "PSAN" '("PSAN"))
(autoload "PWIDTH" '("PWIDTH"))
(autoload "RCLOUD" '("RCLOUD"))
(autoload "RECTANG" '("RECTANG"))
(autoload "REMExtrusion" '("REMExtrusion"))
(autoload "RENUM" '("RENUM"))
(autoload "RESCALE" '("RESCALE"))
(autoload "SAN_RSR" '("SAN_RSR"))
(autoload "SAN_SER" '("SAN_SER"))
(autoload "SLP" '("SLP"))
(autoload "SSX" '("SSX"))
(autoload "STM" '("STM"))
(autoload "STMP_ME" '("STMP_ME"))
(autoload "STM_SER" '("STM_SER"))
(autoload "SUMLIN" '("SUMLIN"))
(autoload "TEXTOUT" '("TEXTOUT"))
(autoload "ties" '("ties"))
(autoload "toelev" '("toelev"))
(autoload "TRENCH" '("TRENCH"))
(autoload "TWEEN" '("TWEEN"))
(autoload "TXT2ATT" '("TXT2ATT"))
(autoload "TXT90" '("TXT90"))
(autoload "TXTANG" '("TXTANG"))
(autoload "WM_SER" '("WM_SER"))
(autoload "XSECT" '("XSECT"))
(autoload "ZPNT" '("ZPNT"))
(autoload "ZPR" '("ZPR"))

(autoload "LegalAid" '("LegalAid"))
(autoload "LX2LS" '("lx2ls")); LayerExpress to LayerState
(autoload "CGLSavePlot" '("cglsaveplot" "CSP")) ;toggles saving of plotfiles on or off. Added 09-18-2003
(autoload "MoveGradLbl" '("movegradlbl" "MGL")); moves GRAD_X block to elev of selected text. Added 03-04-2004

(autoload "FindShape" '("FindShape"))
(autoload "DelShape" '("DelShape"))
(autoload "CountyImageInsert" '("CountyImageInsert" "CII"))
(autoload "Staoff" '("STAOFF"))
(autoload "TOTLEN" '("TOTLEN"))
(autoload "PADPLACER" '("PadPlacer")) ;load the plad placer routine 11:43 AM 7/20/2007
(autoload "FREENUMS" '("FreeNums")) ;load the freenums lisp to list available point numbers 9:56 AM 8/13/2007
(autoload "ACRE" '("ACRE"));loads the ACRE lisp to list areas of closed polylines 2:23 PM 1/7/2008