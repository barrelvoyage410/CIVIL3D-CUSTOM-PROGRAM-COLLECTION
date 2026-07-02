;;;
;;;     LegalAid.lsp
;;;     Copyright (C) 1999 by Emerald Data, Inc.
;;;     All Rights Reserved.
;;;
;;;     Version 6.0, November 17, 1999
;;;

(defun c:legalaid (/ filename macroname BadVersion) 

  ;;; SAVE ECHO AND DIALOG BOX SETTINGS.            
  (setq edicmdecho-save (getvar "CMDECHO"))
  (setq edifiledia-save (getvar "FILEDIA"))
  (setvar "CMDECHO" 0)
  (setvar "FILEDIA" 0)

  ;;; SET UP OUR ERROR HANDLER
  (setq ediOldError *error*)
  (setq *error* edi_LaError)

	;;; TEST FOR CORRECT ACAD RELEASE
	(setq badversion "Invalid AutoCAD version.  Legal-Aid requires release 14 or above.")
  (if (= (substr (ver) 1 19) "AutoLISP Release 10")(progn (alert badversion)(exit)))
  (if (= (substr (ver) 1 19) "AutoLISP Release 11")(progn (alert badversion)(exit)))
  (if (= (substr (ver) 1 19) "AutoLISP Release 12")(progn (alert badversion)(exit)))
  (if (= (substr (ver) 1 19) "AutoLISP Release 13")(progn (alert badversion)(exit)))

  ;;; BUILD DVB PATH AND FILENAME
  ;;; Set path to .dvb file.  Setup program will change
  ;;; the first line to point to new installed location.
  ;;; If filename was not changed (ie: it is still equal
  ;;; to "NONE"), then default to our development location.
  ;;;
  (setq filename "C:\\Program Files\\LegalAid\\LawAcad.dvb")
  (if (= filename "NONE")
    (setq filename "D:\\Dev\\Apps\\LegalAid\\Acad\\LawAcad.dvb")
  )
  (setq macroname (strcat filename "!Legal_Aid.Start"))

  ;;; STARTUP NOTICE
  (Prompt "\nLegal-Aid, Copyright (C) 1989-1999 Emerald Data, Inc.\nLoading...")

  ;;; LOAD AND RUN LEGALAID.DVB
  (if (= (substr (ver) 18 2) "14")
    (progn 
      (command "_vbaload" filename)
      (prompt "...done\n")
      (command "_-vbarun" "Legal_Aid.Start")
     );Acad 14 version       
    (progn                                                    	
      (command "-vbarun" macroname)
      (prompt "...done\n")
    );Acad 2000 version 
  );if


  ;;; RESTORE AUTOCAD SETTINGS
  (setvar "FILEDIA" edifiledia-save)
  (setvar "CMDECHO" edicmdecho-save)
  (setq edifiledia-save nil)
  (setq edicmdecho-save nil)

  ;;; DISCONNECT OUR ERROR HANDLER
  (setq *error* ediOldError)
  (setq ediOldError nil)
  (princ)
)



;
;  Legal-Aid error handler.
;
(defun edi_LaError(ErrMess)
  (setvar "FILEDIA" edifiledia-save)
  (setvar "CMDECHO" edicmdecho-save)
  (if (not (member ErrMess '("console break" "function cancelled" "quit / exit abort")))
    (progn
      (princ (strcat "\nError: " ErrMess))
      (setq *error* ediOldError)
      (setq ediOldError nil)
    )
  )
  (princ)
)


;
;	Utility: UnloadLegalAid
;
(defun C:unloadlegalaid()
  (if (= (substr (ver) 18 2) "14")
    (command "_vbaunload")
    (command "_vbaunload" "LawAcad.dvb")
  )
  (princ)
)

