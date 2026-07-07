;;; ==============================================================================
;;; Lisp Name: PWD.LSP
;;; Author: Lonnie
;;; Date Created: [10-04-23]
;;; Last Edited: [Insert Last Edit Date]
;;;
;;; DESCRIPTION:
;;; A routine to open the folder containing the current AutoCAD drawing file.
;;;
;;; Usage:
;;; 1. Load the Lisp routine.
;;; 2. Run the command "PWD" in AutoCAD.
;;;
;;; Parameters:
;;; None
;;;
;;; Returns:
;;; None
;;;
;;; Notes:
;;; - This routine opens the folder containing the current AutoCAD drawing file.
;;;
;;; ---------------------------- Main program --------------------------------;


(defun C:PWD (/ ShellObject)
(vl-load-com)
	(setq ShellObject (vla-getInterfaceObject (vlax-get-acad-object) "Shell.Application"))
	(vlax-invoke-method ShellObject 'Explore (getvar "dwgprefix"))
	(vlax-release-object ShellObject)
)