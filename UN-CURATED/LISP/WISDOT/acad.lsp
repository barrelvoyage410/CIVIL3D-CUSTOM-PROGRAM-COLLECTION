;;;acad.lsp
;;;Developed for WisDOT Civil 3D 2018 startup
;;;-----------------------------------------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------------------------------------

(vl-load-com)

;;;----SET DEFAULT DRAWING FORMAT to CTB (color-dependent plot style table)
(setvar "PSTYLEPOLICY" 1)

(if (= (getvar "CPROFILE") "2018wisdot")
  (progn
    (princ "\n--------------------------------------------------------------")
    (princ)
    (princ "\n              Loading WisDOT 2018 Environment                 ")
    (princ)
    (princ "\n--------------------------------------------------------------")
    (princ)
    (princ "\n  completed: plot style policy set to CTB                     ")
    (princ)
    (setq WisDOTLocalKey
	    (vl-registry-read
	      "HKEY_LOCAL_MACHINE\\SOFTWARE\\Autodesk\\AutoCAD\\R22.0\\ACAD-1002\\Variables\\WISDOT"
	      "WisDOTkey"
	    )
    )
    (if	(= WisDOTLocalKey "1")
      (progn
;;;----Set System Variables
	      (setvar "ACADLSPASDOC" 0)   
              (setvar "SHOWNEWSTATE" 0)
	      (setvar "APPAUTOLOAD" 2)
	      (setvar "CMDDIA" 1)
	      (setvar "CURSORBADGE" 1)
	      (setvar "DATALINKNOTIFY" 1)
	      (setvar "DTEXTED" 1)
	      (setvar "FIELDEVAL" 15)
	      (setvar "FILEDIA" 1)
	      (setvar "HPORIGINMODE" 5)
	      (setvar "LARGEOBJECTSUPPORT" 1)
	      (setvar "LAYEREVALCTL" 0)
	      (setvar "LWUNITS" 0)
	      (setvar "OBJECTISOLATIONMODE" 1)
	      (setvar "ONLINESYNCTIME" 0)
	      (setvar "PICKFIRST" 1)
	      (setvar "PSTYLEPOLICY" 1)
	      (setvar "RASTERPERCENT" 40)
	      (setvar "RASTERTHRESHOLD" 50)
	      (setvar "RECOVERYMODE" 0)
	      (setvar "RIBBONBGLOAD" 1)
	      (setvar "RIBBONDOCKEDHEIGHT" 0)
	      (setvar "SAVEFIDELITY" 0)
	      (setvar "SELECTIONAREA" 0)
	      (setvar "SSMSHEETSTATUS" 1)
	      (setvar "STANDARDSVIOLATION" 0)
	      (setvar "SYSMON" 0)
	      (setvar "XEDIT" 0)
	      (setvar "XLOADCTL" 2)
	      (setvar "XREFCTL" 0)
	      (setvar "XREFNOTIFY" 1)
	      (setvar "XREFTYPE" 1)
	      (princ)
	      (princ "\n  completed: system variables set                             ")
	      (princ)
      )
    )
	      (setvar "ACADLSPASDOC" 0)
	      (setvar "CMDDIA" 1)
	      (setvar "FILEDIA" 1)
	
	
    ;; SET WISDOT PALETTES TO READ-ONLY  +R equals make read-only
    ;;-----------------------------------------------------------------------------------------------------------

	      (COMMAND "shell" "attrib +R C:\\WisDOT\\Stnd\\C3D2018\\Appdata\\ToolPalette\\* /s /d")
	      (princ "\n  completed: toolpalette set to read-only                     ")
              (princ)

    (if
      (findfile
	"C:\\ProgramData\\Autodesk\\ApplicationPlugins\\WisDOT2018A.bundle\\Contents\\Resources\\WILocalKey.VLX"
      )
        (load
	  "C:\\ProgramData\\Autodesk\\ApplicationPlugins\\WisDOT2018A.bundle\\Contents\\Resources\\WILocalKey.VLX"
	  "\nWILocalKey.VLX file did not load."
        )
    )
  )
)