;; WisDOT setup and startup for each 2018 document.

(vl-load-com)

(if (= (getvar "CPROFILE") "2018wisdot")
  (progn
;;;-----------------------------------------------------------------------------------------------------------
;;;----Query device for internal wisdot key - perform only on wisdot devices
    (setq WisDOTLocalKey
	   (vl-registry-read
	     "HKEY_LOCAL_MACHINE\\SOFTWARE\\Autodesk\\AutoCAD\\R22.0\\ACAD-1002\\Variables\\WISDOT"
	     "WisDOTkey"
	   )
    )
    (if	(= WisDOTLocalKey "1")
      (progn ;;----SET DWG VARIABLES----
	     (setvar "UCSFOLLOW" 0)
	     (setvar "INSUNITS" 0)
	     (setvar "MSLTSCALE" 1)
	     (setvar "GEOMARKERVISIBILITY" 0)
	     (setvar "PLINEGEN" 1)
	     (setvar "PROXYGRAPHICS" 1)
	     (setvar "PROXYSHOW" 1)
	     (setvar "PROXYNOTICE" 1)
	     (setvar "CAMERADISPLAY" 0) ;(setvar "MAXACTVP" 16)
	     (setvar "WHIPTHREAD" 3)
	     (setvar "PDFSHX" 0)
	     (princ "\n")
	     (princ "\n  completed: Drawing variables set                            ")
	     (princ)
      )
    )
    (princ "\n--------------------------------------------------------------")
    (princ)
    (princ "\n              Loading WisDOT 2018 Customization               ")
    (princ)
    (princ "\n--------------------------------------------------------------")
    (princ)
    (if
      (findfile
	"C:\\ProgramData\\Autodesk\\ApplicationPlugins\\WisDOT2018A.bundle\\Contents\\Resources\\wisdotpalettetools22.VLX")
       (load
	 "C:\\ProgramData\\Autodesk\\ApplicationPlugins\\WisDOT2018A.bundle\\Contents\\Resources\\wisdotpalettetools22.VLX"
	 "\nwisdotpalettetools22.VLX file did not load."
       )
    )
    (princ "\n  completed: WisDOT intialization and customization           ")
  )
  (progn
    (princ
      "\n  WisDOT Ribbon loaded without customization files, please verify 2018 WisDOT profile is configured...   ")
    (princ "\n")
  )
)

(if
  (findfile
    "C:\\ProgramData\\Autodesk\\ApplicationPlugins\\WisDOT2018A.bundle\\Contents\\Resources\\wisdotregprofconfig22.VLX")
   (load
     "C:\\ProgramData\\Autodesk\\ApplicationPlugins\\WisDOT2018A.bundle\\Contents\\Resources\\wisdotregprofconfig22.VLX"
     "\nwisdotregprofconfig22.VLX file did not load."
   )
)

(princ "\n--------------------------------------------------------------")
(princ)

(load "FPX" "\nFPX.VLX file did not load.")