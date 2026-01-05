;;; From Jeff Mishler on the Land Desktop customization newsgroup
;;; 9/27/06 11:32 AM
;;; formatted for CGL needs by Mark Evinger
;;; Prints the station/offset/elevation of a selected point via alert box and to text screen.

(defun c:staoff (/ ACADOBJ AECCALIGN AECCALIGNS AECCAPP AECCDOC AECCPROJ
                 AECCUTIL CURALIGNNAME ERR OFF PT1 PTLIST QLIST STA STASTR XY2EN
                )
  (CD_MNL) ;initialize Civil Design
  (and (setq pt1 (getpoint "\nSelect point for station/offset inquiry: "))
       (setq acadObj (vlax-get-acad-object))
       (setq aeccApp (vla-getInterfaceObject acadObj "Aecc.Application"))
;;;use "Aecc.Application.4" for versions 2004-2006
       (setq aeccDoc (vla-get-activedocument aeccApp))
       (setq aeccUtil (vlax-get aeccDoc "utility"))
       (setq aeccProj (vlax-get aeccApp 'activeProject))
       (setq aeccAligns (vlax-get aeccProj 'alignments))
       (setq CurAlignName (vlax-get aeccAligns 'currentalignment))
       (setq aeccAlign (vlax-invoke aeccAligns 'item curalignname))
       (setq aeccFGProfiles (vlax-get aeccAlign 'FGProfiles))
       (setq FGCenter (vlax-invoke aeccFGProfiles 'profilebytype 1)) ;1 is FGCenter
       (setq xy2en (vlax-invoke aeccUtil 'xytoeastnorth pt1))
       (vlax-invoke-method
         aeccAlign
         'stationoffset
         (car xy2en)
         (cadr xy2en)
         'sta
         'off
         'dir
       ) ;_ end of vlax-invoke-method
  ) ;_ end of and
  (if sta
    (progn
      (setq staStr (vlax-invoke-method aeccAligns 'doubletostaformat sta))
      ;;note that the following line will raise an error if the vertical doesn't exist at this point
      (setq FGCtr (vlax-invoke-method FgCenter 'elevationat sta))
      (alert (strcat "\nThe selected point is at Station: " staStr
                     "\nwith an offset of: "   (rtos off)
                     "\nand CL elevation of: " (rtos FGCtr)
             ) ;_ end of strcat
      ) ;_ end of alert
      (princ (strcat "\nThe selected point is at Station: " staStr
                     "\nwith an offset of: "   (rtos off)
                     "\nand CL elevation of: " (rtos FGCtr)
                     "\n======================================="
             ) ;_ end of strcat
      ) ;_ end of princ
    ) ;_ end of progn
  ) ;_ end of if
  (setq qList '(ACADOBJ AECCALIGN AECCALIGNS AECCAPP AECCDOC AECCPROJ AECCUTIL
               )
  ) ;_ end of setq
  (foreach x qlist
    (setq err (vl-catch-all-apply 'vlax-release-object (list (eval x))))
    (set x nil)
  ) ;_ end of foreach
  (princ)
) ;_ end of defun
