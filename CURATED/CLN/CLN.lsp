;; CLEAN ROUTINE TIM C./w FORUM HELP 04DEC2024
;;
;; PURGE TWICE
;; ZOOM EXTENTS Code for active or all present, donated by Sea-Haven
;; REGEN ALL12X
;; QUICK SAVE 2X
;; Purge and Audit Code donated by CodeDing
;; Background info, I use a lot of dynamic blocks in my drawings. Typically, I do not perform block edits in a production drawing
;; Performing block edits may lead to hidden garbage being added to the dynamic block, prevalent in blocks with visstate + attributes
;; This routine clears/prevents that garbage from gathering. You will not know if block garbage has accumulated until you reopen the drawing
;; A clear indication is a recover notice popping up when the drawing is open.
;; I have had many instances of duplicate objects being automatically added inside all the dynamic blocks inside a drawing if I selected YES to recover
;; To avoid that issue, I would manually run these commands after I exited the block editor.
;; To save time, I created this routine.
;; That's my saga; I hope other folks find this useful.
;; One more tip about Block editor issues
;; If I perform multiple block edits and in the mix I select MODIFY and action, specifically POINT MOVE
;; My cursor will disappear when the mouse is over top of the canvas. Moving the point to the ribbon brings it back.
;; I find it is best to restart AutoCAD at this point.
;;
;;

(defun c:CLN (/ response)
  ;; Run PURGE ALL twice with a slight pause in between
  ;; (command "._-PURGE" "ALL" "*" "N")  this would not remove unused blocks if ran from inside a lisp but will if ran from the command line
  ;;  Created function to wrap the command, see below
  (c:purge-all)
  (command "._delay" 250)
  (c:purge-all)

  (command "_.AUDIT" "y")

  ;; Zoom extents
  (setq curtab (getvar "Ctab"))
  (setq this_dwg (vlax-get-acad-object))
  (foreach d (layoutlist)
      (setvar "CTAB" d)
      (vla-ZoomExtents this_dwg)
  )
  (setvar "ctab" curtab)

  ;; Regen all
  (command "_REGENALL")
  (command "._delay" 250)
  (command "_REGENALL")

  ;; Quick save
  (command "_QSAVE")

  ;; Short delay to avoid write error
  (command "._delay" 250)

  ;; Quick save again
  (command "_QSAVE")

  ;; Display completion message and sound
  (alert "CLEAN AS A WHISTLE")

  ;; Prompt to close the file
  (setq response (getstring "\nDo you want to close the file? (Y/N) <N>: "))

  ;; Close the file if the response is "Y" or "y"
  (if (or (= response "Y") (= response "y"))
    (command "_CLOSE")
    (princ "\n")
  )
  (princ)
)


;; This code uses vl-load-com to ensure that COM libraries are loaded and wraps the PURGE command in an undo mark
;; which helps manage the execution context, all in all it seems to be removing unused blocks at this time.

(defun c:purge-all ()
  (vl-load-com)
  (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (command "._-PURGE" "ALL" "*" "N")
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ)
)

;; Display message after LISP is loaded
(princ "\nType \"CLN\" To Initiate Cleanup and Exit with Choice.")
(princ "\n")
