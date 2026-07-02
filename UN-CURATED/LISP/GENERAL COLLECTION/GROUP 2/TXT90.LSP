;; Created 3/3/2000  10:41:24 AM to rotate TEXT and MTEXT +90 degrees
;; around its insertion point. By Mark Evinger (CIS ID 73162,1676)
;;
;; revised 3-16-2000 to remove conflict with CGL routine TXTANG.LSP
;; renamed function GET_TEXT to CGL_GET_TEXT

;; revised 03-20-2007 to check for serial number
;-------------------------------------------------------------------------
(defun rtd (LINE_ANG)
     (/ (* 180 LINE_ANG) pi)
); end defun rads_to_degrees
;-------------------------------------------------------------------------
(Defun DTR (LINE_ANg)
  (/ (* pi LINE_ANG) 180)
);end Degrees_To_Radians
;; --------------------------------------------------------------------------
;; Function: NICE_ENTSEL
;; Purpose : Obtains an entity selection, but unlike (entsel), does not
;;           return nil if user picks nothing.  Returns nil only if user
;;           presses enter.  If user picks nothing or an entity is picked of
;;           a type not in the list of types passed to the function, prompt
;;           is repeated.
;; Params  : PR: prompt to display when selecting object
;;           ETLIST: entity type list of strings. NB: Must be all upper case.
;;                   If nil, then any entity type is allowed
;; Local   : done: boolean which is T only when correct entity type selected
;;           es: (entsel) list of selected object
;; --------------------------------------------------------------------------

(defun CGL_nice_entsel (PR ETLIST / done es)
  (while (not done)
    (setq es (entsel PR))
    (cond
      ((not es)
        (setq done (= (getvar "ERRNO") 52))
      )
      ((or (not ETLIST) (member (cdr (assoc 0 (entget (car es)))) ETLIST))
        (setq done T)
      )
      (T
       (prompt  (strcat "\nEntity is a " (cdr (assoc 0 (entget (car es)))) ", try again.")) 
      ; (prompt "\nTry again. ")
      )
    )
  )
  es
) ; End CGL_nice_entsel
;-------------------------------------------------------------------------
(defun CGL_GET_TEXT ()
  (while
    (setq es (CGL_nice_entsel "\nSelect text for editing..." '("TEXT" "MTEXT")))
    (setq TXT (cdr (assoc 0 (entget (car es)))))
    (cond
       ((= TXT "TEXT")
        ;(command "_.DDEDIT" es "")
	(setq CGL_ROTATION (cdr (assoc 50 (entget (car es)))))
	(setq CGL_ROTATION (rtd CGL_ROTATION))
	(setq NEW_ROTATION (+ 90 CGL_ROTATION))
	(setq NEW_ROTATION (DTR NEW_ROTATION))
	(setq ed (entget (car es)))
	(setq ed
	       (subst (cons 50 NEW_ROTATION)
		      (assoc 50 ed) ed
		      );end subst
	      );end setq
	(princ)
	(entmod ed)
	(princ)
      )
      ((= TXT "MTEXT")
	;(command "_.DDEDIT" es "")
       (setq CGL_ROTATION (cdr (assoc 50 (entget (car es)))))
	(setq CGL_ROTATION (rtd CGL_ROTATION))
	(setq NEW_ROTATION (+ 90 CGL_ROTATION))
	(setq NEW_ROTATION (DTR NEW_ROTATION))
	(setq ed (entget (car es)))
	(setq ed
	       (subst (cons 50 NEW_ROTATION)
		      (assoc 50 ed) ed
		      );end subst
	      );end setq
	(princ)
	(entmod ed)
	(princ)
      )
     );end cond
  );end while
  (princ)
  );end GET_TEXT
;-------------------------------------------------------------------------
(defun c:TXT90 ()
  (if (cgl_check_serial)
    (progn
     (CGL_get_text)
    );end checker progn
    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
    );end if
    (princ)     
)
