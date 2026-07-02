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

(defun C:CGL_MEDIT ()
(while
    (setq es (CGL_nice_entsel "\nSelect text for editing..." '("INSERT" "TEXT" "ATTDEF" "MTEXT")))
    (setq TXT (cdr (assoc 0 (entget (car es)))))
    (cond
      ((= TXT "INSERT")
        (command "_.DDATTE" es); actually, you should first check to see if it has attribs...
      )
      ((or (= TXT "TEXT") (= TXT "ATTDEF"))
        (command "_.DDEDIT" es "")
      )
      ((= TXT "MTEXT")
	(command "_.DDEDIT" es "")
      )
     );end cond
  );end while
); end MEDIT