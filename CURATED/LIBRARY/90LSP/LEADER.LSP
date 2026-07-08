;Leader.LSP:  Draws a leader and offers a choice of arrows. It also allows
;             multiple lines of text to be entered as a note. Leader requires
;             the Dimwar, dimdot, and dimar drawing files to be present in
;             the current search path.
;___________________________________________________________________________

(defun c:leader (/ txtsz start midpt endpt htxts ang1 ang2 pt1 pt2 scle
                   arblk tsize)
(defun *error* (ms)
(setvar "orthomode" *ortho)
(princ ms)
(princ)
)
(setvar "cmdecho" 0)
(if (not *ar_df)(setq *ar_df "Arrow"))
(setq tsize (getvar "textsize"))
(setq *ortho (getvar "orthomode"))
(setq scle  (getvar "dimscale"))
(initget "Arrow Dot Wide")
(setq arblk (getkword (strcat "\nUse Arrow/Wide arrow/Dot <" *ar_df ">: ")))
 (cond
   ((= arblk "Arrow")(setq arblk "Arrow" *ar_df "Arrow"))
   ((= arblk "Wide")(setq arblk "Wide" *ar_df "Wide"))
   ((= arblk "Dot")(setq arblk "Dot" *ar_df "Dot"))
   ((not arblk)(setq arblk *ar_df))
 )
(setq start (getpoint "\nPick arrow end of leader: "))
(setq midpt (getpoint start))
(setq ang1  (angle start midpt))
(command "insert" arblk start scle "" (angtos ang1)
         "line" start midpt "")
(setvar "orthomode" 1)
(setq endpt (getpoint midpt))
(command "line" midpt endpt "")
(setvar "orthomode" *ortho)
(initget "Yes No")
(setq YorN (getkword "\nAdd note? <N> "))
(if (= YorN "Yes")
(progn
(setq htxts (/ tsize 2.0))
(setq ang2  (angle midpt endpt))
(setq pt1   (polar endpt ang2 htxts))
(setq pt2   (polar pt1 4.712389 htxts))
(if (< (car midpt)(car endpt))
(command "dtext" pt2)
(command "dtext" "r" pt2)
);end if
);end progn
);end if
)
