;BANK.LSP                 *** VERSION 1.2 ***               (7/25/92)
;BANK/SLOPE LINES CREATION ROUTINE.
;DRAWS TOP OF SLOPE TRIANGLE, THEN ALTERNATING SCALLOPED LINE TO
;TOE OF SLOPE (AS INDICATED)
;REWRITE OF TRL/SCALLOP.LSP
;LAWRENCE LIEBERMAN - HOGAN, SCHOCH & ASSOCIATES - SEBASTOPOL, CA.
;
;*******************************************   SET/RESET WIGGLE SIZE
(defun C:SIZE ()
(initget "Five-hundreds Tenth Other")
(setq kw (strcase (getkword "\n[T]enth, [F]ive-hundreds, or [O]ther?: ")))
(cond
     ((= kw "TENTH")(setq sz (* scl 0.10)))
     ((= kw "FIVE-HUNDREDS")(setq sz (* scl 0.05)))
     ((= kw "OTHER")(setq sz (* scl (getreal "\nPlot Size of Scallop: "))))
     (T (setq sz (* scl (getreal "\nPlot Size of Scallop: "))))
;     (T (setq sz (* scl 0.05)))
)
(setq kw nil)
(prompt "\nType SIZE to reset size...  ")
(c:BANK)
)
;*******************************************   STEP THROUGH PLINE
(defun RUN ()
(setq ent (entlast))
(setq count 1)
(setq vertex (entnext ent))
(SETQ AFLAG 0)
(while (/= (cdr (assoc 0 (ENTGET vertex))) "SEQEND")
       (if (> aflag 0)(setq aflag 0 arc arc1)(setq aflag 1 arc arc2))
       (BULGE)
       (setq count (1+ count))
       (setq vertex (entnext vertex))
)
(entupd ent)
)
;*******************************************   INSERT BULGE FACTOR
(defun BULGE ()
(setq elist (entget vertex))
(setq elist (subst arc (assoc 42 elist) elist))
(entmod elist)
)
;*******************************************   C:BANK
(defun C:BANK (/ poly vertex count ent elist arc kw exit arc1 arc2 plist
                 bl px px1 px2 px3 px4 px5 bl2 px6 reps pset)
(setq cmd (getvar "cmdecho"))
(setvar "cmdecho" 0)
;*******************************************   SET SCALE
(if (and (getvar "userr2")(> (getvar "userr2") 0))
    (setq scl (getvar "userr2"))
    (setq scl (getreal "\nDrawing Scale?: ")))
;*******************************************   MAKE/SET SLOPE LAYER
(setq clay (getvar "clayer"))
(if (not (tblsearch "layer" "slope"))
    (command "layer" "m" "slope" "c" "15" "" "")
    (command "layer" "s" "slope" ""))
(if (not sz)(c:size))
(setq arc1 (cons 42 0.2))
(setq arc2 (cons 42 -0.2))
(setq plist nil)
(setq plist '())
;*******************************************   DO IT
(prompt "\nSelect Uphill Line: ")
(setq bl (entsel))
(setq px1 (osnap (cadr bl) "nea"))
(setq px2 (osnap (cadr bl) "end"))
(prompt "\nSelect Downhill Line: ")
(setq bl2 (entsel))
(setq px6 (osnap (cadr bl2) "nea"))
(setq ang (angle px1 px6))
(setq px3 (polar px1 (angle px1 px2)(* 1.2 sz)))
(setq px4 (polar px1 (angle px2 px1)(* 1.2 sz)))
(setq px5 (polar px1 ang (* 2.0 sz)))
(setq px px5)
(setq plist (cons px plist))
(command "pline" px3 px5 "")
(command "pline" px4 px5 "")
(setq d (distance px5 px6))
(setq reps (fix (/ d sz)))
(repeat reps
        (setq px (polar px ang sz))
        (setq plist (cons px plist)))
(setq plist (cons px6 plist))
(setq plist (reverse plist))
(foreach p plist (command "pline" "@" p ""))
(setq pset (ssget "c" (getvar "lastpoint") (nth 2 plist)))
(command "pedit" "l" "j" pset "")
(command) ; PRECAUTION
(RUN)
(setvar "cmdecho" cmd)
(command "layer" "s" clay "")
(princ)
)



