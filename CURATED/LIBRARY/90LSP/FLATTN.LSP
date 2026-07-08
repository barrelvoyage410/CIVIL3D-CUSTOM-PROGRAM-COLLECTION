; FLATTEN.LSP - sets Z-coordinates of LINEs, POLYLINEs, CIRCLEs, ARCs,
;  TEXT, Block INSERTs, and POINTs to 0
;
; by Mark Middlebrook, Daedalus Consulting  (415) 547-0602
; version 1.0  12-17-90

(defun Flatten (/ SS1 SS1Len i NumChg NumNot NumNo0 SSNo0 EName Elist EType)

;*get input
   (textscr)
   (prompt "\nFLATTEN sets the Z coordinates of LINEs, POLYLINEs, CIRCLEs, ")
   (prompt "\nARCs, TEXT, Block INSERTs, and POINTs to zero.")
   (prompt "\nFLATLAND must be set to zero for FLATTEN to run.")
   (getstring "\n\nHit enter to continue...")

;*exit if FLATLAND /= 0 [for some reason using (setvar) to change FLATLAND
; within the routine causes some entities to be ignored.
   (if (/= 0 (getvar "FLATLAND"))
      (prompt "\n\nFLATLAND not set to 0 - exiting command.")
      (progn         ;FLATLAND is 0 - proceed
         (graphscr)  
         (prompt "\nChoose entities to FLATTEN, or return for all... ")
         (setq SS1 (ssget))
         (if (null SS1)             ;if enter, ZOOM All and choose everything
            (progn
               (command ".ZOOM" "A")
               (setq SS1 (ssget "C" (getvar "EXTMIN") (getvar "EXTMAX")))
            );progn
         );if

;*initialize variables
         (setq SS1Len (sslength SS1)   ;length of selection set
               i 0                     ;loop counter
               NumChg 0                ;number changed counter
               NumNot 0                ;number not changed counter
               NumNo0 0                ;number not changed and Z /= 0 counter
               SSNo0 (ssadd)           ;selection set of unchanged entities
         );setq

;*do the work
         (prompt "\nWorking.")
         (while (< i SS1Len)                    ;while more members in the SS
            (if (= 0 (rem i 10)) (prompt "."))
            (setq EName (ssname SS1 i)             ;entity name
                  EList (entget EName)             ;entity data list
                  EType (cdr (assoc 0 EList))      ;entity type
            );setq

;*change group 10 Z coordinate to 0 for listed entity types
            (if (member EType 
                  '("LINE" "POLYLINE" "TEXT" "INSERT" "CIRCLE" "ARC" "POINT"))
               (setq EList (zeroz 10 EList)     ;change entities in list above
                     NumChg (1+ NumChg)
               );setq
               (progn                           ;leave others alone
                  (setq NumNot (1+ NumNot))     
                  (if (/= 0.0 (car (reverse (assoc 10 EList))))
                     (progn                  ;add it to special list if Z /= 0
                        (setq NumNo0 (1+ NumNo0))
                        (ssadd EName SSNo0)
                     );progn
                  );if
               );progn
            );if

;*change group 11 Z coordinate to 0 for LINEs and TEXT
            (if (or (= EType "LINE") (= EType "TEXT"))
               (setq Elist (zeroz 11 EList))
            );if

            (setq i (1+ i))               ;next entity
         );while

;*print results
         (prompt (strcat "\n" (itoa NumChg) " entity(s) flattened"))
         (prompt (strcat "\n" (itoa NumNot) " entity(s) not flattened"))

         (if (/= 0 NumNo0)          ;if there any entities in SSNo0, show them
            (progn
               (prompt (strcat "  {" (itoa NumNo0) 
                     " with non-zero base points}"))
               (getstring "\nHit enter to see non-zero unchanged entities... ")
               (command ".SELECT" SSNo0)
               (getstring "\nHit enter to unhighlight them... ")
               (command "")
            );progn
         );if
      );progn [else]
   );if
   (princ)
);defun


;*function to change Z coordinate to 0

(defun zeroz (key ZEList / OPList NPList)
   (setq OPList (assoc key ZEList)
         NPList (reverse (append '(0.0) (cdr (reverse OPList))))
         ZEList (subst NPList OPList ZEList)
   );setq
   (entmod ZEList)
);defun

(flatten)
