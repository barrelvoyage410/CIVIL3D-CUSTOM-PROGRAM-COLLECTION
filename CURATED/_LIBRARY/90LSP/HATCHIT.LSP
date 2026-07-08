;         ************************************************************
;         *  COPYRIGHT (C) 1992  LEO J. WOSTER  ALL RIGHTS RESERVED  *
;         ************************************************************
;
; ***** HATCHING ROUTINES; GLobal variables = set1 set3 lts ljw:acaderr
(defun C:HATCHIT ( / hata hatscc hatsf hatpat plins)
     (um) (set:err)(graphscr)
     (setvar "expert" 2); (command "save" "xxhatbak") 
     (setvar "cmdecho" 0) (setvar "expert" 0) (setvar "highlight" 1)
   (setq hatpat (getstring "\n** HATCHING PATTERN:  or <CR> for Default Pattern = ansi31 .....  "))
      (if (eq hatpat "") (setq hatpat "*ansi31") (setq hatpat (strcat "*" hatpat)))
   (setq hatsf (getreal "\n** PATTERN SCALE:  or <CR> for Default Scale = Ltscale .....  "))
      (cond ((not hatsf) (setq hatsf (getvar "ltscale"))))
   (setq hata (getreal "\n** HATCH ANGLE: or <CR> for Angle = 0.0 ....."))
       (cond ((not hata) (setq hata 0.0)))
   (setq hatscc (getreal "\nENTER SCANNING WIDTH FACTOR NOW  or <CR> for Default Factor = 1.0 .....  "))
      (cond ((not hatscc) (setq hatscc 1.0)))
     (command "layer" "n" "hat" "c" "blue" "hat" "set" "hat" "")
     (setq plins (ssget "X" (list (cons 0 "polyline") (cons 8 "hat"))))
(cond (plins 
     (setvar "cmdecho" 1) (command "ucs" "v")
     (command "hatch" "*ansi31" (* lts hatscc) "-45.0" plins "")
     (command "ucs" "")
     (setq set1 (ssget "X" (list (cons 0 "line") (cons 8 "hat"))))
     (CORRAL SET1) (command "erase" set1 "")
; main hatching
   (setvar "highlight" 1) (command "ucs" "v")
   (command "hatch" hatpat hatsf  hata plins set3 "")
   (command "ucs" "")
   (setq set1 (ssget "X" (list (cons 0 "line") (cons 8 "hat"))))
(setvar "cmdecho" 1) (setvar "highlight" 1)
(print " **** GLOBAL VARIABLES RETURNED:  !SET1 = Exploded Hatching Lines ")
(print " **** GLOBAL VARIABLES RETURNED:  !SET3 = Picked Hatching Entities ")
));end cond plins
(cond ((not plins) (print " ***** POLYLINE BOUNDARIES NOT FOUND ON LAYER {HAT} ??? *****   Check Drawing  & Try Again .... ")))
(setvar "cmdecho" 1) (setvar "highlight" 1) (setvar "expert" 0)
(setq *error* ljw:acaderr)(prin1));end c:hatline
;
; *****ROUTINE TO FILL IN HATCHING LINES MISSED BY ACAD HATCH COMMAND
(defun C:LH ( / otho el ex pt1 pt2 pt3 d10 d11);a line routine for filling in missing hatch lines
            (graphscr)(set:err)
            (setq otho (getvar "orthomode")) (setvar "orthomode" 1)
            (print) (print)
            (setq el (entsel "\n Pick End of Desired Line To Extend ..."))
              (cond (el (setq ex (car el))))
      (cond ((and el (eq "LINE" (azzoc 0 ex)))
            (rc el)
              (setq ex (car el)) (setq pt1 (car (cdr el)))
              (setq d10 (distance pt1 (azzoc 10 ex)) d11 (distance pt1 (azzoc 11 ex)))
              (if (< d10 d11) (setq pt1 (azzoc 10 ex)) (setq pt1 (azzoc 11 ex)))            
              (setq init "T")
         (while init
            (setq pt2 (getpoint pt1 "\n Now Pick Beginning Point ...  ")) 
             (cond (pt2 (setq pt3 (getpoint pt2 "And the Next Point On Line; or <RETURN> to End: "))
                        (command "line" pt2 pt3 "") (setq pt1 pt3)));end cond pt2
             (cond ((not pt2) (setq init nil)))
         );end while         
       ));end el
 (cond ((not (and el (eq "LINE" (azzoc 0 ex)))) (print " ***** ERROR *****  Line Not Found; Try Again ??? ")))
(setvar "orthomode" otho)(prin1));end c:lh 
;
; CONVERT HATCHING LINES INTO A BLOCK <BLKLINE>
(defun C:BLKLINE (/ set1 blk blkck)
   (graphscr) (set:err)
   (initget 1)
   (setq blk (getstring "\n  **** ENTER DESIRED BLOCK NAME NOW - or <CR> for Default Blockname = ljw$hat ..."))
     (cond ((eq blk "") (setq blk "ljw$blk")))
     (setq blkck (tblsearch "block" blk))
(cond ((not blkck) (setvar "cmdecho" 0)
             (setq set1 (ssget "X" (list (cons 0 "line") (cons 8 "hat"))))
        (cond (set1  (setvar "expert" 2)(command "block" BLK "0,0" set1 "")
                     (setvar "expert" 0)(command "insert" BLK "0,0" "" "" "")
                     (setvar "cmdecho" 1))
              ((not set1) (prompt "\n  **** NO LINES FOUND ON LAYER {HAT} ***** ")))
       )
       (blkck (print)(prompt "\nI'm Sorry, But I Cannot Let You Use An Existing Block Name; <CR> to Re-run & Enter New Name ...")) 
)
(prin1));end defun 
;
;*********** ROUTINE TO ROTATE CURSOR TO PICKED OBJECT ************
(DEFUN C:RC (/ a b c d angl pnt1 pnt2 osn)
     (graphscr)(set:err)
     (setq a (entsel))
     (if a
        (progn ; true
         (setq b (entget (car a)) d (cdr a))
         (setq c (cdr (assoc 0 b)));identify the entity
        );end true
        (setq c ""); false-TO PICK ANY 2 POINTS from screen
      );end if a
 (if (or (= c "LINE") (= c "TEXT") (= c "INSERT") (= c "CIRCLE") (= c "ARC") (= c ""))
   (progn; true
        (cond ((= c "LINE") (PROGN ;=line
                             (setq pnt1 (cdr (assoc 10 b)))
                             (setq pnt2 (cdr (assoc 11 b)))
                             (setq angl (angle pnt1 pnt2))
                             (setvar "snapang" angl)
                            ));end line progn & 1st cond test
               ((or (= c "INSERT") (= c "TEXT")) (PROGN ; = INSERT, TEXT
                                 (setq angl (cdr (assoc 50 b)))
                                 (setvar "snapang" angl)                                    
                            )); end insert,txt progn & 2nd cond test 
               ((= c "") (PROGN ; pik 2 pnts
                         (set:err1)
                         (setq pnt1 (getpoint "\nPick 1st Point:  "))
                         (setq pnt2 (getpoint pnt1 "\nPick 2nd Point:  "))
                         (setq angl (angle pnt1 pnt2))
                         (setvar "snapang" angl)
                            )); end pik 2 pnts progn & 3Rd cond test 
               ((or (= c "CIRCLE") (= c "ARC")) (PROGN ; = CIRCLE,ARC
                             (setq pnt1 (cdr (assoc 10 b)))
                             (setq pnt2 (car d))
                             (setq angl (angle pnt1 pnt2))
                             (setvar "snapang" angl)                                    
                            )); end circle,arc progn & 4th cond test 
        );end cond
    );end progn true 
   (PROGN ;FALSE- pick pline or any 2 pts if overide osnap=none from key
     (SETQ OSN (GETVAR "OSMODE")) (COMMAND "OSNAP" "QUICK,NEAR")
     (setq pnt1 (getpoint "\nPick 1st Point: (OSNAP @ NEAR)  "))
     (setq pnt2 (getpoint pnt1 "\nPick 2nd Point: (OSNAP @ NEAR)  "))
     (setq angl (angle pnt1 pnt2))
     (setvar "snapang" angl) (COMMAND "SETVAR" "OSMODE" OSN)
    );end false progn
  );end if 
(prin1)); end defun c:rc
(DEFUN C:RF () (setvar "snapang" 0)(setvar "angbase" 0)(PRIN1))
;
; *********** REFERENCED DEFUNS & INITIALIZED VARIABLES **************
;
(setq lts (getvar"LTSCALE"))
(cond ((not ljw:acaderr) (setq ljw:acaderr *error*)))
(setvar "expert" 0)
;
(defun rc (a / b c pnt1 pnt2 angl zzsnap)
     (graphscr) 
     (setq zzsnap (getvar "osmode")) (setvar "osmode" 0); iniatilize osnap
       (if (listp a)  
           (setq b (entget (car a))); if a selected by entsel; 
           (setq b (entget a)); if a determined by entlast,picked,etc
       );end if          
     (setq c (cdr (assoc 0 b)));identify the entity
        (cond ((= c "LINE")
               (setq pnt1 (cdr (assoc 10 b)))(setq pnt2 (cdr (assoc 11 b)))
               (setq angl (angle pnt1 pnt2)) (setvar "snapang" angl))
        );end cond
(setvar "osmode" zzsnap); reset osnap to initialized
(prin1)); end defun rc
;
****** VIEWPOINT ROTATING ROUTINES *****
(defun C:VP (/) ; rotate viewpoint graphically
     (graphscr) (setq om (getvar "orthomode"))(setvar "orthomode" 1)
     (command "point" (getvar "viewctr") "erase" "l" "")
     (COMMAND "VPOINT" "R" pause "89.9999" "")
     (setvar "orthomode" om))
(defun C:VPP (/) (COMMAND "VPOINT" "0.0,0.0,1.0" "")(prin1))
;
(defun corral (set1 / num ctr num2 ctr2 name set2 ptx pty); final result=set3
         (setq num (sslength set1) ctr 0 set3 (ssadd)) 
 (while (< ctr num)
     (setq linent (ssname set1 ctr) ptx (azzoc 10 linent) pty (azzoc 11 linent)) 
     (entdel linent)
     (setq set2 (ssget "C" ptx pty))
     (setq ctr (1+ ctr)) 
     (cond (set2 (setq num2 (sslength set2) ctr2 0)
        (while (< ctr2 num2)
           (setq name (ssname set2 ctr2))
           (cond ((equal (azzoc 0 name) "TEXT") (setq set3 (ssadd name set3)) (princ ".")))
           (cond ((equal (azzoc 0 name) "INSERT") (setq set3 (ssadd name set3)) (princ ".")))
           (setq ctr2 (1+ ctr2))
        );end while ctr2      
      ));end cond set2
      (print "Please Wait; I'm Hurrying as Fast As I Can ---   ")
 );end while
);end defun corral
;
(defun um () (setvar "cmdecho" 0)(command "undo" "m")
             (setvar "cmdecho" 1))
(defun azzoc (n x / b); assoc n=#,x=entsel(entname+coords),or entlast(name only),etc etc
      (if (listp x); is a list?-entsel is, if entity name only, not a list
           (setq b (entget (car x)));entsel=entity name + coords;  
           (setq b (entget x));entity name only      
      );end if-returns b as entity list
   (cdr (assoc n b)); returns that associated with #n
)
;
; ************ ERROR HANDLERS **************
;
(defun set:err ()
       (setq *error* ljw:err))
(defun ljw:err (msg)
     (setvar "cmdecho" 1) (setvar "highlight" 1) (setvar "expert" 0)
     (setq *error* ljw:acaderr)
     (prompt (strcat "\n*** LJW-ERROR: `" msg "';" " (Entry or Data Type *error* ???);  Try Again ..."))
);end defun
;
(defun set:err1 ()
       (setq *error* ljw:err1))
(defun ljw:err1 (msg);error handler-message only, does not reset error or snapout
     (prompt (strcat "\n*** LJW-ERROR1: `" msg "';" "..."))
      (prin1));end defun
;
(DEFUN C:INFO ()(textscr)
(prompt "\n")(prompt "\n")
(prompt "PRIOR TO USING THE HATCHIT ROUTINE, THE POLYLINE HATCHING BOUNDARIES")
(prompt "\nMUST BE PLACED ON LAYER <HAT>.")
(prompt "\n")
(prompt "\n 1. C:HATCHIT -- AUTOMATIC HATCHING ROUTINE.")
(prompt "\n")
(prompt "\n 3. C:LH  -- Use to Finish Any Hatching Missed By Autocad Hatching Command.")
(prompt "\n")
(prompt "\n 4. C:BLKLINE -- Converts Hatching LINES to A Block Named <BLKLINE>.")
(prompt "\n")
(prompt "\n 5. C:RC -- Rotate Cursor To Block, Text, Line, Polyline, or Two Picked Points.")
(prompt "\n")
(prompt "\n 5. C:RF -- Rotates Cursor To Snapang<le> = Zero.")
(prompt "\n")
(prompt "\n 5. C:VP -- Rotates VIEWPOINT Grahpically; <Use After Cursor Rotate - C:RC>.")
(prompt "\n")
(prompt "\n 5. C:VPP -- Rotates VIEWPOINT Back to 0,0,1 . . ")
(prin1)
)
