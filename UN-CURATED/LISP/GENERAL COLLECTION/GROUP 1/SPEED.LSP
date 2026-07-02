(VMON)
(prompt "\nLoading Lisp Speed and Profile Routines .....")
(defun c:VC()
(setq l (getreal "ENTER LENGTH OF CURVE \n "))
                                              ;***input for length of arc
(setq inc (getreal "ENTER STATION INCREMENT \n"))
                                            ;***input for station increment
(setvar "osmode" 1)
(setq pvi (getpoint "PICK THE PVI FOR CURVE (END)\n"))
                                              ;***input for pvi
(setvar "osmode" 2)
(setq g11 (getpoint " SELECT GRADE 1 TANGENT (MID)\n"))
                                              ;***input for computing g1
(setq g22 (getpoint " SELECT GRADE 2 TANGENT (MID)\n"))
                                             ;***input for computing g2
(setvar "osmode" 0)
(setq g1 (*
         (/
         (- (/(cadr pvi)1)(/(cadr g11)1))
         (- (car pvi)(car g11) )
                                   );***end of /
                                100);***end of *
                                   );***end of setq

                                      ;***COMPUTING G1
(setq g2 (*
         (/
         (-  (/(cadr g22)1) (/(cadr pvi)1) )
         (- (car g22) (car pvi) )
                                    );***end of /
                                 100);***end of *
                                    );***end of setq

                                     ;***COMPUTING G2

(setq pvce (-
           (cadr pvi)
           (*
           (/ g1 100)
           (/ l 2)
                       );***end of *
                       );***end of -
                       );end of setq
                                      ;***PVC ELEVATION


(setq pvcs (-
           (car pvi)
           (/ l 2)
                  );***end of -
                  );***end of setq

                                      ;***PVC STATION


(setq pvc (list pvcs pvce))
                           ;***PVC STATION AND ELEVATION
(setq pvt
         (list
              (+ pvcs l)
              (+ (cadr pvi) 
              (* (/ g2 100) (/ l 2 ) )
                                       );***END OF 1ST +
                                       );***END OF LIST
                                       );***END OF SETQPVT


(setq a
       (/
       (*
       (- g2 G1)
                 50);***end of *
                  l);***end of /
                   );***end of setq
                                     ;***COMPUTING A

(setq i
       (/ pvcs inc)
                   );***end of setq

(setq ii
        (fix i)
             );***end of setq

(setq x1 (* inc (- 1
        (- i ii)
           );***end of *
           );***END OF -
           );***end of setq
      


(setq fp
        (list
        (+ pvcs x1)
        (+
        (+
            (* a
            (expt (/ x1 100)2)    
                              );***END OF *A
            (* g1  (/ x1 100))      ;***END OF *G1
            )  ;***END OF 1ST +
           PVCE )  ;***END OF 2ND +
                )  ;***END OF LIST
      
                               );***END OF SETQ FP
   (command "line" pvc fp "")

  (setq np fp)         ;MAKES FP THE NEXT POINT TO BE EVALUATED
            

(while
       (< x1 l )
       (setq np1
       (list
       (+ pvcs x1
                                );***END OF +PVCS
        (+
        (+
        (* a
            (expt (/  x1 100)
                                       2 );***END OF EXPT
                                         );***END OF *A

        (*
        (/ x1
                             100);***END OF /
                              g1);***END OF *

                                );***END OF 1ST +
                            pvce);***END OF 2ND +


     
                               );***END OF LIST
                       );***END OF SETQ NP1
      (command "line" np np1 "")
(setq x1 (+ inc x1))     (setq np np1)
                               );***END OF WHILE
     (command "line" np1 pvt "")
                             );***END OF DEFUN
;MELBARD PROFILE ROUTINE  Modified by CDP, March 1994

(defun C:PSU () ; ******* Profile Set-Up
   (prompt "Start of Profile Setup: ")(terpri)
   (prompt "Pick Point for Datum Elev. and Start Sta. ")(terpri)
   (setq orpnt (getpoint "Use Object Snap to be Exact: "))(terpri)
   (setq spnt (getreal "Enter Starting Station: "))
   (setq de (getreal "Enter Datum Elevation: "))
   (setq vert (getreal "Enter Vertical Ratio: "))
   (SUOP) ; ********* Run Set-UP Out-Put ( SUOP ) Program
)
(defun SUOP (/ x xx) ; ******** Set-Up Out-Put
      (setq x (fix (/ spnt 100)))
      (setq xx (- spnt (* x 100)))
      (if (or (< (fix xx) 10) (> (fix xx) 0))
         (progn
         (prompt "Start STA  ")(princ x)(prompt "+0")(princ (rtos xx 2 2))
         (prompt "    Datum ELEV  ")(princ (rtos de 2 2))
         (prompt "    Vertical Ratio (V / H)  ")(princ vert)(terpri)
         )
      )
      (if (or (> (fix xx) 10) (< (fix xx) 0))
         (progn
            (prompt "Start STA  ")(princ x)(prompt "+")(princ (rtos xx 2 2))
            (prompt "    Datum ELEV  ")(princ (rtos de 2 2))
            (prompt "    Vertical Ratio (V / H)  ")(princ vert)(terpri)
            )  
         )
)

(defun C:PRO (/ fp mfp np mnp fx fy nx ny x xx)
   (if (= spnt nil)
      (progn
      (prompt "*ERROR* Must run Profile Set Up  < PSU >  first: ")(terpri)
      (getstring "Press any key to continue: ")
      (exit)
      )
   )
   (SUOP) : ****** Run Set-Up Out-Put Program
   (prompt "Start of Profile Routine: ") (terpri)
      (setq fp (getpoint "\nEnter First < STATION , ELEVATION >  "))
      (setq fx (+ (- (car fp) spnt) (car orpnt)))
      (setq fy (* (- (cadr fp) de) vert))
      (setq fy (+ (cadr orpnt) fy))
      (setq mfp (list fx fy))
      (setq np (getpoint "\nEnter Next <STATION , ELEVATION >  "))
      (setq nx (+ (- (car np) spnt) (car orpnt)))
      (setq ny (* (- (cadr np) de) vert))
      (setq ny (+ (cadr orpnt) ny))
      (setq mnp (list nx ny))
      (command "line" mfp mnp "")
      (while np
         (setq mfp (list (car mnp) (cadr mnp)))
         (princ "\nLast < STA , ELEV > Entered:  ")(princ (car np))
            (prompt " , ")(princ (cadr np))
         (setq np (getpoint "\nEnter Next <STATION , ELEVATION >  "))
         (setq nx (+ (- (car np) spnt) (car orpnt)))
         (setq ny (* (- (cadr np) de) vert))
         (setq ny (+ (cadr orpnt) ny))
         (setq mnp (list nx ny))
         (command "line" mfp mnp "")
   )
)

(defun C:PID (/ x xx) ; ******** Point ID
   (if (= spnt nil)
      (progn
      (prompt "*ERROR* Must run Profile Set Up  < PSU >  first: ")(terpri)
      (getstring "Press any key to continue: ")
      (exit)
      )
   )
   (setq pp (getpoint "Snap to a Profile Point for STA. and ELEV. ")) (terpri)
   (setq pidsta (+ (- (car pp) (car orpnt)) spnt))
      (setq x (fix (/ pidsta 100)))
      (setq xx (abs (- pidsta (* x 100))))
   (setq pidelev (- (cadr pp) (cadr orpnt)))
   (setq pidelev (+ (/ pidelev vert) de))
   (if (or (< (fix xx) 10) (> (fix xx) 0))
      (progn
      (prompt "STA. =  ")(princ x)(prompt "+0")(princ (rtos xx 2 2))
      (prompt "     ELEV. =  ")(princ (rtos pidelev 2 2)) (terpri)
      )
   )
   (if (or (> (fix xx) 10) (< (fix xx) 0))
      (progn
      (prompt "STA. =  ")(princ x)(prompt "+")(princ (rtos xx 2 2))
      (prompt "     ELEV. =  ")(princ (rtos pidelev 2 2)) (terpri)
      )
   )
)
(defun C:CSU () ; ******** Current Set-Up
   (if (/= spnt nil) (SUOP)
      (progn
         (prompt "*ERROR* Must run Profile Set Up  < PSU >  first: ")
      )
   )
)
;END PROFILE ROUTINE

(prompt "Done\n") (princ)

(defun C:B () (setvar "osmode" 0)(command "break" pause));*** Break
(defun C:BN () (setvar "blipmode" 1))        ;*** Set Blipmode On
(defun C:BF () (setvar "blipmode" 0))        ;*** Set Blipmode Off
(defun C:C  () (setvar "osmode" 0) (command "change"));*** Change
(defun C:CR () (command "circle"))           ;*** Circle
(defun c:CP () (command "chprop"))           ;*** Chprop
(defun C:CY () (setvar "osmode" 0) (command "copy"));*** Copy
(defun C:UI () (command "ucsicon"))          ;*** UCSicon
(defun C:UIO () (command "ucsicon" "off"))   ;*** UCSicon Off
(defun C:UION () (command "ucsicon" "on"))   ;*** UCSicon On
(defun C:MU () (command "menu"))             ;*** Menu
(defun C:DIV () (command "divide"))          ;*** Divide
(defun C:D  () (command "dist"))             ;*** Distance
(defun C:H  () (command "hatch"))            ;*** Hatch
(defun C:AR () (command "array"))            ;*** Array
(defun C:TX  () (command "text"))            ;*** Text
(defun C:DT () (command "dtext"))            ;*** Dtext
(defun C:SV () (command "save"))             ;*** Save
(defun C:LL () (command "ddlmodes"))         ;*** Ddlmodes
(defun C:DD () (command "ddedit"))           ;*** Ddedit
(defun C:GR () (command "ddrmodes"))         ;*** Ddrmodes
(defun C:PE () (command "pedit"))            ;*** Pedit
(defun C:REG () (command "regen"))           ;*** Regen
(defun C:MIR () (command "mirror"))          ;*** Mirror
(defun C:MR  () (command "mirror"))          ;*** Mirror
(defun C:PT  () (command "point"))           ;*** Point
(defun C:BL () (command "block"))            ;*** Block
(defun C:WB () (command "wblock"))           ;*** Wblock
(defun C:DO () (command "donut"))            ;*** Donut
(defun C:SP () (command "shape"))            ;*** Shape
(defun C:IN () (command "insert"))           ;*** Insert
(defun C:I  () (command "insert"))           ;*** Insert
(defun C:F  () (command "fillet"))           ;*** Fillet
(defun C:EW () (command "erase" "W"))        ;*** Erase Window
(defun C:ET () (print "Pick Lines To Extend To: ");*** Extend
(setvar "osmode" 0) (command "extend" pause))
(defun C:EX () (print "Pick Entity To Explode: "); *** Explode
(setvar "osmode" 0) (command "explode" pause))
(defun C:LI () (setvar "osmode" 0) (command "list"));*** List
(defun C:L? () (command "layer" "?" ))       ;*** Layer Query
(defun C:MW () (command "move" "W"))         ;*** Move Window
(defun C:MEA () (command "measure"))         ;*** Measure
(defun C:OO () (setvar "osmode" 0))          ;*** Osnap 'non'
(defun C:OC () (setvar "osmode" 4))          ;*** Osnap 'center'
(defun C:OE () (setvar "osmode" 1))          ;*** Osnap 'end'
(defun C:EE () (setvar "osmode" 1))          ;*** Osnap 'end'
(defun C:OI () (setvar "osmode" 32))         ;*** Osnap 'intersec'
(defun C:OM () (setvar "osmode" 2))          ;*** Osnap 'midpt'
(defun C:ON () (setvar "osmode" 8))          ;*** Osnap 'node'
(defun C:OS () (command "offset"))           ;*** Offset
(defun C:QY () (command "QUIT" "Y" 0) )      ;*** Quit, Yes
(defun C:RT () (command "rotate"))           ;*** Rotate
(defun C:S  () (command "scale"))            ;*** Scale
(defun C:T  () (print "Pick Cutting Edges: ")
(setvar "osmode" 0)(command "trim" pause))   ;*** Trim
(defun C:V  () (setvar "osmode" 0) (command "view"))   ;*** View
(defun C:VS () (setvar "osmode" 0) (command "VIEW" "S"));*** VIEW Save
(defun C:VR () (setvar "osmode" 0) (command "VIEW" "R"));*** VIEW Restore
(defun C:ZD () (setvar "osmode" 0) (command "ZOOM" "D"));*** Zoom Dyn
(defun C:ZA () (setvar "osmode" 0) (command "ZOOM" "A"));*** Zoom All
(defun C:ZW () (setvar "osmode" 0) (command "zoom" "w"));*** Zoom Window
(defun C:ZP () (command "ZOOM" "P"))         ;*** Zoom Previous
(defun C:ZE () (command "ZOOM" "E" ))        ;*** Zoom Extents

(defun C:LAS () (command "LOAD" "apw") (command "LOAD" "shapes")) ;Load

(defun C:AC ( / A B C )     ;*** Calculate Acres From Last Area Command
(if (getvar "area")
  (progn
    (setq B (GETVAR "AREA")  C (/ B 2.0)  A (/ B 43560.0))
    (PROMPT "\n ACRES        SQ. FT.   HALF SQ. FT.\n")
    (&PLIST (list
      (rtos A 2 4) "   " (rtos B 2 4) "  " (rtos C 2 4)
  ) ) )
  (print "Must Use The Acad 'AREA' Command First: "))
)

(defun C:CL ( / A B C )           ;*** Change Layer Of Entities By Picking
(prompt "\nPick Entities To Change The Layer Of")   (setq A (ssget))
(setq B (car (entsel "\nPick An Entity On The Layer To Change To")))
(setq C (cdr (assoc 8 (entget B))))
(command "change" A "" "p" "la" C "")  ) ;End C:CL

(defun C:LD ( / A B )     ;*** Deletes All Entities On The Specified Layer
  (setq A (strcase (getstring "\nEnter Layer To Delete: ")) B (entnext))
  (while B    (IF (= A (cdr (assoc 8 (entget B))))  (entdel B)  )
    (setq B (entnext B))  )) ;End while, C:LD

(defun C:LF ()                               ;*** Layer(s) To Freeze
  (setq L (getstring "\nEnter Layer(s) To Freeze: "))
  (command "layer" "F" L "")  )

(defun C:LT ()                               ;*** Layer(s) To Thaw
  (setq L (getstring))
  (command "layer" "T" L "")  )

(defun C:LP ( / L)                  ;*** Set Current a Layer By Picking It
  (setq L (cdr (assoc 8 (entget (car (entsel
                                        "\nPick Layer To Change To!"))))))
  (command "layer" "s" L "")  )
(
defun C:LO ( / L )                 ;*** Turn Off A Layer By Picking It
           (setvar "osmode" 0)
  (setq L (cdr (assoc 8 (entget (car (entsel "\nPick Layer To Turn Off!"))))))
  (command "layer" "off" L "")  )

(defun C:LOFF ( / L )                   ;*** Typing In Layer(s) To Turn Off
  (setq L (getstring))  (command "layer" "OFF" L "")  )

(defun C:LON ( / L )                    ;*** Typing In Layer(s) To Turn On
  (setq L (getstring))  (command "layer" "ON" L "")  )

(defun C:READ ( / A B C D E F)            ;*** Read In An ASCII File
  (setq A (getstring "Enter File-Name To Read: ")
        B (open A "r")  C (getreal "Enter Text Height: ")
        D (getpoint "Enter Starting Point: ")  E (* C 1.667))
  (while  (setq F (read-line B))
    (command "text" D C "0" F)
    (setq D (list (car D) (- (cadr D) E)))
  ) ;End while
  (close B) (princ) ) ;End READ

(defun C:R180 ( / A B C )               ;*** Rotate An Entity 180 Degrees
(setq C (getvar "aunits")) (setvar "aunits" 1)
(setq A (getpoint "\nPick Entity To Rotate 180 degrees "))
(while A    (setq B (getpoint "\nPick The Rotation Point "))
  (command "rotate" A "" B 180)
  (setq A (getpoint "\nPick Another Or Return To Quit "))
) (setvar "aunits" C) ) ;End while, C:R180

(defun C:R90 ( / A B C )                ;*** Rotate An Entilty 90 Degrees
(setq C (getvar "aunits"))  (setvar "aunits" 1)
(setq A (getpoint "\nPick Entity To Rotate 90 Degrees "))
  (while A    (setq B (getpoint "\nPick The Rotation Point "))
    (command "rotate" A "" B 90)
    (setq A (getpoint "\nPick Another Or Return To Quit "))
) (setvar "aunits" C)  );End while, C:R90

;                                  *** CHGTEXT command - rudimentary text editor
(defun C:TC (/ p l n e os as ns st s nsl osl sl si chf chm cont)
(setq chm 0 p (ssget))            ; Select objects
(if p (progn (setq cont t)        ; If any objects selected
  (while cont
    (setq osl (strlen (setq os (getstring "\nOld string: " t))))
    (if (= osl 0)
      (princ "Null input invalid")
      (setq cont nil)
  ) )
  (setq nsl (strlen (setq ns (getstring "\nNew string: " t))))
  (setq l 0 n (sslength p))
  (while (< l n)              ; For each selected object...
    (if (= "TEXT"            ; Look for TEXT entity type (group 0)
    (cdr (assoc 0 (setq e (entget (ssname p l))))))
      (progn
        (setq chf nil si 1)
        (setq s (cdr (setq as (assoc 1 e))))
        (while (= osl (setq sl (strlen (setq st (substr s si osl)))))
          (if (= st os)
            (progn
              (setq s (strcat (substr s 1 (1- si)) ns
                (substr s (+ si osl))))
              (setq chf t)    ; Found old string
              (setq si (+ si nsl))
            ) ;End progn
            (setq si (1+ si))
      ) ); End Progn, If
      (if chf (progn           ;*** Substitute New String For Old
                  (setq e (subst (cons 1 s) as e))
                  (entmod e)         ; Modify the TEXT entity
                  (setq chm (1+ chm))
               ))
         )  )   (setq l (1+ l))    )   ))
   (princ "Changed ")                ; Print total lines changed
   (princ chm)
   (princ " text lines.")
   (terpri)
) ;End TC

(prompt " Done\n")(princ)


(defun adjusta (ang)
(setq ang
  (if
    (and
      (>= ang (/ (* 3.0 pi) 2.0))
      (<= ang (* 2.0 pi))
    )
    (- ang (* 2.0 pi))
    ang
  )
 ))

