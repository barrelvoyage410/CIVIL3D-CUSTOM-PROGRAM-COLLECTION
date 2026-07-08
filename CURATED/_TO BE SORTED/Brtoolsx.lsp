
(defun RTD (rad) (* 180.0 (/ rad pi)))

;!!!!!!!!!!!!!!!!!!!!!! Bill Renieris,  Email: "odsinc@dccnet.com" !!!!!!!!!!!!!!!!!!!!!!!!
 ;**********************************************************************************************
 ;-----------------Draw line @ an given angle and specified "visual orientation".
 ;                 From X axis (Right, Left) or Y axis (Top, Bottom).
(defun c:QL  (/ oldortho olderr oldecho a b ori ang ent1 ent2 ent3 ent4 ent5 Bill_Err)
  (defun Bill_Err  (s) (if (/= s "Function cancelled")(princ (strcat "\nError: " s)))
    (if ent1(command "u" "erase" ent1 ""))
    ;(command "u" "erase" ent1 "")
    (setvar "angdir" dir)
    (setvar "cmdecho" oldecho)
    (setvar "orthomode" oldortho)
    (setq *error* olderr)
    (princ))
(setq olderr   *error*
        *error*  Bill_Err
        oldecho  (getvar "cmdecho")
        oldortho (getvar "orthomode")
        dir      (getvar "angdir"))
  (setvar "cmdecho" 0)(setvar "angdir" 0)(setvar "orthomode" 0)
  (setq a (getpoint "\n<From Point: @> ") b (polar a 0 (getdist a "\nTo point/length: ")))
  (command "line" "_none" a "_none" b "")
  (setq ent1 (entlast) ang  (* 57.29577951 (getangle a "\nRotation Angle:")))
  (command "undo" "be")   
  (command "xline" "a" 45 a "") (setq ent2 (entlast))
  (command "xline" "a" -45 a "")(setq ent3 (entlast))
  (command "xline" "h" a "")(setq ent4 (entlast))
  (command "xline" "v" a "")(setq ent5 (entlast))
  (setq ori (getorient a "\nOrientation reference from Horizontal or Vertical: "))
  (if (and (<= ori 5.4977871) (>= ori 4.7123889)) (setq ang (+ 270 ang)))
  (if (and (<= ori 4.7123889) (>= ori 3.9269908)) (setq ang (- 270 ang)))
  (if (and (<= ori 3.9269908) (>= ori 3.1415927)) (setq ang (+ 180 ang)))
  (if (and (<= ori 3.1415927) (>= ori 2.3561945)) (setq ang (- 180 ang)))
  (if (and (<= ori 2.3561945) (>= ori 1.5707963)) (setq ang (+ 90 ang)))
  (if (and (<= ori 1.5707963) (>= ori 0.7853982)) (setq ang (- 90 ang)))
  (if (>= ori 5.4977871)(setq ang (- 360 ang)))
  (command "undo" "b" "rotate" ent1 "" a ang)
  (setvar "angdir" dir)(setvar "cmdecho" oldecho)(setq *error* olderr) (setvar "orthomode" oldortho)(princ))
 ;*****************************************************************************************************

 ;**********************************************************************************************************
 ;------------------- Sets current "text style" to that of the selected font
(Defun c:jt  (/ txtname)
  (setq txtname (cdr (assoc 7 (entget (CAR (ENTSEL))))))
  (command "textstyle" txtname)
  (princ))

 ;***********************************************************************************************************
 ;---------------------------------DDEDIT of multiple selections of "TEXT, MTEXT, DIMENSION"
(defun C:d  (/ i u ss1 ssi)
  (setq i 0 ss1 (ssget'((-4 . "<or") (0 . "text") (0 . "Mtext") (0 . "dimension") (-4 . "or>"))) u (sslength ss1))
        (while (< i u)
        (setq ssi (ssname ss1 i))
        (command "ddedit" ssi "")
        (setq i (1+ i)))(princ))
 ;***********************************************************************************************************
;--------------------------------------- Fractions ------------------------------------------------
 ;This routine is to display "Imperia Meas. fractions" by geting a distance on screen or typing it.
 ;It is using the DIMLINEAR command with your current DIMSTYLE 
 ;It is using "dimtfac" 0.75 for the fraction size, you may change it.
 
(defun c:fr  (/ oldosmode oldecho oldortho Bill_Err loc1 loc2 loc3 dq da ent1 e1 ss ss1 d1 d2 txt ansr)
  (defun Bill_Err  (msg)
    (if (member msg '("console break" "Function cancelled" "cancel"))
    (princ "\n*User ") (princ (strcat "\nError: " msg)))
    (command "dimstyle" "r" dq "dimaso" da "cmdecho" oldecho "orthomode" oldortho "osmode" oldosmode)
    (setq *error* acaderr)(princ))

  (setq oldecho  (getvar "cmdecho")
        oldosmode  (getvar "osmode")        
        oldortho (getvar "orthomode")
        dq       (getvar "dimstyle")
        da       (getvar "dimaso")
        acaderr  *error*
        *error*  Bill_Err)
  (setvar "cmdecho" 0)
  (setvar "orthomode" 1)
  (command "dimalt"   "off"      "dimunit"  4          "dimtfac"  0.75      "dimdec"   4          "dimzin"   3 
           "dimtol"   0          "dimlfac"  1          "dimsd1"   "on"       "dimsd2"   "on"       "dimse1"   "on" 
           "dimse2"   "on"       "dimfit"   1          "dimupt"   "on"       "dimaso"   "off"      "dimasz"   0  
           "dimblk1"  "none"     "dimblk2"  "none"     "dimgap"   0.0000001  "dimtofl"  "off")
  (setq loc1 (getpoint "\nLocation<@>: "))
  (setq loc2 (getpoint loc1 "\nTo point/<Length>: "))
  (setq loc3 (list (+ 0.001 (car loc2)) (+ 0.001 (cadr loc2)) (caddr loc2))) 
(setvar "osmode" 0)  
  (command "dimaligned" loc1 loc2 loc3 "")  
  (setq ent1 (entlast))
  (command "move" ent1 "" loc3 loc1)
  (command "dimstyle" "r" dq "dimaso" da "orthomode" oldortho "osmode" oldosmode) (princ) 
  (initget 1 "Yes")
  (setq ansr (getkword "\nExplode MTEXT and Clean-Up ? <Yes / Cancel> :"))
  (cond ((= ansr "Yes")
        (command "UCS" "W" "explode" ent1 "UCS" "P")
        (setq e1 (entnext ent1) ss (ssadd)) (ssadd e1 ss)
        (while (setq e1 (entnext e1)) (setq e1 e1) (ssadd e1 ss))
        (setq ss1 (ssget "p" '((0 . "line"))))
        (if (> (sslength ss) 1) (command "erase" ss1 ""))
        (setq d1 (entget (entlast)) d2 (assoc 1 d1) txt (cdr d2) txt (substr txt 1 (- (strlen txt) 1)))
        (setq d1 (subst (cons 1 txt) d2 d1))
        (entmod d1)
  )
    )
 (princ))
 ;****************************************************************************************************
 ;----------------------------------Turns dims Up-Right 
 (defun C:du  (/ i u ss1 dimname dimdata)
  (setq ss1 (ssget '((0 . "dimension"))))
  (setq u (sslength ss1)i 0)
  (while (< i u)
    (setq dimname (ssname ss1 i) dimdata (entget dimname) dimdata (subst (cons 51 0.0) (assoc 51 dimdata) dimdata))
    (entmod dimdata)
    (setq i (1+ i)))(princ))
 ;***************************************************************************************************
 ;---------------------------------Sets a point half-way between two given points 
 ;                                  Invoke transperantly
(defun C:MID2  (/ pt1 pt2 pt3 oldos)
(setq pt1 (getpoint) pt2 (getpoint))
(setq oldos (getvar "osmode")) (setvar "osmode" 0)
(command "point" (polar pt1 (angle pt1 pt2) (/ (distance pt1 pt2) 2.00)) "osmode" oldos))
 ;***************************************************************************************************
;--------------------------------------Mirror with deleting old objects
(defun c:33 (/ SS loc loc1)
(setq ss (ssget)loc (getpoint "First point: ") loc1 (getpoint  loc "\nSecond Point:"))
(command "mirror" ss "" "none" loc "none" loc1 "y")(princ))
;***************************************************************************************************
;-------------------------------------Breaks a line at an user's location
(defun c:BB (/ ent1 loc)
(setq  ent1 (entsel "Line to break:") loc (getpoint "\nSet break point: "))
(command "break" ent1 "f" "none" loc "none" loc )(princ))
;***************************************************************************************************
;-------------------------------------Entity particulars
(defun c:ent (/ z) (setq z (entget(car (entsel))))(princ z)(princ))
;***************************************************************************************************
;---------------------------------Display X,Y,Z coordinates
(defun C:df  (/ u up p px py pz pall)
(setq u  (getvar "dimunit") up (getvar "dimdec") p  (getpoint) 
       px (rtos (car p) u up) py (rtos (cadr p) u up) pz (rtos (caddr p) u up)
       pall (strcat "X= " px "\n" "Y= " py "\n" "Z= " pz))     
(alert pall)
(princ))
;*******************************************************************************************
;-----------------------------------UCS TO OBJECT, ROTATED X & Y TO POSITIVE
(defun C:oz(/ ent a aa b bb)
(setq ent (entsel "Select object: "))(command "ucs" "object" "none" ent)
(setq a (getvar "ucsxdir") aa (car a) b (getvar "ucsydir") bb (cadr b))
(if (< aa 0) (command "ucs" "y" "180")) (if (< bb 0) (command "ucs" "x" "180"))
(setq a (getvar "ucsxdir") aa (cadr a)) ;(if (< aa 0) (command "ucs" "z" "180"))
(princ))
;*******************************************************************************************
;-----------------------------------Copy & Rotate
(defun c:RR (/ ss1 loc)
(defun Bill_Err  (msg)  (command "undo" "m") (princ))
(setq ss1(ssget) loc (getpoint "Rotate about point: ") *error*  Bill_Err)
(command "undo" "m" "copy" ss1 "" "none" loc "none" loc "rotate" ss1 "" "none" loc pause "undo" "m")
(princ))
;*******************************************************************************************
;-----------------------------------Copy & Stretch
(defun c:ss (/ ss1 loc loc1 p1 p2)
(defun Bill_Err  (msg) (command "undo" "m")(princ))
(setq loc (getpoint "First corner: ") loc1 (getcorner loc "\nSecond corner: ") ss1 (ssget "w" loc loc1) *error*  Bill_Err)
(cond   ((not (= ss1 nil))
          (command "undo" "m" "copy" ss1 "" "none" loc "none" loc "stretch" "c" "none" loc "none" loc1 "r" ss1 "" pause pause ));"undo" "e" ))
        ((= ss1 nil)
          (command "stretch" "c" "none" loc "none" loc1 "" pause pause )
          (princ "\nNothing copied")
          (princ)))
(princ))
;*******************************************************************************************
;-----------------------------------SQUARE SOLID
(defun c:sd (/ loc loc1 loc2 loc3 loc4 w h ent)
(setq loc   (getpoint "Mid-left point: ")
       w    (getdist loc "\nWidth (on X-Axis): ")
       h    (getdist "\nHight (On Y-Axis): ")
       loc1 (list (car loc) (-(cadr loc)(/ h 2)) (caddr loc))
       loc2 (polar loc1 0 w)
       loc3 (list (car loc) (+(cadr loc)(/ h 2)) (caddr loc))
       loc4 (polar loc3 0 w))
(command "solid" "none" loc1 "none" loc2 "none" loc3 "none" loc4 "")
(setq ent (entlast))
(command "copy" ent "" "m" "none" loc pause)
(princ))
;*******************************************************************************************

;***********************************************************************************************************
;EXPLODES THE DIMENSION AND CLEANS THE FRACTION ASSUMING THAT THE SYMBOL (") IS THE LAST CHARACTER OF THE DIMENSION.
;CREATES A LAYER NAMED "$$EXPL-DIMS$$" AND PUTS ALL EXPLODED ENTITIES.
;THE DEFPOINTS LAYER MUST BE UNLOCKED (TO BE ABLE TO ERASE THE POINTS FROM THE EXPLODED DIMENSION) 
;------If a fabricator does not like the fractional line and the " symbol.
;------Best way to use this routine is, Just before you plot your drawing, clean-it up
;-------and then type "undo" "back" to restore the assoc. dimensions.
(defun C:cleanup  (/ i u ssall ssi ent1 e1 d1 d2 txt ltxt)
(setvar "cmdecho" 0)
(setq ssall (ssget '((0 . "dimension"))) i 0 u (sslength ssall))
(command "undo" "m")
(command "-layer" "m" "$$expl-dims$$" "")
 (while (< i u)
  (setq ssi (ssname ssall i)) (command "explode" ssi)          
  (setq ssi (ssget "p")) (command "chprop" ssi "" "la" "$$expl-dims$$" "")       
  (setq ssi (ssget "p" '((0 . "point")))) (command "erase" ssi "") (setq ent1 (entlast))
  (setq ename (cdr (assoc 0 (entget ent1))))
   (if (= ename "MTEXT") (progn
     (command "UCS" "W" "explode" ent1 "UCS" "P")
     (setq e1 (entnext ent1) ssi (ssadd)) (ssadd e1 ssi)
     (while (setq e1 (entnext e1)) (setq e1 e1) (ssadd e1 ssi))
     (setq ssi (ssget "p" '((0 . "line"))))(command "erase" ssi "")
     (setq d1 (entget (entlast)) d2 (assoc 1 d1) txt (cdr d2))
     (setq ltxt (substr txt (strlen txt)))     
      (if (= ltxt (chr 34)) (progn
      (setq txt (substr txt 1 (- (strlen txt) 1)))
      (setq d1 (subst (cons 1 txt) d2 d1))
      (entmod d1)
      )
       )
   )
    )
 (setq i (1+ i))
 )
(princ)
)
 ;***********************************************************************************************************
 ;------------------- Sets "text style, size, layer" of selected text to that of an other
(Defun c:ct  (/ i u ss1 ssi ssih ssiw txtname namefrom nameto heightfrom heightto widthfrom widthto layfrom layto ansr)
(command "undo" "m")
(prompt "Text style to be changed:")
(setq i 0 ss1 (ssget'((-4 . "<or") (0 . "text") (0 . "Mtext") (-4 . "or>"))) u (sslength ss1))
(prompt "Style to: ")
(setq txtname (entget (car (entsel))))
(setq nameto   (assoc  7 txtname))
(setq heightto (assoc 40 txtname))
(setq widthto  (assoc 41 txtname))
(setq layto    (assoc  8 txtname))
(while (< i u) 
  (setq ssi (ssname ss1 i) namefrom (assoc 7 (entget ssi)) ssi (subst nameto namefrom (entget ssi)))
  (entmod ssi)  (setq i (1+ i)))

(initget  "No Yes")
(setq ansr (getkword "\nChange Height ? (No <Yes>) :"))
(cond ((or (= ansr "Yes")(= ansr nil)) (setq i 0)
(while (< i u) 
  (setq ssi (ssname ss1 i) heightfrom (assoc 40 (entget ssi))
        ssi (subst heightto heightfrom (entget ssi)))
        (entmod ssi) (setq i (1+ i)))))

(initget  "No Yes")
(setq ansr (getkword "\nChange Width ? (No <Yes>) :"))
(cond ((or (= ansr "Yes")(= ansr nil)) (setq i 0)
(while (< i u) 
  (setq ssi (ssname ss1 i) widthfrom (assoc 41 (entget ssi))
        ssi (subst widthto widthfrom (entget ssi)))
        (entmod ssi) (setq i (1+ i)))))

(initget "No Yes")
(setq ansr (getkword "\nChange Layer ? (No <Yes>) :"))
(cond ((or (= ansr "Yes")(= ansr nil)) (setq i 0)
(while (< i u) 
  (setq ssi (ssname ss1 i) layfrom (assoc 40 (entget ssi)) ssi (subst layto layfrom (entget ssi)))
  (entmod ssi) (setq i (1+ i)))))
(princ)
)
 ;***********************************************************************************************************
; ---------------------It takes a line of text and puts it in the center of a circle
; ---------------------good for setting up Grids in diagrams

(defun C:cct  (/ cir cpoint txt txtname txtdata tpoint)
(setq txt (ssget '((0 . "text"))))
(setq cir (ssget '((0 . "circle"))))
(setq cirpoint (cdr(assoc 10 (entget (ssname cir 0)))))
(setq txtname (ssname txt 0) txtdata (entget txtname))
(setq txtdata (subst (cons 50 0.0) (assoc 50 txtdata) txtdata))(entmod txtdata)
(setq txtdata (subst (cons 72 1) (assoc 72 txtdata) txtdata))(entmod txtdata)
(setq txtdata (subst (cons 73 2) (assoc 73 txtdata) txtdata))(entmod txtdata)
(setq txtdata (subst (cons 11 cirpoint) (assoc 11 txtdata) txtdata))(entmod txtdata)
(princ cirpoint)
(princ)
)

;*****************************************************************************************************
;--------------It aligns the ft-inch and the fraction in "Mtext" (R14 bug- fixed in acad2000)
;---If you explode a dimension and edit the Mtext .....
(Defun c:xxx  (/ ssall u i ent txt txtfrom txtto)
(setq ssall (ssget '((0 . "mtext"))) i 0 u (sslength ssall)) (command "undo" "m")
(while (< i u)
(setq ent (entget(ssname ssall i)) txt (assoc 1 ent) txtfrom (cdr txt) txtto (strcat "\\A1;" txtfrom))
(setq ent (subst (cons 1 txtto) txt ent))
(entmod ent)
(setq i (1+ i)))(princ))
