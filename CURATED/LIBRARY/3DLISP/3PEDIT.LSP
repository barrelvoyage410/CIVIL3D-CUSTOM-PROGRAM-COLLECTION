; 3D Utility             3Pedit.LSP          Ver 1.3           E Batson
; Convert 2d polyline, 3dface, line, arc, & circle to 3d polyline
; 1. Join 3Dpoly's (ends should meet).
; 2. If you accidently pick a 3DPoly, it is just drawn over again.
; 3. The Join function will replace the two 3DPolys with a single 3dPoly.
; 4. The Change function will just draw over the existing entity.
; 5. For a mesh , first explode it into faces, then change to 3dpoly(s).
; 6. Resolution will control smoothnes of curves, also make various shapes
;    such as... 6 = hex,  3 = triangle,  4 = square,  etc....
;*****************************************************************************
(princ "\nLoading...")
;..............................................................................
; Join two 3dpoly lines
(defun join3d
 (/ en flag1 flag2 en1 list1 list2 p1a p1b p2a p2b)
 (princ "\nJoin two 3DPolys.")
 (setq ss1 (entsel "\nSelect first 3dPoly.."))
 (redraw (car ss1) 3)
 (setq ss2 (entsel "....select second 3dPoly.."))
 (redraw (car ss2) 3)
 (setvar "blipmode" 0)
 (setq en1 (car ss1)
     poly1 (entget en1)
     flag1 (cdr(assoc 70 poly1))
       en2 (car ss2)
     poly2 (entget en2)
     flag2 (cdr(assoc 70 poly2))
 )
 (if (and (= (logand flag1 8) 8)(= (logand flag2 8) 8))  ; both 3D Polys ?
  (progn
    (setq lyr    (cdr(assoc 8 (entget en1)))             ; get first 3dpoly
          en     (entnext en1)                           ; stuff.
          list1 (cdr(assoc 10 (entget en)))
          chk1   (cdr(assoc 10 (entget en)))
          p1a    list1
    )
    (setq list1 (list list1))
    (while (= (cdr(assoc 0(entget(setq en(entnext en)))))"VERTEX")
       (setq list1 (append list1 (list(cdr(assoc 10(entget en))))))
       (setq p1b (cdr(assoc 10(entget en))))
    )
    (setq en     (entnext en2)                           ; get second 3dpoly
          list2 (cdr(assoc 10 (entget en)))              ; stuff.
          p2a list2
          chk2   (cdr(assoc 10 (entget en)))
    )
    (setq list2 (list list2))
    (while (= (cdr(assoc 0(entget(setq en(entnext en)))))"VERTEX")
      (setq list2 (append list2 (list(cdr(assoc 10(entget en))))))
      (setq p2b (cdr(assoc 10(entget en))))
    )
;-check for alignment of endpoints
    (cond
     ((equal p1b p2b 0.0001)                    ;if ---1----> <---2----
      (setq list2 (reverse list2)))             ; reverse #2.

     ((equal p1a p2a 0.0001)                    ;if <---1---- ---2---->
      (setq list1 (reverse list1)))             ; reverse #1.

     ((equal p1a p2b 0.0001)                    ;if ----2---> ---1---->
        (setq tmp list1 list1 list2 list2 tmp)) ; swap them.
    );end cond

    ;---------- do the ends meet ? ---------------------------
    (if (or                                     ; Check to see if the two
         (equal p1a p2a 0.0001)                 ; 3Dpolys meet.
         (equal p1b p2b 0.0001)
         (equal p1a p2b 0.0001)
         (equal p1b p2a 0.0001)
         )
     (progn                                      ; ok, they meet.
   ;-erase old stuff
       (entdel en1);<ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄ remove these two commands
       (entdel en2);<ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ    if you wish NOT to erase
       (princ "\nWorking, please wait.....");      the original entities.
   ;-draw new 3dPoly
       (command "layer" "s" lyr "")               ; draw the new 3dpoly in the
       (command "3dpoly")                         ; layer of the first selection.
       (foreach n list1 (command n))
       (setq list2 (cdr list2))                    ; remove the first point of
       (foreach n list2 (command n))               ; list2
       (command)
     )
     (progn
       (princ "\nEnds do not meet.")               ; not ok
       (redraw en1)
       (redraw en2)
       (exit)
     )
    );endif ends meet
    ;-----------------------------------------------------------
  );end main PROGN
  (progn
   (princ "\nAt least one of the lines selected was not a 3dPoly.")
   (redraw en1)
   (redraw en2)
  )
 );end if 3dPoly
 (prin1)
);end join3d

; Circle function
(defun cir (e / ia p1 eg cn rd step za)
 (princ " Circle")
 (setq eg    (entget e)
       za    (cdr(assoc 210 eg))
       cn    (cdr(assoc 10 eg))
       rd    (cdr(assoc 40 eg))
       step  (/ (* 2 pi) #res)
       p1    (polar cn pi rd)
       ia    (angle cn p1)
 )
 (command "UCS" "ZA" "" ZA)                          ; set to entity's ucs
 (command "3dpoly")
 (command p1)
 (repeat #res                                         ; follow curve
   (setq p1 (polar cn (setq ia (+ ia step)) rd))      ; with 3dpoly
   (command  p1)
 )
 (command)
 (command "ucs" "w")
)

; Line function
(defun lin (e)                                  ; simple stuff
 (princ " Line")
 (command "3dpoly")
  (command
   (cdr(assoc 10(entget e))))
  (command
   (cdr(assoc 11(entget e))))
 (command)
)

; 3dface function
(defun 3df (e)                                  ; simple stuff
 (princ " 3DFace")
 (command "3dpoly")
  (command
   (cdr(assoc 10(entget e))))
  (command
   (cdr(assoc 11(entget e))))
  (command
   (cdr(assoc 12(entget e))))
  (command
   (cdr(assoc 13(entget e))))
  (command "c")
)

; Bulge function. Draws short 3DPolys along curve..resolution in #res
 (defun bulge
  (p1 p2 bulge / ia step chd anga ica rad cha cen)
  (setq ica    (* 4 (atan bulge))                      ; included angle
        chd    (distance p1 p2)
        anga   (- (/ pi 2) (/ ica 2))                  ; 180ø- « of incl. ang.
        rad    (abs (/ (/ chd 2) (cos anga)))          ; radius
        cha    (angle p1 p2)
        step   (/ ica #res)
 );endsetq
 (if (minusp bulge)
   (setq cen (polar p2 (- cha anga) rad))             ; curve direction ??
    (setq cen (polar p1 (+ cha anga) rad)))
 (setq ia (angle cen p1))                             ; incrementing angle
 (command p1)
 (repeat #res                                         ; follow curve
   (setq p1 (polar cen (setq ia (+ ia step)) rad))    ; with 3dpoly
   (command  p1))
);end bulge function

; polyline function
(defun poly (e / za cl fp en vx cv nx)
 (princ " Polyline")
 (setq en e
       za (cdr(assoc 210(entget en)))                     ; get ucs data
       cl (if(=(cdr(assoc 70(entget en)))1)1)             ; closed flag 1=yes
       fp (cdr(assoc 10(entget(entnext en))))             ; save first point
       en (entnext en))                                   ; leave header
 (command "UCS" "ZA" "" za)                               ; set to entity's ucs
 (command "3dpoly")
 (command fp)                                             ; id first vertex
 (while (=(cdr(assoc 0(setq el(entget en))))"VERTEX")     ; do while not end
  (setq vx (cdr(assoc 10 el))                             ; this vertex
        cv (cdr(assoc 42 el))                             ; bulge
        nx (cdr(assoc 10(entget(entnext en)))))           ; next vertex
  (if (/= cv 0.0)
    (if nx (bulge vx (if nx nx (if cl fp)) cv))           ; a curve? (closed?)
    (command (if nx nx (if cl fp))))                      ; a line? (closed?)
    (setq en (entnext en))                                ; loop thru database
 );endwhile
 (command)
 (command "ucs" "w")
);end poly

(defun name (e / name)                                   ; tired of typing.
 (setq name (cdr(assoc 0(entget e))))
 (eval name)                                             ; return
)

(defun change_to_3d (/ res ss ssl e cnt)
 (setvar "blipmode" 0)
 (setq #res (if #res #res 20))                          ; initialize resolution
 (setq res (getint (strcat "\nCurve Resolution<"(itoa #res)">: ")))
 (if(boundp 'res)(setq #res res))
 (setq cnt -1)
 (setq ss (ssget))                                      ; get the stuff
 (princ "\nChanging..")
 (setq ssl (sslength ss))
 (repeat ssl                                            ; do 'em all.
  (setq e (ssname ss (setq cnt (1+ cnt))))
  (cond
   ((= (name e) "POLYLINE")(poly e))                    ; choices
   ((= (name e) "CIRCLE")(cir e))
   ((= (name e) "LINE")(lin e))
   ((= (name e) "ARC")                                  ; If its an ARC,
     (progn(command "pedit" e "y" "")(poly (entlast)))) ; change to polyline.
   ((= (name e) "3DFACE")(3df e))
  )
 )
)

;..........Main function....................
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Global variable = #res  (curve resolution)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun c:3pedit (/ choice)
 (initget "C J")
 (setq choice (getkword "\nChange/Join <J>: "))
 (cond
  ((= choice "C")
    (change_to_3d))
  (T
    (join3d))
 )
 (setvar "blipmode" 1)
 (command "ucs" "w")
 (princ)
);end c:3pedit
(princ "\n3Pedit.LSP    - Ver 1.3 -     Compliments of Batson Tool Corp.")
(princ "\nUsage -> Command: 3Pedit ")
(prin1)
