;Fence.LSP: Cut out areas in your drawing using a fence.
;
;______________________________________________________________________________


(defun CUTCR (cpt1 lpt1 lpt2 / cent ang1
                  dst1 dst2 dst3 dst4 dst5 cord ang2 wkpt cpt2 cpt3)
  (setq cent (cdr (assoc 10 obj)))     ;center
  (setq rad1 (cdr (assoc 40 obj)))                   ;find radius of circle
  (setq ang1 (- (angle lpt1 cent)(angle lpt1 lpt2))) ;find difference of angles
  (setq dst1 (distance lpt1 cent))                   ;find dist.lpt1 to cent
  (setq dst2 (* dst1 (sin ang1)))                    ;find side of triangle
  (setq cord (sqrt (abs (-(* rad1 rad1)(* dst2 dst2)))))    ;find half cord
  (setq ang2 (- (angle lpt1 lpt2) 1.57))             ;find perpend angle
  (setq wkpt (polar cent ang2 dst2))                 ;find workpoint
  (setq dst3 (distance cent wkpt))
  (Setq dst4 (distance lpt1 cent))
  (Setq dst5 (distance lpt2 cent))
  (if (< dst3 rad1)
    (progn
      (if (and (> dst4 rad1)(> dst5 rad1))
       (progn
       (entdel cpt1)
       (setq cpt2 (polar wkpt (angle lpt1 lpt2) cord))    ;find first intersect
       (setq cpt3 (polar wkpt (angle lpt2 lpt1) cord))    ;find second intersect
         (command "arc" "c" cent cpt2 cpt3                  ;draw first circle seg.
                  "arc" "c" cent cpt3 cpt2                  ;draw second circle seg.
          );command                                    ;close command funct.
       );progn if and
      );if and
      (if (and (> dst4 rad1)(< dst5 rad1))
       (progn
        (entdel cpt1)
        (setq cpt2 (polar wkpt (angle lpt1 lpt2) cord))    ;find first intersect
        (command "arc" "c" cent cpt2 "a" "359.9")
       );progn if and
      );if and
      (if (and (< dst4 rad1)(> dst5 rad1))
       (progn
        (entdel cpt1)
        (setq cpt2 (polar wkpt (angle lpt2 lpt1) cord))    ;find first intersect
        (command "arc" "c" cent cpt2 "a" "359.9")
       );progn if and
      );if and
    );progn if < dst3
  );if
)                                                    ;close defun




;Function to create list of polyline vertices----------------------------------

(defun getver (EntNme / SubEnt VerLst vertex vertex2)
 (setq SubEnt (entnext EntNme))                     ;get first vertex
 (setq VerLst '())                                  ;setup vertex list
 (setq vertex (cdr (assoc 10 (entget SubEnt))))     ;get first vertex point
 (while (/= vertex nil)
   (setq VerLst (append VerLst (list vertex)))      ;add vertex to verlst
   (setq SubEnt (entnext SubEnt))                   ;go to next vertex
   (setq vertex (cdr (assoc 10 (entget SubEnt))))   ;get first vertex point
 )
 VerLst                                             ;return vertex list
)

( DEFUN POLYMK (pEnt a b wid / verlst ptyp newpt newver int y-int newlst)
  (setq VerLst (getver pEnt))                       ;extract vertices
  (setq newlst '())
  (setq vert1 (car verlst))
  (setq ptyp (assoc 70 (entget pEnt)))
      (while (cadr VerLst)
          (setq NewVer (append NewVer (list (car VerLst) ) ) )
          (setq int    (inters a b (car VerLst)(cadr verLst) ) )
          (if int
            (setq
               NewVer (append NewVer (list int))
               newlst (append newlst (list newver))
               newver (LIST INT)
               y-int t
            );setq
          );IF INT
          (setq VerLst (cdr VerLst))
      );end while
  (setq NewVer (append NewVer (list (car VerLst))))
    (if (= (cdr ptyp) 1)
      (setq NewVer (append NewVer (list Vert1)))
    )
(setq newlst (append newlst (list newver)))
;if there is an intersection ***********************************************
(if Y-int
(progn
(entdel pent)
(foreach newver newlst
 (progn
;    (entdel pent)
        (if(or (not col)(= col 0))
           (command "color" "bylayer")
           (command "color" col)
        )
        (if(= ltp nil)
           (command "linetype" "S" "bylayer" "")
           (command "linetype" "S" ltp "")
        )
  (command "layer" "S" la1 "")
  (command "erase" pEnt "")                         ;erase old pline
  (command "pline")                                 ;start pline command
  (foreach n NewVer (command n))                    ;insert points from NewVer
  (COMMAND)
  (if (> wid 0)
     (command "pedit" "l" "w" wid "")
  )
  (COMMAND "PLINE")
  (FOREACH N NEWVER2 (COMMAND N))
  (COMMAND)
  (if (> wid 0)
     (command "pedit" "l" "w" wid "")
  )
 );progn foreach
);foreach
);prog if y-int
);if y-int
;end of "if there is an intersection **********************************
)

(Defun linemk ()
(entdel nam)
  (if(or (not col)(= col 0))
     (command "color" "bylayer")
     (command "color" col)
  );IF COL
  (if(= ltp nil)
     (command "linetype" "S" "bylayer" "")
     (command "linetype" "S" ltp "")
  );IF LTP
(command "layer" "S" la1 ""
         "line" pt1 int pt2 "" )
(setq col nil ltp nil)
)

(defun mem (d l / h y last)
(setq h (list d l))
(while (and l (/= l "Close"))
   (setq y l)
   (initget "Close")
   (setq l (getpoint l "\nNext point/Close: "))
   (if (and l (/= l "Close"))
      (progn
      (setq last l)
      (grdraw l y -1 1)
      (setq h (append h (list l)))
      )
   )
   );while
 (if (= l "Close")
  (progn
  (setq h (append h (list first) ))
  (grdraw last first -1 1)
  )
 )

h
)

(defun c:fence ( / x y u b v1 v2 pt1 pt2 obj ccol first
                 cltp int nam typ la1 col ltp cla CUTENT NXTNME)
(setq cla (getvar "clayer"))
(setq ccol (getvar "cecolor"))
(setq cltp (getvar "celtype"))
(setvar "osmode" 0)
(setvar "cmdecho" 0)
(command "ucs" "W")
(initget 1 "Select")
(setq x (getpoint "\n<Pick first point of fence>/Select: "))
(if (= x "Select")
  (progn
     (setq cutent (CAR (entsel "\nPick line or pline defining cut location: ")))
       (setq ctyp (cdr (assoc 0 (entget cutent))))
       (if (= ctyp "POLYLINE")
         (PROGN
           (setq u (Getver cutent))
           (IF (= (CDR (ASSOC 70 (ENTGET CUTENT))) 1)
               (SETQ U (APPEND U (LIST (CAR U))))
           );if assoc 70
         );progn
       );if pline
       (IF (= CTYP "LINE")
           (SETQ U
             (LIST
              (setq pt1(cdr(assoc 10 (ENTGET CUTENT))))
              (setq pt2(cdr(assoc 11 (ENTGET CUTENT))))
             );list
           );setq u
      );if line
  );progn
  (PROGN
    (setq y (getpoint x "\nNext point: "))
    (setq first x)
    (grdraw x y -1 1)
    (setq u (mem x y))
  );progn
);if select
(setq a (car u))
(setq u (cdr u))
 (while (/= u nil)
  (setq b (car u))
  (setq v1(ssget "c" a b))
  (setq v2 0)
    (if (/= v1 nil)
    (while (< v2 (sslength v1))
      (setq obj (entget (SETQ NXTNME(ssname v1 v2))))
      (setq nam(cdr(assoc -1 obj)))
      (setq typ(cdr(assoc 0 obj)))
      (setq la1(cdr(assoc 8 obj)))
      (if (AND (= typ "POLYLINE")(/= NXTNME CUTENT))
          (PROGN
          (setq col(cdr(assoc 62 obj)))     ;color
          (setq ltp(cdr(assoc 6 obj)))      ;linetype
          (setq pwid (cdr (assoc 41 obj)))  ;pline width
          (polymk nam a b pwid)
          );PROGN
      )
      (if (AND (= typ "LINE")(/= NXTNME CUTENT))
          (progn
          (setq col(cdr(assoc 62 obj)))     ;color
          (setq ltp(cdr(assoc 6 obj)))      ;linetype
          (setq pt1(cdr(assoc 10 obj)))     ;beginning point
          (setq pt2(cdr(assoc 11 obj)))     ;end point
          (setq int(inters a b pt1 pt2 ))
          (if int (linemk))
          );progn
      );if LINE
      (if (= typ "CIRCLE")
          (progn
          (setq col(cdr(assoc 62 obj)))     ;color
          (setq ltp(cdr(assoc 6 obj)))      ;linetype
          (cutcr nam a b)
          )
      )

     (setq v2(+ v2 1))
    );while < v2
   );if
  (setq a (car u))
  (setq u (cdr u))
  );while
  (command "color" ccol
           "linetype" "S" cltp ""
           "layer" "S" cla ""
           "ucs" "P" )

)
