;*-----------------------------------------------------------------------
;* APPLICATION: 3DPEDIT 
;* By GLENN WILSON
;*    74730,2726
;*    CENTRE DE TECHNOLOGIE NORANDA
;*    MONTREAL P.Q.
;*-----------------------------------------------------------------------
;* FILE:3DPEDIT.LSP 
;* FUNCTIONS: revline,nextvert,mkplist,mkpline,markpt,sublist,3djoin
;*            delents, bits
;* C:3DPEDIT
;*-----------------------------------------------------------------------
;* GLOBAL VARS
;    #plist     ==> vertex list
;    #count     ==> counter (current vertex)
;    #plong     ==> length of vertex list
;    #code70    ==> polyline 70 code
;    #del_list  ==> list of entities to erase
;
;-----------------------------------------------------------------------
; FUNCTION: revline
; DESCRIPTION: function to reverse a polyline list
;              
; CALLS: NOTHING
; RETURNS: nothing but reverses #plist
;-----------------------------------------------------------------------
(defun revline ()

  (setq #plist (reverse #plist) 
        #count 0
  );* END SETQ
  (redraw)

(markpt (nth #count #plist))

);* END DEFUN
;-----------------------------------------------------------------------
; FUNCTION: nextver
; DESCRIPTION: function to place a cross on the next vert
;              
; CALLS: NOTHING
; RETURNS: sfa but increments #count
;-----------------------------------------------------------------------
(defun nextvert ()
  
(markpt (nth #count #plist))

  (setq #count (if (eq #count #plong) 0 (1+ #count)))

(markpt (nth #count #plist))

);* END DEFUN
;-----------------------------------------------------------------------
; FUNCTION: mkplist
; DESCRIPTION: function to make a list from a pline given the polyline
;              entity
; CALLS: NOTHING
; RETURNS: the list of the pline
;-----------------------------------------------------------------------
(defun mkplist (polyline / vert vertg list_of_verts)

    (setq vert (entnext polyline))
    (setq vertg (entget vert))

      (while (/= (cdr (assoc 0 vertg)) "SEQEND")

        (setq list_of_verts (cons (cdr (assoc 10 vertg)) list_of_verts))

          (setq vert (entnext vert))
          (setq vertg (entget vert))

      );* END WHILE

    (setq list_of_verts (reverse list_of_verts))

);* END DEFUN
;-----------------------------------------------------------------------
; FUNCTION: mkpline 
; DESCRIPTION: function to create the new pline
;              
; CALLS: NOTHING
; RETURNS: nothing but draws the new 3dpoly
;-----------------------------------------------------------------------
(defun mkpline (pline_layer pline_color / counter p-length new_pline)

  (command "layer" "s" pline_layer "")

    (setq p-length (1- (length #plist)))
    (setq counter 0)

      (command "3DPOLY")

         (while (<= counter p-length)
            (command (nth counter #plist))
            (setq counter (1+ counter))
         );* END WHILE

      (if (bits 1 #CODE70) (command "C")
                           (command "")
      );* END IF
  
         (if pline_color
          (progn

           (setq new_pline (entget (entlast)))
           (setq new_pline (subst (con 62 pline_color) 
                                  (assoc 62 new_pline) 
                                  new_pline
                           );* END SUBST
           );* END SETQ

            (entmod new_pline)

          );* END PROGN
         );* END IF

);* END DEFUN     
;-----------------------------------------------------------------------
; FUNCTION: MARKPT
; DESCRIPTION: Function to draw a special tick at a point location
;
; CALLS: NOTHING
; RETURNS: NOTHING
;-----------------------------------------------------------------------
(defun markpt (pt / hi)

  (setq hi (/ (getvar "VIEWSIZE") 40.0))
  (setq pt (trans pt 0 1))

  (grdraw (polar pt (* pi 0.25) (- hi)) (polar pt (* pi 0.25) hi) -1)
  (grdraw (polar pt (* pi 0.75) (- hi)) (polar pt (* pi 0.75) hi) -1)

);defun MARKPT
;-----------------------------------------------------------------------
; FUNCTION: sublist
; DESCRIPTION: function to change the starting order of a list
;              
; CALLS: NOTHING
; RETURNS: sfa but modifies #plist and #count
;-----------------------------------------------------------------------
(defun sublist (/ sub subbylong)

 (markpt (nth #count #plist))

  (setq subbylong (1- (length #plist)))

       (repeat subbylong

         (setq sub (cons (nth #count #plist) sub))
         (setq #count (if (eq #count subbylong) 0 (1+ #count)))

       );* END REPEAT

    (setq sub (cons (nth #count #plist) sub))
    (setq #plist (reverse sub))
    (setq #count 0)

(markpt (nth #count #plist))

);* END DEFUN
;-----------------------------------------------------------------------
; FUNCTION: 3djoin
; DESCRIPTION: function to join 2 3dpoly's
;              
; CALLS: NOTHING
; RETURNS: 
;-----------------------------------------------------------------------
(defun 3djoin (/ line_2_join join_list)

  (setq line_2_join (car (entsel "\nSelect 3Dpoly: ")))

   (if line_2_join
    (progn

     (if (bits 8 (cdr (assoc 70 (entget line_2_join))))
      (progn

        (setq join_list (mkplist line_2_join))

          (grdraw (last #plist) (car join_list) -1)

        (setq #plist (append #plist join_list))
        (setq #plong (1- (length #plist)))

        (setq #del_list (cons line_2_join #del_list))

      );* END PROGN

      (princ "\nTHIS IS NOT A 3D POLYLINE")

     );* END IF
    
    );* END PROGN
    
    (princ "\nNO OBJECT SELECTED")
   );* END IF

);* END DEFUN
;-----------------------------------------------------------------------
; FUNCTION: delents
; DESCRIPTION: function to delete a list of entities
;              
; CALLS: NOTHING
; RETURNS: 
;-----------------------------------------------------------------------
(defun delents (ents_2_wipe_out / dead_ent)

   (setq dead_ent (1- (length ents_2_wipe_out)))

     (while (>= dead_ent 0)

       (entdel (nth dead_ent ents_2_wipe_out))
       (setq dead_ent (1- dead_ent))

     );* END WHILE

);* END DEFUN
;-----------------------------------------------------------------------
; FUNCTION: bits
; DESCRIPTION: return T if bit1 is present in the int fullbit
;              fullbit can be nil
; CALLS: NOTHING
; RETURNS: the bit or nill
;-----------------------------------------------------------------------
(defun bits (bit1 fullbit)
 (if (not fullbit) (setq fullbit 0))
  (setq fullbit (/ fullbit bit1))
    (if (zerop (rem fullbit 2)) nil bit1)
);* END DEFUN
;-----------------------------------------------------------------------
; FUNCTION: 3dpedit command
; DESCRIPTION: command to edit 3d polylines
;              
; CALLS: NOTHING
; RETURNS: 
;-----------------------------------------------------------------------
(defun c:3dpedit (/ answ p-line entlayer entcolor cl)

 (setvar "cmdecho" 0)
 (setvar "highlight" 0)
 (setvar "blipmode" 0)
 (setq #plist nil)
 (setq #count nil)
 (setq #del_list nil)

  (setq p-line (car (entsel "\nSelect polyline: ")))

   (if p-line
    (progn

     (if (bits 8 (cdr (assoc 70 (entget p-line))))
      (progn

       (setq #plist (mkplist p-line) 
             #plong (1- (length #plist)) 
             #count 0 
             cl (getvar "clayer") 
             entlayer (cdr (assoc 8 (entget p-line))) 
             entcolor (cdr (assoc 62 (entget p-line)))
             #CODE70 (cdr (assoc 70 (entget p-line))) 
       );* END SETQ

        ;*// MARK THE FIRST POINT IN THE LIST //*
        (markpt (nth #count #plist))

 (while (and (/= answ "Quit") (/= answ "Go"))

   (initget "Start Reverse Next Quit Go Join")                               
   (setq answ (getkword                                         
              (strcat "\nStart/Reverse/Join/Go/Quit/<Next>: ")     
              );* END GETKWORD 
   );* END SETQ                                                 
                                                                
     (cond ((eq answ "Reverse") (revline));* END FIRST COND    
                                                              
           ((eq answ "Start")   (sublist));* END OF 2ND COND   
                                                                
           ((eq answ "Go")      (mkpline entlayer entcolor));* END 3RD COND

           ((eq answ "Quit")    (setq answ "Quit"));*  END 4TH COND

           ((eq answ "Join")    (3djoin));* END OF 5TH COND

           (T                   (nextvert));* END OF 6TH COND

     ); END OF COND

  );* END WHILE

      (entdel p-line)

       (if #del_list (delents #del_list))

      (command "layer" "s" cl "")
      (redraw)

      (setq #plist nil)
      (setq #count nil)
      (setq #del_list nil)

      );* END PROGN

      (princ "\nTHIS IS NOT A 3D POLYLINE")

     );* END IF
    
    );* END PROGN
    
    (princ "\nNO OBJECT SELECTED")
   );* END IF

      (setvar "blipmode" 1)
      (setvar "highlight" 1)

 (princ)

);* END DEFUN
