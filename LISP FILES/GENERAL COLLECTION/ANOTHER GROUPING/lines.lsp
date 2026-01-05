; lines.lsp
;
; This program accumulated lengths of LINEs, LWPOLYLINEs and 
; ARCs entities on layers.
;
; View result in AutoCAD Text Window (F2).
;
; This code is product of two programs:
; PLENG.LSP  Theo L.A. Groenenberg
;                       Leusden NL
;                       acadvice@worldonline.nl
;                       http://www.dra.nl/~acadvice
; and ADDLENTH.LSP unknown autor.
;
; Maximov Alexander , 08-1998 , maximov@ecoprog.ru
;
; Ken Switzer , 05-1999 , kswitzer@team-psc.com
; Modified -
;  added some choices to select a line on a layer or do all layers
;  added an alert box option to show the length in a dialog box one layer at a time
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:lines ()
  (setq llst (list (tblnext "LAYER" T)))
  (while (setq lay (tblnext "LAYER"))
    (if (/= lay "")
      (setq llst (cons lay llst))
    )
  )
  (setq n 0)
  (initget "All Select A S")
  (setq NUMLAY (getkword (strcat "\nDo you want all layers or select one.. (<All> or Select) ")))
  (if (or (= NUMLAY "Select")(= NUMLAY "S"))
    (progn
      (setq LAYER (setq a (cdr (assoc 8 (entget (car (entsel)))))))           
      (setq LLST (tblsearch "LAYER" LAYER))
      (setq REP 1)
    )
    (setq REP (length llst))
  )
  (initget "Y N")
  (setq DIA (getkword (strcat "\nDo you want a dialog box..(Y or <N>) ")))
;  (repeat (length llst)
  (repeat REP
    (if (= REP 1)
      (setq  name (cdr (assoc 2 llst)))
      (setq  name (cdr (assoc 2 (nth n llst))))
    )
    (setq selset(ssget "X" (list(cons 8 name))))
    (setq tot_len 0)
    (if selset
      (progn (setq c 0)
        (repeat (sslength selset)
          (setq en (ssname selset 0))
          (setq ed (entget en))
          (setq e_type (cdr (assoc '0 ed)))
          (cond
            ((= e_type "LINE") (ad_lines001))
            ((= e_type "ARC") (ad_arcs001))
            ((= e_type "POLYLINE") (ad_poly001))
	    ((= e_type "LWPOLYLINE") (ad_poly001))
            ((or
               (/= e_type "LINE")
               (/= e_type "ARC")
               (/= e_type "POLYLINE")
	       (/= e_type "LWPOLYLINE")
            )
	     
            (ssdel en selset));or
          );cond 
          (setq c (1+ c))
        )
      )
    )
    (setq n (+ 1 n))        
    (cond 
      ((> (strlen name) 27)
        (setq name (strcat name "\t"))
      )
      ((> (strlen name) 20)
       (setq name (strcat name "\t\t"))
      )
      ((> (strlen name) 13)
       (setq name (strcat name "\t\t\t"))
      )
      ((> (strlen name) 6)
       (setq name (strcat name "\t\t\t\t"))
      )
      ((> (strlen name) 0)
       (setq name (strcat name "\t\t\t\t\t"))
      )
    )
    (prompt (strcat "layer: " name  "Total length is: " (rtos tot_len 2 2)))(terpri)
    (if (= DIA "Y")
     (alert (strcat "layer: " name  "Total length is: " (rtos tot_len 2 2)))
    )
  );repeat
  (princ)
);defun


 (defun ad_lines001 (/ pt1 pt2 line_len)
   (setq pt1 (cdr (assoc '10 ed)))
   (setq pt2 (cdr (assoc '11 ed)))
   (setq line_len (distance pt1 pt2))
   (setq tot_len (+ tot_len line_len))
   (ssdel en selset)
 )
 
 (defun ad_arcs001 (/ CEN RAD DIA CIRCUM S_ANG E_ANG N_ANG N_ANG_1 PART_CIRC A_LEN)
   (SETQ CEN (CDR (ASSOC '10 Ed))
         RAD (CDR (ASSOC '40 Ed))
         DIA (* RAD 2.0)
         CIRCUM (* (* RAD PI) 2.0)
         S_ANG (CDR (ASSOC '50 Ed))
         E_ANG (CDR (ASSOC '51 Ed))
   )
   (IF (< E_ANG S_ANG)
     (SETQ E_ANG (+ E_ANG (* PI 2.0)))
   )
   (SETQ
         N_ANG (- E_ANG S_ANG)
         N_ANG_1 (* (/ N_ANG PI) 180.0)
         PART_CIRC (/ N_ANG_1 360.0)  
         A_LEN (* PART_CIRC CIRCUM)
   )
   (setq tot_len (+ tot_len a_len))
   (PRIN1)
   (SSDEL EN selset)
 )

 (defun ad_poly001 ()
   (command "area" "e" en)
   (setq tot_len (+ tot_len (getvar "perimeter")))
   (ssdel en selset)
 )      
(princ)