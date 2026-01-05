;; revised to TEXTPROP.lsp 7-15-1998 by Mark Evinger @ CGL
;  $Id: ctext.lsp,v 1.1.1.1 1993/10/18 17:54:46 rlm Exp $


;*CTEXT.LSP                  8/6/1990  PMH  
;*Version 11.00 Steel Detailer
;*Version 11.00 Structural Designer
;*Version 11.00 Structural Modeler

;GLOBAL CHANGE OF TEXT BY SELECTION SET   
;EEDIT FINDS SELECTED TYPES OF ENTITIES IN SELSET AND UPDATES ACAD DATABASE
(defun eedit (dindex selset oldval newval / scount lcount ename edata hnval
              align fit)
  (setq
    scount 0
    lcount (sslength selset)
  )
  (prompt "\nSearching...")
  (while (< scount lcount)
    (prompt ".")
    (setq
      ename (ssname selset scount)
      edata (entget ename)
    )
    (if (= (dxf 0 edata) "TEXT")
      (if (= dindex 7)
        (progn
          (setq
            hnval (dxf 40 (tblsearch "STYLE" newval))
            align (= (dxf 72 (entget ename)) 3)
            fit (= (dxf 72 (entget ename)) 5)
          )
          (setvar "GRIDMODE" 0)
          (command "_.UCS" "_E" ename) 
          (if oldval
            (if (= (strcase oldval) (dxf 7 edata))
              (if (zerop hnval)
                (if align
                  (command "_.CHANGE" ename "" "" "" newval "")
                  (if fit
                    (command "_.CHANGE" ename "" "" "" newval "" "")
                    (command "_.CHANGE" ename "" "" "" newval "" "" "")
                  )
                )
                (if (or align fit)
                  (command "_.CHANGE" ename "" "" "" newval "")
                  (command "_.CHANGE" ename "" "" "" newval "" "")
                )
              )
            )
            (if (zerop hnval)
              (if align
                (command "_.CHANGE" ename "" "" "" newval "")
                (if fit
                  (command "_.CHANGE" ename "" "" "" newval "" "")
                  (command "_.CHANGE" ename "" "" "" newval "" "" "")
                )
              )
              (if (or align fit)
                (command "_.CHANGE" ename "" "" "" newval "")
                (command "_.CHANGE" ename "" "" "" newval "" "")
              )
            )
          )
          (command "_.UCS" "_P") 
        )
        (progn
          (if oldval 
            (if (= oldval (dxf dindex edata))
              (entmod (subst (cons dindex newval) (assoc dindex edata) edata))
            )
            (entmod (subst (cons dindex newval) (assoc dindex edata) edata))
          )
          (entupd ename)
        )
      )
    )
    (setq scount (1+ scount))
  )
)
(defun C:TEXTPROP ( / tset dfield oldval dindex newval) ;was c:ctext, now c:textprop 7/15/1998 ME
   (if (setq tset (ssget))
      (progn
         (initget "Style Height Angle Text")
         (setq #errval (mapcar 'getvar (setq #errvar '("GRIDMODE")))
            Dfield (getkword "\nChange Height/Angle/Text/<Style>: ")
         )
         (cond   
            ((or (= Dfield "Style")(null Dfield))
               (setq Dindex 7
                  Oldval (getstring 0 "\nOld style name or RETURN for all: ")
               )
               (if (< (strlen Oldval) 1)
                  (setq Oldval nil)
               )
               (setq Newval (getstring 1 
                  (strcat "\nNew style name <" (getvar "TEXTSTYLE") ">: "))
               )
               (if (< (strlen Newval) 1)
                  (setq Newval(getvar "TEXTSTYLE"))
               )
               (while (not (tblsearch "STYLE" Newval))
                  (prompt "\nStyle not defined. Enter another name.")
                  (setq Newval (getstring 1 
                     (strcat "\nNew style name <" (getvar "TEXTSTYLE") ">: "))
                  )
                  (if (< (strlen Newval) 1)
                     (setq Newval(getvar "TEXTSTYLE"))
                  )
               )
            );s
            ((= Dfield "Height") 
               (setq Dindex 40)
               (setq Oldval (getdist "\nOld text height or RETURN for all: ")) ;changed to use getdist from lisp
               (setq Newval (getdist "\nNew text height: ")) ;changed to use getdist from lisp
            );h
            ((= Dfield "Angle")
               (setq Dindex 50
                  Oldval (getangle "\nOld rotation angle or RETURN for all: ")
               )
               (initget 1)
               (setq Newval (getangle (strcat "\nNew rotation angle <" (aangtos 0.0 (getvar "aunits")(getvar "auprec")) ">: ")))
            );a
            ((= Dfield "Text") 
               (setq Dindex 1
                  Oldval (getstring 0 "\nOld text or RETURN for all: ")
                  Newval (getstring 1 "\nNew text: ")
               )
               (if (< (strlen Oldval) 1)
                  (setq Oldval nil)
               )
            );t
         );cond
         (eedit dindex tset oldval newval)
         (mapcar 'setvar #errvar #errval)
         (setq #errvar nil #errval nil)
      );progn
   );if
   (princ)
);defun

(defun dxf (code elist / a)
  (setq a (cdr (assoc code elist)))
  (if (not a) (if (or (= code 38) (= code 39))  (setq a 0.0)))
  a
)

(princ)


