;TIP1222.LSP:     TB.LSP     Text Break     (c)1996, Yuqun Lian

;;; This routine writes a text string to the drawing and then breaks any
;;; lines, polylines, etc. that intersect an imaginary box around the text.
;;; The text is placed on the current layer using the current style. The
;;; default input and repeat capabilities of TB.LSP make multiple labeling
;;; very convenient.

;;; Yuqun Lian - SimpleCAD,  http://www.simplecad.com	    
;;; ------------------------------------------------------------------------
(defun tberror (S)
  (if (/= S "Function cancelled")
    (princ (strcat "\nError: " S))
  )
  (setvar "CLAYER" TEMPLA)
  (setvar "BLIPMODE" TEMPBLIP)
  (setvar "OSMODE" TEMPOS)
  (setvar "CMDECHO" TEMPCMD)
  (setq *error* OLDERR)
  (princ)
) ;end tberror

(defun C:TB ( / TEMP FIRST TX ANG TEMPLA TEMPCMD TEMPBLIP
  TEMPOS TXTST TXTH)
  (setq OLDERR *error* 
  *error* TBERROR)
  (setq TEMPCMD (getvar "CMDECHO")
    TEMPLA  (getvar "CLAYER")
    TEMPBLIP (getvar "BLIPMODE")
    TEMPOS (getvar "OSMODE")
    TXTST (getvar "TEXTSTYLE")
  *TXTH (getvar "TEXTSIZE"))
  (setvar "CMDECHO" 0) 
  (setvar "BLIPMODE" 0)
  (setq TXTH (cdr (assoc 40 (tblsearch "style" TXTST)))) 

  (setq TEMP T)
  (setq FIRST T) 
  (while TEMP
    (setvar "OSMODE" 512)     
    (setq PT1 (getpoint "\nInsertion point for text: "))     
    (setvar "OSMODE" 0)
    (cond
      ((/= PT1 nil)
        (if FIRST
          (progn

            (if (= TXTH 0)
              (progn
                (princ "\nHeight <")
                (princ *TXTH)
                (setq H (getreal ">: "))
                (if (= H nil) (setq H *TXTH)(setq *TXTH H))
              ) 
            )

            (if (not *ANG)(setq *ANG 0))
            (princ "\nRotation angle <")
            (princ (* *ANG (/ 180 3.1415926)))
            (setq ANG (getangle PT1 ">: "))
            (if (not ANG)(setq ANG *ANG)(setq *ANG ANG))
            (setq ANG (* ANG (/ 180 3.1415926)))    

            (if (not *TEXT)(setq *TEXT "XXX"))
            (princ "\nText <")
            (princ *TEXT)
            (setq TX (getstring T ">: "))
            (if (= TX "") (setq TX *TEXT)(setq *TEXT TX))
          ) ;end progn
        ) ;end first

        (if (= TXTH 0)
          (command "text" "j" "mc" PT1 *TXTH ANG TX )
        (command "text" "j" "mc" PT1  ANG TX )) 
        (trimbox)
      ) ;end pt1

      ((null PT1)
      (setq TEMP nil))

    );end cond
    (setq FIRST nil)
  );end while

  (setvar "CLAYER" TEMPLA)
  (setvar "BLIPMODE" TEMPBLIP)
  (setvar "OSMODE" TEMPOS)
  (setvar "CMDECHO" TEMPCMD)
  (princ)
)      

(defun trimbox (/ TEXTENT TRIMFACT TB GAP FGAP LL UR 
  PTB1 PTB2 PTB3 PTB4 PTF1 PTF2 PTF3 PTF4 BX)
  (setq TEXTENT (entlast))
  (setq TRIMFACT 0.5) ;trim gap and text height ratio  
  (command "ucs" "Entity" TEXTENT)
  (setq TB (textbox (list (cons -1 TEXTENT)))
    LL (car TB)
    UR (cadr TB)
  )
  (setq GAP (* *TXTH TRIMFACT))     
  (setq FGAP (* GAP 0.5))
  (setq PTB1 (list (- (car LL) GAP) (- (cadr LL) GAP))
    PTB3 (list (+ (car UR) GAP) (+ (cadr UR) GAP))
    PTB2 (list (car PTB3) (cadr PTB1))
    PTB4 (list (car PTB1) (cadr PTB3))
    PTF1 (list (- (car LL) FGAP) (- (cadr LL) FGAP))
    PTF3 (list (+ (car UR) FGAP) (+ (cadr UR) FGAP))
    PTF2 (list (car PTF3) (cadr PTF1))
    PTF4 (list (car PTF1) (cadr PTF3))
  )
  (command "pline" PTB1 PTB2 PTB3 PTB4 "c")
  (setq BX (entlast))
  (command "trim" BX "" "f" PTF1 PTF3 PTF4 PTF1 "" "")
  (entdel BX)
  (redraw TEXTENT)
  (command "ucs" "p")
  (princ)
) ;end trimbox

(princ "\nWritten by Yuqun Lian")
(princ "\nType TB to start") 
(princ); end tb.lsp
