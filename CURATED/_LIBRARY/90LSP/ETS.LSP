;Ets.LSP: Ets lets you convert a series of text objects into a set of
;         sequential numbers.
;____________________________________________________________________________

 (defun c:ets ( / txt v1 v2 v3 tp snew new old unt prec)
 (setvar "cmdecho" 0)
 (if (not *uglb)(setq *uglb 2))
 (if (not *prglb)(setq *prglb 0))
 (if (not *incglb)(setq *incglb 1.0))
          (princ "Pick numbers to be changed in order of increment: ")
          (setq v1 (ssget '((0 . "TEXT")) ))
          (initget "Scientific Decimal Engin Arch")
          (setq tp (getkword "\nEnter unit type Scientific/Decimal/Engin/Arch <D>: "))
          (cond
                ((= tp "Scientific")(setq unt 1))
                ((= tp "Decimal")(setq unt 2))
                ((= tp "Engin")(setq unt 3))
                ((= tp "Arch")(setq unt 4))
                ((not tp) (setq unt *uglb))
          )
          (setq uglb unt)
          (setq prec (getint (strcat "\nEnter precision value <" (itoa *prglb) ">: ")))
          (if (not prec)
              (setq prec *prglb)(setq *prglb prec)
          )
          (setq inc (getreal (strcat "\nEnter increment value <" (rtos *incglb 2 2) ">: ")))
          (if (not inc)
              (setq inc *incglb)(setq *incglb inc)
          )
          (setq new (getreal "\nEnter new beginning value: "))
          (setq v2 0)
             (if (and v1 new)
                 (while (< v2 (sslength v1))
                        (setq snew (rtos new unt prec))
                        (setq snew (cons 1 snew))
                        (setq txt (ssname v1 v2))
                        (setq old (assoc 1 (setq v3 (entget txt))))
                        (entmod (subst snew old v3))
                        (entupd txt)
                        (setq new (+ new inc))
                        (setq v2 (+ v2 1))
                 )
             )
(princ)
)






