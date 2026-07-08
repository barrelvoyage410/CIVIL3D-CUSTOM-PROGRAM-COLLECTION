;BK
;BREAKER.LSP                   *** VERSION 1.2 ***              (4/11/89)
;
;WILL BREAK A LINE AT AN INTERSECTION WITH ANOTHER LINE. AUTOMATICALLY SETS
;OSNAP TO "INTERSECT".                                              *[LL]  
;
(defun C:BK (/ ln bkpt)
(setq om (getvar "osmode"))
(setvar "cmdecho" 0)
(setvar "osmode" 32)
(setq ln (entsel "\nChoose Line to Break..."))
(setq bkpt (getpoint "\nPick Break Point.. "))
(command "break" ln "f" bkpt "@")
(setvar "osmode" om)
(setvar "cmdecho" 1)
(prin1)
)