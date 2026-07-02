;sumlines.lsp
;3/2/87
;>>>Lansing Pugh (512) 441-4666, CIS 74375,457.<<<

;If you are experienced enough to be able to read this, you probably
;understand the concept of shareware.  I'd like to try to recover some
;of the cost of my time developing this and other lisp routines for Autocad
;by seeking voluntary payments.  If this routine is useful to you in doing
;paying work, please send $5 to:
;     Lansing Pugh
;     2316 Westrock Drive
;     Austin, Texas 78704
;If you decide to incorporate part or all of this routine in a commercial
;software package, please send a copy of that package to the same address.
;No acknowledgements in the software are required.
;If you just want to learn from this routine (its not a great example of
;LISP, it just works for me), no payment is sought.  If you do write some-
;thing you feel is pretty neat, I'd like a copy in exchange.
;
;Please give others copies of this routine. Do not remove these comments.
;Thanks!


(defun C:SUMLIN ()
(setq total 0)

(setq p nil)
(gc)

(prompt "\nSelect the lines you want to sum the lengths of: ")
(setq p (ssget))
(if p 
   (progn
     (setq n (sslength p))
     (setq l 0)
       (while (< l n)
           (if (= "LINE" (cdr (assoc 0 (setq elist (entget (ssname p l))))))
              (progn
                (setq start (cdr (setq as (assoc 10 elist))))
                (setq finish (cdr (setq as (assoc 11 elist))))
                (setq delta (distance start finish))
                (grdraw start finish -1)
                (setq total (+ total delta))
                (grtext -2 (strcat "Total: " (rtos total)))
              )
           )
            (setq l (1+ l))
          )
    )  
)
(print)
(write-line (strcat "Variable TOTAL has the sum of the length of the lines: " (rtos total)))
(graphscr)
)
