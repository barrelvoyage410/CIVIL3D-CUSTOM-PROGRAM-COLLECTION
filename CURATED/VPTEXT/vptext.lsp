; VPTEXT - This command scales all text in a drawing by
; a factor equal to the viewport scale, which is entered
; by the user.
; Usage: enter the viewport (MS)
;        enter: vptext at the command line.
;        enter the viewport scale factor at the prompt.
;        select your text objects, or type all.
;
; written by The CAD Guy - www.mycadsite.com
; August 11, 2000 - freeware.
; tested on acad2000, R14
;
(defun C:vptext (/ a ts n vpsf th index b1 b c d b2)           ;defines function and variables
     (setq vpsf (getdist "\nEnter viewport scale factor: "))   ; get input from user
     (setq a (ssget))                                          ; defines selection set
     (setq n (sslength a))                                     ; gets number of items selected
     (setq index 0)                                            ; sets counter for loop to 0
     (repeat n                                                 ; begin loop for number of items
          (setq b1 (entget (ssname a index)))                  ; get the entity name
          (setq index (+ index 1))                             ; adds to counter
          (setq b (assoc 0 b1))                                ; pulls out entity type 
          (if (= "TEXT" (cdr b))                               ; checks for text entity
               (progn                                          ; begins actual program 
                    (setq c (assoc 40 b1))                     ; gets text height
		    (setq th (cdr c))                          ; multiplies textheight by viewport scale
		    (setq ts (* vpsf th))                      ; sets new textheight
	            (setq d (cons (car c) ts))                 ; reconstructs entity with new textheight
                    (setq b2 (subst d c b1))                   ;   "                "
                    (entmod b2)                                ; replaces entity with updated one
               )
          )
     )
  (print)                                                      ; cleans command line
)

(princ "VPTEXT Loaded...scales text to viewport scale.")     ; loading information