;;;**************************************
;;;*****************************************
;;;   Superior Designs - Custom programming available
;;;   http://www.inil.com/users/ccarr/sdi/acad.htm
;;;   e-mail ccarr@inil.com
;;;*****************************************
;;;Lisp routine for drawing a line and then entering text

(defun c:LE () (setq iterr *error* ad 1 radi ( * 0.1875 (getvar "dimscale")) clyr (getvar "clayer"))
(setvar "cmdecho" 0)(defun *error* (msg) (command "layer" "s" clyr "")
(setq *error* iterr)(setvar "cmdecho" 1)(princ))
(command "layer" "s" dimL "")(initget 1 "A")(setq ldr1 (getpoint "\nAdd/<From point>: "))
(if (= (type ldr1) 'LIST)(progn(initget 1)(setq ldr2 (getpoint ldr1 "\nTo point: "))
(setq ldr3 (getpoint ldr2 "\nTo point: "))(if (= ldr3 nil)
(progn(setq ang(angle ldr1 ldr2) dist(-(distance ldr1 ldr2) radi))
(command "LINE"  ldr1 (polar ldr1 ang dist) ^c)(setq ldr3 ldr2))
(progn(setq ang(angle ldr2 ldr3) dist(-(distance ldr2 ldr3) radi))
(command "LINE" ldr1 ldr2 (polar ldr2 ang dist) ^c)))
(command "layer" "s" TEXTL "")
(initget 1)(setq c(getstring t "\nEnter item #: "))(command "text" "m" ldr3 (* (GETVAR "DIMSCALE") (GETVAR "DIMTXT") ) 0 c)))
(if (= (strcase ldr1) "A")(progn(setq ldr3(cdr(assoc 10(entget(car(entsel "\npick existing balloon: "))))))
(setq dir(getangle ldr3 "\nDirection for balloon: "))
(if (and (<= dir 2.35) (>= dir 0.78))(setq ldr3 (list (car ldr3) (+ (cadr ldr3) (* 2 radi)))))
(if (and (< dir 3.92) (> dir 2.35))(setq ldr3 (list (- (car ldr3) (* 2 radi)) (cadr ldr3))))
(if (and (<= dir 5.49) (>= dir 3.92))(setq ldr3 (list (car ldr3) (- (cadr ldr3) (* 2 radi)))))
(if (or (< dir 0.78) (> dir 5.49))(setq ldr3 (list (+ (car ldr3) (* 2 radi)) (cadr ldr3))))
(command "layer" "s" dimL "")(command "circle" ldr3 radi)(command "change" "l" "" "p" "c" "green" "")
(command "layer" "s" TEXTL "")
(setq c(getstring t "\nEnter item #: "))(command "text" "m" ldr3 (* (GETVAR "DIMSCALE")(GETVAR "DIMTXT")) 0 c)))
(while ad(setq ad "")   ;;;;(getstring "\nAdd adjacent balloon:<Y> ")   )
(if (OR (= AD "") (= (strcase ad) "Y"))  (progn(setq dir(getangle ldr3 "\nDirection for balloon: "))
(if (and (<= dir 2.35) (>= dir 0.78))(setq ldr3 (list (car ldr3) (+ (cadr ldr3) (* 2 radi)))))
(if (and (< dir 3.92) (> dir 2.35))(setq ldr3 (list (- (car ldr3) (* 2 radi)) (cadr ldr3))))
(if (and (<= dir 5.49) (>= dir 3.92))(setq ldr3 (list (car ldr3) (- (cadr ldr3) (* 2 radi)))))
(if (or (< dir 0.78) (> dir 5.49))(setq ldr3 (list (+ (car ldr3) (* 2 radi)) (cadr ldr3))))
(command "layer" "s" dimL "")(command "circle" ldr3 radi)(command "change" "l" "" "p" "c" "green" "")
(setq c(getstring t "\nEnter item #: "))
(command "layer" "s" TEXTL "")(command "text" "m" ldr3 (* (GETVAR "DIMSCALE")(GETVAR "DIMTXT")) "0" c)) (setq ad nil)))
(command "layer" "s" clyr "")(setq *error* iterr)(setvar "cmdecho" 1) (princ))
;;;*****************************************
