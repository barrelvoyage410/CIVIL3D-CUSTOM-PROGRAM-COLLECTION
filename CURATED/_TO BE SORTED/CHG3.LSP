;;; -*-  Mode: LISP -*-  (C) Ben Olasov 1988, 1989
;;; Entity edit function C:CHG  v. 2.2
;;; Displays and modifies properties of individual entities.
;;; 2.0 Revisions: Date: December 10, 1989
;;; ANSI menu added: January 23, 1990
;;; Color ANSI graphics added January 30, 1990

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File: CHG.LSP     Copyright (C) Ben Olasov 1989                         ;;;
;;; Inquiries:                                                              ;;;
;;;                                                                         ;;;
;;;     Ben Olasov                                                          ;;;
;;;     Graphic Systems, Inc.:                                              ;;;
;;;                                                                         ;;;
;;;                    New York, NY:   PH (212) 725-4617                    ;;;
;;;                    MCI-Mail:       GSI-NY   344-4003                    ;;;
;;;                    Arpanet:        olasov@cs.columbia.edu               ;;;
;;;                                                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This program is provided 'as is' without warranty of any kind, either 
;; expressed or implied, including, but not limited to the implied warranties of
;; merchantability and fitness for a particular purpose.  The entire risk as to
;; the quality and performance of the program is with the user.  Should the 
;; program prove defective, the user assumes the entire cost of all necessary 
;; servicing, repair or correction. 
;; AutoLisp and AutoCad are registered trademarks of AutoDesk, Inc.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; CHG displays and modifies the properties of individual entities.          ;;
;;                                                                           ;;
;; CHG creates a numbered menu of the selected entities properties, and      ;;
;; then prompts the user to select the number of the property to modify.     ;;
;; CHG then prompts for a new value for that property, which may be a        ;;
;; point (list), real, integer, or string.                                   ;;
;;                                                                           ;;
;; Any changes made by CHG can be undone using AutoCad's 'U' command.        ;;
;; Doing so will return the drawing to its state before using CHG.           ;;
;;                                                                           ;;
;; A random example of using CHG:                                            ;;
;; In a drawing containing two valid blocks A and B, an individual           ;;
;; iteration of block A can be transformed to an iteration of block B by     ;;
;; giving B as its new name. All of its previous insertion parameters will   ;;
;; remain the same, but its identity will be changed to block B. If the      ;;
;; name of the layer in which the entity resides is changed to the name of   ;;
;; an existing layer, the entity will change its residence to that layer.    ;;
;; However, if the new layer name is the name of a non-existing layer, a     ;;
;; layer with that name will be created, and the entity will be transferred  ;;
;; to that layer.                                                            ;;
;;                                                                           ;;
;; Syntax: CHG                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(gc)
(vmon)
(graphscr)
(princ "\nLoading- please wait..  ")

(defun descriptor (key e_type)
       (cond ((null key) nil)
             ((= key -1) "ENTITY NAME <RO>")
             ((= key 0) "ENTITY TYPE")
             ((= key 5) "HANDLE <RO>")
             ((= key 6) "LINETYPE NAME <RO>")
             ((= key 7) "TEXT STYLE NAME <RO>")
             ((= key 8) "LAYER")
             ((= key 9) "VARIABLE NAME IDENTIFIER")
             ((and (>= key 10)
                   (<= key 18)) (point_handler key ent))
             ((= key 38) "ELEVATION")
             ((= key 39) "THICKNESS")
             ((and (>= key 1)
                   (<= key 72)
                   (or (= e_type "TEXT")
                       (= e_type "ATTDEF"))) (text_handler key ent))
             ((= key 1) "TEXT VALUE")
             ((and (>= key 2)
                   (<= key 71)
                   (or (= e_type "BLOCK")
                       (= e_type "INSERT"))) (block_handler key ent))
             ((= key 2) "NAME")
             ((or (= key 3)
                  (= key 4)) "OTHER NAME VALUE")
             ((= key 20) "PRIMARY Y COORDINATE")
             ((and (>= key 21) (<= key 28)) "OTHER Y COORDINATE")
             ((and (>= key 31) (<= key 36)) "OTHER Z COORDINATE")
             ((and (>= key 40)
                   (<= key 48)
                   (or (= e_type "CIRCLE")
                       (= e_type "ARC"))) "RADIUS")
             ((and (>= key 40)
                   (<= key 75)
                   (= e_type "POLYLINE")) (pline_handler key ent))
             ((and (>= key 40)
                   (<= key 48)) "FLOATING POINT VALUE")
             ((= key 49) "REPEATED VALUE")
             ((and (>= key 50)
                   (<= key 58)) "ANGLE")
             ((= key 62) "COLOR #")
             ((= key 66) "VERTICES FOLLOW <RO>")
             ((and (= key 70)
                   (= e_type "3DFACE")) (3dface_handler key ent))
             ((= key 71) "MIRROR DIRECTION")
             ((and (>= key 70) (<= key 78)) "INTEGER VALUE")
             ((or (= key 210)
                  (= key 220)
                  (= key 230)) "EXTRUSION DIRECTION COORDINATES")
             ((= key 999) "COMMENTS")
             (T "UNCLASSIFIED VALUE")))

(princ "\rLoading- please wait... \\")

(defun format-input (key / val label)
       (if (null key) nil
           (progn (setq val (cdr (assoc key entity)))
                  (graphscr)
                  (cond ((= (type val) 'STR)
                         (setq label (strcat (descriptor key etyp) ": "))
                         (princ (strcat "\nCurrent " label))
                         (princ val)
                         (getstring T (strcat "\nNew " label)))
                        ((= (type val) 'REAL)
                         (cond ((and (>= key 40)
                                     (<= key 48)
                                (or (= etyp "CIRCLE")
                                    (= etyp "ARC")))
                                (setvar "coords" 2)
                                (princ "\nCurrent angle: ")
                                (princ val)
                                (getdist (cdr (assoc 10 entity)) "\nNew radius: "))
                               ((and (>= key 50) (<= key 58))
                                (setvar "coords" 2)
                                (princ "\nCurrent angle: ")
                                (princ val)
                                (getangle (cdr (assoc 10 entity)) "\nNew angle: "))
                              (T (setq label (strcat (descriptor key etyp) ": "))
                                 (princ (strcat "\nCurrent " label))
                                 (princ val)
                                 (getreal (strcat "\nNew " label)))))
                         ((= (type val)  'INT)
                          (setq label (strcat (descriptor key etyp) ": "))
                          (princ (strcat "\nCurrent " label))
                          (princ val)
                          (getint (strcat "\nNew " label)))
                         ((= (type val) 'LIST)
                          (setvar "coords" 2)
                          (princ "\nCurrent point value: ")
                          (princ val)
                          (getpoint val "\nNew point: "))))))

(princ "\rLoading- please wait... \|")

(defun C:CHG (/ entity i ctr num tag new)
       (if (setq ename (entsel))
           (progn (setq ent (entget (car ename))
                        entity (aux_remove (assoc 0 ent) ent)
                        etyp (cdr (assoc 0 ent))
                        header (strcat etyp " PROPERTY TABLE")
                        num_props (length entity)
                        i 0
                        ctr 0)
                  (setq items nil)
                  (foreach e entity
                           (setq items
                                 (cons (strcat (descriptor (car e) etyp) 
                                               "\:  " (stringify (cdr e))) items)))
                  (setq num (menu-operation header
                                            (reverse items)
                                            "Number of property to change: "
                                             (ran_color)))
                  (if (and num
                           (> num 0)
                           (<= num num_props))
                      (progn (setq tag (car (nth (1- num) entity))
                                   new (format-input tag))
                             (if new
                                 (progn (setq ent (subst (cons tag new)
                                                         (assoc tag entity) ent)
                                              cmd (getvar "cmdecho"))
                                        (setvar "cmdecho" 0)
                                        (command "undo" "mark")
                                        (setvar "cmdecho" cmd)
                                        (entmod ent))
                                  (princ "\Null input.")))
                      (princ "\nInvalid number.")))
           (princ "\nNo entity selected."))
       (princ))

(princ "\rLoading- please wait... \/")

(defun text_handler (key elist)
       (setq bit_code (cdr (assoc key elist)))
       (cond ((= key 1) "TEXT VAL")
             ((= key 2) "ATTRIBUTE TAG")
             ((= key 40) "TEXT HGT")
             ((= key 41) "RELATIVE X SCL FACTOR")
             ((= key 50) "ROTATION ANG")
             ((= key 51) "OBLIQUING ANG")
             ((= key 70)
              (strcat "ATTRIBUTE "
                      (cond ((= bit_code 1) "<INVISIBLE>")
                            ((= bit_code 2) "<CONSTANT>")
                            ((= bit_code 4) "<VERIFICATION REQD>")
                            ((= bit_code 8) "<PRESET>")
                            (T "<UNKNOWN BIT CODE>"))))
             ((= key 71)
              (strcat "TEXT GENERATION "
                      (cond ((= bit_code 0) "")
                            ((= bit_code 2) "<BACKWARDS>")
                            ((= bit_code 4) "<UPSIDE DOWN>")
                            (T "<UNKNOWN BIT CODE>"))))
             ((= key 72)
              (strcat "TEXT JUSTIFICATION "
                      (cond ((= bit_code 0) "<LEFT JUSTIFIED>")
                            ((= bit_code 1) "<CENTERED ALONG BASELINE>")
                            ((= bit_code 2) "<RIGHT JUSTIFIED>")
                            ((= bit_code 3) "<ALIGNED BTWN 2 POINTS>")
                            ((= bit_code 4) "<MIDDLE CENTERED>")
                            ((= bit_code 5) "<FIT BETWEEN 2 POINTS>")
                            (T "<UNKNOWN CODE>"))))
            (T "UNKNOWN FLAG")))

(princ "\rLoading- please wait... \-")

(defun pline_handler (key elist)
       (setq bit_code (cdr (assoc key elist)))
       (cond ((= key 40) "STARTING WIDTH")
             ((= key 41) "ENDING WIDTH")
             ((= key 66) "VERTICES FOLLOW <RO>")
             ((= key 70)
              (strcat "POLYLINE "
                      (cond ((= bit_code 1)  "<CLOSED>")
                            ((= bit_code 2)  "<CURVE-FIT VERTICES ADDED>")
                            ((= bit_code 4)  "<SPLINE-FIT VERTICES ADDED>")
                            ((= bit_code 8)  "<3D POLYLINE>")
                            ((= bit_code 16) "<3D MESH>")
                            ((= bit_code 32) "<3D MESH CLOSED IN N DIRECTION>")
                            (T "<UNKNOWN BIT CODE>"))))
             ((or (= key 71)
                  (= key 72)) (strcat "POLYGON MESH "
                                      (if (= key 71) "M" "N")
                                     " COUNT"))
             ((or (= key 73)
                  (= key 74)) (strcat "POLYGON MESH "
                                      (if (= key 73) "M" "N")
                                      " DENSITY"))
             ((= key 75)
              (strcat "SMOOTH SURFACE TYPE "
                      (cond ((= bit_code 0)  "<NO SMOOTH SURFACE FITTED>")
                            ((= bit_code 5)  "<QUADRATIC B-SPLINE>")
                            ((= bit_code 6)  "<CUBIC B-SPLINE>")
                            ((= bit_code 8)  "<BEZIER SURFACE>")
                            (T "<UNKNOWN BIT CODE>"))))
             (T "UNKNOWN POLYLINE FLAG")))

(princ "\rLoading- please wait... \\")

(defun 3dface_handler (key elist)
       (setq bit_code (cdr (assoc key elist)))
       (cond ((= key 70)
              (strcat "INVISIBLE EDGE FLAG "
                      (cond ((= bit_code 0)  "<NO")
                            ((= bit_code 1)  "<1ST")
                            ((= bit_code 2)  "<2ND")
                            ((= bit_code 4)  "<3RD")
                            ((= bit_code 8)  "<4TH")
                            (T "UNIDENTIFIED"))
                      " EDGE INVISIBLE>"))))

(princ "\rLoading- please wait... \|")

(defun block_handler (key elist)
       (setq bit_code (cdr (assoc key elist)))
       (cond ((= key 2) "BLOCK NAME")
             ((= key 41) "X SCALE FACTOR")
             ((= key 42) "Y SCALE FACTOR")
             ((= key 43) "Z SCALE FACTOR")
             ((= key 44) "COLUMN SPACING")
             ((= key 45) "ROW SPACING")
             ((= key 50) "ROTATION ANG")
             ((= key 66) "ATTRIBUTES FOLLOW <RO>")
             ((and (= key 70)
                   (= etyp "BLOCK"))
              (strcat "BLOCK TYPE"
                      (cond ((= bit_code 1) "ANONYMOUS")
                            ((= bit_code 2) "ATTRIBUTES")
                            (T "UNKNOWN BIT CODE"))))

             ((= key 70) "COLUMN COUNT")
             ((= key 71) "ROW COUNT")
             (T "UNCLASSIFIED VALUE")))

(princ "\rLoading- please wait... \/")

(defun point_handler (key elist)
       (setq bit_code (cdr (assoc key elist)))
       (cond ((= key 10) "ORIGIN PT")
             ((and (= key 11)
                   (or (= etyp "LINE")
                       (= etyp "TEXT"))) "END PT")
             ((= key 11) "2ND PT")
             ((= key 12) "3RD PT")
             ((= key 13) "4TH PT")
             ((and (>= key 14)
                   (<= key 18)) "OTHER POINT COORDINATE")))

(princ "\rLoading- please wait... \-")

(defun aux_remove (atm lst) 
       (cond ((null lst) nil) 
             ((null (member atm lst)) lst)
             ((equal atm (car lst)) (cdr lst))
             (t (append (reverse (cdr (member atm (reverse lst))))
                        (cdr (member atm lst))))))


(princ "\rLoading- please wait... \\")

(defun stringify (exp)
       (cond ((null exp) "nil")
             ((= (type exp) 'STR) exp)
             ((= (type exp) 'ENAME) "")
             ((= (type exp) 'INT) (itoa exp))
             ((= (type exp) 'REAL) (rtos exp 2 6))
             ((= (type exp) 'LIST)
              (strcat "\(" (rtos (car exp) 2 4)
                           (if (cadr exp) (strcat " "(rtos (cadr exp) 2 4)) "")
                           (if (caddr exp) (strcat " " (rtos (caddr exp) 2 4)) "")
                       "\)"))
             (T "")))

(princ "\rLoading- please wait... \\") 

(DEFUN MENU-OPERATION (HEADER ITEM-LIST PRMPT COLOR / HGT WDT I L-COL)
       (MENU_INIT COLOR)
       (PAINT_BKGRND TOP_MARG L_COL HGT WDT COLOR)
       (PAINT_FRAME TOP_MARG L_COL HGT WDT)
       (PRINT_HEADER TOP_MARG L_COL WDT)
       (PRINT_ITEMS ITEM-LIST TOP_MARG L_COL COLOR)
       (PRINT_PRMPT PRMPT TOP_MARG L_COL HGT)
       (USR_VAL))

(princ "\rLoading- please wait... \|") 

(DEFUN MENU_INIT (COLOR)
       (TEXTSCR)
       (CLS)
       (NORMAL)
       (PRINC (STRCAT "\e[" (ITOA COLOR) "m"))
       (IF (/= (REM (STRLEN HEADER) 2) 0) (SETQ HEADER (STRCAT HEADER " ")))
       (SETQ HGT (+ 5 (LENGTH ITEM-LIST))
             WDT (+ 10 (MAX (LONGEST ITEM-LIST) (STRLEN HEADER))))
       (IF (/= (REM HGT 2) 0) (SETQ HGT (1+ HGT)))
       (IF (/= (REM WDT 2) 0) (SETQ WDT (1+ WDT)))
       (SETQ L_COL (- 40 (/ WDT 2))
             i 0
             TOP_MARG (- 12 (/ HGT 2))))

(princ "\rLoading- please wait... \/") 

(DEFUN PAINT_BKGRND (RW CL HT WD COLOR)
       (IF (> COLOR 40)  ;;don't try to paint invisible backgrounds
           (PROGN (GOTO (1+ RW) (1+ CL))
                  (REPEAT (- HT 1)
                          (REPEAT (- WD 2) (PRINC " " ))
                                  (NEXTROW (- WD 2))))))

(princ "\rLoading- please wait... \-") 

(DEFUN PAINT_FRAME (RW CL HT WD)
       (GOTO RW CL)           ;; position cursor at top left corner of frame 
       (PRINC (CHR 201))      ;; paint top left corner of frame
       (REPEAT (- WD 2)       ;; paint top of frame
               (PRINC (CHR 205)))
       (PRINC (CHR 187))      ;; paint top right corner of frame
       (REPEAT 3
               (NEXTROW WD)
               (PRINC (CHR 186))      ;; print side-of-frame char
               (MOVE (- WD 2) "C")    ;; move to right side of frame
               (PRINC (CHR 186)))     ;; print side-of-frame char
       (NEXTROW WD)
       (PRINC (CHR 204))                     ;;paint middle bar
       (REPEAT (- WDT 2) (PRINC (CHR 205)))
       (PRINC (CHR 185))
       (REPEAT (- HT 5)
               (NEXTROW WD)
               (PRINC (CHR 186))      ;; print side-of-frame char
               (MOVE (- WD 2) "C")    ;; move to right side of frame
               (PRINC (CHR 186)))     ;; print side-of-frame char
       (NEXTROW WD)
       (PRINC (CHR 200))
       (REPEAT (- WDT 2) (PRINC (CHR 205)))
       (PRINC (CHR 188)))

(princ "\rLoading- please wait... \\") 

(DEFUN PRINT_HEADER (RW CL WD)
       (GOTO (+ RW 3)
             (+ CL (- (/ WD 2) (/ (STRLEN HEADER) 2))))
       (BOLD)
       (PRINC HEADER)
       (NORMAL))
 
(DEFUN PRINT_HEADER (RW CL WD)
       (GOTO (+ RW 2)
             (+ CL (- (/ WD 2) (/ (STRLEN HEADER) 2))))
       (BOLD)
       (PRINC HEADER))
 
(princ "\rLoading- please wait... \|") 

(DEFUN PRINT_ITEMS (ITM_LST RW CL COLOR)
       (PRINC (STRCAT "\e[0m\e[" (ITOA COLOR) "m")) ;;restore normal screen
       (SETQ I 0)                                   ;;& then init user color
       (FOREACH ITEM ITM_LST
                (SETQ I (1+ I))
                (GOTO (+ RW 4)
                      (+ CL 2))
                (MOVE I "B")     ;; move I spaces down
                (PRINC (STRCAT " "
                               (IF (< I 10) " " "")
                               (RTOS (FLOAT I) 2 0) "] " ITEM))))

(princ "\rLoading- please wait... \/") 

(DEFUN PRINT_PRMPT (PRMPT RW CL HT)
       (NORMAL)
       (GOTO (+ RW HT 3) 0)
       (PRINC PRMPT)
       (GC))

(princ "\rLoading- please wait... \-") 

(DEFUN USR_VAL ()
       (NORMAL)
       (SETQ CHOICE (GETINT))
       (WHILE (OR (< CHOICE 1) (> CHOICE (LENGTH ITEM-LIST)))
              (SETQ CHOICE (GETINT "Choice is out of range, try again: ")))
       (CLS) CHOICE)

;;length of longest string in a list of strings
(princ "\rLoading- please wait... \\") 

(DEFUN LONGEST (LST)
       (APPLY 'MAX (MAPCAR '(LAMBDA (ITM) (STRLEN ITM)) LST)))

(princ "\rLoading- please wait... \|") 

(DEFUN BOLD ()
       (PRINC "\e[1m"))

(princ "\rLoading- please wait... \/") 

(DEFUN NORMAL ()
       (PRINC "\e[0m"))

(DEFUN RVRS ()
       (PRINC "\e[7m"))

(princ "\rLoading- please wait... \-")

(defun MOVE (NO DIR) ;;DIR ARG: A=UP   B=DOWN   C=RIGHT   D=LEFT
       (princ (strcat "\e[" (itoa NO) DIR)))

(princ "\rLoading- please wait... \/") 

(defun CLS () (textscr)
       (princ "\e[2J"))

(princ "\rLoading- please wait... \-") 

(defun goto (ROW COL)
       (princ (strcat "\e[" (itoa row) "\;" (itoa col) "H")))

(princ "\rLoading- please wait... \\") 

(defun nextrow (cols)
       (princ (strcat "\e[" (itoa cols) "D" "\e[1B")))

(defun ran_color (/ *s)
       (setq s (if s (rem (+ (* s 15625.7) 0.21137152) 1)
                      0.3171943)
             s (* 50 s))
       (cond ((< s 31) (setq *s (fix (max 31 (/ (+ s 46) 2)))))
             ((> s 46) (setq *s (fix (min 46 (/ (+ s 31) 2)))))
             (T (setq *s (fix s)))))

(princ "\rCommand level function C:CHG loaded.  Type CHG to begin.")
(princ)

