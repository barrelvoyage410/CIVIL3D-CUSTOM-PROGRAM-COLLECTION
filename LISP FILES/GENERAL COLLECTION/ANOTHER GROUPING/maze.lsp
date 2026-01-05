;Tip1464.LSP:     MAZE.LSP    Fun with Mazes   (c)1998, Bill Fane

(setq ~ "20 January 1998")
(princ (strcat
"\nA-MAZE copyright " ~ " by Bill Fane.  May not be sold for cash or"
"\ntraded for anything of value except for scuba tank refills."
"\nNo Warranties expressed or implied."

"\n\nThis is the world's least useful AutoLisp program.  It randomly produces"
"\na different maze each time it runs.  Each maze has a single entrance, a single"
"\nexit, and only one solution path.  All other paths are dead ends.  There are no"
"\nshort-cuts or islands."
"\n\nWhen started, it will ask you for the desired size of maze. The number you"
"\nprovide defines the number of cells in the X direction.  The program calculates"
"\nthe number of cells in the Y direction so you can print to fit an 8.5x11 sheet."

"\n\nYou will be asked for a size, which defaults to 30 the first time.  If you run"
"\nit again it will default to the previous size."
"\nIf you save a maze and open it again later the program will recognize that it is"
"\na maze and will default to its size."
"\nThe program runs faster if you do not change the size because it does not have"
"\nto generate a new grid."

"\n\nThe size must be 4 or more.  Start small, because the time to run goes up"
"\nexponentially.  To avoid geologic run times, it is currently limited to a"
"\nmaximum of 120. This will take over an hour on a Pentium 133.  A size of 30"
"\nwill typically take less than a minute."
"\nIf you want to create larger mazes you can override the upper limit control by"
"\nentering (setq BIG 'n') at AutoCAD's Command: prompt before running the program,"
"\nwhere 'n' is the upper limit you want.  It must be an integer."

"\n\nIf you want to watch it thinking, enter (setq SHOW) at the Command: prompt"
"\nfirst, but this will slow it down by 2/3. To turn it off, enter (setq SHOW nil)."

"\n\nAfter it runs, thaw the SOLUTION layer to see the solution."

"\n \n \nEnter the command A-MAZE to generate a maze,"
"\n or press <F2> for more instructions."
"\n" 
))

(setq ~ nil)

; All variables are local unless you create BIG and/or SHOW.
; System variable USERI3 holds the current size.
; As well as the command c:A-MAZE, this routine creates three new functions:
; RANDOM, DO-PATH, and DRAW-GRID.

(defun c:A-MAZE ( /
                XMAX YMAX XY NX NY OLD-ERR NEWVARS OLDVARS X X1 X2 X3 Y1 Y2 Y3 COUNT TCOUNT
                SS SM DONE SOLN NC ENTER DIRN MP NC EN START ET D10 NPC RND
                )

;; SIZE OF MAZE

        (if (= (setq NX (getvar "useri3")) 0)
                (setq NX 30)
        )
        (initget (+ 2 4))        ;; no zero or negative
        (if (not BIG) (setq SM 120)(setq SM BIG))
        (while (or (< XMAX 4)(> XMAX SM))
                (setq XMAX
                        (getint
                                (strcat
                                        "Enter size of grid <"
                                        (itoa NX)
                                        "> "
                                )
                        )
                )
                (if (not XMAX)
                        (setq XMAX NX)
                )
                (cond
                        ((< XMAX 4)(alert "Must be 4 or larger"))
                        ((> XMAX SM)(alert (strcat "Must be " (itoa SM) " or smaller")))
                )
                
        )
        (setq
                YMAX        (fix (* XMAX 0.772727))                 ;; to fit 8-1/2 x 11 paper
                XY        (* XMAX YMAX)
        )

;; INITIAL CONDITIONS

        (princ
                (strcat
                        "\rSetting up "
                        (itoa XMAX)
                        " x "
                        (itoa YMAX)
                        " grid...."
                )
        )
        (setq
                OLD-ERR        *error*
                NEWVARS
                        '(
                                ("blipmode" . 0)("cmdecho" . 0)("pickbox" . 1)
                                ("ucsicon" . 0)("expert" . 5)
                        )
                OLDVARS        nil
        )
        (foreach X NEWVARS
                (setq OLDVARS
                        (cons
                                (cons
                                        (car X)
                                        (getvar (car X))
                                )
                                OLDVARS
                        )
                )
                (setvar (car X)(cdr X))
        )
        (command "undo" "c" "n")
        (defun *ERROR* (S / S)
                (foreach X OLDVARS
                        (setvar (car X)(cdr X))
                )
                (command "undo" "a")
                (princ (strcat "\n" S "\n"))
                (setq *error* OLD-ERR)
                (princ)
        )

        (command "layer" "set" "0")                        
        (if (not (tblsearch "layer" "done"))
                (command "new" "done,path,solution" "")
                (command "")
        )
        (setq
                NX        (1+ XMAX)
                NY        (1+ YMAX)
                COUNT        0
                TCOUNT        0
                X1        1
                Y1        1
        )
        (if (/= XMAX (getvar "useri3"))
                (draw-grid)
        )

        (if (not (tblsearch "Ltype" "hiddenx2"))
                (command "linetype" "load" "hiddenx2" "" "")
        )

        (command        "layer"
                        "color" "white" "done"
                        "color" "red" "path,solution"
                        "thaw" "path,done"
                        "freeze" "solution"
                        "Ltype" "hiddenx2" "solution"
                        ""
        )
        

        (if (setq SS (ssget "x" '((8 . "done"))))
                (command "chprop" SS "" "la" "0" "")
        )

        (if (setq SS (ssget "x"
                        '(
                                (-4 . "<or")
                                        (8 . "path")
                                        (8 . "solution")
                                        (-4 . "or>")
                                )
                        )
                )
                (command "erase" SS "")
        )
        (setq DONE (list '(0 0)))
        (repeat NX
                (setq
                        DONE        (cons (list X1 0) DONE)
                        DONE        (cons (list X1 NY) DONE)
                        X1        (1+ X1)
                )
        )
        (repeat NY
                (setq
                        DONE        (cons (list 0 Y1) DONE)
                        Y1        (1+ Y1)
                )
        )

;; LOCATE ENTRY POINT

        (setq        START (getvar "date"))
        (setq
                X1        0
                Y1        (+ 2 (random (- NY 4)))
                ENTER        (list -1 Y1)
                DONE        (cons ENTER DONE)
                SS2        (ssadd
                                (ssname
                                        (ssget
                                                (list 0.5 Y1)
                                        )
                                        0
                                )
                        )
                X2 1
                Y2 Y1
                MP (list X2 Y2)   ;; MaxPoint to restart after dead end
                SOLN        (list MP)
        )

        (if SHOW (entmake (list '(0 . "line") '(8 . "path") (list 10 0 Y1 0) (list 11 1 Y1 0))))


;; DEFINE SOLUTION PATH

        (princ
                (strcat
                        "\rChecking "
                        (itoa XY)
                        " cells......\n"
                )
        )
        (while (< X2 NX)
                (do-path)
                (if (= NC 4)
                        (setq
                                X1        (car MP)
                                Y1        (cadr MP)
                                SOLN        (member MP SOLN)
                        )
                )

        )

        (setq
                SOLN        (reverse SOLN)
                D10        ENTER
                Y1        0
        )

;; CREATE SOLUTION PATH LINE

        (entmake '((0 . "polyline")(8 . "solution")))
        (entmake (list '(0 . "vertex")(cons 10 ENTER)))
        (foreach D10 SOLN
                (entmake
                        (list
                                '(0 . "vertex")
                                '(8 . "SOLUTION")
                                (cons 10 D10)
                        )
                )
        )
        (entmake '((0 . "seqend")))
        (setq SOLN nil)
        (gc)


        (repeat NY
                (setq
                        DONE        (cons (list NX Y1) DONE)
                        Y1        (1+ Y1)
                )
        )


;; DEAD ENDS

        (setq
                X3        1
                Y3        1
                DIRN        1
        )
        (while (<= TCOUNT XY)
                (if (member (list X3 Y3) DONE)
                        (progn
                                (setq
                                        NPC        (list -10 -10)
                                        X1        X3
                                        Y1        Y3
                                        NC        0
                                )
                                (while (/= NC 4)
                                        (do-path)
                                        (if (/= NC 4)(setq NC 0))
                                )
                        )
                )
                (setq Y3 (1+ Y3))
                (if (>= Y3 NY)
                        (setq
                                X3        (1+ X3)
                                Y3        1
                        )
                )
                (if (>= X3 NX)
                        (setq
                                X3        1
                                Y3        1
                        )
                )
        )


        (setq ET (* 1440 (- (getvar "date") START)))
;        (princ (strcat "\rThis took " (rtos ET 2 4) " minutes."))

;; FINISH IT OFF

        (command
                "chprop" SS2 "" "la" "done" ""
                "layer" "f" "done,path" ""
                "undo" "a"
        )
        (foreach X OLDVARS
                (setvar (car X)(cdr X))
        )
        (setq *error* OLD-ERR)

        (princ "\r   All done!  Thaw layer SOLUTION to see the solution.")
        (princ "\n   Command is A-MAZE to run again.")
        (princ)
)

;; *******************************************************************************************

;;   SUBROUTINES START HERE

;; *** RANDOM

(defun RANDOM (R / RND-R)

;; R is Range 1->R for random integer output

        (if (not RND)
                (setq
                        RND   (* 1000000 (getvar "cdate"))
                        RND   (- RND (fix RND))
                )
        )
        (setq
                RND-R (/ (* 10000 (sqrt RND)) PI)
                RND   (- RND-R (fix RND-R))
        )
        (1+ (fix (* R RND)))
)

;; *** DRAW INITIAL SEUP

(defun DRAW-GRID ()
        (if (setq SS (ssget "x"))
                (command "erase" SS "")
        )
        (command
                "_zoom" "w" "-0.0,-0.0" (list (1+ XMAX)  (1+ YMAX))
                "_limits" "1,1" (list XMAX YMAX)
                "_line" "0.5,0.5" "@1,0" "@0,1" ""
                "_array" "w" "0,0" "2,2" "" "R" YMAX XMAX "1" "1"
                "_line" (list (+ XMAX 0.5)(+ YMAX 0.5)) (list 0.5 (+ YMAX 0.5))
        )
        (repeat YMAX
                (command "@0,-1")
        )
        (command "")
        (setvar "useri3" XMAX)
)

;; *** DO-PATH

(defun DO-PATH ()
        (setq
                DIRN        (random 4)
                NC        0
        )
        (while (< NC 4)
                (if (> DIRN 4)(setq DIRN (- DIRN 4)))
                (cond
                        ((= DIRN 1)
                                (setq
                                        X2 (1+ X1)
                                        Y2 Y1
                                )
                        )
                        ((= DIRN 3)
                                (setq
                                        X2 X1
                                        Y2 (1+ Y1)
                                )
                        )
                        ((= DIRN 4)
                                (setq
                                        X2 (1- X1)
                                        Y2 Y1
                                )
                        )
                        ((= DIRN 2)
                                (setq
                                        X2 X1
                                        Y2 (1- Y1)
                                )
                        )
                        (T (princ DIRN))
                )
                (setq NPC (list X2 Y2))
                (if (not (member NPC DONE))  ;; CHANGE CELL WALL TO 'DONE' LAYER
                        (progn
                                (setq
                                        DONE        (cons NPC DONE)
                                        COUNT        (1+ COUNT)
                                        TCOUNT        (1+ TCOUNT)
                                        NC        5
                                        EN        (car
                                                        (nentselp 
                                                                (list
                                                                        (/ (+ X1 X2) 2.0)
                                                                        (/ (+ Y1 Y2) 2.0)
                                                                )
                                                        )
                                                )
                                        SS2        (ssadd EN SS2)
                                )
                                (if SOLN (setq SOLN (cons NPC SOLN)))
                                (if (= COUNT 60)
                                        (progn
                                                (princ "\r     ")
                                                (princ (fix (* 100.0 (/ TCOUNT (float XY)))))
                                                (princ "% ")
                                                (setq COUNT 0)
                                        )
                                )

                                (if SHOW (progn
                                        (entmake
                                                (list
                                                        (cons 0 "line")(list 10 X1 Y1 0)
                                                        (list 11 X2 Y2 0)(cons 8 "path")
                                                )
                                        )
                                        (princ "") 
                                ))

                                (setq
                                        X1 X2
                                        Y1 Y2
                                )
                                (if
                                        (> X1 (car MP))
                                        (setq MP (list X1 Y1))
                                )
                        )
                        (setq
                                DIRN        (1+ DIRN)
                                NC        (1+ NC)
                        )
                )
        )
)

;;(princ "\n      Command is A-MAZE to create a maze.")
(princ)
