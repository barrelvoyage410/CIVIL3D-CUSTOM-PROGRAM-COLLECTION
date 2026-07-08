

; by Simon Jones - Autodesk UK Ltd.
; and Duff Kurland - Autodesk, Inc.
; November, 1986


; Syetem variable save
(defun modes (a)
   (setq MLST nil)
   (repeat (length a)
      (setq MLST (append MLST (list (list (car a) (getvar (car a))))))
      (setq a (cdr a)))
)

; Syetem variable restore
(defun moder ()
   (repeat (length MLST)
      (setvar (caar MLST) (cadar MLST))
      (setq MLST (cdr MLST))
   )
)

; Convert degrees to radians
(defun dtr (a)
   (* pi (/ a 180.0))
)

; Select all entities added since checkpoint.
(defun selstuff (e)
   (setq ss nil)                      ; Free old selection-set if present
   (setq ss (ssadd))                  ; Form empty selection-set
   (if (null e)                       ; No previous stuff in drawing?
       (setq ss (ssadd (setq e (entnext)) ss))  ; Start with what we drew
   )
   (while (setq e (entnext e))        ; Scan until end of drawing
       (setq ss (ssadd e ss))         ; Add each entity to selection-set
   )
   ss                                 ; Return selection-set
)

; Draw Spring
(defun spring (/ beta cen cosa deltal deltat e flop j numrseg numtseg
                px1 px2 px3 px4 
                py1 py2 py3 py4
                pz1 pz2 pz3 pz4
                ss1 ss2 hf hinc totseg arrfac
                radl radt sina x xorg yorg zorg)

   (initget (+ 1 16))                 ; Center point - 3D okay, cannot be null
   (setq cen (getpoint "\nSpring center point: "))

   (setq radl -1 radt 0)
   (while (> radt radl)
       (initget 7 "Diameter")         ; Radius cannot be zero, neg, or null
       (setq radl (getdist cen "\n<Spring radius>/Diameter: "))
       (if (= radl "Diameter")
           (progn
               (initget 7)            ; Diameter cannot be zero, neg, or null
               (setq radl (/ (getdist cen "\nSpring diameter: ") 2.0))
           )
       )

       (initget 7 "Diameter")         ; Radius cannot be zero, neg, or null
       (setq radt (getdist cen "\n<Tube radius>/Diameter: "))
       (if (= radt "Diameter")
           (progn
               (initget 7)            ; Diameter cannot be zero, neg, or null
               (setq radt (/ (getdist cen "\nTube diameter: ") 2.0))
           )
       )
       (if (> radt radl)
           (prompt "\nTube radius cannot exceed spring radius.")
       )
   )

   (setq hf (getdist cen "\nHeight per rotation: ")) ;NEW
   (setq #rot (getint "\nNumber of rotations: "))

   (while (or (< numrseg 8) (> numrseg 24))
       (initget 6)                    ; Cannot have zero or negative segs
       (setq numrseg (getint "\nNumber of radial segments (8-24) <16>: "))
       (if (null numrseg)
           (setq numrseg 16)
       )
       (if (or (< numrseg 8) (> numrseg 24))
           (prompt "\nOutside acceptable range.")
       )
   )

   (setq hinc (/ hf numrseg))     ;NEW
   (setq totseg (* #rot numrseg))

   (while (or (< numtseg 8) (> numtseg 24))
       (initget 6)                    ; Cannot have zero or negative segs
       (setq numtseg (getint "\nNumber of tube segments (8-24) <16>: "))
       (if (null numtseg)
           (setq numtseg 16)
       )
       (if (or (< numtseg 8) (> numtseg 24))
           (prompt "\nOutside acceptable range.")
       )
   )

   (setvar "BLIPMODE" 0)
   (setq e (entlast)                   ; Take database checkpoint
         cmark (entlast)
         deltat (* 2.0 (/ pi numtseg))
         deltal (* 2.0 (/ pi numrseg))
         cosa (cos deltal)
         sina (sin deltal)
         xorg (car cen)
         yorg (cadr cen)
         zorg (caddr cen)
         x (+ radl radt)
         px1 (+ x xorg)
         py1 yorg
         pz1 zorg
         px2 (+ xorg (* x cosa))
         py2 (+ yorg (* x sina))
         pz2 (+ pz1 hinc)
   )
   (command "3DFACE" (list px1 py1 pz1) (list px2 py2 pz2))

   (setq doneface T j 1 flop 0)
   (while (<= j numtseg)
       (setq beta (* j deltat)
             x (+ radl (* radt (cos beta)))
             px3 (+ xorg (* x cosa))
             py3 (+ yorg (* x sina))
             pz3 (+ zorg (* radt (sin beta)) hinc)
             px4 (+ xorg x)
             py4 yorg
             pz4 (- pz3 hinc)
       )
       (if (= 1 flop)
           (command (list px4 py4 pz4) (list px3 py3 pz3))
           (command (list px3 py3 pz3) (list px4 py4 pz4))
       )
       (setq flop (- 1 flop) j (+ j 1))
   )
   (command "")

   (setq ss1 (selstuff e))
   (setq ss nil)
   (setq arrfac 1)
   (while (> numrseg arrfac)
    (setq e (entlast))
    (command "array" ss1 "" "c" cen (* arrfac (/ 360.0 numrseg)) "2" "y")
    (setq ss2 (selstuff e))
    (command "move" ss2 "" (list 0.0 0.0 0.0) (list 0.0 0.0 (* hinc arrfac)))
    (setq ss2 nil)
    (setq arrfac (+ arrfac 1))
   )
   (setq ss1 nil)
   (setq ss3 (selstuff cmark))
   (setq ctr 1)
   (while (> #rot ctr)
    (command "copy" ss3 "" (list 0 0 0) (list 0 0 (* ctr hf)))
    (setq ctr (+ ctr 1))
   )
   (setq ss3 nil)
   (setq ss nil)
)


; Main program
(defun C:SPRING (/ doneface olderr ss *error*)
   (setq olderr *error* doneface nil)
   (defun *error* (s)                  ; If an error (such as CTRL-C) occurs
                                       ; while this command is active...
       (if (/= s "Function cancelled")
           (princ (strcat "\nError: " s))
       )
       (if doneface
           (progn                      ; If we're drawing 3DFACEs...
               (command)               ;   simulate CTRL-C (cancel 3DFACE cmd)
               (command "UNDO" "END")  ;   terminate Undo group
               (princ " ...undoing ")  ;   erase partially-drawn stuff
               (command "U")
           )
       )
       (moder)                         ; Restore modified modes
       (setq ss nil)                   ; Free selection-set if any
       (setq *error* olderr)           ; Restore old *error* handler
       (princ)
   )
   (modes '("CMDECHO" "BLIPMODE" "HIGHLIGHT" "ELEVATION" "THICKNESS"))
   (setvar "CMDECHO" 0)
   (setvar "HIGHLIGHT" 0)
   (spring)
   (moder)
   (princ)
)
