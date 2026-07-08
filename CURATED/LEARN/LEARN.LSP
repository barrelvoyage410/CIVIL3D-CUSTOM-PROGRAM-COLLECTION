;***** LEARN *****
; This routine creates a macro for AutoCAD commands entered from both the
; keyboard and the digitizer. All points entered from the keyboard are
; maintained as constant points. All points selected from the digitizer are
; considered variable points. Variable text can be entered if the word
; "PAUSE" is entered from the keyboard. AutoLISP expressions can also be
; entered.
;
(defun c:learn ()

  ; prompt user for the name of the macro and provide instructions
  (setq macroname (strcat "c:" (getstring "LEARN: Enter macro name: ")))
  (terpri) (prompt
  "LEARN: Enter AutoCAD commands from keyboard or pick point from digitizer")
  (prompt "\nLEARN: ")

  ; Initialize variables
  (setq getinput T string "" point nil cmdlist nil cmd nil)

  ; Get and process data
  (while (= getinput t)                          ; Do while input is requested
    (setq inkey (grread))                        ; prompt and read data

    (cond
      ((= (car inkey) 2) (kbprocess))                       ; keyboard input
      ((and (= (car inkey) 3) (= string "")) (dgprocess))   ; digitizer input
      (t (prompt "\nINPUT ERROR")                           ; unrecognized code
         (prompt "\nLEARN: ") (prompt string))              ; reprompt
    ); end cond

  ); end while (= getinput t)

  (bldmacro) ; build macro

); end macro


;***** kbprocess *****
; this routine processes the keyboard data received via a grread
;
(defun kbprocess ()
  (setq char (cadr inkey)) ; get keyboard character

  (cond

    ((= char 8)  ; char is backspace

      (cond ; delete previous character in data string and on display
        ((/= (strlen string) 0)
          (setq string (substr string 1 (1- (strlen string))))
          (prompt (chr 8))
        ); end (/= (strlen string) 0)
      ); end cond

    ); end (= char 8)

    ((= char 27) (setq getinput nil))

    ((not (or (= char 13) (= char 32))) ; char is not <cr> and not <space>
      (setq string (strcat string (chr char))) ; add character to string
      (prompt (chr char))                      ; display character on screen
    ); end (not (or (= char 13) (= char 32)))

    (t ; character is an AutoCAD command terminator (<cr> or <space>)
       (setq cmdlist (append cmdlist (list string))) ; save command parameters
       (if (= (strcase string) "PAUSE")
           (setq string (getstring "\nEnter actual response to ACAD prompt: ")))
      (prompt " ACAD command: ")   ; inform operator of ACAD prompt
      (command string)             ; execute ACAD command
      (setq string "")             ; reinitialize string
      (prompt " LEARN: ")          ; inform operator to input data
    ); end t

  ); end cond

); end kbprocess


;***** dgprocess *****
; this routine processes the digitizer data via a grread
;
(defun dgprocess ()
  (setq point (cadr inkey))                    ; get data point
  (prompt " ACAD command: ")                   ; inform operator of ACAD prompt
  (command point)                              ; execute AutoCAD command
  (setq point "pt")                            ; set variable point
  (setq cmdlist (append cmdlist (list point))) ; save point in command list
  (prompt " LEARN: ")                          ; inform operator to input data

); end dgprocess


;***** bldmacro *****
; this routine builds a macro based on the keystrokes saved
;
(defun bldmacro ()

  (prompt (strcat "\nBuilding Macro: " macroname))  ; prompt for name of macro
  (setq macro (list 'defun (read macroname) '() ))  ; initialize macro
  (setq subcmd (list 'command))                     ; temporary command stack

  (while (/= cmdlist nil)                         ; do while commands exist
    (setq nextcmd (car cmdlist))                  ; get next command from stack

    (cond

      ((= nextcmd "pt")          ; if next command is a variable point then

        (cond
          ((> (length subcmd) 1) ; if commands exist in temp command stack then
            ; append commands to macro and reinitialize the temp command stack
            (setq macro (append macro (list subcmd)))
            (setq subcmd (list 'command))
          ); end (> (length subcmd) 1)
        ); end cond

        ; append variable point commands to macro
        (setq macro (append macro (list (list 'setq 'pt '(getpoint)))))
        (setq macro (append macro (list (list 'command 'pt))))

      ); end (= nextcmd "pt")

      ((or (= nextcmd "pause") (= nextcmd "PAUSE"));if next cmd is var prompt

        (cond
          ((> (length subcmd) 1) ; if commands exist in temp command stack then
            ; append commands to macro and reinitialize the temp command stack
            (setq macro (append macro (list subcmd)))
            (setq subcmd (list 'command))
          ); end (> (length subcmd) 1)
        ); end cond

        ; append variable strings to macro
        (setq macro (append macro (list (list 'setq 'prmpt '(getstring)))))
        (setq macro (append macro (list (list 'command 'prmpt))))

      ); end (or (= nextcmd "pause") (= nextcmd "PAUSE"))

      (t (setq subcmd (append subcmd (list nextcmd))))

    ); end cond

    (setq cmdlist (cdr cmdlist)) ; remove used command from command stack

  ); end while

  ; append macro with final commands
  (if (> (length subcmd) 1) (setq macro (append macro (list subcmd))))
  (setq macro (append macro (list ''DONE)))
  (eval macro)

  ; indicate end of macro building process
  (prompt "\n")
  'DONE

); end bldmacro
