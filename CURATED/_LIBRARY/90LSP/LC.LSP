
; LaunchCAD Autolisp program version 4.01

; Note: add the line:	 (load"lc")     to the end of your ACAD.LSP file.
; If you do not have an ACAD.LSP file then you can rename this file to ACAD.LSP
; or add the contents of this file to your present ACAD.LSP file.

(princ "\nLoading LaunchCAD...")
(if(< (substr(getvar "ACADVER")1 2) "11")
  (setvar "MENUECHO" 1))                        ; this makes INSERT work right

(defun lc_shell (mode dt / ce me dir f)         ; call LaunchCAD in shell mode
  (setq ce (getvar "CMDECHO")
	me (getvar "MENUECHO"))
  (setvar "CMDECHO" 0)                          ; don't echo commands
  (setvar "MENUECHO" 1)                         ; do echo menu commands
  (command ".SCRIPT" "BLANK")
; (command "sh" "mode co80")                    ; decomment for dual screen
  (if (= dt "d") (progn
    (setq dir (getvar "DWGPREFIX"))
    (command "launchcad" (strcat mode " " dir)) ; invoke LaunchCAD
  ) ;else
    (command "launchcad" mode)
  )
; (command "sh" "mode mono")                    ; decomment for dual screen
; (graphscr)					;    "           "
  (if(null(setq username (getenv "USERNAME")))
    (setq username "LC"))
  (command ".SCRIPT" username)                  ; run the script
  (if (setq f (open(findfile (strcat username ".SCR")) "w"))
    (setq f (close f)))                         ; just in case...
  (setvar "CMDECHO" ce)                         ; restore CMDECHO
  (setvar "MENUECHO" me)                        ; restore MENUECHO
  (princ)
)

; Note: You may edit this lisp file to rename the following functions
; to any name that you wish so long as it does not conflict with an
; AutoCAD internal command.

; Execute LaunchCAD from within AutoCAD (in shell mode)

(defun c:lc ()	 (lc_shell "/s" "d"))       ; shell mode menu
(defun c:lce ()  (lc_shell "/e" "d"))       ; end and select another
(defun c:lcw ()  (lc_shell "/w" "d"))       ; wblock *;quit and select another
(defun c:lcq ()  (lc_shell "/q" "d"))       ; quit and select another
(defun c:ins ()  (lc_shell "/i" "s"))       ; Insert
(defun c:*ins () (lc_shell "/*" "s"))       ; Exploding Insert
(defun c:lisp () (lc_shell "/l" "s"))       ; Lisp
(defun c:dxf ()  (lc_shell "/d" "s"))       ; DXFIN
(defun c:dxb ()  (lc_shell "/b" "s"))       ; DXBIN
(defun c:vs ()	 (lc_shell "/v" "s"))       ; VSlide
(defun c:mu ()	 (lc_shell "/m" "s"))       ; Menu
(defun c:sty ()  (lc_shell "/t" "s"))       ; Text Style
(defun c:xr ()	 (lc_shell "/x" "s"))       ; XREF attach
(defun c:ads ()  (lc_shell "/a" "s"))       ; ADS (XLOAD"XX")
(defun fi ()	 (lc_shell "/f" "s"))       ; invoke LaunchCAD file Mode

(defun lc_var(var)
  (cadr(assoc var (read(getenv "LAUNCHCAD"))))
)

(defun ck_slide ( / sldfile)
  (if(and(not checked) (= T (lc_var "MSLIDE"))) (progn
    (setq checked T
	  sldfile (strcat (getvar "DWGNAME") ".sld"))
    (if(not(findfile sldfile)) (progn
        (princ "\nCreating Slide File...")
        (command "MSLIDE" "")
	(princ "done \n")
    ))
  ))
)

(defun date(val / s y m d)
  (setq s (rtos val 2 0)
	y (substr s 3 2)
	m (substr s 5 2)
	d (substr s 7 2)
  )
  (strcat m "/" d "/" y)
)

(defun time(val / str h m s)
  (setq str (rtos val 2 6)
	h (substr str 10 2)
	m (substr str 12 2)
	s (substr str 14 2)
  )
  (strcat h ":" m ":" s)
)

(defun log ( which / fp logfile q c)
  (if(= T (lc_var "LOG")) (progn
    (setq logfile(strcat(getvar"DWGNAME")(lc_var"LOG_EXT"))
	  fp (open logfile "a")
	  q "\"" c ", "
    )
    (princ "\nUpdating Log File...")
    (princ(strcat q which q
		  c q (getvar"DWGNAME") q
		  c q (if(/= username "LC") username "") q
		  c q (date(getvar"CDATE")) q
		  c q (time(getvar"CDATE")) q
		  c (rtos(* 24.0 (getvar"TDINDWG")) 2 3)
;		   c (rtos(getvar"DATE") 2 9)           ; decomment for more
;		   c (rtos(getvar"TDUPDATE") 2 9)       ; data in log file
		  "\n")
    fp)
  (close fp)
  (princ "done ")
  ))
)

; Redefine the QUIT command to run quit.scr in order to
; bypass the AutoCAD opening menu.

(defun c:quit ()
  (setvar "CMDECHO" 0)
  (initget "No Yes")
  (if(= "Yes" (getkword
    "\nDo you really want to discard\nall changes to drawing[y/N]: "))
  (progn
    (log "QUIT")
    (command ".SCRIPT" "QUIT")
  ))
  (princ)
)

; Redefine the END command to run end.scr in order to
; bypass the AutoCAD opening menu.

(defun c:end ()
  (setvar "CMDECHO" 0)
  (ck_slide)
  (log "END")
  (command ".SCRIPT" "END")              ; run end.scr which bypasses menu
  (princ)
)

(defun c:save ()
  (ck_slide)
  (log "SAVE")
  (command ".SAVE")
  (princ)
)

(defun c:wend ()
  (setvar "CMDECHO" 0)
  (log "WEND")
  (command ".SCRIPT" "WEND")             ; run wend.scr which saves drawing
  (princ)				 ; using wblock * and bypasses menu
)

; Undefine the AutoCAD drawing editor END and QUIT commands so that
; the end and quit functions defined above will work in place of the
; internal commands (.end and .quit will still work like normal)
; NOTE: (lc:su) MUST be in STARTUP.LSP or the S::STARTUP function
; in ACAD.LSP

(defun lc:su()
  (command "UNDEFINE" "QUIT")
  (command "UNDEFINE" "SAVE")
  (command "UNDEFINE" "END")
  (princ "\nEND, SAVE and QUIT commands redefined...")
  (if(null(setq username (getenv "USERNAME")))
    (setq username "LC"))
  (if (setq f (open(findfile (strcat username ".SCR")) "w"))
    (setq f (close f))) 			; delete script file
  (log "OPEN")
  (princ "\nLaunchCAD initialized.")
  (princ)
)

(princ "\nLaunchCAD version 4.0 ")
(princ)

