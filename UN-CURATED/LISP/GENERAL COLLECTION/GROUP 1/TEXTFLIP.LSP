;;Text Flip - Flips Selection Set of Text & Mtext in Place
;;Uses (bns_trot) from Express Tools (acettxt.lsp)
(defun c:tf (/ ssrotate #sslen ename textang ssrotate2)
  (if (not bns_trot) (load "acettxt.lsp"))
  (setq	ssrotate (ssget '((0 . "*TEXT"))) ;select text objects only
	#sslen	 (sslength ssrotate)
  )
  (ucscheck);set ucs to world and save current
  (while (> #sslen 0)
    (setq #sslen (- #sslen 1))		;set index #
    (setq ename (ssname ssrotate #sslen)) ;get name
    (setq textang (cdr (assoc 50 (entget ename)))) ;get rotation
    (bns_trot (setq ssrotate2 (ssadd ename)) (+ textang pi)) ;do rotate
  )
  (ucsrestore);restore ucs if any
)


					;--------------UCS TRAPPING------------------
(DEFUN UCSCHECK ()
  (SETQ UCSSTR NIL)
  (IF (= (GETVAR "WORLDUCS") 0)		;SAVE UCS IF NOT WORLD
    (PROGN
      (SETQ UCSSTR (STRCAT "U" (RTOS (c:rand) 2 0)))
					;GENERATE RANDOM NAME
      (COMMAND "UCS" "S" UCSSTR "UCS" "W") ;SAVE AND SET TO WCS
    )
  )
)
(DEFUN UCSRESTORE ()
  (IF UCSSTR				;RESET TO UCS IF APPLICABLE
    (COMMAND "UCS" "R" UCSSTR "UCS" "D" UCSSTR)
					;RESTORE AND DELETE TEMP NAME
					;ELSE
    (command "ucs" "w")			;RESET TO WORLD
  )
  (SETQ UCSSTR NIL)
)
					;--------------UCS TRAPPING------------------