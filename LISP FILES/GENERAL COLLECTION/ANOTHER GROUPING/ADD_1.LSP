;;; revised 03-20-2007 to check for serial number       ;;;ME


(defun C:ADD_1 (/ N TXT CGL_ANS)
  (if (cgl_check_serial)
    (progn

  (SETQ N (GETINT "\nEnter starting number: "))
  (SETQ TXT N)
  (initget "I D")
  (setq CGL_ANS (getkword "\n Increase or Decrease number? < I > "))
  (cond ((or (= CGL_ANS "D") (= CGL_ANS "d"))
         (progn
           (princ "\n You selected DECREASING text")
           (WHILE
             (SETQ
               PT1 (GETPOINT
                     (strcat "\nPick center of text for number " (rtos TXT 2 0))
                   ) ;_ end of GETPOINT
             ) ;_ end of SETQ
              (COMMAND "TEXT" "J" "C" PT1 "0" TXT)
              (SETQ N (- N 1))
              (SETQ TXT N)
           ) ;END WHILE
         ) ;end progn
        ); end decreasing text
        ((or (= CGL_ANS "I") (= CGL_ANS "i") (= CGL_ANS nil))
         (progn
           (Princ "\n You selected INCREASING text")
           (WHILE
             (SETQ
               PT1 (GETPOINT
                     (strcat "\nPick center of text for number " (rtos TXT 2 0))
                   ) ;_ end of GETPOINT
             ) ;_ end of SETQ
              (COMMAND "TEXT" "J" "C" PT1 "0" TXT)
              (SETQ N (+ N 1))
              (SETQ TXT N)
           ) ;END WHILE
         ) ;end progn
        ); end increasing text
  ) ;_ end of cond
  (princ)
  (princ)
    );end checker progn
    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
    );end if
    (princ)

) ; END ADD_ONE
