(defun C:ADD_ONE ()
        (SETQ N (GETINT "\nEnter starting number: "))
        (SETQ TXT N)
                (WHILE (SETQ PT1 (GETPOINT "\nPick center of text: "))
                        (COMMAND "TEXT" "J" "C" PT1 "0" TXT) 
                        (SETQ N (+ N 1))
                        (SETQ TXT N)
                       ) ;END WHILE
) ; END ADD_ONE
