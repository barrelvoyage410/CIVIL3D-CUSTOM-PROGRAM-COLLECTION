;;;                                                                                     ;;;
;;;                 Created 3/2/2004 4:30 PM by Mark Evinger                            ;;;
;;;                                                                                     ;;;
;;; MoveGradlbl.lsp processes the GRAD_X block and moves it to a selected text          ;;;
;;; elevation. Elevations can be just numbers, "TF= xxx.xx" type strings or enclosed    ;;;
;;; in ().                                                                              ;;; v1.0.0
;;;                                                                                     ;;;
;;;                                                                                     ;;;
;;;                                                                                     ;;;
;;;Mark Evinger  email: Mark.Evinger@cgl-ltd.com                                        ;;;
;;;MIS Manager                                                                          ;;;
;;;Cowhey Gudmundson Leder, Ltd.                                                        ;;;
;;;300 Park Boulevard                                                                   ;;;
;;;Suite 350                                                                            ;;;
;;;Itasca, IL 60143                                                                     ;;;
;;;Phone: 630-250-9595 x242  Fax: 630-250-9644                                          ;;;
;;;Website: http://www.cgl-ltd.com                                                      ;;;
;;;                                                                                     ;;;
;;; revised 03-20-2007 to check for serial number                                       ;;; v1.0.2

;; GRDRAW functions from David Bethel
;;;GRDRAW X -> pt color flag
(defun CGL_grx (p c f / CGL_size)
  (setq CGL_size (/ (getvar "VIEWSIZE") 66))
  (grdraw (polar p (* pi 1.25) CGL_size)
          (polar p (* pi 0.25) CGL_size)
          c
          f
  )
  (grdraw (polar p (* pi 1.75) CGL_size)
          (polar p (* pi 0.75) CGL_size)
          c
          f
  )
)

;;;GRDRAW BOX -> pt color
(defun CGL_grb (p c / size)
  (setq size (/ (getvar "VIEWSIZE") (getvar "PICKBOX") 16))
  (grdraw (polar p (* pi 0.25) size)
          (polar p (* pi 0.75) size)
          c
  )
  (grdraw (polar p (* pi 0.75) size)
          (polar p (* pi 1.25) size)
          c
  )
  (grdraw (polar p (* pi 1.25) size)
          (polar p (* pi 1.75) size)
          c
  )
  (grdraw (polar p (* pi 1.75) size)
          (polar p (* pi 0.25) size)
          c
  )
)

 ; ************************************************************************
 ;                             SPLITSTR.LSP
 ;
 ; This routine accepts a string and a delimiter character
 ; It returns a list containing two strings
 ; The first returned string contains all of the characters from the beginning
 ; of the string to the delimiter
 ; The second string in the returned list contains the remainder of the string
 ; starting after the delimiter
 ;
 ; External routines used:
 ; Internal routines used:
 ;
 ; Permission is hereby granted to use this routine on the condition that
 ; this header remains with the routine. No warrantity is implied or
 ; expressed. The routine is not to be modified without permission of the
 ; author.
 ;
 ; Original version    3/6/94      John Gibb/Paladin
 ;                                 Compuserve #71530,3004
 ; ************************************************************************

(defun SPLIT_STR
                 (LINE CHAR / TEMPVAR TEMP-LIST CNT LOOP LINE-LEN TEMP-LIST)
  (setq TEMPVAR  ""
        CNT      1
        LOOP     t
        LINE-LEN (strlen LINE)
  )
  (if (= LINE-LEN 0)
    (setq TEMPVAR "")
    (while LOOP
      (if (> CNT LINE-LEN)
        (setq LOOP NIL)
        (progn
          (if (and (= (substr LINE CNT 1) CHAR) (/= CNT LINE-LEN))
            (setq LINE (substr LINE (1+ CNT) (- (strlen LINE) CNT))
                  LOOP NIL
            )
            (progn
              (setq TEMPVAR (strcat TEMPVAR (substr LINE CNT 1)))
              (if (= CNT LINE-LEN)
                (setq LOOP NIL)
              ) ;end if
            ) ;end progn
          ) ;end if
        ) ;end progn
      ) ;end if cnt > length
      (setq CNT (1+ CNT))
    ) ;end while
  ) ;end if line length = 0
  (setq TEMP-LIST (cons LINE TEMP-LIST)
        TEMP-LIST (cons TEMPVAR TEMP-LIST)
  )
) ;end split_str
 ;***************************************************************


(defun moveGradLBL (/) ;ss_gradx ss_text E E1 n_elev pntblock )
  (prompt "\nSelect Grad_X block to modify: ") ;get the block to move
  (while
    (setq ss_gradx (ssget ":S" '((0 . "INSERT") (2 . "grad_x"))))
     (if ss_gradx
       (progn
         (setq gr_start (cdr (assoc 10 (entget (ssname ss_gradx 0)))))
         (cgl_grx gr_start 1 0) ;draw a vector X to show what you picked
 ;(setq vector_start (list (car gr_start) (cadr gr_start)))
         ;;(setq gr_start_01 (polar gr_start (+ 0    (/ pi 2)) 2))
         ;;(setq gr_start_02 (polar gr_start (+ 90   (/ pi 2)) 2))
         ;;(setq gr_start_03 (polar gr_start (+ 180  (/ pi 2)) 2))
         ;;(setq gr_start_04 (polar gr_start (+ 270  (/ pi 2)) 2))
         ;;(grdraw gr_start gr_start_01 1)
         ;;(grdraw gr_start gr_start_02 1)
         ;;(grdraw gr_start gr_start_03 1)
         ;;(grdraw gr_start gr_start_04 1)
;;;         (grvecs '(1 gr_start gr_start_01
;;;                   1 gr_start gr_start_02
;;;                   1 gr_start gr_start_03
;;;                   1 gr_start gr_start_04
;;;                   )
;;;                 );end grvecs
         (prompt
           "\nSelect text to use as Z-Elevation for GRAD_X block: "
         )
 ;(setq ss1 (ssget '((0 . "TEXT"))))
         (setq ss_text (ssget ":S" '((0 . "*TEXT"))))
         (SETQ E (SSNAME ss_text 0))
         (SETQ E1 (ENTGET (SSNAME ss_text 0)))
         (setq tx_start (cdr (assoc 10 (entget (ssname ss_text 0)))))
         (cgl_grb tx_start 1) ;draw a vector box to show what text was picked
 ;(setq n_elev (atof (cdr (assoc 1 e1)))); set n_elev to value of text
 ;=====================================
         (setq CGL_OLD_TEXT (cdr (assoc 1 E1)))
         (cond
           ((= t (wcmatch CGL_OLD_TEXT "*=*")) ; if there is an "=" anywhere in text
            (setq CGL_SPLIT_STR (SPLIT_STR CGL_OLD_TEXT "=")) ; split the string in 2
            (if (= (substr (cadr CGL_SPLIT_STR) 1 1) " ") ; if the 1st char of 2nd string is " "
              (setq CGL_TEXT1 (strcat (car CGL_SPLIT_STR) "= ")) ;true, has form of "TF= "
              (setq CGL_TEXT1 (strcat (car CGL_SPLIT_STR) "=")) ;false, has form of "TF="
            ) ;end if
            (if
              (wcmatch (cadr cgl_split_str) "*[)]*") ; back half has ) in it
               (progn
                 (setq CGL_BACK_PAREN T)
                 (setq CGL_Back_Half (vl-string-right-trim ")" (cadr cgl_split_str))) ;remove the back )
                 (setq CGL_OLD_NUM CGL_Back_Half)
                 (setq OLD_NUM (atof CGL_OLD_NUM))
 ;(setq NEW_NUM (+ CGL_NEW_TF OLD_NUM))
               ) ;end progn true condition
               (progn
                 (setq CGL_OLD_NUM (cadr CGL_SPLIT_STR))
                 (setq OLD_NUM (atof CGL_OLD_NUM))
 ;(setq NEW_NUM (+ CGL_NEW_TF OLD_NUM))
               ) ;end progn back half does not have )
            ) ;end if
           ) ;end string has "=" in it

           ((= T (wcmatch CGL_OLD_TEXT "*(*,*)*")) ;if a () in the string
            (if (= (substr CGL_OLD_TEXT 1 1) "(") ; if 1st char of 1st string is (
              (progn
                (setq CGL_FRONT_HALF (vl-string-left-trim "(" CGL_OLD_TEXT)) ;remove ( from start of string
                (setq CGL_FRONT_HALF (vl-string-right-trim ")" CGL_FRONT_HALF)) ;remove ) from end of string
              ) ;end progn
            ) ;end if
            (if (wcmatch CGL_FRONT_HALF "*[A-Z]*,*[a-z]*") ;if there is text in the string between ()
              (setq NEW_TEXT CGL_FRONT_HALF) ;do nothing
              (progn
                (setq OLD_NUM (atof CGL_FRONT_HALF))
              ) ;end progn
            ) ;end if
           ) ; text has () in it

           ((/= t (wcmatch CGL_OLD_TEXT "*=*")) ; no text, no "=", no "%", just numbers
            (setq OLD_NUM (atof CGL_OLD_TEXT))
           ) ;end string has no = in it

         ) ;end conditional
 ;=====================================
         (setq n_elev CGL_OLD_NUM)
         (setq pntblock (entget (ssname ss_gradx 0))) ; set pntblock to list of reals (xyz)
         ;;substitute new elev for old
         (setq pntblock (subst (list 10
                                     (cadr (assoc 10 pntblock))
                                     (caddr (assoc 10 pntblock))
                                     OLD_NUM
                               )
                               (assoc 10 pntblock)
                               pntblock
                        )
         )
         (entmod pntblock) ;update point block
         (prompt "\nSelect Grad_X block to modify: ") ;get the block to move
       ) ;end progn
       (prompt "\nNo Grad_X block selected") ;(else do other stuff )
     ) ;end if

 ;get the text to parse
 ;(setq ss_gradx nil)
 ;(setq ss_text nil)
     (princ)
  ) ;end while
) ;end program
(defun c:mgl ()
    (if (cgl_check_serial)
    (progn
  (vmon)
  (MoveGradLBL)
    );end checker progn
    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
    );end if
    (princ)  
  )
(defun c:MoveGradLBL ()
  (if (cgl_check_serial)
    (progn
  (vmon)
  (MoveGradLBL)
    );end checker progn
    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
    );end if
    (princ)  
  )
(princ)
(princ)