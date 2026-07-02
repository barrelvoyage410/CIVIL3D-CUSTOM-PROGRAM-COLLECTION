;;;;;;Revised by Dave Perry to work in Release 14 12/2/98
;; Visit http://davescogo.virtualave.net/
;;;;;;Note: Revisions that I made are noted.
;***  TX2MTX.lsp
;	By Don J. Buschert
;	Southern Alberta Institute of Technology
;	1301 - 16th Ave. N.W.
;	Calgary, Alberta Canada
;	T2M 0L4
;	don.buschert@sait.ab.ca
;
;	Version 1.0	05/09/96
;
;	TX2MTX converts multiple lines of TEXT objects to an MTEXT object using
;	the current text height while maintaining overall string width.  Text can
;	be selected by picking, windowing or crossing; TX2MTX will sort the lines
;	by their Y value (note:  The selected text must have a rotation of 0 in
;	order for this feature to work).  The selected text's layer and color will
;	be passed to the new mtext object.  If selected lines of text have different
;	layers, colors, and styles; the line which has the greatest Y value will 
;	determine the layer and color for the mtext.
;
;
;***	Function TX2MTX_PROCESS
;This functions processes the selected text by sorting the text object selection 
;set in order with the highest Y values at the beginning.  Then it erase the
;original selected text and draws the mtext object.
(defun TX2MTX_PROCESS ( / )
  (princ "\nSorting text...")
  ;create dotted pair list of text Y insertions points and string
  (setq indx 0);set the index
  (setq text_widt 0.0);set the text width to zero
  (repeat (sslength sst2);repeat for each text object
    (setq enam (ssname sst2 indx));get object name
    (setq yval (caddr (assoc '10 (entget enam))));get y value
    (if (> (setq widt (car (cadr (textbox (entget enam)))));is text width
           text_widt                                       ;greater than current?
        )
      (setq text_widt widt);if so, replace original text width
    )
    
    (setq dtpr (cons yval indx));create dotted pair
    (if dtpr_list ;if dotted pair list exists,
      (setq dtpr_list (cons dtpr dtpr_list));then append
      (setq dtpr_list (list dtpr));else create it
    )
    ;increase index
    (setq indx (1+ indx));set the index    
  )

  ;create list of y values
  (foreach n dtpr_list ;for each item in the dotted pair list...
    (setq yval (car n));get the y value
    (if yval_list ;if the y value list exists,
      (setq yval_list (cons yval yval_list));add
      (setq yval_list (list yval));else create
    ) 
  )

  ;sort the y value list
  (while (> (length yval_list) 1);as long as more than 1 element...
    (setq yval (apply 'MIN yval_list));get the min Y value
    (if yval_slis ;if the Y value sorted list exists,
      (setq yval_slis (cons yval yval_slis));add
      (setq yval_slis (list yval));else create
    )
    ;remove the y value from the y value list
    (setq yval_list (remove_elem yval yval_list))

  )

  ;add the last element to the sorted y value list
  (if yval_slis ;if the Y value sorted list exists,
    (setq yval_slis (cons (car yval_list) yval_slis));add
    (setq yval_slis (list (car yval_list)));else create
  )

  ;create a text index list of text based on the sorted Y value list
  (foreach n yval_slis ;for each element in the sorted Y value list...
    (setq text (cdr (assoc n dtpr_list)));get the text index
    ;if text index list exists
    (if text_list
      (setq text_list (cons text text_list));add to list
      (setq text_list (list text));else create it.
    )
  )

  ;reverse the order of the text index list
  (setq text_list (reverse text_list))
  
  ;create a text string list for each element in the text index list
  (foreach n text_list
    (setq text_valu (cdr (assoc 1 (entget (ssname sst2 n)))));get string value
    (if text_valu_list 
      (setq text_valu_list (cons text_valu text_valu_list));add to list
      (setq text_valu_list (list text_valu));else, create
    )
  )

  ;reverse the order of the text value list
  (setq text_valu_list (reverse text_valu_list))
  ;set the text string value of the mtext based on text_valu_list
  (setq text_strg "")
  (foreach n text_valu_list
    (setq text_strg (strcat text_strg n " "))
  )
  ;grab insertion point, justification, width, layer and color of first text object
  (setq text_inse (cdr (assoc 10 (entget (ssname sst2 (car text_list))))))
  (setq text_ins2 (cdr (assoc 11 (entget (ssname sst2 (car text_list))))))
  (setq text_horj (cdr (assoc 72 (entget (ssname sst2 (car text_list))))))
  (setq text_verj (cdr (assoc 73 (entget (ssname sst2 (car text_list))))))
  (setq text_layr (cdr (assoc  8 (entget (ssname sst2 (car text_list))))))
  (setq text_colr (cdr (assoc 62 (entget (ssname sst2 (car text_list))))))
  (setq text_heig (cdr (assoc 40 (entget (ssname sst2 (car text_list))))))
  (setq text_styl (cdr (assoc  7 (entget (ssname sst2 (car text_list))))))
  
  ;determine mtext attach based on first text line...
  (cond
    ((eq text_horj 0);left justified
      (setq text_atch "TL" )
      (setq text_inse (list (car text_inse)(+ (cadr text_inse) text_heig)(caddr text_inse)))
    )
    ((and (eq text_horj 1)(eq text_verj 0));center justified
      (setq text_atch "TC" )
      (setq text_inse (list (car text_ins2)(+ (cadr text_ins2) text_heig)(caddr text_ins2)))
    )
    ((and (eq text_horj 1)(eq text_verj 1));bottom center justified
      (setq text_atch "TC" )
      (setq text_inse
        (list (car text_ins2)
              (+ (cadr text_ins2) (+ text_heig (- (cadr text_inse)(cadr text_ins2))))
              (caddr text_ins2)
        )
      )
    )
    ((and (eq text_horj 1)(eq text_verj 2));middle center justified
      (setq text_atch "TC" )
      (setq text_inse (list (car text_ins2)(+ (cadr text_ins2) (/ text_heig 2))(caddr text_ins2)))
    )
    ((and (eq text_horj 1)(eq text_verj 3));top center justified
      (setq text_atch "TC" )
      (setq text_inse (list (car text_ins2)(cadr text_ins2)(caddr text_ins2)))
    )
    ((and (eq text_horj 2)(eq text_verj 0));right justified
      (setq text_atch "TR" )
      (setq text_inse (list (car text_ins2)(+ (cadr text_ins2) text_heig)(caddr text_ins2)))
    )
    ((and (eq text_horj 2)(eq text_verj 1));bottom right justified
      (setq text_atch "TR" )
      (setq text_inse
        (list (car text_ins2)
              (+ (cadr text_ins2) (+ text_heig (- (cadr text_inse)(cadr text_ins2))))
              (caddr text_ins2)
        )
      )
    )
    ((and (eq text_horj 2)(eq text_verj 2));middle right justified
      (setq text_atch "TR" )
      (setq text_inse (list (car text_ins2)(+ (cadr text_ins2) (/ text_heig 2))(caddr text_ins2)))
    )
    ((and (eq text_horj 2)(eq text_verj 3));top right justified
      (setq text_atch "TR" )
      (setq text_inse (list (car text_ins2)(cadr text_ins2)(caddr text_ins2)))
    )
    ((and (eq text_horj 4)(eq text_verj 0));middle justified
      (setq text_atch "MC")
      (setq text_inse (list (car text_ins2)(+ (cadr text_ins2) text_heig)(caddr text_ins2)))
    )
    (T ;if fit or aligned
      (setq text_atch "TL" )
      (setq text_inse (list (car text_inse)(+ (cadr text_inse) text_heig)(caddr text_inse)))      
    )
  )  
  ;set the layer and color
  (setvar "CLAYER" text_layr)
  (if text_colr;if color in entity list
    (setvar "CECOLOR" (itoa text_colr))
  )
  ;erase selected text
  (command ".erase" sst2 "")
;;;;;;;;;;
;;;;;;;;;;
  ;insert mtext
;  (command ".-mtext"
;     text_inse "a" text_atch "s" text_styl "h" text_heig "w" text_widt text_strg "")
;;;;;;;;;;
;;;;;;;;;; the following was added by Dave Perry 12/2/98.
  (setq dp2 (strlen text_strg))
  (while (> dp2 250)
                 (setq str1 (substr text_strg 1 250))
                 (setq text_strg (substr text_strg 251))
                 (setq dp2 (strlen text_strg))
                 (setq dp1 (append dp1 (list (cons 3 str1))))
                 );while
                (setq dp1 (append dp1 (list (cons 1 text_strg))))
 (entmake (append (list '(0 . "MTEXT")
                '(100 . "AcDbEntity")
                '(67 . 0)
                '(8 . "0")
                '(100 . "AcDbMText")
                 (list 10 (car text_inse)(cadr text_inse) 0)
                 (cons 40 text_heig)
                 (cons 41 text_widt)
                '(71 . 1)
                '(72 . 1)
                (cons 7 text_styl)
                '(210 0.0 0.0 1.0)
                '(11 1.0 0.0 0.0)
                 (cons 42 text_widt)
                '(43 . 2)
                '(50 . 0)
           );list
           dp1 );append
 );entmake
;;end of addition by Dave Perry.
;;;;;;;;
;;;;;;;;
(princ)
)
;***	Function TX2MTX
;This program controls and executes the text to mtext functions
(defun C:TX2MTX ( / coun ;counter
                  dtpr          ;dotted pair
                  dtpr_list             ;dotted pair list
                  enam          ;entity name
                  indx          ;index
                  sst1          ;object selection set
                  sst2          ;text only selection set
                  sv_cecolor            ;"CECOLOR" system variable
                  sv_clayer             ;"CLAYER" system variable
                  text          ;text index
                  text_atch             ;mtext attachment type
                  text_colr             ;"text object color
                  text_heig             ;text object height
                  text_horj             ;text horizontal justification code
                  text_ins2             ;text alignment point
                  text_inse             ;text insertion point
                  text_layr             ;text object layer
                  text_list             ;text index list
                  text_strg             ;text string list
                  text_styl             ;text object style
                  text_valu             ;text object value
                  text_valu_list        ;list of text values
                  text_verj             ;text vertical justification code
                  text_widt             ;width of textbox
                  widt          ;text width test
                  yval          ;Y value
                  yval_list             ;list of Y values
                  yval_slis             ;Y value sorted list
                  str1                  ;string sorting variable
                  dp1                   ;list for str1 added by Dave Perry
                  dp2                   ;string length added by Dave Perry
                 
              )
  
  (setq sv_clayer (getvar "clayer"))
  (setq sv_cecolor (getvar "cecolor"))

;Define error routine for this command
  (defun tx2mtx_error (s)
    (if (/= s "Function cancelled.");if ^c occurs...
      (princ (strcat "\nError: " s))
    )
    (if old_error (setq *error* old_error))
    (setvar "clayer" sv_clayer)
    (setvar "cecolor" sv_cecolor) 
    (princ)
  )
  (setq old_error *error*)
  (setq *error* tx2mtx_error)
 
  ;select text objects
  (setq sst1 (ssget))
  ;if objects selected...
  (if sst1
    (progn
      ;create list of text objects only
      (setq coun 0);set counter
      (setq sst2 (ssadd))
      (repeat (sslength sst1)
        ;get ename
        (setq enam (ssname sst1 coun))
        ;test to see if entity is a text object
        (if (eq (cdr (assoc '0 (entget enam))) "TEXT")
          ;check to see if text has rotation angle of 0
          (if (<= (cdr (assoc '50 (entget enam))) 1.0)
            (setq sst2 (ssadd enam sst2));add to selection
          )
        )
        (setq coun (1+ coun))
      )
      (if sst2
        (princ
          (strcat "\n" (itoa (sslength sst2))
                  " text objects found, with horizontal rotation..."
          )
        )
      )
      ;if sst2 is empty, then clear it...
      (if (eq (sslength sst2) 0)
        (setq sst2 nil)
      )
    )
  )
  ;sort text entities by Y value
  (if sst2 (tx2mtx_process))
  
  ;restore system variables
  (setvar "clayer" sv_clayer)
  (setvar "cecolor" sv_cecolor)
  (setq *error* old_error)
 (princ)
)
;Support functions
;***	REMOVE.lsp  by c.bethea
;This function removes an element from a list and return the list without
;the element
;
(defun REMOVE_ELEM (what from)
  (append 
    (reverse 
       (cdr (member what (reverse from)))
    )
    (cdr (member what from))   
  )
)
(princ "\nType TX2MTX to begin.")
(princ)
