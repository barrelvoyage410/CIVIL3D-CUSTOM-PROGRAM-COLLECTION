(defun C:EDITNUM ( / *error* @reset |e |dimzin |dp |zp |cmd |ss |i |ans |ent $value
                   |c |c1 |num |txt |char |nlist1 |nlist2 |prec |cprec |txtlen |filter
                   |nt |na @rprec @sprec |layer @switch @editnum @what |prompt |done
                   |layers @modtxt |e |etyp @signof @Unformat @modstr
                   @cv_add_list @cv_list2str @cv_check_lock)
   ;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
   ;*                                                                           *
   ;*         EDITNUM.LSP   by  John F. Uhden                                   *
   ;*                           2 Village Road                                  *
   ;*                           Sea Girt, NJ  08750                             *
   ;*                                                                           *
   ;* * * * * * * * * * * * * * * * * * * * * * * *

   ; Adjusts any imbedded numeric strings of a selection set of text entities.

   ; v2.4 (3-1-96) added (max ... (@prec $cv_diff)) in line 133
   ; v2.5 (3-4-96) changed @prec to @rprec for real and added @sprec for strings.
   ; v2.6 (12-11-99) added multiplcation option as requested by Jeremy Lange of
   ;      Maser (Toms River); also added numeric text filter '(1 . "*#*")
   ; v15.00 (04-07-00) for R15
   ;        (12-17-00) added multiple picks for Picklayer option
   ; v15.01 (09-05-02) added support for Inserts with Attributes
   ; v15.02 (01-09-03) revised to update attributes.
   ; v15.03 (02-24-04) revised to try and handle K&K's large number.
   ; v17.00 (03-03-09) revised to handle Mtext
   (gc)
   (prompt "\nEDITNUM v17.00 (c)1994-2009, John F. Uhden")
   (or *acad* (setq *acad* (vlax-get-acad-object)))
   (or *doc* (setq *doc* (vla-get-ActiveDocument *acad*)))
   (defun *error* (error)
     (if (= (type |cmd) 'INT)(setvar "cmdecho" |cmd))
     (if (= (type |dimzin) 'INT)(setvar "dimzin" |dimzin))
     (vla-endundomark *doc*)
     (cond
       ((not error))
       ((wcmatch (strcase error) "*QUIT*,*CANCEL*"))
       (1
         (princ (strcat "\nERROR: " error))
       )
     )
     (princ)
   )
   ;;--------------------------------------------
   ;; Intitialize drawing and program variables:
   ;;
   (setq |i 0
         |nt 0
         |na 0
         |nlist1 '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-")
         |nlist2 '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "." "-")
         |cmd (getvar "cmdecho")
         |dimzin (getvar "dimzin")
         |filter
         '(
            (-4 . "<OR")
              (-4 . "<AND")(0 . "MTEXT,TEXT")(1 . "*#*")(-4 . "AND>")
              (-4 . "<AND")(0 . "INSERT")(66 . 1)(-4 . "AND>")
            (-4 . "OR>")
          )
   )
   (if (/= (type $cv_diff) 'REAL)
      (setq $cv_diff 0.0)
   )
   (if (/= (type $cv_factor) 'REAL)
      (setq $cv_factor 1.0)
   )
   (if (not (vl-position $cv_editnum '("Difference" "Factor")))
      (setq $cv_editnum "Difference")
   )
   (vla-endundomark *doc*)
   (vla-startundomark *doc*)
   (setvar "cmdecho" 0)
   (setvar "dimzin" 1)
   (command "_.expert" (getvar "expert")) ;; dummy command
   
  ;;----------------------------------------------------------
  ;; Little function created 12-17-00 to add an item to a list
  ;; only if it's not already a member:
  ;;
  (defun @cv_add_list (item lst)
    (if (vl-position item lst)
      lst
      (cons item lst)
    )
  )

  ;;----------------------------------------------
  ;; ADDSTR (c)1995, John F. Uhden, CADvantage
  ;; Function adds to any list a list of comma-delimited
  ;; strings with a maximum length set by limit.
  ;; Simplified (3-08-99)
  (defun @cv_addstr (|list |str |limit / |last)
    (if (= (length |list) 0)
      (cons |str |list)
      (setq |last (last |list)
            |list (if (<= (+ (strlen |last)(strlen |str)) |limit)
                     (subst (strcat |last "," |str) |last |list)
                     (append |list (list |str))
                  )
      )
    )
  )

  ;;-----------------------------------------------------------------
  ;; This function takes a list of strings, and returns a
  ;; comma-delimited string of all items in the list, or "" if empty:
  ;; Revised 12-17-00
  (defun @cv_list2str (|lst / |str |item)
    (setq |str "")
    (foreach |item |lst
      (if (= (type |item) 'STR)     ; make sure item is a string
        (setq |str (strcat |str "," |item))
      )
    )
    (substr |str 2)
  )
  ;;------------------------------------------------
  ;; CHK_LOCK (c)1995, John F. Uhden, CADvantage
  ;; Function to check for locked layers:
  (defun @cv_check_lock (SS Att / Layer Layers e ent a i Llist elist cmd)
    (setq i 0 Llist nil Layers nil elist nil)
    (repeat (sslength SS)
      (setq e (ssname SS i)
          ent (entget e)
            i (1+ i)
        Layer (cdr (assoc 8 ent))
      )
      (if (= 4 (logand (cdr (assoc 70 (tblsearch "LAYER" Layer))) 4))
        (progn
          (setq elist (cons e elist))
          (if (or (not Llist)(not (vl-position (strcase Layer)(mapcar 'strcase Llist))))
            (setq Llist (cons Layer Llist))
          )
        )
      )
      ;; If Att is not nil, then check for attribute layers.
      (if (and Att (= (cdr (assoc 0 ent)) "INSERT")(vl-position (cons 66 1) ent))
        (progn
          (setq a (entnext e))
          (while (null (assoc -2 (setq ent (entget a))))
            (setq Layer (cdr (assoc 8 ent))
                  a (entnext a)
            )
            (if (= 4 (logand (cdr (assoc 70 (tblsearch "LAYER" Layer))) 4))
              (progn
                (if (not (vl-position e elist))
                  (setq elist (cons e elist))
                )
                (if (not (vl-position (strcase Layer)(mapcar 'strcase Llist)))
                  (setq Llist (cons Layer Llist))
                )
              )
            )
          )
        )
      )
    )
    (if Llist
      (progn
        (foreach Layer Llist (setq Layers (@cv_addstr Layers Layer 256)))
        (prompt "\nThe following layers are locked...\n")
        (foreach Layer Layers (princ Layer)(princ "\n"))
        (initget "Yes No")
        (if (= (getkword "UNlock them?  <Yes>/No: ") "No")
          (foreach e elist (setq SS (ssdel e SS)))
          (progn
            (setq cmd (getvar "cmdecho"))
            (setvar "cmdecho" 0)
            (foreach Layer Layers
              (command "_.layer" "_UN" Layer "")
            )
            (setvar "cmdecho" cmd)
          )
        )
      )
    )
    SS
  )
  ;;----------------------------------------------------------
  ;; UNFORMAT.LSP (c)2003, John F. Uhden, Cadlantic/CADvantage
  ;; v1.0 (04-01-03)
  ;; Removes MTEXT formatting with option to retain the "\\P" LineFeeds
  ;;
  ;; Rev. (05-09-05)
  (defun @UnFormat (Mtext KeepLF / Old New Tmp Str)
    (cond
      ((= (type Mtext) 'VLA-Object))
      ((= (type Mtext) 'Ename)
        (setq Mtext (vlax-ename->vla-object Mtext))
      )
      (1 (setq Mtext nil))
    )
    (and
      Mtext
      (= (vlax-get Mtext 'ObjectName) "AcDbMText")
      (setq Old (vlax-get Mtext 'TextString))
      (setq Tmp Old)
      (setq New "")
      (while (/= Tmp "")
        (cond
          ((wcmatch (strcase (setq Str (substr Tmp 1 2))) "\\[\\{}`~]")
            (setq Tmp (substr Tmp 3)
                  New   (strcat New Str)
            )
          )
          ((wcmatch (substr Tmp 1 1) "[{}]")
            (setq Tmp (substr Tmp 2))
          )
          ((and KeepLF (wcmatch (strcase (substr Tmp 1 2)) "\\P"))
            (setq New (strcat New (substr Tmp 1 2))
                  Tmp (substr Tmp 3)
            )
          )
          ;; added "\n" (03-03-08)
          ((and KeepLF (wcmatch (strcase (substr Tmp 1 1)) "\n"))
            (setq New (strcat New (substr Tmp 1 1))
                  Tmp (substr Tmp 2)
            )
          )
          ((wcmatch (strcase (substr Tmp 1 2)) "\\[LOP]")
            (setq Tmp (substr Tmp 3))
          )
          ((wcmatch (strcase (substr Tmp 1 2)) "\\[ACFHQSTW]")
            (setq Tmp (substr Tmp (+ 2 (vl-string-search ";" Tmp))))
          )
          (1
            (setq New (strcat New (substr Tmp 1 1))
                  Tmp (substr Tmp 2)
            )
          )
        )
      )
      (/= Old New)
      (not
        (vl-catch-all-error-p
          (vl-catch-all-apply
            'vlax-put (list Mtext 'TextString New)
          )
        )
      )
    )
  )
   ;;------------------------------------------------
   ;; Function to get the precision of a real number:
   ;;
   (defun @rprec (|n / |str |dp |zp)
      (setq |str (rtos (rem |n 1) 2 14)
            |dp 1
            |zp (strlen |str)
      )
      (while (/= (substr |str |dp 1) ".")(setq |dp (1+ |dp)))
      (while (= (substr |str |zp 1) "0")
         (setq |zp (1- |zp))
      )
      (- |zp |dp)
   )
   ;;---------------------------------------------------
   ;; Function to get the precision of a string number:
   ;;
   (defun @sprec (|str / |str |dp |zp)
      (setq |dp 1 |zp (strlen |str))
      (while (and (<= |dp |zp)(/= (substr |str |dp 1) "."))(setq |dp (1+ |dp)))
      (- |zp |dp)
   )
   ;;--------------------------------------------------
   ;; Function to switch between Difference and Factor:
   ;;
   (defun @switch ()
      (if (= $cv_editnum "Difference")
         (setq |prompt "\nDifference/Factor <Difference = "
               $value $cv_diff
         )
         (setq |prompt "\nDifference/Factor <Factor = "
               $value $cv_factor
         )
      )
      (setq |cprec (@rprec $value))
   )
   ;; Function added (02-24-04)
   (defun @signof (n)
     (if (minusp n) -1.0 1.0)
   )
   ;; Function created (03-03-09) to break text into
   ;; a list of strings for each line (soft or hard return)
   (defun @modtxt (Obj / Old Str pos1 pos2 pos Lines n)
     (if (= (type Obj) 'EName)
       (setq Obj (vlax-ename->vla-object Obj))
     )
     (setq Old (vlax-get Obj 'TextString))
     (setq Str Old)
     (while (> (strlen Str) 0)
       (or
         (setq pos1 (vl-string-search "\n" Str 1))
         (setq pos1 (strlen Str))
       )
       (or
         (setq pos2 (vl-string-search "\\P" Str 1))
         (setq pos2 (strlen Str))
       )
       (setq pos (min pos1 pos2))
       (setq Lines (cons (substr Str 1 pos) Lines)
             Str (substr Str (1+ pos))
       )
     )
     (and
       (setq Lines (reverse Lines))
       (setq Str (apply 'strcat (mapcar '@modstr Lines)))
       (/= Old Str)
       (or (vlax-put Obj 'TextString Str) 1)
     )
   )
     
   ;;-----------------------------------
   ;; Function subdivided out (09-05-02)
   ;; to handle attributes as well:
   ;; Revised (01-09-03) to update attributes
   ;; Revised (02-24-04) to try and handle K&K's large number.
   (defun @modstr (|txt / |c |txtlen |num |char |pos
                   !num !numdec !val !valdec !dec |dec +- |pos)
      (setq |c 1
            |txtlen (strlen |txt)
            |num ""
      )
      (while (and (< |c |txtlen)(not (vl-position (substr |txt |c 1) |nlist1)))
         (setq |c (1+ |c))
      )
      (setq |c1 (1- |c))
      (while (vl-position (setq |char (substr |txt |c 1)) |nlist2)
         (setq |num (strcat |num |char) |c (1+ |c))
      )
      (setq |c (1- |c))
      (if (equal @what +)
         (and
           (/= |num "")
           (if (setq |pos (vl-string-search "." |num))
              (setq |dec (substr |num (1+ |pos))
                    |num (substr |num 1 |pos)
              )
              (setq |dec "")
           )
           (setq !num (atof |num)
                 !numdec (atof |dec)
                 !valdec (rem $value 1)
                 !val (float (fix $value))
           )
           (cond
             ((= (@signof !num)(@signof $value)))
             ((minusp !valdec)
               (setq !numdec (1+ !numdec)
                     !num (1- !num)
               )
             )
             ((and (zerop !valdec)(>= $value !num))
               (setq !valdec (@signof !val)
                     !val (- !val (@signof !val))
               )
             )
             (1 1)
           )
           (setq !dec (+ !numdec !valdec)
                 !num (+ !num !val (fix !dec))
                 !dec (rem !dec 1)
           )
           (setq |prec (max 0 (@rprec $value)(@sprec |dec))
                 |dec  (rtos !dec 2 |prec)
                 |dec  (if (zerop |prec) "" (substr |dec (1+ (vl-string-search "." |dec))))
           )
           (if (and (zerop !num)(minusp !dec))
             (setq |num (strcat "-0" |dec))
             (setq |num  (strcat (rtos !num 2 0) |dec))
           )
           (setq |txt  (strcat (substr |txt 1 |c1) |num (substr |txt (1+ |c))))
         )
         (and
           (/= |num "")
           (setq |prec (max |cprec (@sprec |num)(@rprec $value))
                 |num (rtos (@what (atof |num) $value) 2 |prec)
                 |txt (strcat (substr |txt 1 |c1) |num (substr |txt (1+ |c)))
           )
         )
      )
      |txt
   )
   ;;------------------------------------------------------------------------
   ;; Function to select TEXT entities, check for locked layers, and process:
   ;;
   (defun @editnum ()
      (initget "All Layers Manually Picklayer")
      (setq |ans (getkword "\nText selection method, All/Layers/Picklayer/<Manually>: "))
      (cond
         ((= |ans "All")
            (prompt "\nGetting ALL text with numbers... ")
            (setq |ss (ssget "X" |filter))
            (prompt "DONE.")
         )
         ((= |ans "Layers")
            (setq |ans (getstring "\nLayer names <*>: "))
            (if |ans (setq |layer |ans)(setq |layer "*"))
            (prompt "\nGetting all text with numbers on selected layer(s)... ")
            (setq |ss (ssget "X" (append |filter (list (cons 8 |layer)))))
            (prompt "DONE.")
         )
         ((= |ans "Picklayer")
            (while (setq |e (car (entsel "\nSelect object on desired layer:")))
               (setq |layer (cdr (assoc 8 (entget |e)))
                     |layers (@cv_add_list |layer |layers)
               )
               (princ (strcat "\nLayers: " (@cv_list2str |layers)))
            )
            (if |layers
               (progn
                  (setq |layers (@cv_list2str |layers))
                  (prompt (strcat "\nGetting all text with numbers on layer(s) " |layers "... "))
                  (setq |ss (ssget "X" (append |filter (list (cons 8 |layers)))))
                  (prompt "DONE.")
               )
            )
         )
         (1 (prompt "\nDon't worry about selecting objects that are not text with numbers.")
            (prompt "\nThey will be filtered out of selection set.")
            (setq |ss (ssget |filter))
         )
      )
      (princ "\n")
      (if |ss (setq |ss (@cv_check_lock |ss 1))) ; check for locked layers

      (if (and |ss (> (sslength |ss) 0))
         (repeat (sslength |ss)
            (prompt (strcat "\rProcessing # " (itoa (1+ |i))))
            (setq |e (ssname |ss |i)
                  |ent  (entget |e)
                  |etyp (cdr (assoc 0 |ent))
                  |i (1+ |i)
            )
            (cond
               ((vl-position |etyp '("MTEXT" "TEXT"))
                  (@Unformat |e 1)
                  (if (@modtxt |e)(setq |nt (1+ |nt)))
               )
               ((= |etyp "INSERT")
                  (while
                     (and
                        (setq |e (entnext |e))
                        (setq |ent (entget |e))
                        (= (cdr (assoc 0 |ent)) "ATTRIB")
                        (wcmatch (cdr (assoc 1 |ent)) "*#*")
                     )
                     (if (@modtxt |e)(setq |na (1+ |na)))
                  )
               )
            )
         )
      )
      (if |ss
         (prompt
            (strcat
               "\nModified " (itoa |nt) " text object(s) and " (itoa |na) " attribute(s)."
            )
         )
      )
   )
   ;;----------------------------------------
   ;; Get the value of the amount to change:
   ;;
   (@switch) ; to set up $value and |cprec
   (setq |done nil)
   (while (not |done)
      (initget "Difference Factor")
      (setq |ans (getreal (strcat |prompt (rtos $value 2 |cprec) ">: ")))
      (cond
         ((or (= |ans "Difference")(= |ans "Factor"))
            (setq $cv_editnum |ans)
            (@switch)
         )
         ((not |ans)
            (setq |done 1)
         )
         ((= (type |ans) 'REAL)
            (setq $value |ans |done 1)
         )
      )
   )
   (setq |cprec (@rprec $value))
   (cond
      ((= $cv_editnum "Difference")
         (setq $cv_diff $value @what +)
         (if (zerop $cv_diff)
            (prompt "\nNothing to do (no difference).")
            (@editnum)
         )
      )
      ((= $cv_editnum "Factor")
         (setq $cv_factor $value @what *)
         (if (= (rtos $cv_factor 2 |cprec)(rtos 1.0 2 |cprec))
            (prompt "\nNothing to do (factor is unity).")
            (@editnum)
         )
      )
   )
   (*error* nil)
)
(defun C:EN ()(C:EDITNUM))

