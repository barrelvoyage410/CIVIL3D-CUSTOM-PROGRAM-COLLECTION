;
; -- Function CreateDict
; Creates a new or returns an existing dictionary.
; Arguments [Typ]:
;   Nme = Name of the new dictionary [STR]
; Return [Typ]:
;   > Entity name of new/existing dictionary [ENAME]
; Notes:
;   None
;
(defun CreateDict (Nme / DicEnt NewDic TmpLst)
 (if (not (setq NewDic (GetDictEnt Nme)))
  (setq TmpLst '((0 . "DICTIONARY") (100 . "AcDbDictionary"))
        DicEnt (entmakex TmpLst)
        NewDic (dictadd (namedobjdict) Nme DicEnt)
  )
  NewDic
 )
)
;
; -- Function DelDict
; Deletes the specified dictionary.
; Arguments [Typ]:
;   Nme = Name of the dictionary to delete [STR]
; Return [Typ]:
;   > Entity name [ENAME]
; Notes:
;   None
;
(defun DelDict (Nme)
 (dictremove (namedobjdict) Nme)
)
;
; -- Function AddDictRec
; Adds a record to a dictionary.
; Arguments [Typ]:
;   Nme = Name of dictionary to access [STR]
;   Key = Keyname for the record object [STR]
;   Lst = Data list '((Key1 . Value1)...(Keyx . Valuex)) [LIST]
; Return [Typ]:
;   > Entity name of modified dictionary [ENAME]
; Notes:
;   None
;
(defun AddDictRec (Nme Key Lst / DicEnt TmpLst)
 (setq TmpLst (append
              '((0 . "XRECORD") (100 . "AcDbXrecord"))
               Lst
              )
       DicEnt (entmakex TmpLst)
 )
 (dictadd (GetDictEnt Nme) Key DicEnt)
)
;
; -- Function GetDictRec
; Retrieves the data list by key from a dictionary.
; Arguments [Typ]:
;   Nme = Name of dictionary to access [STR]
;   Key = Keyname for the record object [STR]
; Return [Typ]:
;   > Data list [LIST]
; Notes:
;   None
;
(defun GetDictRec (Nme Key)
 (cdr (cddddr (cddddr (dictsearch (GetDictEnt Nme) Key))))
)
;
; -- Function ChgDictRec
; Redefines the specified record of a dictionary.
; Arguments [Typ]:
;   Nme = Name of dictionary to access [STR]
;   Key = Keyname for the record object [STR]
;   Lst = Data list '((Key1 . Value1)...(Keyx . Valuex)) [LIST]
; Return [Typ]:
;   > Entity name of modified dictionary [ENAME]
; Notes:
;   None
;
(defun ChgDictRec (Nme Key Lst)
 (if (GetDictRec Nme Key)
  (progn
   (DelDictRec Nme Key)
   (AddDictRec Nme Key Lst)
  )
 )
)
;
; -- Function DelDictRec
; Deletes the specified record from a dictionary.
; Arguments [Typ]:
;   Nme = Name of dictionary to access [STR]
;   Key = Keyname for the record object [STR]
; Return [Typ]:
;   > Entity name of modified dictionary [ENAME]
; Notes:
;   None
;
(defun DelDictRec (Nme Key)
 (dictremove (GetDictEnt Nme) Key)
)
;
; -- Function GetDictKeys
; Returns a list of keynames from the specified dictionary.
; Arguments [Typ]:
;   Nme = Name of dictionary to access [STR]
;   Key = Keyname for the record object [STR]
; Return [Typ]:
;   > Key list [LIST]
; Notes:
;   None
;
(defun GetDictKeys (Nme / TmpLst)
 (cond
  ((setq TmpLst (GetDict Nme)) (GetMassoc 3 TmpLst))
  (T nil)
 )
)
;
; -- Function GetDictKeysVals
; Returns a list of keynames with associated values from the
; specified dictionary.
; Arguments [Typ]:
;   Nme = Name of dictionary to access [STR]
; Return [Typ]:
;   > Key value list '((Key1 . Value1)...(Keyx . Valuex)) [LIST]
; Notes:
;   None
;
(defun GetDictKeyVals (Nme)
 (mapcar
 '(lambda (l)
   (cons l (cdar (GetDictRec Nme l)))
  ) (GetDictKeys Nme)
 )
)
;
; -- Function ListDicts
; Returns a list of all dictionaries in the current drawing.
; Arguments [Typ]:
;   --- =
; Return [Typ]:
;   > List of dictionaries [LIST]
; Notes:
;   None
;
(defun ListDicts ()
 (GetMassoc 3 (entget (namedobjdict)))
)
;
; -- Function GetDict
; Retrieves the entity definition list of the specified dictionary.
; Arguments [Typ]:
;   Nme = Name of the dictionary [STR]
; Return [Typ]:
;   > Entity definition of dictionary [LIST]
; Notes:
;   None
;
(defun GetDict (Nme)
 (dictsearch (namedobjdict) Nme)
)
;
; -- Function GetDictEnt
; Retrieves the entity name of the specified dictionary.
; Arguments [Typ]:
;   Nme = Name of the dictionary [STR]
; Return [Typ]:
;   > Entity name of dictionary [ENAME]
; Notes:
;   None
;
(defun GetDictEnt (Nme / TmpLst)
 (cond
  ((setq TmpLst (GetDict Nme)) (GetAssoc -1 TmpLst))
  (T nil)
 )
)
;
; -- Function GetMassoc
; Get multiple associative values from a list.
; Arguments [Typ]:
;   Key = Key to search [INT]
;   Lst = Dotted pair list [LIST]
; Return [Typ]:
;   > List of values [LIST]
; Notes:
;   Published by T.Tanzillo
;
(defun GetMassoc (Key Lst)
 (apply 'append
  (mapcar
   '(lambda (l) (if (eq (car l) Key) (list (cdr l)))) Lst
  )
 )
)
;
; -- Function GetAssoc
; Get associative value from a list.
; Arguments [Typ]:
;   Key = Key to search [INT]
;   Lst = Dotted pair list [LIST]
; Return [Typ]:
;   > Value [ALL]
; Notes:
;   None
;
(defun GetAssoc (Key Lst)
 (cdr (assoc Key Lst))
)
