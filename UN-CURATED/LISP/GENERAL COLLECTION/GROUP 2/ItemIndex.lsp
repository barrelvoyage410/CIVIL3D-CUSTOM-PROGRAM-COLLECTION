; =============================================================================
; Filename    :   ItemIndex.lsp
; Datum       :   14.01.2002
; Author      :   jme
; Copyright   :   MENZI ENGINEERING GmbH
; Revision  1 :   01.05.2004 jme - Block handling added
; Revision  2 :   __.__.____ ___ -
; -----------------------------------------------------------------------------
; Description:
; Item numbering with numbers or letters (1 2 3... or a b c... or A B C...).
; -----------------------------------------------------------------------------
; Global variables:
; Gb:Ill Gb:Ilu Gb:Inu Gb:Mde
; -----------------------------------------------------------------------------
; Internal LISP-functions:
; ChgAttVal ChkChrInp ListToStr GetChrInp ModLstPos IncrChr SelectNumBlock
; -----------------------------------------------------------------------------
; External LISP-functions:
;
; -----------------------------------------------------------------------------
; Version notes:
; AutoCAD:	Version:	Language:	AddIns:
; 14 up		1.01		English		...
; -----------------------------------------------------------------------------
;
; == Message on loading =======================================================
;
(princ "\nItemIndex v1.01")
;
; == Subs =====================================================================
;
(defun ChkChrInp (Val Len / ChkVal ChrCnt CurChr)
 (setq ChrCnt 1)
 (repeat (strlen Val)
  (setq CurChr (strcase (substr Val ChrCnt 1))
        ChrCnt (1+ ChrCnt)
        ChkVal (cons (and (>= CurChr "A") (<= CurChr "Z")) ChkVal)
  )
 )
 (and (<= (strlen Val) Len) (apply 'and ChkVal))
)

(defun StrToList (Val Len / ChrCnt CurChr ResLst)
 (setq ChrCnt 1)
 (repeat Len
  (setq CurChr (substr Val ChrCnt 1)
        ChrCnt (1+ ChrCnt)
        ResLst (cons (ascii CurChr) ResLst)
  )
 )
 (reverse ResLst)
)

(defun ListToStr (Lst)
 (apply 'strcat (mapcar 'chr Lst))
)

(defun GetChrInp (Pmt Def Len Lwc / CurVal GoLoop RetVal TmpStr)
 (setq GoLoop T
       TmpStr ""
 )
 (repeat Len (setq TmpStr (strcat TmpStr "z")))
 (while GoLoop
  (setq CurVal (getstring (strcat Pmt " <" (ListToStr Def) ">: ")))
  (cond
   ((eq CurVal "")
    (setq GoLoop nil
          RetVal Def
    )
   )
   ((not (ChkChrInp CurVal Len))
    (princ (strcat "requires a value between 'a' and '" TmpStr "'."))
   )
   (T
    (setq GoLoop nil
          RetVal (StrToList (strcase CurVal Lwc) Len)
    )
   )
  )
 )
 RetVal
)

(defun ModLstPos (Lst Pos Par / LstCnt)
 (setq LstCnt -1)
 (mapcar
 '(lambda (l)
   (if (eq Pos (setq LstCnt (1+ LstCnt))) Par l)
  ) Lst
 )
)

(defun IncrChr (Lst Len Lwc / CurPos LstCnt MaxNum MinNum NxtPos RetVal)
 (setq MaxNum (if Lwc 122 90)
       MinNum (if Lwc 97 65)
       LstCnt 0
       RetVal Lst
 )
 (if (>= (apply '+ RetVal) (* Len MaxNum))
  (progn
   (setq RetVal (list MinNum))
   (repeat (1- Len)
    (setq RetVal (cons 0 RetVal))
   )
   (setq RetVal (reverse RetVal))
  )
  (repeat Len
   (setq CurPos (nth LstCnt RetVal)
         NxtPos (cond ((nth (1+ LstCnt) RetVal)) (T 0))
   )
   (cond
    ((and (< CurPos MaxNum) (>= NxtPos MaxNum))
     (setq RetVal (ModLstPos RetVal LstCnt (1+ CurPos))
           RetVal (ModLstPos RetVal (1+ LstCnt) (1- MinNum))
     )
    )
    ((and (>= CurPos MaxNum) (= NxtPos 0))
     (setq RetVal (ModLstPos RetVal LstCnt MinNum)
           RetVal (ModLstPos RetVal (1+ LstCnt) (1- MinNum))
     )
    )
    ((and (> CurPos 0) (= NxtPos 0))
     (setq RetVal (ModLstPos RetVal LstCnt (1+ CurPos)))
    )
    (T
     (setq RetVal (ModLstPos RetVal LstCnt CurPos))
    )
   )
   (setq LstCnt (1+ LstCnt))
  )
 )
 RetVal
)

(defun SelectNumBlock (Pmt / CurEnt ExLoop) 
 (while (not ExLoop)
  (initget " ")
  (setq CurEnt (entsel Pmt))
  (cond
   ((= (type CurEnt) 'STR)
    (setq CurEnt nil
          ExLoop T
    )
   )
   ((and
     CurEnt
     (eq (cdr (assoc 0 (entget (car CurEnt)))) "INSERT")
     (eq (cdr (assoc 2 (entget (car CurEnt)))) "NumBlock") ;Block name!!!
    )
    (setq ExLoop T)
   )
   (CurEnt
    (prompt "Selected object is not a NumBlock. ")
   )
   (T
    (prompt "1 selected, 0 found.")
   )
  )
 )
 CurEnt
)

(defun ChgAttVal (Ent Lst / CurEnl CurEnt CurNme)
 (setq CurEnt Ent)
 (while CurEnt
  (setq CurEnl (entget CurEnt))
  (if (eq (cdr (assoc 0 CurEnl)) "ATTRIB")
   (progn
    (setq CurNme (cdr (assoc 2 CurEnl)))
    (if (member CurNme (mapcar 'car Lst))
     (setq CurEnl (entmod
                   (subst
                    (cons 1 (cdr (assoc CurNme Lst)))
                    (assoc 1 CurEnl)
                    CurEnl
                   )
                  )
     )
    )
   )
  )
  (setq CurEnt (entnext CurEnt))
 )
 (entupd Ent)
)
;
; == Main =====================================================================
;
(defun C:ItemIndex ( / CurEnt ExLoop InsPnt MaxChr MaxCnt NumVal OrgCmd OrgOsm
                       TmpStr)
 (initget "Numeric letterUpper letterLower")
 (setq Gb:Mde (cond (Gb:Mde) (T "Numeric"))
       TmpStr (strcat "\nNumeric or Letter upper-/lowercase "
                      "[Numeric/letterUpper/letterLower] <" Gb:Mde ">: "
              )
       Gb:Mde (cond ((getkword TmpStr)) (T Gb:Mde))
       MaxCnt 999			;Limit of numbering
       MaxChr 3				;Limit number of letters
       OrgCmd (getvar "CMDECHO")
       OrgOsm (getvar "OSMODE")
 )
 (setvar "CMDECHO" 0)
 (initget "selectBlock Reset")
 (setq TmpStr "\nInsert point of item [selectBlock/Reset] <Exit>: "
       InsPnt (getpoint TmpStr)
 )
 (cond
  ((eq InsPnt "selectBlock")
   (setq CurEnt (SelectNumBlock "\nSelect Block object <Exit>: ")
         InsPnt nil
   )
  )
  ((eq InsPnt "Reset")
   (setq Gb:Inu 1
         Gb:Ilu '(65 0 0)		;List length depends on MaxChr
         Gb:Ill '(97 0 0)		;List length depends on MaxChr
         InsPnt nil
   )
  )
 )
 (while (or InsPnt CurEnt)
  (setvar "OSMODE" 0)
  (setq Gb:Inu (cond (Gb:Inu) (1))
        Gb:Ilu (cond (Gb:Ilu) ('(65 0 0)))
        Gb:Ill (cond (Gb:Ill) ('(97 0 0)))
  )
  (cond
   ((eq Gb:Mde "Numeric")
    (initget 6)
    (setq TmpStr (strcat "\nDefault item number <" (itoa Gb:Inu) ">: ")
          Gb:Inu (cond ((getint TmpStr)) (T Gb:Inu))
    )
   )
   ((eq Gb:Mde "letterUpper")
    (setq Gb:Ilu (GetChrInp "\nDefault item letter" Gb:Ilu MaxChr nil))
   )
   ((eq Gb:Mde "letterLower")
    (setq Gb:Ill (GetChrInp "\nDefault item letter" Gb:Ill MaxChr T))
   )
  )
  (setq NumVal (cond
                ((eq Gb:Mde "Numeric") (itoa Gb:Inu))
                ((eq Gb:Mde "letterUpper") (ListToStr Gb:Ilu))
                ((eq Gb:Mde "letterLower") (ListToStr Gb:Ill))
               )
  )
  (if CurEnt
   (ChgAttVal (car CurEnt) (list (cons "NUMATT" NumVal))) ;Attribute name!!!
   (command "_.TEXT" "_S" "STANDARD" "_M" InsPnt "" 0 NumVal)
  )
  (cond
   ((eq Gb:Mde "Numeric") (setq Gb:Inu (if (< Gb:Inu MaxCnt) (1+ Gb:Inu) 1)))
   ((eq Gb:Mde "letterUpper") (setq Gb:Ilu (IncrChr Gb:Ilu MaxChr nil)))
   ((eq Gb:Mde "letterLower") (setq Gb:Ill (IncrChr Gb:Ill MaxChr T)))
  )
  (setvar "OSMODE" OrgOsm)
  (cond
   (InsPnt (setq InsPnt (getpoint "\nInsert point of item <Exit>: ")))
   (CurEnt (setq CurEnt (SelectNumBlock "\nSelect Block object <Exit>: ")))
  )
 )
 (setvar "CMDECHO" OrgCmd)
 (princ)
)
;
; == Copyright - Note (May be never deleted) ==================================
;
(princ "\n------------------------------------------------")
(princ "\n ©2002-2004 MENZI ENGINEERING GmbH, Switzerland ")
(princ "\n------------------------------------------------")
(princ "\nType 'ItemIndex' in the command line to start the programm...")
(princ)
;
; == End ItemIndex ============================================================

