;;; ETABLES.LSP
;;; Copyright 1992,93 by Mountain Software
;;;
;;; This program requires ELF, the Extended Lisp Function library
;;;
;;; THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED
;;; WARRANTY.  ALL IMPLIED WARRANTIES OF FITNESS FOR ANY PARTICULAR
;;; PURPOSE AND OF MERCHANTABILITY ARE HEREBY DISCLAIMED.
;;;    
;;; adapted from: TABLES.LSP
;;; by Duff Kurland - Autodesk, Inc.

(Princ "\nLoading eTables.Lsp")
(Load"ELF")

;;;  Dump all the symbol tables

(DeFun C:TABLES (/ olderr ocmd key key2 mlist clist video vcol vrow)
  (SetQ olderr	*error*
	*error*  myerror
	ocmd	 (GetVar "cmdecho")
	mlist	 '("Layer" "LineType" "View" "Style" "Block" "UCS" "ViewPort")
	clist	 '(layer ltype view style block ucs vport)
	video	 (Get_Video )
	vcol	 (1-(Car video))
	vrow	 (1-(Cadr video))
	dashline (Strnset "=" 80)
  )
  (SetVar "cmdecho" 0)
  (Cls 7) (Set_Color 23)
  (While(And (/= key2 F10_Key)(/= key Esc_Key))
    (Wopen 0 0 vcol 3 30 23 2)
    (Wputcen "ELF Tables")
    (Scr_Fill 0 vrow vcol 1 32 65)
    (Prts 1 vrow "F1 - Help  Esc - Exit  F10 - Exit Symbol Leaving Data" 65)
    (SetQ ans (Wmenu mlist -1 -1 23 23 65 "Select")
	  key (Cadr ans)
	  i   (Car ans))
    (If (= key Enter_Key)
      (SetQ key2 (Eval(List(Nth i clist)))))	  ; execute the command
    (If (/= key2 F10_Key)
      (Cls 7))
  )
  (SetVar "cmdecho" ocmd)
  (SetQ *error* olderr) 	      ; Restore old *error* handler
  (Princ)
)

(DeFun C:ETABLES() (C:TABLES))	      ; A command alias

(DeFun MYERROR (s)		      ; If an error (such as CTRL-C) occurs
                                      ; while this command is active...
  (Beep)
  (Wmsg (Strcat "eTables ERROR\n" s) 1 (| white red_bg))
  (SetVar "cmdecho" ocmd)             ; Restore saved modes
  (SetQ *error* olderr)               ; Restore old *error* handler
  (WcloseAll)			      ; Close any open windows
  (Cls 7)
  (Princ)
)

;;;  (LAYER) - Dump the layer table

(DeFun LAYER ( / c d f ln lt ly n x layers)
  (Wmsg "Loading Layers..." nil 64)
  (WgotoXY 0 2)
  (tblset "layer")
  (SetQ layers (List "  Layer           Status  Color Linetype     Description"
		     dashline)
	cl (GetVar "clayer")          ; get current layer
	n  0
	x  (next T))		      ; get first layer
  (While x
    (SetQ n  (1+ n)
          ly (fld  2 x)               ; layer name
          ln (fld  6 x)               ; linetype name
          c  (fld 62 x)               ; color number
          f  (LogAnd (fld 70 x) 1)    ; "frozen" flag
          lt (TblSearch "ltype" ln)   ; linetype table entry
          d  (fld  3 lt)              ; linetype prose description
    )
    (Werase_Line 2)
    (Wputcen ly)
    (applst 'layers (Sprintf "%s %-15.15s %-7.7s %-5d %-12.12s %-.30s"
      (If (= ly cl) "*" " ")      ; flag current layer
      ly
      (Cond 
	((= f 1) "Frozen") ; edit status
	((< c 0) "Off")
	(T	 "On")
      )
      (Abs c) ln d
    ))
    (SetQ x (next nil)) 	      ; get next layer entry
  )
  (If (= n 0)
    (applst 'layers "  -None-"))
  (Wclose)
  (Symbol layers)                      ; display it
)

;;;  (LTYPE) - Dump the linetype table

(DeFun LTYPE ( / a cl d f lt n s x linetype)
  (Wmsg "Loading Linetypes..." nil 64)
  (WgotoXY 0 2)
  (tblset "ltype")
  (SetQ linetype (List "  Linetype    Align  Segs  Description" dashline)
	cl (GetVar "celtype")         ; get current linetype
	f  "*")                       ; set default "current" flag

  ;;  If current linetype is "BYLAYER", look up the linetype
  ;;  associated with the current layer, and change the
  ;;  "current" flag from "* " to "L ".

  (SetQ cl
    (Cond 
      ((= cl "BYBLOCK") "")
      ((= cl "BYLAYER") 
        (SetQ f "L ")
        (fld 6 (TblSearch "layer" (GetVar "clayer")))
      )
      (T cl)
    )
  )
  (SetQ n 0)
  (SetQ x (next T))                   ; first linetype
  (While x
    (SetQ n  (1+ n)
          lt (fld  2 x)               ; linetype name
    )
    (Werase_Line 2)
    (Wputcen lt)
    (applst 'linetype
      (Sprintf "%2.2s%-12.12s%-7.7c%-6d%-30.30s"
	(If (= lt cl) f "")           ; flag current entity linetype
	lt			      ; edit linetype name
	(fld 72 x)		      ; alignment code
	(SetQ s (fld 73 x))	      ; number of dash length items
	(fld  3 x)		      ; linetype prose description
    ))
    (If (> s 0) 
      (Progn

        ;;;  Edit dash length items

        (SetQ x (Member (Assoc 49 x) x)) ; get list of dash items
        (While x
	  (SetQ s (cdar x))	     ; get dash length
	  (applst 'linetype (Sprintf "                           %s"
	    (Cond 
	      ((= s 0) "Dot")
	      ((> s 0) (StrCat "Pen down " (RtoS s 2 4)))
	      (T       (StrCat "Pen up   " (RtoS (Abs s) 2 4)))
	    )
	  ))
          (SetQ x (Cdr x))            ; get next dash item
        )
      )
    )
    (SetQ x (next nil))               ; get next linetype entry
  )
  (If (= n 0)
    (applst 'linetype "  -None-"))
  (Wclose)
  (Symbol linetype)
)

;;;  (VIEW) - Dump the named view table

(DeFun VIEW ( / c d h n v w x views)
  (Wmsg "Loading Views..." nil 64)
  (tblset "view")
  (SetQ views (List
	  "  View        Height x Width        Center        Direction" dashline)
	n 0
	x (next T))		      ; get first view
  (While x
    (SetQ n  (1+ n)
          v  (fld  2 x)               ; view name
          c  (fld 10 x)               ; center point
          d  (fld 11 x)               ; view direction
          h  (fld 40 x)               ; height
          w  (fld 41 x)               ; width (valid only for windows)
    )
    (applst 'views
      (Sprintf "  %-10.10s%8.2f x %-8.2f:%8.2f,%-8.2f:%.2f,%.2f,%.2f"
	v h w (Car c) (Cadr c) (Car d) (Cadr d) (Caddr d)))
    (SetQ x (next nil))               ; get next view entry
  )
  (If (= n 0)
    (applst 'views "  -None-"))
  (Wclose)
  (Symbol views)
)

;;;  (STYLE) - Dump the text style table

(DeFun STYLE ( / cs fb ff g h n o s w x styles)
  (Wmsg "Loading Styles..." nil 64)
  (WgotoXY 0 2)
  (tblset "style")
  (SetQ styles (List
	  "  Text style    Height  Width   Slant  Flags  Font      Bigfont"
	  dashline)
	cs (GetVar "textstyle")       ; get current style
	n  0
	x  (next T))		      ; get first style
  (While x
    (SetQ n  (1+ n)
          s  (fld  2 x)               ; style name
          ff (fld  3 x)               ; primary font file
          fb (fld  4 x)               ; big font file
          h  (fld 40 x)               ; height
          w  (fld 41 x)               ; width factor
          o  (fld 50 x)               ; obliquing angle
          g  (fld 71 x)               ; generation flags
    )
    (Werase_Line 2)
    (Wputcen s)
    (applst 'styles (Sprintf "%s %-12.12s%8.4f%8.4f%7s%7d  %-10s%-20s"
	(If (= s cs) "*" " ")         ; flag current style
	s h w (AngtoS o 0 2) g ff fb
    ))
    (SetQ x (next nil))               ; get next style entry
  )
  (If (= n 0)
    (applst 'styles "  -None-"))
  (Wclose)
  (Symbol styles)
)

;;;  (BLOCK) - Dump the block definition table

(DeFun BLOCK ( / b e ec ed et f n o x blocks)
  (Wmsg "Loading Blocks..." nil 64)
  (WgotoXY 0 2)
  (tblset "block")
  (SetQ blocks (List "  Block       Flags  Origin" dashline)
	n 0
	x (next T))		      ; get first block definition
  (While x
    (SetQ n  (1+ n)
          b  (fld  2 x)               ; block name
          o  (fld 10 x)               ; origin X,Y,Z
          f  (fld 70 x)               ; flags
    )
    (Werase_Line 2)
    (Wputcen b)
    (applst 'blocks (Sprintf "  %-12.12s%-7d%.4f, %.4f, %.4f"
      b f (Car o) (Cadr o) (Caddr o)))
    ;;;  Display interesting facts about the entities comprising
    ;;;  this block definition.

    (If(= (SubStr b 1 1) "*")          ; skip anonomous blocks
      (applst 'blocks (Sprintf "%14sAnonomous Block (Hatch)" ""))
    ;else
    (Progn
      (SetQ e (fld -2 x))               ; point to first entity
      (While e
        (SetQ ed (EntGet e)             ; get the entity data
              et (fld  0 ed)            ; entity type
              ec (fld 62 ed))           ; entity color
        (applst 'blocks (Sprintf "%14s%9s on layer %s with color %s"
          " " et
          (fld 8 ed)                  ; edit layer name
          (Cond 
            ((= ec 0)  "BYBLOCK")     ; edit color number
            ((Null ec) "BYLAYER")
            (T         (ItoA ec))
          )
        ))
        (If (SetQ e (EntNext e))        ; if there's another entity,
            (SetQ ed (EntGet e))        ; read its data
        )
      )
    ))
    (SetQ x (next nil))               ; get next block entry
  )
  (If (= n 0)
    (applst 'blocks "  -None-"))
  (Wclose)
  (Symbol blocks)
)

;;;  (UCS) - Dump the UCS table

(DeFun UCS ( / n x na o xd yd oa xs ys)
  (Wmsg "Loading UCS..." nil 64)
  (tblset "ucs")
  (SetQ ucses (List
	 "  UCS         Origin              X axis direction    Y axis direction"
	 dashline)
       n  0
       x  (next T))		     ; get first ucs
  (While x
    (SetQ n  (1+ n)
          na (fld  2 x)               ; UCS name
          o  (fld 10 x)               ; origin
	  os (Sprintf "(%.2f,%.2f,%.2f)" (Car o) (Cadr o) (Caddr o))
          xd (fld 11 x)               ; X axis direction
	  xs (Sprintf "(%.2f,%.2f,%.2f)" (Car xd) (Cadr xd) (Caddr xd))
          yd (fld 12 x)               ; Y axis direction
	  ys (Sprintf "(%.2f,%.2f,%.2f)" (Car yd) (Cadr yd) (Caddr yd))
    )
    (applst 'ucses
      (Sprintf "%s %-12.12s%-20.20s%-20.20s%-20.20s"
	(If (= na cucs) "*" " ")      ; flag current UCS
	na os xs ys)
    )
    (SetQ x (next nil))               ; get next UCS entry
  )
  (If (= n 0)
    (applst 'ucses "  -None-"))
  (Wclose)
  (Symbol ucses)
)

;;;  (VPORT) - Dump the viewport table

(DeFun VPORT ( / n x na ll ur v)
  (Wmsg "Loading Vports..." nil 64)
  (SetQ prev nil)
  (tblset "vport")
  (SetQ vports (List "  Viewport    Lower left     Upper Right     View Mode"
		     dashline)
	n  0
	x  (nextvp T prev))	      ; get first viewport
  (While x
    (SetQ n  (1+ n)
          na (fld  2 x)               ; viewport name
          ll (fld 10 x)               ; lower left corner
	  ls (Sprintf "(%.2f,%.2f)" (Car ll) (Cadr ll))
          ur (fld 11 x)               ; upper right corner
	  rs (Sprintf "(%.2f,%.2f)" (Car ur) (Cadr ur))
          v  (fld 71 x)               ; view mode
    )
    (applst 'vports
      (Sprintf "  %-10.10s  %-15.15s%-15.15s %f" na ls rs v))
    (SetQ x (nextvp nil prev))        ; get next viewport entry
  )
  (If (= n 0)
    (applst 'vports "  -None-"))
  (Wclose)
  (Symbol vports)
)

;;; append a value to a list

(DeFun APPLST (&lst val)
  (SetQ lst (Append (Eval &lst) (List val)))
  (Set &lst lst)
)

;;;  Return the value associated with a particular entity field

(DeFun FLD (num lst)
  (Cdr (Assoc num lst))
)

;;;  Set up to process specified symbol table.
;;;  obtain all entries and sort them forming TBLENTS list.

(DeFun TBLSET (tbl / new s)
  (SetQ tblname tbl)                  ; set table name
  (SetQ tblents nil)		  ; start with null list
  (SetQ new (Cdr (Assoc 2 (TblNext tbl T)))) ; get first entry name
  (While new
    (SetQ tblents (Cons new tblents)) ; add to list
    (SetQ new (Cdr (Assoc 2 (TblNext tbl)))) ; get next entry name
  )
  (SetQ tblents (Qsort tblents)) ; sort the name list
)

;;;  Obtain next (Or first) entry from sorted entry list.

(DeFun NEXT (first / temp)
  (SetQ temp (Car tblents))        ; get next name from list
  (If temp
    (Progn                         ; if not end of list...
      (SetQ tblents (Cdr tblents)) ; chop from list
      (TblSearch tblname temp)     ; get table entry for this name
    )
  )
)

;;;  Obtain next (Or first) vports entry from sorted entry list.

(DeFun NEXTVP (first prev / temp)
  (If first
    (SetQ temp (Car tblents))     ; get first name from list
    (Progn
      (SetQ prev (Car tblents))   ; store previous name
      (SetQ temp (Cadr tblents))  ; get next name from list
    )
  )
  (If temp
    (Progn
      (If (Null first)
        (SetQ tblents (Cdr tblents)); chop from list
      )
      (If (= prev temp)
        (Progn
          (SetQ prev temp)
          (TblNext tblname first) ; get next table entry
        )
        (Progn
          (SetQ prev temp)
          (TblSearch tblname temp T) ; get table entry for this name
        )
      )
    )
  )
)

(Princ "\neTables.Lsp loaded, Enter \"ETABLES\" or \"TABLES\" to run...")
(Princ)

