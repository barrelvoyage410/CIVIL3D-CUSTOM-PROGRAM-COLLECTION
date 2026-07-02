; ----------------------------------------------------------------------
;                   (Converts 3D Faces to 3D Polylines)
;            Copyright (C) 1998 DotSoft, All Rights Reserved
;                      Website: www.dotsoft.com
; ----------------------------------------------------------------------
; DISCLAIMER:  DotSoft Disclaims any and all liability for any damages
; arising out of the use or operation, or inability to use the software.
; FURTHERMORE, User agrees to hold DotSoft harmless from such claims.
; DotSoft makes no warranty, either expressed or implied, as to the
; fitness of this product for a particular purpose.  All materials are
; to be considered ‘as-is’, and use of this software should be
; considered as AT YOUR OWN RISK.
; ----------------------------------------------------------------------

(defun c:3dfto3dp ()
  (setq cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "UNDO" "G")
  (setq sset (ssget '((0 . "3DFACE"))))
  (if sset
    (progn
      (setq itm 0 num (sslength sset))
      (while (< itm num)
        (setq hnd (ssname sset itm))
        (setq ent (entget hnd))
        (setq pt1 (cdr (assoc 10 ent)))
        (setq pt2 (cdr (assoc 11 ent)))
        (setq pt3 (cdr (assoc 12 ent)))
        (setq pt4 (cdr (assoc 13 ent)))
        (entdel hnd)
        (command "_3DPOLY" pt1 pt2 pt3 pt4 "C")
        (setq itm (1+ itm))
      )
      (princ ", Done.")
    )
  )
  (setq sset nil)
  (command "UNDO" "E")
  (setvar "CMDECHO" cmdecho)
  (princ)
)

(princ "DS> 3DFTO3DP.LSP loaded ... type 3DFTO3DP to run.")
