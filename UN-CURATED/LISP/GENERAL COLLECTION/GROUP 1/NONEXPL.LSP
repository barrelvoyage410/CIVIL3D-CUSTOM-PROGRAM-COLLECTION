;			NONEXPL-P
;	Original Code writtrn by Gilles Chanteau, Autodesk Expert Elite. Gilles wrote the code for "expl-P" to make blocks explodeable.
;	I changed the command name, copied the code, and changed the vlax-true statment to vlax-false, to make blocks non-explodeable.
;
;	Thanks Gilles Chanteau
;
(defun c:nonexpl-p ()
 (vl-load-com)
 (vlax-for b (vla-get-Blocks
 (vla-get-ActiveDocument (vlax-get-acad-object))
 )
 (or (wcmatch (vla-get-Name b) "`**_Space*")
 (vla-put-explodable b :vlax-false)
 )
 )
 (princ)
 ) 
;
;
(defun c:expl-p ()
 (vl-load-com)
 (vlax-for b (vla-get-Blocks
 (vla-get-ActiveDocument (vlax-get-acad-object))
 )
 (or (wcmatch (vla-get-Name b) "`**_Space*")
 (vla-put-explodable b :vlax-true)
 )
 )
 (princ)
 ) 	