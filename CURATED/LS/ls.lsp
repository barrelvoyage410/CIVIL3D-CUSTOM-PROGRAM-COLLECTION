;;;;;;;;;;;;***********
;; Use this to set layers or create them "on the fly"
;;;*****************************************
;;;   Superior Designs - Custom programming available
;;;   Copyright (C) 1994-1997
;;;   Written by Craig Carr - 1994
;;;   http://www.inil.com/users/ccarr/sdi/acad.htm
;;;   e-mail ccarr@inil.com
;;;*****************************************
;;;Lisp routine for setting layers. Winner in Cadalyst


(defun c:ls () (setvar "cmdecho" 0)(setq rnlr 1 ls:err *error*)
(defun *error* () (msg)(setq *error* ls:err)(setvar "cmdecho" 1))
(IF (= (SETQ lslyr(GETSTRING "\nPress RETURN to select entity <Layer Name> ")) "")
(PROGN(while rnlr(setq layrset (entsel "\nSelect enity on target layer: "))
(if (/= layrset nil) (setq rnlr nil)(princ"\nNo enity found")))
(setq lslyr (cdr(assoc 8(entget(car layrset))))) ) ;END PROGN 

  (IF (= (TBLSEARCH "LAYER" lslyr) nil)(PROGN (PRINC (STRCAT "LAYER '" lslyr "' DOESN'T EXIST"))
         (INITGET 1 "C S")(SETQ LSASK(GETKWORD (STRCAT "\n(C)reate new layer<" lslyr ">/(S)earch for existing layer: ")))
         (IF (= LSASK "C")(PROGN (IF (= (SETQ lslyrCLR(GETSTRING (STRCAT "\nCOLOR OF LAYER " lslyr " <white>: "))) "")(SETQ lslyrCLR "WHITE"))
                           (IF (= (SETQ lslyrLTP(GETSTRING (STRCAT "\nLINETYPE FOR LAYER " lslyr " <continuous>: "))) "")(SETQ lslyrLTP "CONTINUOUS"))
                                 (COMMAND "LAYER" "N" lslyr "C" lslyrCLR lslyr "LT" lslyrLTP lslyr "") ) ;; END "c"
                          (PROGN (COMMAND "LAYER" "?" "" "")
                                 (SETQ lslyr(GETSTRING "\n CHOOSE EXISTING LAYER NAME: ")) ) ) ;; END "s" END IF LSASK
           ))) ;; END PROGN END IF TBLSEARCH ;END IF lslyr
(command "layer" "s" lslyr "")(terpri)(setq *error* ls:err)(setvar "cmdecho" 1)(princ))

;;;;**************