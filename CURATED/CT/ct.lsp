
;; This program is used to copy text and then prompt to change the text
;;;*****************************************
;;;   Superior Designs - Custom programming available
;;;   Copyright (C) 1995-1997
;;;   Written by Craig Carr - 1995
;;;   http://www.inil.com/users/ccarr/sdi/acad.htm
;;;   e-mail ccarr@inil.com
;;;*****************************************
;;;Lisp routine for doing a copy and change of text.

;;;;;;**********************
(DEFUN C:CT()(SETVAR "CMDECHO" 0)
(SETQ CPT1 (CADR (setq t1 (entsel "Select Text")) ))       
(PRINC "\nCopy To Point ")
(COMMAND "COPY" t1 "" CPT1 pause)
(command "change" "l" "" "" "" "" "" "")
(SETVAR "CMDECHO" 1)
(PRIN1))
;;;;;;**********************