;ALOAD.LSP
;program to load Lisp functions without (" ")
(Defun C:ALOAD ()
   (setq LSP (strcat (getstring "\nLisp Program Name: ") ".lsp"))
   (if (findfile LSP) (load LSP))
   (setq LSP nil)
   (princ)
)
