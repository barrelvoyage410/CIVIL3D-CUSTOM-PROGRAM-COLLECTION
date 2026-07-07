"Replace the file path below with your own, doubling every backslash (1 to 2, 2 to 4)"
"Make sure the file name itself is included in the path, not just the folder location."
"Make sure quotation marks are on each end of the path."


(defun c:RLL ()
  (setq filePath "C:\\Users\\ryan\\Downloads\\Reload Lisp.lsp")
  (princ (strcat "\nFile path: " filePath))
  (if (findfile filePath)
    (progn
      (load filePath)
    ) ; end of progn
    (princ (strcat "\nError: File not found - " filePath))
  )
  (princ)
) ; end of defun




"Replace 'Hello' below w/ other text, to check if autocad has successfully reloaded this lisp."
(princ (strcat "\nHello"))



"Place the lisp you're curretly testing below, save, then type 'RLL' w/in AutoCAD to quickly reload it."