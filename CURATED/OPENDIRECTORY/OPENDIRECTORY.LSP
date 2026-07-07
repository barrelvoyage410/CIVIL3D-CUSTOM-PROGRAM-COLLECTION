;; Open folder current directory
(defun c:cd (/ _opendirectory)
  (defun _opendirectory	(path / sa)
    (cond ((and	(eq 'str (type path))
		(findfile (vl-string-right-trim "\\" path))
		(setq sa (vlax-create-object "Shell.Application"))
	   )
	   ;; ;; RJP   2024-02-14
	   ;; Using 'open vs 'explore will not open multiple windows with the same path
	   ;; https://www.theswamp.org/index.php?topic=59280.msg619291#msg619291
	   (vlax-invoke sa 'open path)
	   (vlax-release-object sa)
	  )
    )
    (princ)
  )
  (_opendirectory (getvar 'dwgprefix))
)