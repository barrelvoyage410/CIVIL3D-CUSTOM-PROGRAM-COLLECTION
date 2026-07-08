(defun c:cp (/ ob) ;changes a line or arc to a polyline;
  (setq ob (entsel))
	(princ "Select line or arc: ")
(command "_pedit" ob "_y" ""))