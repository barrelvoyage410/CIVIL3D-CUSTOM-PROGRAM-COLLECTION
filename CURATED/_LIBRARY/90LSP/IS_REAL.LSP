(defun is_real (str_value minv maxv)		;string value, min/max ranges
	(if									
		(distof str_value)					;is it a number?
		(and
			(if
				(not minv)		;min range nil?
				t				;return t
				(>= (distof str_value) minv)	;above min range?
			)
			(if
				(not maxv)		;max range nil?
				t				;return t
				(<= (distof str_value) maxv)	;below max range?
			)
		)
		nil							;not a number
	)
)
