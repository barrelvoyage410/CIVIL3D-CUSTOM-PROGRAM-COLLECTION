(defun is_int (str_value minv maxv / x)		;string value and min/max ranges
	(if                                     ;for 32-bit integers
      (and
		(setq x (distof str_value))			;is it a number?
		(>= x -2147483648)					;within range of 32-bit ints?
		(<= x  2147483647)
      )
		(progn
			(if
				(eq 'INT (type (read str_value)))	;is it an integer?
				(if
					(and
						(if
							(not minv)		;min range nil?
							t				;return t
							(>= (read str_value) minv)	;above min range?
						)
						(if
							(not maxv)		;max range nil?
							t				;return t
							(<= (read str_value) maxv)	;below max range?
						)
					)
					(read str_value)		;return the integer
					nil						;not in range
				)
				nil							;not an integer
			)
		)
		nil									;not a number
	)
)

