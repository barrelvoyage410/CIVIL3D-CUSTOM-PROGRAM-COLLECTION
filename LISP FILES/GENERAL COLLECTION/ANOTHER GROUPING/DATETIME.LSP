(defun C:datetime ()
	(setq DATST (rtos (getvar "cdate") 2 16))
	(setq HRS (atoi (substr DATST 10 2)))
	(cond
		((= HRS 00)
			(setq NHRS (itoa (+ HRS 12)))
			(setq XTR "a.m.")
		)
		((< HRS 12)
			(setq NHRS (itoa HRS))
			(setq XTR "a.m.")
		)
		((> HRS 12)
			(setq NHRS (itoa (- HRS 12)))
			(setq XTR "p.m.")
		)
	)
	(setq MO (substr DATST 5 2))
		(cond
			((= MO "01")
				(setq MONTH "JANUARY")
			)
			((= MO "02")
				(setq MONTH "FEBRUARY")
			)
			((= MO "03")
				(setq MONTH "MARCH")
			)
			((= MO "04")
				(setq MONTH "APRIL")
			)
			((= MO "05")
				(setq MONTH "MAY")
			)
			((= MO "06")
				(setq MONTH "JUNE")
			)
			((= MO "07")
				(setq MONTH "JULY")
			)
			((= MO "08")
				(setq MONTH "AUGUST")
			)
			((= MO "09")
				(setq MONTH "SEPTEMBER")
			)
			((= MO "10")
				(setq MONTH "OCTOBER")
			)
			((= MO "11")
				(setq MONTH "NOVEMBER")
			)
			((= MO "12")
				(setq MONTH "DECEMBER")
			)
		); end MO cond
	(setq DATE-STRING
		(strcat
			MONTH
			" "
			(substr DATST 7 2)","   ; day
			" "
			(substr DATST 1 4)      ; year
			" "
			NHRS                    ; hours
			":"
			(substr DATST 12 2)     ; minutes
			":"
			(substr DATST 14 2)     ; seconds
			" "
			XTR                     ; am/pm
		)
	)
	(setq INSP (getpoint "\nSelect text insertion point"))
	(command "text" "J" "R" INSP "0" DATE-STRING)
	(terpri)
); end datetime
