;;==========================================================================

;; This code is use to break multiple lines at intersection with another lines

;; with an option of just break a line or leave a gap in between.

;;==========================================================================

(defun C:MBREAK (/ P1 P2 P3 P4 P5 P6 P7 SS1 SS2 INDEX1 INDEX2 ENAME1 ENAME2 ELIST1 ELIST2 BRKLEN OFFDISDAT)

  (setq OSMDE (getvar "osmode")) 

  (setvar "osmode" 0)

  (setq PCKBX (getvar "pickbox"))

  (setq OFFDISDAT (getstring (strcat "\nSpecify offset distance from intersections: <" OFFDISNUM ">: ")))

  (if (/= OFFDISDAT "")

    (setq OFFDISNUM OFFDISDAT)

  )

  (setq BRKLEN (atof OFFDISNUM))

  (prompt "\nSelect breaking lines ...")

  (while (or (not SS1) (= 0 (sslength SS1)))

    (setq SS1 (ssget '((0 . "LINE"))))

  )

  (setq INDEX1 0)

  (repeat (sslength SS1)

    (progn

      (setq ENAME1 (ssname SS1 INDEX1))

      (setq ELIST1 (entget ENAME1))

      (setq P1 (cdr (assoc 10 ELIST1)))

      (setq P2 (cdr (assoc 11 ELIST1)))

      (prompt "\nSelect lines that intersect …")

      (while (or (not SS2) (= 0 (sslength SS2)))

        (setq SS2 (ssget '((0 . "LINE"))))

      )

      (setq INDEX2 0)

      (repeat (sslength SS2)

        (progn

          (setq ENAME2 (ssname SS2 INDEX2))

          (setq ELIST2 (entget ENAME2))

          (setq P3 (cdr (assoc 10 ELIST2)))

          (setq P4 (cdr (assoc 11 ELIST2)))

          (setq P5 (inters P1 P2 P3 P4 onseg))

          (setq P6 (polar P5 (angle P3 P4) BRKLEN))

          (setq P7 (polar P5 (angle P4 P3) BRKLEN))

          (command "draworder" ENAME2 "" "F")

          (if (= BRKLEN 0)

            (command "break" P5 "@")

            (command "break" P6 "none" P7)

          )

        )

        (setq INDEX2 (1+ INDEX2))

      )

    )

    (setq INDEX1 (1+ INDEX1))

  )

  (setvar "osmode" OSMDE)

  (princ (strcat "\n" (itoa (sslength SS2)) " Lines have been broken"))

  (princ)

)

(setq OFFDISNUM "0")

;;==========================================================================