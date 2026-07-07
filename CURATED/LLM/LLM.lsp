```autolisp
;; =============================================================================
;; LLM.lsp - Length Lister & Measurement Toolkit for AutoCAD
;;
;; Developed by Timmay!
;; Version: 1.79      Date: August 04, 2025
;;
;; DESCRIPTION:
;; This utility computes the total length of selected objects and presents the
;; result in a flexible, user-configurable format. Features include:
;;
;;   • Rounding modes (Up, Down, Nearest)
;;   • Multi-unit outputs (inches, feet, meters, etc.)
;;   • Locale-aware decimal formatting (US/EU)
;;   • Optional label copying to clipboard
;;   • Optional clipboard copying toggle (default: ON)
;;   • Popup alert or quiet command-line display
;;   • Support for predefined output modes (P1-P9)
;;   • Pasting results to the clipboard
;;   • Expanded display for mode 8 with breakdowns
;;
;; ORIGINS & CREDITS:
;; This program draws inspiration and foundational logic from the following:
;;
;;   • TLEN.LSP (Total Length of Selected Objects)
;;     © 1998 Tee Square Graphics
;;     Retrieved from archived source:
;;     http://web.archive.org/web/20201112011622/http://www.turvill.com/t2/free_stuff/tlen.lsp
;;     Provided the original measurement and distance aggregation structure.
;;
;;   • ROT Function - by Lee Mac
;;     https://lee-mac.com
;;     A robust string rotation utility used in earlier formatting logic.
;;     While not present in the current version, it influenced early iterations
;;     of label alignment and string manipulation.
;;
;;   • LL_LengthLister.lsp - Internal prototype by Tim
;;     Served as the foundation for formatting logic, clipboard integration,
;;     and the modular structure that evolved into the current LLM system.
;;
;; All current rounding logic, formatting features, command orchestration, and UI
;; options have been extensively rewritten and expanded for modularity, clarity,
;; and enhanced configurability.
;; =============================================================================


;; GLOBAL SESSION DEFAULTS - Only runs once per LSP load
;; SESSION INITIALIZER
(defun LLM-SessionDefaults ()
  (if (not (boundp '*llm-copy-label*))      (setq *llm-copy-label* NIL))
  (if (not (boundp '*llm-copy-unit*))       (setq *llm-copy-unit* T))
  (if (not (boundp '*llm-clipboard-copy*))  (setq *llm-clipboard-copy* T)) ; New: Default clipboard copy enabled
  (if (not (boundp '*llm-locale*))          (setq *llm-locale* "US"))
  (if (not (boundp '*llm-alert-enabled*))   (setq *llm-alert-enabled* T))
  (if (not (boundp '*llm-output-mode*))     (setq *llm-output-mode* "P8"))
  (if (not (boundp '*llm-rounding-mode*))   (setq *llm-rounding-mode* "UP")) ; Force UP to override legacy values
  (if (not (boundp '*llm-foot-symbol*))     (setq *llm-foot-symbol* "ft"))
  (if (not (boundp '*llm-space-enabled*))   (setq *llm-space-enabled* NIL))
  (if (not (boundp '*llm-length-value*))    (setq *llm-length-value* 0.0))
  (if (not (boundp '*llm-length-unit*))     (setq *llm-length-unit* "ft"))
  (if (not (boundp '*llm-expanded-display*))(setq *llm-expanded-display* NIL))
)

(LLM-SessionDefaults)


(defun vl-string-split (str sep / res pos part)
  (setq res '())
  (while (setq pos (vl-string-search sep str))
    (setq part (vl-string-trim " " (substr str 1 pos)))
    (setq res (append res (list part)))
    (setq str (substr str (+ pos (strlen sep) 1))))
  (if (> (strlen str) 0)
    (progn
      (setq res (append res (list (vl-string-trim " " str))))))
  res)


(defun pad-right (val width / gap pad-source)
  ;; Pads a string on the left so it's right-aligned to a fixed width
  ;; - val: string value to align
  ;; - width: total output column width
  ;; Returns: left-padded string using space padding

  ;; Enough padding to safely cover up to 100-character columns
  (setq pad-source "                                                                                                    ") ; 100 spaces
  (setq gap (max 0 (- width (strlen val))))
  (strcat (substr pad-source 1 gap) val))


(defun llm-pad-right (txt width)
  ;; Pads the string on the right to a fixed width for left-aligned column blocks
  (strcat txt (substr "                                                                                                    " 1 (max 0 (- width (strlen txt))))))


(defun llm-format-aligned-alert (label val)
  ;; Restores tab-based alignment to match previous column layout
  ;; Pads label to 17 characters, adds tab, then appends result value
  ;; Returns (cons formattedDisplay rawValue)

  (cons
    (strcat (substr (strcat label "                 ") 1 17) "\t" val)
    val))


(defun llm-format-expanded-alert (label geom user total / a b c d out)
  (setq a (llm-pad-right label 24)
        b (llm-pad-right geom  24)
        c (llm-pad-right user  24)
        d (llm-pad-right total 24)
        out (strcat a b c d))
  (cons out (strcat label ": " total)))


(defun group-thousands (numStr / main dec sign out cnt split)
  ;; Adds comma separators for thousands
  ;; - Handles optional decimals (e.g., "1234567.89" → "1,234,567.89")
  ;; - Preserves negative sign if present

  ;; Detect sign and strip
  (setq sign (if (= (substr numStr 1 1) "-") "-" ""))
  (setq numStr (if (= sign "-") (substr numStr 2) numStr))

  ;; Split integer and decimal parts
  (setq split (vl-string-split numStr "."))
  (setq main (car split)
        dec  (cadr split))

  ;; Build grouped integer portion
  (setq out "" cnt 0)
  (repeat (strlen main)
    (setq out (strcat (substr main (- (strlen main) cnt) 1) out)
          cnt (1+ cnt))
    (if (and (/= cnt (strlen main)) (= 0 (rem cnt 3)))
      (setq out (strcat "," out))))

  ;; Recombine full value
  (strcat sign out (if dec (strcat "." dec) "")))


;;SETTINGS
(defun c:LLM-S ()
  ;; Valid entries
  (setq valid-entries
    (mapcar 'strcase
      '("1" "2" "3" "4" "5" "6" "7" "8"
        "P1" "P2" "P3" "P4" "P5" "P6" "P7" "P8" "P9"
        "L" "F" "A" "S" "'" "R" "C" "E" "T")))

  ;; Main loop
  (setq done nil)
  (while (not done)
    (princ "\n--- Current LLM Settings ---")
    (princ (strcat "\n Output Mode(s)     : " *llm-output-mode*))
    (princ (strcat "\n Rounding Mode      : " *llm-rounding-mode*))
    (princ (strcat "\n Clipboard Labels   : "
      (cond
        ((and *llm-copy-label* *llm-copy-unit*)       "LABEL ON, UNIT ON")
        ((and (not *llm-copy-label*) *llm-copy-unit*) "LABEL OFF, UNIT ON")
        ((and *llm-copy-label* (not *llm-copy-unit*)) "LABEL ON, UNIT OFF")
        (T "LABEL OFF, UNIT OFF"))))
    (princ (strcat "\n Locale Format      : " *llm-locale*))
    (princ (strcat "\n Alert Box          : " (if *llm-alert-enabled* "ON" "OFF")))
    (princ (strcat "\n Foot Unit Symbol   : " *llm-foot-symbol*))
    (princ (strcat "\n Unit Spacing       : " (if *llm-space-enabled* "SPACE ON" "SPACE OFF")))
    (princ (strcat "\n Add-on Length      : " (rtos *llm-length-value* 2 4) " " *llm-length-unit*))
    (princ (strcat "\n Expanded Display   : " (if *llm-expanded-display* "ON" "OFF")))

    ;; Menu
    (princ "\n\n Units:")
    (foreach msg '("1 = Inches" "2 = Centimeters" "3 = Feet (decimal)" "4 = Feet-Inches"
                   "5 = Meters" "6 = Yards" "7 = Kilometers" "8 = All Units (1-7)")
      (princ (strcat "\n   " msg)))

    (princ "\n\n Predefined Formats:")
    (foreach fmt '("-P1 = FEET-INCHES [ METERS ]"
                   "-P2 = [ FEET / METERS ]"
                   "-P3 = [ FEET : METERS ]"
                   "-P4 = FEET / METERS"
                   "-P5 = FEET : METERS"
                   "-P6 = [ METERS / FEET ]"
                   "-P7 = [ METERS : FEET ]"
                   "-P8 = METERS / FEET"
                   "-P9 = METERS : FEET"
                   " Use commas for multiple (e.g., 1,4,7)")
      (princ (strcat "\n   " fmt)))

    (princ "\n\n Toggles:")
    (foreach tg '("R = Toggle Rounding Mode (Up/Down/Nearest)"
                  "C = Toggle Clipboard Data Copy"
                  "L = Cycle Clipboard Label States"
                  "F = Toggle Locale Format"
                  "A = Toggle Alert Box"
                  "S = Toggle Space Between Unit and Result"
                  "' = Toggle Foot Unit Symbol"
                  "X = Toggle Expanded Result Report for Mode 8")
      (princ (strcat "\n   " tg)))

    (princ "\n\n Length Settings:")
    (foreach ls '("E = Enter Length Value"
                  "T = Select Length Unit Type (1-7)")
      (princ (strcat "\n   " ls)))

    (princ "\n\n <Enter> or ESCAPE = Exit")

    ;; Input
    (setq ch (strcase (getstring "\nEnter choice: ")))

    (cond
      ((or (= ch "") (= ch "ESCAPE"))
       (setq done T)
       (princ "\nLLM-S exited."))

      ((not (member ch valid-entries))
       (setq done T)
       (princ "\nLLM-S exited."))

      ((= ch "R")
       (setq *llm-rounding-mode*
             (cond
               ((= *llm-rounding-mode* "UP") "DOWN")
               ((= *llm-rounding-mode* "DOWN") "NEAREST")
               ((= *llm-rounding-mode* "NEAREST") "UP")
               (T "UP")))
       (princ (strcat "\nRounding mode toggled to: " *llm-rounding-mode*)))

      ((= ch "C")
       (setq *llm-clipboard-copy* (not *llm-clipboard-copy*))
       (princ (strcat "\nClipboard Data Copy: " (if *llm-clipboard-copy* "ON" "OFF"))))

      ((= ch "L")
       (cond
         ((and *llm-copy-label* *llm-copy-unit*)       (setq *llm-copy-label* NIL *llm-copy-unit* T))
         ((and (not *llm-copy-label*) *llm-copy-unit*) (setq *llm-copy-label* T   *llm-copy-unit* NIL))
         ((and *llm-copy-label* (not *llm-copy-unit*)) (setq *llm-copy-label* NIL *llm-copy-unit* NIL))
         (T (setq *llm-copy-label* T *llm-copy-unit* T)))
       (princ "\nClipboard label configuration updated."))

      ((= ch "F")
       (setq *llm-locale* (if (equal *llm-locale* "US") "EU" "US"))
       (princ (strcat "\nLocale set to: " *llm-locale*)))

      ((= ch "A")
       (setq *llm-alert-enabled* (not *llm-alert-enabled*))
       (princ (strcat "\nAlert box mode: " (if *llm-alert-enabled* "ON" "OFF"))))

      ((= ch "S")
       (setq *llm-space-enabled* (not *llm-space-enabled*))
       (princ (strcat "\nUnit spacing is now " (if *llm-space-enabled* "ON (space)" "OFF (no space)"))))

      ((= ch "'")
       (setq *llm-foot-symbol* (if (equal *llm-foot-symbol* "ft") "'" "ft"))
       (princ (strcat "\nFoot unit symbol is now set to: " *llm-foot-symbol*)))

      ((= ch "X")
       (setq *llm-expanded-display* (not *llm-expanded-display*))
       (princ (strcat "\nExpanded Result Report for Mode 8: " (if *llm-expanded-display* "ON" "OFF"))))

      ((= ch "E")
       (initget 128)
       (setq tempVal (getreal "\nEnter length to add (0 disables add-on): "))
       (if (and tempVal (>= tempVal 0.0))
         (progn
           (setq *llm-length-value* tempVal)
           (princ (strcat "\nLength value set to: " (rtos *llm-length-value* 2 4))))
         (princ "\nInvalid entry — must be 0 or a positive number.")))

      ((= ch "T")
       (prompt "\nSelect length unit for add-on:")
       (foreach u '("1 = Inches" "2 = Centimeters" "3 = Feet (decimal)" "4 = Feet-Inches"
                    "5 = Meters" "6 = Yards" "7 = Kilometers")
         (princ (strcat "\n   " u)))
       (setq mode (getstring "\nEnter unit number (1-7): "))
       (cond
         ((= mode "1") (setq *llm-length-unit* "in"))
         ((= mode "2") (setq *llm-length-unit* "cm"))
         ((= mode "3") (setq *llm-length-unit* "ft"))
         ((= mode "4") (setq *llm-length-unit* "ft"))
         ((= mode "5") (setq *llm-length-unit* "m"))
         ((= mode "6") (setq *llm-length-unit* "yd"))
         ((= mode "7") (setq *llm-length-unit* "km"))
         (T (prompt "\nInvalid unit entry - unit unchanged.")))
       (princ (strcat "\nLength unit set to: " *llm-length-unit*)))

      (T
       ;; Set output mode(s)
       (setq *llm-output-mode* ch)
       (princ (strcat "\nOutput mode(s) set to: " ch)))))
  (princ))


(defun rtoc (n p / factor n-round s dot int dec out cnt)
  ;; Apply precision-aware rounding based on rounding mode and locale formatting
  (setq factor (expt 10.0 p))
  ;; Round based on user mode
  (setq n-round
    (cond
      ((= *llm-rounding-mode* "UP") ; Round Up to next whole number
       (if (> (- n (fix n)) 1e-8) ; Use small epsilon to handle float precision
         (float (1+ (fix n))) ; Ceiling: increment to next integer
         (float (fix n))))    ; No fractional part: keep as is
      ((= *llm-rounding-mode* "DOWN") (/ (float (fix (* n factor))) factor)) ; Down
      ((= *llm-rounding-mode* "NEAREST") (/ (float (fix (+ (* n factor) 0.5))) factor)) ; Nearest
      (T n))) ; Fallback for legacy or undefined
  ;; Convert to string and parse decimal
  (setq s (rtos n-round 2 p)
        dot (vl-string-position 46 s)) ; 46 = ASCII "."
  (if dot
    (setq int (substr s 1 dot)
          dec (substr s (+ dot 2)))
    (setq int s dec ""))
  ;; Group thousands
  (setq out "" cnt 0)
  (repeat (strlen int)
    (setq out (strcat (substr int (- (strlen int) cnt) 1) out)
          cnt (1+ cnt))
    (if (and (/= cnt (strlen int)) (= 0 (rem cnt 3)))
      (setq out (strcat "," out))))
  ;; Inject decimal based on locale, include non-empty decimal part
  (if (and (/= dec "") (not (member *llm-rounding-mode* '("UP" "DOWN"))))
    (setq out (strcat out (if (eq *llm-locale* "EU") "," ".") dec)))
  out)


;; Precision-aware rtos wrapper: hides ".00" in Up/Down mode for cleaner inch display
;; Converts number to formatted string using rtos,
;; and suppresses trailing ".00" when rounding mode is Up (U) or Down (D).
;; Useful for hiding unnecessary decimal precision — e.g., showing "4" instead of "4.00" inches
;; when formatting feet-inches notation or clipboard values.
(defun llm-rtos-clean (val prec / raw dot int dec)
  ;; Converts number to string, suppresses ".00" if rounding mode is UP or DOWN
  (setq raw (rtos val 2 prec)
        dot (vl-string-position 46 raw)) ; ASCII for "."
  (if dot
    (setq int (substr raw 1 dot)
          dec (substr raw (+ dot 2)))
    (setq int raw dec ""))
  ;; Suppress .00 only if rounding mode is UP or DOWN
  (if (and (member *llm-rounding-mode* '("UP" "DOWN")) (= dec "00"))
    int
    raw))


(defun parse-output-modes (modeStr / cleanModes validModes)
  ;; Expands "8" to full set; splits comma list; filters valid entries

  (setq validModes
    '("1" "2" "3" "4" "5" "6" "7"
      "P1" "P2" "P3" "P4" "P5" "P6" "P7" "P8" "P9"))

  (setq cleanModes
    (if (equal (strcase modeStr) "8")
      '("1" "2" "3" "4" "5" "6" "7")
      (mapcar 'strcase
        (vl-remove-if 'null
          (mapcar '(lambda (x)
                     (setq x (vl-string-trim " " x))
                     (if (member x validModes) x))
                  (vl-string-split modeStr ","))))))

  cleanModes)


;;FUTURE USE TO WORK OUT MULTIPLIER FOR DRAWING 
;; (defun getUnitMultiplier ()
;;  (cond
;;    ((= (getvar 'insunits) 1) 1.0)
;;    ((= (getvar 'insunits) 2) 12.0)
;;    ((= (getvar 'insunits) 4) 0.0393701)
;;    ((= (getvar 'insunits) 5) 0.393701)
;;    ((= (getvar 'insunits) 6) 39.3701)
;;    (T 1.0)))


(defun llm-format-for-clipboard (pair / label raw)
  (setq label (car pair)
        raw   (cdr pair))
  (cond
    ;; Clipboard OFF: just return raw value
    ((not *llm-copy-label*) raw)

    ;; Label ON, Unit OFF: just return label
    ((and *llm-copy-label* (not *llm-copy-unit*)) label)

    ;; Special case: feet-inches format (mode 4), don't append "ft"
    ((and *llm-copy-label* *llm-copy-unit*
          (wcmatch raw "*'*\"")) ; catches formats like 5'-7"
     (strcat label " " raw))     ; no added unit

    ;; Default: label + unitized raw value
    ((and *llm-copy-label* *llm-copy-unit*)
     (strcat label " " raw))

    ;; Fallback
    (T raw)))


(defun copy-to-clipboard (txt / tf fh)
  (if txt
    (progn
      ;; Create temporary file
      (setq tf (vl-filename-mktemp "llm"))
      (setq fh (open tf "w"))
      (write-line txt fh)
      (close fh)

      ;; Pipe file contents to clipboard
      (startapp "cmd.exe" (strcat "/c type \"" tf "\" | clip"))

      ;; Confirm success
      (prompt (strcat "\nCopied to clipboard:\n" txt)))))


(defun LLM-ConvertLengthFrom (source-unit / conv)
  ;; Converts *llm-length-value* from given unit into feet
  (setq conv
    (cond
      ((= source-unit "in") (/ 1.0 12.0))
      ((= source-unit "cm") (/ 1.0 30.48))
      ((= source-unit "m")  (/ 1.0 0.3048))
      ((= source-unit "yd") (/ 1.0 3.0))
      ((= source-unit "km") (/ 1.0 0.0003048))
      ((= source-unit "ft") 1.0)
      (T nil)))
  (if (and conv (/= *llm-length-value* 0.0))
    (* *llm-length-value* conv)
    0.0))


(defun LLM-ConvertLengthTo (target-unit / ft conv)
  ;; Converts *llm-length-value* from *llm-length-unit* into target unit
  ;; Centralized two-way conversion engine

  ;; Normalize inputs
  (setq target-unit (strcase target-unit)
        *llm-length-unit* (strcase *llm-length-unit*))

  ;; Step 1: Convert source value to feet
  (setq ft
    (cond
      ((= *llm-length-unit* "IN") (/ *llm-length-value* 12.0))
      ((= *llm-length-unit* "CM") (/ *llm-length-value* 30.48))
      ((= *llm-length-unit* "M")  (/ *llm-length-value* 0.3048))
      ((= *llm-length-unit* "YD") (* *llm-length-value* 3.0))
      ((= *llm-length-unit* "KM") (* *llm-length-value* 3280.84))
      ((= *llm-length-unit* "FT") *llm-length-value*)
      (T nil)))

  ;; Step 2: Convert feet to target
  (setq conv
    (cond
      ((= target-unit "IN") 12.0)
      ((= target-unit "CM") 30.48)
      ((= target-unit "M")  0.3048)
      ((= target-unit "YD") 0.333333)
      ((= target-unit "KM") 0.0003048)
      ((= target-unit "FT") 1.0)
      (T nil)))

  ;; Final output
  (if (and ft conv)
    (* ft conv)
    0.0))


(defun group-thousands (numStr / main dec sign out cnt split)
  ;; Groups thousands, preserves sign & decimals
  (setq sign (if (= (substr numStr 1 1) "-") "-" ""))
  (setq numStr (if (= sign "-") (substr numStr 2) numStr))
  (setq split (vl-string-split numStr "."))
  (setq main (car split)
        dec  (cadr split))
  (setq out "" cnt 0)
  (repeat (strlen main)
    (setq out (strcat (substr main (- (strlen main) cnt) 1) out)
          cnt (1+ cnt))
    (if (and (/= cnt (strlen main)) (= 0 (rem cnt 3)))
      (setq out (strcat "," out))))
  (strcat sign out (if dec (strcat "." dec) "")))


(defun llm-format-num (val dec) (group-thousands (rtoc val dec)))


(defun LLM-Core (ss copy? suppress / total idx ent typ len
                     rawModes modeList txt txt-clp m line vlaObjResult
                     spaceSep footUnit header
                     geomIn geomCm geomFt geomM geomYd geomKm
                     manualIn manualCm manualFt manualM manualYd manualKm
                     totalIn totalFt totalCm totalM totalYd totalKm
                     ftDecFormatted inPartFormatted metersFormatted
                     fullP1 fullP2 fullP3 fullP4 fullP5 fullP6 fullP7 fullP8 fullP9
                     ftPart inPart geomFormatted manualFormatted totalFormatted addOnPrec addOnFormatted addOnUnit)
  ;; Format settings
  (setq spaceSep (if *llm-space-enabled* " " "")
        footUnit (if (= *llm-foot-symbol* "ft") "'" (strcat spaceSep "ft")))
  ;; Accumulate geometry (in inches)
  (setq total 0.0
        idx (1- (sslength ss)))
  (while (>= idx 0)
    (setq ent (entget (ssname ss idx))
          typ (cdr (assoc 0 ent))
          len
          (cond
            ((= typ "LINE")  (distance (cdr (assoc 10 ent)) (cdr (assoc 11 ent))))
            ((= typ "ARC")
             (* (cdr (assoc 40 ent))
                (abs (- (cdr (assoc 51 ent)) (cdr (assoc 50 ent))))))
            (T
             (setq vlaObjResult
               (vl-catch-all-apply
                 '(lambda () (vla-get-length (vlax-ename->vla-object (ssname ss idx))))))
             (if (numberp vlaObjResult) vlaObjResult 0.0))))
    (setq total (+ total len)
          idx (1- idx)))
  ;; Convert geometry and manual to all target units
  (setq geomIn  total
        geomFt  (/ total 12.0)
        geomCm  (* total 2.54)
        geomM   (/ total 39.3701)
        geomYd  (/ total 36.0)
        geomKm  (/ total 39370.1)
        manualIn (LLM-ConvertLengthTo "IN")
        manualFt (LLM-ConvertLengthTo "FT")
        manualCm (LLM-ConvertLengthTo "CM")
        manualM  (LLM-ConvertLengthTo "M")
        manualYd (LLM-ConvertLengthTo "YD")
        manualKm (LLM-ConvertLengthTo "KM")
        totalIn (+ geomIn manualIn)
        totalFt (+ geomFt manualFt)
        totalCm (+ geomCm manualCm)
        totalM  (+ geomM manualM)
        totalYd (+ geomYd manualYd)
        totalKm (+ geomKm manualKm))
  ;; Format feet/inches split
  (setq ftPart (if (= *llm-rounding-mode* "UP")
                 (if (> (- totalFt (fix totalFt)) 1e-8) (1+ (fix totalFt)) (fix totalFt))
                 (fix totalFt))
        inPart (* (- totalFt ftPart) 12.0)
        ftDecFormatted (rtoc totalFt 2)
        inPartFormatted (if (= *llm-rounding-mode* "UP")
                          (rtoc inPart 0) ; Force whole-number rounding for inches
                          (llm-rtos-clean inPart 2))
        metersFormatted (rtoc totalM 2))
  ;; Composite outputs
  (setq fullP1 (strcat (group-thousands (itoa ftPart)) "'-" inPartFormatted "\" [" metersFormatted spaceSep "m]")
        fullP2 (strcat "[" ftDecFormatted footUnit " / " metersFormatted spaceSep "m]")
        fullP3 (strcat "[" ftDecFormatted footUnit " : " metersFormatted spaceSep "m"])
        fullP4 (strcat ftDecFormatted footUnit " / " metersFormatted spaceSep "m")
        fullP5 (strcat ftDecFormatted footUnit " : " metersFormatted spaceSep "m")
        fullP6 (strcat "[" metersFormatted spaceSep "m / " ftDecFormatted footUnit "]")
        fullP7 (strcat "[" metersFormatted spaceSep "m : " ftDecFormatted footUnit "]")
        fullP8 (strcat metersFormatted spaceSep "m / " ftDecFormatted footUnit)
        fullP9 (strcat metersFormatted spaceSep "m : " ftDecFormatted footUnit))
  ;; Determine precision and unit display for add-on length
  (setq addOnPrec (if (= (strcase *llm-length-unit*) "KM") 3 2)
        addOnFormatted (llm-format-num *llm-length-value* addOnPrec)
        addOnUnit (if (= (strcase *llm-length-unit*) "M") "m" *llm-length-unit*))
  ;; Initialize text buffer with rounding mode and add-on length
  (setq txt (strcat "Total Length Information (Rounding: " *llm-rounding-mode* ", Add-on: " addOnFormatted " " addOnUnit "):\n")
        txt-clp ""
        modeList (parse-output-modes *llm-output-mode*))
  ;; Header for expanded display
  (if (and *llm-expanded-display* (member "8" modeList))
    (setq txt (strcat txt "\n"
      (strcat
        (llm-pad-right "UNITS" 24)
        (llm-pad-right "OBJ GEOM" 24)
        (llm-pad-right "+ADD-ON LENGTH" 24)
        "TOTAL LENGTH\n"))))
  ;; Build output per mode
  (foreach m modeList
    (setq line
      (cond
        ((= m "1")
         (if *llm-expanded-display*
           (progn
             (setq geomFormatted (strcat (llm-format-num geomIn 2) spaceSep "in")
                   manualFormatted (strcat (llm-format-num manualIn 2) spaceSep "in")
                   totalFormatted (strcat (llm-format-num totalIn 2) spaceSep "in"))
             (llm-format-expanded-alert "Inches" geomFormatted manualFormatted totalFormatted))
           (llm-format-aligned-alert "Inches:" (strcat (llm-format-num totalIn 2) spaceSep "in"))))
        ((= m "2")
         (if *llm-expanded-display*
           (progn
             (setq geomFormatted (strcat (llm-format-num geomCm 2) spaceSep "cm")
                   manualFormatted (strcat (llm-format-num manualCm 2) spaceSep "cm")
                   totalFormatted (strcat (llm-format-num totalCm 2) spaceSep "cm"))
             (llm-format-expanded-alert "Centimeters" geomFormatted manualFormatted totalFormatted))
           (llm-format-aligned-alert "Centimeters:" (strcat (llm-format-num totalCm 2) spaceSep "cm"))))
        ((= m "3")
         (if *llm-expanded-display*
           (progn
             (setq geomFormatted (strcat (llm-format-num geomFt 2) footUnit)
                   manualFormatted (strcat (llm-format-num manualFt 2) footUnit)
                   totalFormatted (strcat (llm-format-num totalFt 2) footUnit))
             (llm-format-expanded-alert "Feet (decimal)" geomFormatted manualFormatted totalFormatted))
           (llm-format-aligned-alert "Feet (decimal):" (strcat (llm-format-num totalFt 2) footUnit))))
        ((= m "4")
         (if *llm-expanded-display*
           (progn
             (setq geomFormatted (strcat (group-thousands (itoa (fix geomFt))) "'-" (llm-rtos-clean (* (- geomFt (fix geomFt)) 12.0) 2) "\"")
                   manualFormatted (strcat (group-thousands (itoa (fix manualFt))) "'-" (llm-rtos-clean (* (- manualFt (fix manualFt)) 12.0) 2) "\"")
                   totalFormatted (strcat (group-thousands (itoa ftPart)) "'-" inPartFormatted "\""))
             (llm-format-expanded-alert "Feet-Inches" geomFormatted manualFormatted totalFormatted))
           (llm-format-aligned-alert "Feet-Inches:" (strcat (group-thousands (itoa ftPart)) "'-" inPartFormatted "\""))))
        ((= m "5")
         (if *llm-expanded-display*
           (progn
             (setq geomFormatted (strcat (llm-format-num geomM 2) spaceSep "m")
                   manualFormatted (strcat (llm-format-num manualM 2) spaceSep "m")
                   totalFormatted (strcat (llm-format-num totalM 2) spaceSep "m"))
             (llm-format-expanded-alert "Meters" geomFormatted manualFormatted totalFormatted))
           (llm-format-aligned-alert "Meters:" (strcat (llm-format-num totalM 2) spaceSep "m"))))
        ((= m "6")
         (if *llm-expanded-display*
           (progn
             (setq geomFormatted (strcat (llm-format-num geomYd 2) spaceSep "yd")
                   manualFormatted (strcat (llm-format-num manualYd 2) spaceSep "yd")
                   totalFormatted (strcat (llm-format-num totalYd 2) spaceSep "yd"))
             (llm-format-expanded-alert "Yards" geomFormatted manualFormatted totalFormatted))
           (llm-format-aligned-alert "Yards:" (strcat (llm-format-num totalYd 2) spaceSep "yd"))))
        ((= m "7")
         (if *llm-expanded-display*
           (progn
             (setq geomFormatted (strcat (llm-format-num geomKm 3) spaceSep "km")
                   manualFormatted (strcat (llm-format-num manualKm 3) spaceSep "km")
                   totalFormatted (strcat (llm-format-num totalKm 3) spaceSep "km"))
             (llm-format-expanded-alert "Kilometers" geomFormatted manualFormatted totalFormatted))
           (llm-format-aligned-alert "Kilometers:" (strcat (llm-format-num totalKm 3) spaceSep "km"))))
        ((= m "P1") (llm-format-aligned-alert "Feet-Inches + Meters:" fullP1))
        ((= m "P2") (llm-format-aligned-alert "Feet / Meters:" fullP2))
        ((= m "P3") (llm-format-aligned-alert "Feet : Meters:" fullP3))
        ((= m "P4") (llm-format-aligned-alert "Feet / Meters:" fullP4))
        ((= m "P5") (llm-format-aligned-alert "Feet : Meters:" fullP5))
        ((= m "P6") (llm-format-aligned-alert "Meters / Feet:" fullP6))
        ((= m "P7") (llm-format-aligned-alert "Meters : Feet:" fullP7))
        ((= m "P8") (llm-format-aligned-alert "Meters / Feet:" fullP8))
        ((= m "P9") (llm-format-aligned-alert "Meters : Feet:" fullP9))
        (T nil)))
    (if line
      (progn
        (setq txt (strcat txt "\n" (car line)))
        (if copy?
          (setq txt-clp (strcat txt-clp "\n" (cdr line)))))))
  (if (and copy? (/= txt-clp ""))
    (copy-to-clipboard txt-clp))
  (if (not suppress)
    (if *llm-alert-enabled*
      (alert txt)
      (prompt (strcat "\n" txt)))
    (prompt (strcat "\n" txt)))
  (princ))


(defun c:LLM ()
  (prompt (strcat "\n" (if *llm-clipboard-copy* "LLM Repeat Measure + Clipboard Copy" "LLM Repeat Measure")))
  (setq run T)
  (while run
    (prompt "\nSelect objects to measure.")
    (if *llm-clipboard-copy*
      (prompt "\nTip: After results are shown, you can press Ctrl+V to paste the clipboard contents elsewhere."))
    (setq ss (ssget '((0 . "LINE,ARC,CIRCLE,LWPOLYLINE,POLYLINE,SPLINE,ELLIPSE"))))
    (if ss
      (LLM-Core ss *llm-clipboard-copy* NIL)
      (progn
        (prompt "\nLLM exited - no objects selected.")
        (setq run nil))))
  (princ))


(defun c:LLM-? ()
  (prompt "\n--- LLM Command Reference ---")
  (prompt (strcat "\n LLM       : Measure in repeat mode" (if *llm-clipboard-copy* " with clipboard copy" " (clipboard copy is currently disabled)") " (shows alert or command line)"))
  (prompt "\n LLM-S     : Configure units, rounding, clipboard & locale")
  (prompt "\n LLM-R     : Reset to default parameters")
  (prompt "\n LLM-?     : Show this help summary")
  (if (not *llm-clipboard-copy*)
    (prompt "\nNote: Clipboard copy is currently disabled."))
  (prompt (strcat "\n Current Units: " *llm-output-mode*
                  ", Rounding: " *llm-rounding-mode*
                  ", Clipboard Copy: " (if *llm-clipboard-copy* "ON" "OFF")
                  ", Label Copy: " (if *llm-copy-label* "ON" "OFF")
                  ", Unit Copy: " (if *llm-copy-unit* "ON" "OFF")
                  ", Locale: " *llm-locale*))
  (princ))


(defun c:LLM-R ()
  (LLM-SessionDefaults)
  (princ "\nLLM parameters reset to defaults.")
  (princ))


;; show a little load-time banner
(princ "\nLLM v1.79.lsp loaded.")
(princ "\n  - LLM        : repeat measure + clipboard")
(princ "\n  - LLM-S      : configure units, rounding, clipboard & locale")
(princ "\n  - LLM-R      : reset to default parameters")
(princ "\n  - LLM-?      : show this help summary")

;; final princ with no args returns control to AutoCAD
(princ)
```