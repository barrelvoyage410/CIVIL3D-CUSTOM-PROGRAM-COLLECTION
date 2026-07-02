;TIP1259.LSP:  PIPE.LSP  Storm Pipe   (c),1996  

(defun DTR (A) (* pi (/ A 180.0)))
(defun C:PIPE (/ PNT1 PNT2 D A ET HD L1 L2 L3 L4 OTM OSM)
   (setvar "CMDECHO" 0)
   (setq OTM (getvar "ORTHOMODE"))
   (setvar "ORTHOMODE" 0)
   (setq OSM (getvar "osmode"))
   (setvar "OSMODE" 0)
   (setq
      PNT1 (getpoint "\nPick Start of Pipe: ")
   )
   (setq PNT2 (getpoint PNT1 "\nPick End of Pipe: "))
   (setq
      D (getdist "\nEnter Pipe Width: ")
   ) ;In Engineering Units (ie. 2.5 etc.)
   (setq HD (/ D 2))
   (setq A (angle PNT1 PNT2))
   (command "layer" "m" "P-U-SD-HATCH" "c" "34" "" "")
   (command "Pline" PNT1 PNT2 "")
   (setq ET (entlast))
   (setq L1 (polar PNT1 (- A (DTR 90)) HD))
   (setq
      L2 (polar PNT2 (- A (DTR 90)) HD)
   )
   (setq L3 (polar PNT1 (+ A (DTR 90)) HD))
   (setq
      L4 (polar PNT2 (+ A (DTR 90)) HD)
   )
   (command "layer" "m" "P-U-SD" "c" "190" "" "")
   (command "line" L1 L2 "")
   (command "line" L3 L4 "")
   (command
      "Change" ET "" "P" "LT" "dashed" "" ""
   ) ;change linetype to suit
   (command "Pedit" ET "W" D "" "")
   (setvar "ORTHOMODE" OTM)
   (setvar "OSMODE" OSM)
   (command "Redraw")
) ;end pipe.lsp
