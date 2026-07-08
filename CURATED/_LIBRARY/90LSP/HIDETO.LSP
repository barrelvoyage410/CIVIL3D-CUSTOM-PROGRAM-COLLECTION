;    *** PROGRAM TO HIDE LAYERS TO A CORRESPONDING ***
;    *** LAYER WITH H- AND THE ITEM'S LAYER NAME   ***
;
;        Ed Galicki
;        San Diego, Ca.

(defun C:HIDETO (/ OLDLAY HLAY LAYR A E C1)
   (SETVAR "CMDECHO" 0)
   ;(SETQ OLDLAY (GETVAR "CLAYER"))
   ;(PROMPT "\nWILL HIDE TO H- & LAYER OF SELECTED ENTITY :  \n")
   ;*****GET NAME OF LAYER TO SELECT
   (SETQ A (ENTSEL "\nSELECT ITEM TO PLACE ON H-LAYERNAME : \n"))
   (SETQ E (ENTGET(CAR A)))
   (SETQ C1 (ASSOC 8 E))
   (SETQ LAYR (CDR C1))

         (IF
         (/=  (SUBSTR LAYR 1 2) "H-")
         (SETQ HLAY (STRCAT "H-" LAYR))
         (SETQ HLAY LAYR)
         )

         (SETQ LAEX (TBLSEARCH "LAYER" HLAY))
         (IF
             (NOT LAEX)
             (COMMAND "LAYER" "N" HLAY "")
         )

         (COMMAND "CHANGE" A "" "P" "LA" HLAY "")

         (COMMAND "LAYER" "F" HLAY "")


    (SETVAR "CMDECHO" 1)
    (PROMPT (STRCAT "\n" "\nPLACING ITEM ON : " HLAY "\n"))
    (PRIN1)
)  ;HIDETO
