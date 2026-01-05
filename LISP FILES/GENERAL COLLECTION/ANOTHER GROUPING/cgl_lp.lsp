;;;-------------------------------------------------------------------------
;;;Layer Prefix / suffix
;;;By selecting an entity(ies) with this routine, it will extract
;;;the layer that it is on and the information about that layer.
;;;With the prefix, which is entered at the beginning of the routine,
;;;A new layer will be created and the entity will be put on the new layer.
;;;Free for whomever so wishes to use it.  Works best for moving entity(ies)
;;;onto layers with the prefix of DEMO or EXIST or any other name.
;;;
;;;Written by Jamie Myers with the much appreciated help of
;;;Luis Esquivel, Ken Alexander and Juerg Menzi.  Thanks guys.
;;;

;;; revised 5:54 PM 6/16/2004 by Mark Evinger to allow for a suffix too.  The "_" is no
;;; longer hard-coded into the routine, you MUST add in your own delimeters.  for example
;;;  prefix = "NEW_"    suffix = "--EX"


(defun c:CGL_LP (/ Codes Count LayerColor LayerPrefix LayerLineType LayName NewLayName Objects SS
                 LayerSuffix)
  ; I ditched the (strcase) to keep case-sensitive layer names.  Works in r2000+, since there was no layer name length check.
  ;(setq LayerPrefix (strcase (getstring "What Prefix do you want the layers to have?:  ")))
  (setq LayerPrefix (getstring "What Prefix do you want the layers to have?:  "))
  (setq LayerSuffix (getstring "What Suffix do you want the layers to have?:  "))
 ;select entity(ies)
  (princ "\nSelect all layers to append:  \n")
  (setq ss (ssget)) ;get selection set
  (setq Count 0) ;sets Count at zero
  (if ss ;makes sure something is in ss before going through the routine
    (progn
      (repeat (sslength ss) ;repeats equal to the number of entities selected
        (setq Objects (ssname ss Count)) ;retreive the name of an entity in your ssget..
                                         ;where Count go from 0 (the first entity)
        (setq Codes (entget Objects))    ;retreive the dxf Codes of the entity
                                         ;retreive the layer name.....
        (setq LayName (cdr (assoc 8 Codes)))
 ;to solve the problem of "bylayer" returning nil for the color
        (if (= (cdr (assoc 62 Codes)) nil)
          (setq LayerColor (cdr (assoc 62 (tblsearch "layer" LayName))))
          (setq LayerColor (cdr (assoc 62 Codes)))
        ) ;if
 ;to solve the problem of "bylayer" returning nil for the linetype
        (if (= (cdr (assoc 6 Codes)) nil)
          (setq LayerLineType
                 (cdr (assoc 6 (tblsearch "layer" LayName)))
          ) ;setq
          (setq LayerLineTYPE (cdr (assoc 6 Codes)))
        ) ;if
 ;checks to make sure that the prefixed layer does not already exist
        (if (wcmatch LayName (strcat LayerPrefix "*" LayerSuffix))
          (setq NewLayName LayName)
          (setq NewLayName (strcat LayerPrefix LayName LayerSuffix))
        ) ;if
 ;Check for new layer name
        (if (= (tblsearch "layer" NewLayName) NIL)          ;Set properties of new layer
          (command "-layer" "n" NewLayName "c" LayerColor NewLayName "l" LayerLineType NewLayName "") ;_ end of command
        ) ;if
        (command "CHANGE" Objects "" "PROP" "LAYER" NewLayName "") ;puts entity on prefixed layer
        (setq Count (+ 1 Count)) ;steps up Count
      ) ;repeat
    ) ;progn
  ) ;if
) ;defun
;;;end
;;;
;;;
;;;-- 
;;;Rabbit @ home, down in the hole
;;;I had self taught schooling,
;;;I was in a public school
;;;
