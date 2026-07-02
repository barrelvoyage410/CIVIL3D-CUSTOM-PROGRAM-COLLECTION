;;; Revised by Mark Evinger <Mark.Evinger@cgl-ltd.com> to fix the dang
;;; routine so it would work at all.  7-31-2001  
;;; Load up using standard autolisp  (load "digiair")
;;; Call from command prompt same way      Command: Digiair
;;; Routine prompts for filename and will use .jgw file to insert and scale to state plane coords (NAD83)
;----------------------------------------
;Engineering Mapping Solutions, Inc. 1998
;----------------------------------------
;
; Strip path from filename
;
(defun strippath(fullname)
 (setq cntr (strlen fullname))
 (setq newname "")
 (setq loop T)
 (while loop
 (setq ch (substr fullname cntr 1))
  (if (= ch (chr 92))
   (progn
    (setq newname (substr fullname (1+ cntr)))
    (setq loop nil)
   );progn
  );if
 (setq cntr (- cntr 1))
 (if (= ch "")(setq loop nil))
 );while
 newname
);defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
(defun c:DigiAir()
   (setq oldosmode (getvar "osmode"))
   (command "osnap" "")
    (setq spath (getvar "dwgprefix"))
    (setq tfwfile (getfiled "Select World File" spath "JGW;TFW;PGW" 0))
    (setq filet (open tfwfile "r"))
    (setq px1 (atof (read-line filet)))
    (setq px2 (atof (read-line filet)))
    (setq py1 (atof (read-line filet)))
    (setq py2 (atof (read-line filet)))
    (setq llx (atof (read-line filet)))
    (setq lly (atof (read-line filet)))
    (close filet)
    ;(princ (strcat tfwfile "-" (rtos px1 2 6)))
    (setq pixsiz (sqrt (+ (* px1 px1)(* px2 px2))))
    (setq wldExt (strcase (substr tfwfile (- (strlen tfwfile) 3))))

        (cond
      ((= wldExt ".JGW") (setq wldExt ".JPG"))
      ((= wldExt ".TFW") (setq wldExt ".TIF"))
      ((= wldExt ".PGW") (setq wldExt ".PNG"))
    )
    (setq tiffile (strcat (substr tfwfile 1 (- (strlen tfwfile) 4)) wldExt))
    (setq ifile (substr (strippath tfwfile) 1 (- (strlen (strippath tfwfile)) 4)))
    (command "-image" "d" ifile)
    (setq ipoint '(-10000 -10000 0.0))
    (command "-image" "a" tiffile ipoint "1" "0")
    (setq headlist (cdr (assoc 13 (entget (entlast)))))
    (if (setq ssi (ssget "x" '((0 . "IMAGE")))) (command "-image" "d" ifile))

    (setq ipoint (list (- llx (/ pixsiz 2)) (- lly (* pixsiz (+ (- (nth 1 headlist) 1) 0.5))) 0.0 ))
    (setq xscale (* px1 (nth 0 headlist)))
    
    ; the following is only true with images with resoultion info in the header
    ;;;(command "-image" "a" tiffile ipoint "u" "u" xscale "0") ;;What are the unloads for??
    ; otherwise use
    (command "-image" "a" tiffile ipoint xscale "0")
    
 
   (setq ans (strcase (getstring "\nSend Image to Back Y/<N> ? ")))
   (if (= ans "Y")(command "draworder" "l" "" "b"))
  (setvar "osmode" oldosmode)
  (princ)
);defun
