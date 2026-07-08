(defun c:sper(/ spSet ptLst Dr Ang sCurve oldEcho
         oldOsm dataLst fPt curLen Ans)
 
  (vl-load-com)

    (defun *error* (msg)
    (setvar "CMDECHO" oldEcho)
    (setvar "OSMODE" oldOsm)
    (princ)
    ); end of *error*

    (setq oldEcho(getvar "CMDECHO")
          oldOsm(getvar "OSMODE")
          ); end setq
  (setvar "CMDECHO" 0)
  (princ "\n<<< Select Spline >>>")
  (if
    (setq spSet
      (ssget "_:S" '((0 . "SPLINE"))))
    (progn
      (setq  ptLst(mapcar 'cdr
         (vl-remove-if
              '(lambda(x)(/= (car x) 11))
                (entget(ssname spSet 0))))
       sCurve(vlax-ename->vla-object
          (ssname spSet 0))
       dataLst '()
       curLen T
       ); end setq
      (setvar "OSMODE" 0)
      (foreach pt ptLst
   (setq Dr(vlax-curve-getFirstDeriv sCurve
              (vlax-curve-getParamAtPoint sCurve pt))
         Ang(- pi(atan(/(car dr)(cadr dr))))
    dataLst(append dataLst
         (list(list(trans pt 0 1) ang)))
         ); end setq
   ); end foreach
      (while(and curLen dataLst)
   (setq curLen
          (getreal "\n>>> Specify perpendicular length: ")
         fPt(caar dataLst)
         Ang(cadar dataLst)
         ); end setq
   (if curLen
   (command "_.line" fPt
       (trans(polar fPt Ang curLen)0 1)"")
     ); end if
   (initget "Yes No")
   (setq Ans(getkword "\n>>> Mirror line? [Yes/No] <No>: "))
   (if(null Ans)(setq Ans "No"))
   (if(= Ans "Yes")
     (progn
       (command "_.erase" (entlast) "")
       (command "_.line" fPt
       (trans(polar fPt(- Ang Pi)curLen)0 1)"")
       ); end progn
     ); end if
   (setq dataLst(cdr dataLst))
   (if(not dataLst)
     (princ "\n*** End of Spline. Quit. ***")
     ); end if
   ); end while
      ); end progn
    ); end if
    (setvar "CMDECHO" oldEcho)
    (setvar "OSMODE" oldOsm)
  (princ)
  ); end of c:sper