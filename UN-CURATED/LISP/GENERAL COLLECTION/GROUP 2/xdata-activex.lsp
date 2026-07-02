;;;posted by Bobby C. Jones
;;;Active X method to retrieve Xdata
;;;Retrieve XData and convert to a list
;;;Arguments - ActiveX object and appID of registered xdata app
;;;Retval - eed list
(defun vla:GetXData (vlaObj AppID / xType XData)
  (vla-getxdata vlaObj AppID 'xType 'xData)
  (mapcar '(lambda (x y)
             (cons x
                   (if (/= x 1010)
                     (vlax-variant-value y)
                     (vlax-safearray->list (vlax-variant-value y))
                   ) ;_ end of if
             ) ;_ end of cons
           ) ;_ end of lambda
          (vlax-safearray->list xType)
          (vlax-safearray->list xData)
  ) ;_ end of mapcar
) ;_ end of defun

;;;Attach XData to an object
;;;Arguments - ActiveX object and an eed list
;;;in the same format that vla:GetXData returns
(defun vla:PutXData (vlaObj XData)
  (foreach pnt (massoc XData 1010)
    (setq XData
           (subst (cons 1010
                        (vlax-variant-value
                          (vlax-3d-point (cdr pnt))
                        ) ;_ end of vlax-variant-value
                  ) ;_ end of cons
                  pnt
                  XData
           ) ;_ end of subst
    ) ;_ end of setq
  ) ;_ end of foreach
  (setq XData (buildFilter XData))
  (vla-setXData vlaObj (car XData) (cadr XData))
) ;_ end of defun

;;;Frank Oquendo www.acadx.com
(defun buildFilter (filter)
  (mapcar '(lambda (lst typ)
             (vlax-make-variant
               (vlax-safearray-fill
                 (vlax-make-safearray
                   typ
                   (cons 0
                         (1- (length lst))
                   ) ;_ end of cons
                 ) ;_ end of vlax-make-safearray
                 lst
               ) ;_ end of vlax-safearray-fill
             ) ;_ end of vlax-make-variant
           ) ;_ end of lambda
          (list (mapcar 'car filter) (mapcar 'cdr filter))
          (list vlax-vbInteger vlax-vbVariant)
  ) ;_ end of mapcar
) ;_ end of defun
;--
;Bobby C. Jones
;p.s. - notice who wrote the buildfilter function?  kinda ironic isn't it?
