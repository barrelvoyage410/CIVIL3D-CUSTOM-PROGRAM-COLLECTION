;;; revised 03-20-2007 to check for serial number         ;;;ME
;;; revised 11/13/2007 10:45 AM to modify Mtext as well.  ;;;ME

(defun c:addtext ( / PRE SUF SSET N INDEX EL C OLD NEW D B2)
  (if (cgl_check_serial)
    (progn
     (setq COUNT 0)
     (setq PRE (getstring 1 "\nEnter text PREfix :"))
     (setq SUF (getstring 1 "\nEnter text SUFfix :"))
     (princ "\nSelect text to change :")
     (if (setq SSET (ssget))
          (progn
               (setq N (sslength SSET))
               (setq INDEX 0)
               (repeat N
                    (setq EL (entget (ssname SSET INDEX)))
                    (setq INDEX (1+ INDEX))
                    (if (wcmatch (cdr (assoc 0 EL)) "*TEXT")
                         (progn
                              (setq C (assoc 1 EL)
                                   OLD (cdr c)
                                   NEW (strcat PRE OLD SUF)
                                   D (cons (car C) NEW)
                                   B2 (subst D C EL)
                                   COUNT (1+ COUNT)
                              );end setq
                              (entmod B2)
                         );end progn
                         nil
                    );end if
               );end repeat
          );end progn
          (princ "\nNo entities were selected!")
     );end if
     (princ (strcat "\n" (itoa COUNT)
     " TEXT entities were processed."))
     (princ)
    );end checker progn
    (alert "  Not running on an authorized CGL install.\n\nCopyright Cowhey Gudmundson Leder, Ltd.")
    );end if
    (princ)
);end addtext.lsp
