;;;                 GO.LSP created 09:56:30 Wednesday, 01-22-1997
;;;                    by Mark Evinger, CIS ID 73162,1676
;;;                 Copyright (c) 1997, Cad Elf Inc. *<:)-|-<
;;;                     email:  Acad_Guru@compuserve.com
;;;
;;; Created to change to CGL jobs on our cadd server.  Requires DOSLIB 3.0
;;; from Robert McNeel & Assoc. to work  correctly.   Provides  quick  and
;;; painless  way   to  set  current  server  directory  to  specific  job
;;; avoid having to drill through hundreds of job numbers!


(defun C:GO ( / IN_PATH FULL_PATH ERRTEST )
  ;Load DosLib
  (cond
    ((not (null DOS_ABOUT))
      T
    )
    ((= (atoi (substr (getvar "acadver") 1 2)) 12)
      (xload "doslib12")
    )
    ((= (atoi (substr (getvar "acadver") 1 2)) 13)
      (xload "doslib13" nil)
    );end cond
    ((= (atoi (substr (getvar "acadver") 1 2)) 14)
      (arxload "doslib14" nil)
    );end cond
    (T
      (prompt "DosLib AutoCad r12 - 14")
    )
  );end cond
  (setq IN_PATH (strcat "F:\\CADD\\" (getstring "\nEnter JOB NUMBER and subdirectory: ")))
  (setq FULL_PATH (dos_splitpath IN_PATH))
  (dos_drive (car FULL_PATH))
  (setq ERRTEST (dos_chdir IN_PATH))
  (if (= ERRTEST nil)
    (prompt (strcat "\n"IN_PATH" Does NOT exist on the server."))
    (prompt (strcat "\nDirectory Changed to "IN_PATH))
  );end if
  (princ)
);end defun c:go
