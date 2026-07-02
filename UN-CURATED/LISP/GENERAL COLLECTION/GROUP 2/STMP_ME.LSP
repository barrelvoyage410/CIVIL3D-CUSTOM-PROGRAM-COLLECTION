;STMP_ME by Mark Evinger 73162,1676 (Acad_Guru@CompuServe.com)
; Mark Evinger
; MIS Manager
; Mark.Evinger@cgl-ltd.com
; 
;at Cowhey Gudmundson Leder, Ltd. Itasca, Illinois.
; modified 08-13-1993 to work full 24 hrs per day (didn't used to work
; between noon and 1:00 p.m.)
; modified 08-27-1993 to cover for ATTDIA variable
; revised 6-14-94 to automatically use LTSCALE to size stamp block
; revised 13:40:14 Friday, 01-10-1997 to add the Acad Version to the date/time line
; revised 10:26:40 Tuesday, 01-21-1997 to remove space at end of xref line
; *all* text now fully right-justified.
; revised 08:48:28 Thursday, 07-24-1997 for Release 9-14 compatibility
; revised 1:10:27 PM 02/08/2001 to cover for 64 char filenames and test for " "  and "&"
; revised 10:14:00 AM 02/21/2001 to test and print last Layer Express group used
; revised 04:20:00 PM 05-28-2002 to remove the .LX last group used as LayerExpress does not run under LDT3
; revised 03:00:00 PM 08/28/2002 to add SURVEY in dwgname for Survey group members
; revised 04:00:00 PM 11/05/2002 to force users to eliminate spaces and ampersands from filenames.
; revised 5:17 PM 11/4/2005 to force plotting from six folders and less deep in file tree
;=========================================================================
;Fm: Richard Halle [AVE TM] 73417,340
;=========================================================================
(defun get_all_slash_pos (path / cnt pos-list);function provided from AutoLisp Programmer's Toolbox
; this function returns a list of all of the positions of a slash in a string
 (setq cnt 1)
 (repeat (strlen path)                        ; spin through the path string
  (if (or (= (substr path cnt 1) "/")         ; is the character a slash?
          (= (substr path cnt 1) "\\"))
   (setq pos-list (cons cnt pos-list))        ; add the position to the list
  );end if
  (setq cnt (1+ cnt))
 );end repeat
 (if pos-list                                 ; was any slashes found?
  (reverse pos-list)                          ; return the list of positions if any
  nil                                         ; return nil if none found
 );end if
)
;=========================================================================
(defun rtrim (string1 / cnt temp len test);function provided from AutoLisp Programmer's Toolbox
; This routine trims off trailing spaces from a string
; and returns the string
 (setq cnt (strlen string1)
       len (strlen string1)
       temp string1
       test T)
 (repeat len
  (if (and test (= (substr temp cnt 1) " "))
   (setq temp (substr temp 1 (1- cnt)))
   (setq test nil)
  );end if
  (setq cnt (1- cnt))
 );end repeat
 temp
)
;=========================================================================
(defun listxref ( / data work first)
  (setq ME_AcadVersion (getvar "AcadVer"))
  (cond
    ((>= 12 (atoi ME_AcadVersion))
      (setq first t)                              ;set rewind flag
      (while (setq data (tblnext "block" first))  ;rewind 1st time thru
        (setq first nil)                          ;don't want to rewind anymore
        (if (= 68 (logand 68 (cdr (assoc 70 data))))  ;XREFed >AND<  ref'd Block?
          (setq work (cons (cdr (assoc 2 data)) work));If "Yup", add name to list
        );end if
      ); end while
    ); end yes to r12 and under

    ((= 13 (atoi ME_AcadVersion))
      (setq first t)                              ;set rewind flag
      (while (setq data (tblnext "block" first))  ;rewind 1st time thru
        (setq first nil)                          ;don't want to rewind anymore
        (if (= 1 (logand 82 (cdr (assoc 70 data))))  ;XREFed >AND<  ref'd Block?
          (setq work (cons (cdr (assoc 2 data)) work));If "Yup", add name to list
        ); end if
      ); end while
    ); end r13

    ((<= 14 (atoi ME_AcadVersion))
(setq ME_XrefUnloaded nil
      ME_XrefAttached nil)
      (setq first t)                              ;set rewind flag
      (while (setq data (tblnext "block" first))  ;rewind 1st time thru
        (setq first nil)                          ;don't want to rewind anymore
        (if (= 0 (logand 34 (cdr (assoc 70 data))))  ;XREFed >AND< ref'd Block?
          (setq ME_XrefUnloaded (cons (cdr (assoc 2 data)) ME_XrefUnloaded));If "Yup", add name to list (assoc 2 means just filename, not whole path)
        ); end if
      ); end while ME_XrefUnloaded

      (setq first t)                              ;set rewind flag
      (while (setq data (tblnext "block" first))  ;rewind 1st time thru
        (setq first nil)                          ;don't want to rewind anymore
        (if (= 36 (cdr (assoc 70 data)))  ;XREFed >AND< ref'd Block?
          (setq ME_XrefAttached (cons (cdr (assoc 2 data)) ME_XrefAttached));If "Yup", add name to list (assoc 2 means just filename, not whole path)
        ); end if
      ); end while
    ); end for r14+
  ): end cond
  ME_XrefAttached                                            ;returns the list
;  ME_XrefUnloaded                                            ;returns the list
); end defun listxref
;===========================================================================
;Fm: Mark Evinger 73162,1676 -> code modified from July 93 Cadalyst tip #881
(defun datetime ()
  (setq DATST (rtos (getvar "cdate") 2 16))
  (setq HRS (atoi (substr DATST 10 2)))
  (cond
    ((= HRS 00)
      (setq NHRS (itoa (+ HRS 12)))
      (setq XTR "a.m.")
    )
    ((< HRS 12)
      (setq NHRS (itoa HRS))
      (setq XTR "a.m.")
    )
    ((> HRS 12)
      (setq NHRS (itoa (- HRS 12)))
      (setq XTR "p.m.")
    )
    ((= HRS 12)
      (princ "\nHey! It's lunchtime, stop working! <g>")
      (setq NHRS (itoa HRS))
      (setq XTR "p.m.")
    )
  )
  (setq MO (substr DATST 5 2))
  (cond
    ((= MO "01")
      (setq MONTH "January")
    )
    ((= MO "02")
      (setq MONTH "February")
    )
    ((= MO "03")
      (setq MONTH "March")
    )
    ((= MO "04")
      (setq MONTH "April")
    )
    ((= MO "05")
      (setq MONTH "May")
    )
    ((= MO "06")
      (setq MONTH "June")
    )
    ((= MO "07")
      (setq MONTH "July")
    )
    ((= MO "08")
      (setq MONTH "August")
    )
    ((= MO "09")
      (setq MONTH "September")
    )
    ((= MO "10")
      (setq MONTH "October")
    )
    ((= MO "11")
      (setq MONTH "November")
    )
    ((= MO "12")
      (setq MONTH "December")
    )
  ); end MO cond
  (setq DATE-STRING
    (strcat
      MONTH
      " "
      (substr DATST 7 2)","   ; day
      " "
      (substr DATST 1 4)      ; year
      " "
      NHRS                    ; hours
      ":"
      (substr DATST 12 2)     ; minutes
      ":"
      (substr DATST 14 2)     ; seconds
      " "
      XTR                     ; am/pm
      " "  
      "AcadVer:"              ; AutoCad version
      (getvar "acadver")
    ); end strcat
  ); end setq date-string
); end defun datetime
;=========================================================================
(defun C:STMP_ME ()
;;;  (/
;;;    ip ips scaf sca rot rotn
;;;    dwg data myxref xlist first ssa tdata tname ddata dname gdata gname
;;;    xdata xname n BLOCK_NUMBER
;;;  )
  ;_________________________________________________________________________
  ;
  ; Donald Pirl 71174,1113 Yamabe & Horn Engineering, Clovis, CA
  ; with appologies and due credit to:
  ;   Richard Henley 73260,2346 Scanlon and Associates, Albuquerque NM
  ;   Jason Osgood 73417,1756 Alacrity BBS (206) 746-0680, Bellevue WA
  ; Lisp Routine to Place Current Time, Date, Drawing Name and operator's
  ; initials at selected location(s) and rotation (entered or picked).
  ; STMP.DWG accompanying this file must be in the ACAD path.
  ; Suggestions:  insert STMP.DWG in your prototype drawing,
  ;               include the command C:STMP in menu or keyboard save macros.
  ;       To include operator's initials, put the line
  ;           set intl=XXX
  ;           (where XXX is the operator's initials)
  ;       in the autoexec.bat or acad.bat file
  ; Text will be L60 (.06") when plotting scale = stamp block insertion
  ; scale, but the routine can be easily modified to suit.
  ;__________________________________________________________________________
  ;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
  ;Revisions by ME... I wasn't happy with the way the time & date were
  ;       displayed in the original lisp (stmp.lsp), so I modified all
  ;       the things I needed to such that the time, date and  drawing
  ;       name are now displayed like this
  ;
  ;         July 08, 1993 7:56:32 p.m. AcadVer:12_c3
  ;             Drawing: C:\DATA\ME1177\1177_P10.DWG
  ;       Plotted Xrefs: 1117.DWG 2436.DWG 2436A.DWG
  ;
  ;       The stmp.dwg has been modified so that all the attributes are
  ;       right-justified, and the block insertion point  is  the  text
  ;       insertion point of the drawing name. (Xrefs are not  normally
  ;       used in our drawings). This setup  allows  us  to  insert the
  ;       block so that it is right-justified in the  lower  right-hand
  ;       corner of our various border sheets.  The stmp_me.dwg will be
  ;       inserted on the current layer. One last addendum is the ever-
  ;       present menu modification code, use or discard as ya see fit!
  ;
  ;       [STAMP_ME]^C^C^C(IF (not c:stmp_me);(LOAD "stmp_me"));stmp_me;
  ;
  ;       Good luck with this program, it was fun to edit it! ME
  ;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
  ;==========================================================================
  ; Modifications by Mark Evinger 73162,1676 - with help from Richard Halle
  ;==========================================================================
  (setq DIALOG_BOX (getvar "ATTDIA")) ; save the current dialog box setting
  (setvar "ATTDIA" 0)                 ; turn dialog boxes off while running
  (datetime)
  (setq dt DATE-STRING)
  (setq dt2 "")
  (setq dwg
    (strcat "Drawing: " (strcase (getvar "dwgprefix")) (strcase (getvar "dwgname")))
  )
  ;;; Next section checks for complete dwgname (file and path) over 64 characters and alerts
  ;;; the user if the filename is too long.  Win98 cannot run command prompt stuff more than 127
  ;;; characters or so, and excessive filenames will not autospool correctly in the CGL network.
  (if (> (strlen (strcat (getvar "dwgprefix") (getvar "dwgname"))) 64 )
    (Alert (strcat "The file path and file name are over 64 characters!! \n          This file may not spool to the printer.\n" dwg))
    ;(else do this)
    ); end if dwgname over 64 characters


  ;;; Next section checks for folders deeper than 6 subdirectories in on a drive, if too deep don't plot
(setq cgl_TMPME (strcat (getvar "dwgprefix") (getvar "DWGname")))
(setq cgl_SlashList (get_all_slash_pos cgl_TMPME))
(if (< 7 (length cgl_SlashList))
  (progn
    (setq cgl_MoveUp (rtos (- (length cgl_SlashList) 7) 2 0))
  (alert
    (strcat "Path is too deep, please move drawing file up at least " cgl_MoveUp " folders in directory tree!!\n\n" dwg)
  ) ;_ end of alert
    (exit);force the user to change the file name before allowing a plot
    (princ)
    );progn
  (princ "Path OK")
) ;_ end of if
  (setenv "CGL_FullDrawingFILENAME" (strcat (getvar "dwgprefix")(getvar "dwgname")))

  ;;; next section checks for " " or "&" in filename and warns user
  (repeat (strlen (strcat (getvar "dwgprefix") (getvar "dwgname"))))
  (if (wcmatch (strcat (getvar "dwgprefix") (getvar "dwgname")) "*&*")
    (progn
    (Alert (strcat "The file name or path name contains an ampersand '&'.
    This file will not be logged correctly, and will not be plotted.
    Please change the filename to exclude the '&'. \n \n" dwg))
    (exit);force the user to change the file name before allowing a plot
    (princ)
    );end progn
    ;(else)
    );end if
(if (wcmatch  (strcat (getvar "dwgprefix") (getvar "dwgname")) "* *")
  (progn
    (Alert (strcat "The file name or path name contains a space ' '.
    This file will not be logged correctly, and will not be plotted.
    Please change the filename to exclude the ' '. \n \n" dwg))
    (exit);force the user to change the file name before allowing a plot
    (princ)
    );progn
    ;(else)
    );end if

;;; Next section tests for .lx file and if the ";;LASTGRP=" line is there, appends the LX group to the dwg path...
  (setq CGL_LX_GROUP ""); blank out CGL_LX_GROUP
;;;  (setq CGL_LX_TEST (findfile (strcat (getvar "dwgprefix")(substr (getvar "dwgname") 1 (- (strlen (getvar "dwgname")) 4))".lx"))); find .LX file
;;;  (if (/= CGL_LX_TEST nil); if .LX file exists (not nil)
;;;    (progn
;;;      (setq FileHandle (open CGL_LX_TEST "r")) ;r MUST be in lowercase for this to work... opens .lx file for read
;;;       (while (setq CGL_CurrentLine (read-line FileHandle)); read file, look for ;;LASTGRP
;;;	(if (wcmatch CGL_CurrentLine ";;LASTGRP*")
;;;	  (if (wcmatch CGL_CurrentLine ";;LASTGRP=        "); is LASTGRP blank?
;;;	    (setq CGL_LX_GROUP "");if lastgrp is blank, set to ""
;;;	    (setq CGL_LX_GROUP (strcat " LXGRP:"(rtrim (substr CGL_CurrentLine 11 )))); else set to group list name
;;;	    );end if lastgrp is blanks
;;;	  );end if
;;;	);end while
;;;      (close FileHandle); close the file so others can use it again!
;;;    ); end progn
;;;    ;(else this)
;;;    );end if .LX file does exist
  
  (if (getenv "intl")
    (progn
      (setq init (strcase (getenv "intl")))
      (if (= "T" (getenv "SRV"))
      (setq dwg (strcat "SURVEY Drawing: " (strcase (getvar "dwgprefix")) (strcase (getvar "dwgname"))" ("init")" CGL_LX_GROUP))
      (setq dwg (strcat "Drawing: " (strcase (getvar "dwgprefix")) (strcase (getvar "dwgname"))" ("init")" CGL_LX_GROUP))
	);end if SRV
    ); end progn then
    (progn
      (if (= "T" (getenv "SRV"))
      (setq dwg (strcat "SURVEY Drawing: " (strcase (getvar "dwgprefix")) (strcase (getvar "dwgname")) CGL_LX_GROUP))
      (setq dwg (strcat "Drawing: " (strcase (getvar "dwgprefix")) (strcase (getvar "dwgname")) CGL_LX_GROUP))
      );end if SRV
    ); end progn else
  ); end if
  (setq myxref (listxref))
  (setq xlist (strcat "Plotted Xrefs:"))
  (if (= myxref nil) (setq xlist (strcat " ")))
  (mapcar
    '(lambda (l)
      (setq xlist (strcat xlist " "l ".DWG")) ;add space in front of dwgname L in xreflist
    )
    myxref
  ) ; end of xref lister routine
  (if
    (setq ssa (ssget "X" '((0 . "INSERT") (2 . "STMP_ME")))) ;does the block exist?
    (progn
      (setq n (sslength ssa))
      (setq BLOCK_NUMBER n)
      (while (> n 0)
        (setq n (- n 1)
          tname (entnext (ssname ssa n))
          tdata (entget tname)
          dname (entnext tname)
          ddata (entget dname)
          gname (entnext dname)
          gdata (entget gname)
          xname (entnext gname)
          xdata (entget xname)
          tdata (subst (cons 1 dt2) (assoc 1 tdata) tdata)
          ddata (subst (cons 1 dt) (assoc 1 ddata) ddata)
          gdata (subst (cons 1 dwg) (assoc 1 gdata) gdata)
          xdata (subst (cons 1 xlist) (assoc 1 xdata) xdata)
        )
        (entmod tdata)
        (entmod ddata)
        (entmod gdata)
        (entmod xdata)
        (entupd gname)
      ); end if stmp_me blk exists
      (if (> BLOCK_NUMBER 1)
        (progn
          (setq text (rtos BLOCK_NUMBER 2 0)) ;sets text to # of blocks found in dwg
          (princ (strcat "\n"text" Stamp blocks \"STMP_ME\" updated.")) ;reports # of blocks updated
        ); end progn
        (princ "\nStamp block \"STMP_ME\" updated.") ; only if 1 block found
      );end if
    )
    (progn
      (princ "\nSTMP_ME block not found - Now inserting.\n")
      (setq ips (getpoint "\nInsertion point: <0.0,0.0,0.0> "))
      (if (null ips) (setq ip (list 0.0 0.0 0.0)) (setq ip ips))
      (setq TEMP_SCL (getvar "ltscale"))
      (setq TEMP_ANS TEMP_SCL)
      (setq TEMP_ME (getreal (strcat "\n Enter SCALE: >> "(rtos TEMP_ANS 2 1)" << ")))
      (if (not (null TEMP_ME)) (setq SCA TEMP_ME ) (setq SCA TEMP_ANS))
      ;(setq scaf (getreal "\nScale factor: <1.0> "))
      ;(if (null scaf) (setq sca "1.0") (setq sca scaf))
      (setq rotn (getangle ip "\nEnter Text Rotation: <0.0> "))
      (if (null rotn) (setq rot "0.0") (setq rot (angtos rotn 0 6)))
      (command ".INSERT" "STMP_ME"
      ip sca sca rot dt2 dt dwg xlist)
    )
  );end if STMP_ME block

      (if
	(setq ssa (ssget "X" '((0 . "INSERT") (2 . "STMP_SRV"))));does the block exist?
	 (progn
	   (setq n (sslength ssa))
	   (setq BLOCK_NUMBER n)
	   (if (> BLOCK_NUMBER 1)
	     (progn
	       (setq text (rtos BLOCK_NUMBER 2 0));sets text to # of blocks found in dwg
	       (princ
		 (strcat "\n" text " Stamp blocks \"STMP_SRV\" found in drawing."));reports # of blocks updated
	     ); end progn
	     (princ "\nStamp block \"STMP_SRV\" found in drawing."); only if 1 block found
	   );end if
	 );end progn STMP_SRV block exists in file
	 (if (= "T" (getenv "SRV"))
	   (progn
	     (princ "\nSTMP_SRV block not found - Now inserting.\n")
	     (setq ips (getpoint "\nInsertion point: <0.0,0.0,0.0> "))
	     (if (null ips)
	       (setq ip (list 0.0 0.0 0.0))
	       (setq ip ips)
	     )
	     (setq TEMP_SCL (getvar "ltscale"))
	     (setq TEMP_ANS TEMP_SCL)
	     (setq TEMP_ME (getreal (strcat "\n Enter SCALE: >> "(rtos TEMP_ANS 2 1)" << ")))
	     (if (not (null TEMP_ME))
	       (setq SCA TEMP_ME)
	       (setq SCA TEMP_ANS))
	     (setq rotn (getangle ip "\nEnter Text Rotation: <0.0> "))
	     (if (null rotn)
	       (setq rot "0.0")
	       (setq rot (angtos rotn 0 6))
	     )
	     (command ".INSERT" "STMP_SRV" ip sca sca rot)
	   );end progn insert STMP_SRV block
	 );end inner if
      );end outer IF STMP_SRV exists
  
  (setvar "ATTDIA" DIALOG_BOX) ;return dialog box setting to original value
  (princ)
); End Of File