;;; ==============================================================================
;;; Lisp Name: archive_and_save.lsp
;;; Author: Lonnie
;;; Date Created: 2024-06-06
;;; https://www.theswamp.org/index.php?topic=59596.msg621100#msg621100
;;; Last Edited: [Insert Last Edit Date]
;;;
;;; DESCRIPTION:
;;; A routine to archive and save files related to the current AutoCAD drawing.
;;;
;;; Usage:
;;; 1. Load the Lisp routine.
;;; 2. Run the command "DCARCH" in AutoCAD.
;;;
;;; Parameters:
;;; dwgDir currentDate year month day formattedDate archiveDir foldername dwgFiles txtFiles csvFiles rvtFiles allFiles pngFiles jpgFiles xlsxFiles
;;;
;;; Returns:
;;; None
;;;
;;; Notes:
;;; - This routine archives and saves files (DWG, TXT, CSV, RVT, PNG, JPG, XLSX) 
;;;   related to the current AutoCAD drawing in a dated subfolder within an 
;;;   "archive" directory.
;;;
;;; ---------------------------- Main program --------------------------------;

(defun C:dcarch (/ dwgDir currentDate year month day formattedDate archiveRootDir archiveDir foldername dwgFiles txtFiles csvFiles rvtFiles allFiles pngFiles jpgFiles xlsxFiles userResponse)

  ;; Get the directory of the current drawing
  (setq dwgDir (getvar "dwgprefix"))
  
  ;; Get and format the current date
  (setq currentDate (rtos (getvar "cdate") 2 5))
  (setq year (substr currentDate 1 4))
  (setq month (substr currentDate 5 2))
  (setq day (substr currentDate 7 2))
  (setq formattedDate (strcat year "-" month "-" day))
  
  ;; Define the archive root directory and the dated subdirectory
  (setq archiveRootDir (strcat dwgDir "Archive\\"))
  (setq archiveDir (strcat archiveRootDir formattedDate))
  (setq foldername (strcat archiveDir "\\"))


  ;; Check if the archive root directory exists
  (if (not (vl-file-directory-p archiveRootDir))
    (progn
      ;; Prompt the user to create the archive directory
      (setq userResponse (getstring (strcat "The archive directory does not exist. Do you want to create it? (Y/N): ")))
      ;; If the user wants to create it, do so
      (if (or (equal (strcase userResponse) "Y") (equal (strcase userResponse) "YES"))
        (vl-mkdir archiveRootDir)
        (progn
          (prompt "\nThe archive directory does not exist and was not created. Exiting command.")
          (exit)
        )
      )
    )
  )

  ;; Create the dated subdirectory
  (vl-mkdir archiveDir)

  ;; Function to list files in a directory matching a specific pattern (case-insensitive)
  (defun list-files (dir pattern)
    (vl-remove-if 'null
      (mapcar '(lambda (file)
                 (if (wcmatch (strcase file) (strcase pattern))
                   (strcat dir file)
                 )
               )
               (vl-directory-files dir)
      )
    )
  )

  ;; Function to copy a file from source to destination
  (defun copy-file (src dst)
    (vl-file-copy src dst)
  )

  ;; Get lists of files with specified extensions
  (setq dwgFiles (list-files dwgDir "*.DWG"))
  (setq txtFiles (list-files dwgDir "*.TXT"))
  (setq csvFiles (list-files dwgDir "*.CSV"))
  (setq rvtFiles (list-files dwgDir "*.RVT"))
  (setq rfaFiles (list-files dwgDir "*.RFA"))
  (setq pngFiles (list-files dwgDir "*.PNG"))
  (setq jpgFiles (list-files dwgDir "*.JPG"))
  (setq xlsxFiles (list-files dwgDir "*.XLSX"))

  ;; Combine all file lists
  (setq allFiles (append dwgFiles txtFiles csvFiles rvtFiles rfaFiles jpgFiles pngFiles xlsxFiles))

  ;; Copy each file to the archive directory
  (foreach file allFiles
    (copy-file file (strcat foldername (vl-filename-base file) (vl-filename-extension file)))
  )
)