; --- This LSP File sets global variables for different file locations, and 
; --- loads individual LSP files which are separated for the purpose of basic organization
; --- if you choose to change some of the code, and there is an error in the code, then
; --- debugging the code is much easier with it set up this way - extra paratheses are a common problem
; --- there is a shortcut for reloading code inside bricscad without closing it down. The command 
; --- is sss at the CAD command line - this makes it easier to test code, all it does is reload 

; Global version variable
(setq $septicadVersion "SeptiCAD Free Edition 2026 w/ new HHE-200 Form")

; Global Path variables
(setq $septicadPathRoot "c:/SeptiCADv5/")
(setq $septicadPathBlock (strcat $septicadPathRoot "BLOCKS/"))
(setq $septicadPathChambers (strcat $septicadPathBlock "CHAMBERS/"))
(setq $septicadPathBlockRequired (strcat $septicadPathBlock "REQUIRED/"))
(setq $septicadPathDCL (strcat $septicadPathRoot "DCL/"))
(setq $septicadPathText (strcat $septicadPathRoot "TEXT/"))
(setq $septicadPathHTML (strcat $septicadPathRoot "HELP/HTML/"))
(setq $septicadPathPDF (strcat $septicadPathRoot "HELP/HTML/PDF/"))
(setq $septicadPathLSP (strcat $septicadPathRoot "septicadv5.lsp"))


;;; load the septicad lisp file
(defun load_septicad_lsp_files (/ fil fileList)
  
  (setq fileList (list
                  (strcat $septicadPathRoot "LSP/main.lsp")                   
                  (strcat $septicadPathRoot "LSP/pref.lsp")
                  (strcat $septicadPathRoot "LSP/tools.lsp")
                  (strcat $septicadPathRoot "LSP/form.lsp")
                  (strcat $septicadPathRoot "LSP/scarify.lsp")
                  (strcat $septicadPathRoot "LSP/util.lsp")
                  (strcat $septicadPathRoot "LSP/soil_log.lsp")
                  (strcat $septicadPathRoot "LSP/systems.lsp")
                ))
 
 (foreach fil fileList
  (if (FINDFILE fil)
    (IF (LOAD fil)(princ (strcat "****** " fil " loaded\n")))
  )
 )  
  
 (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load_septicad_lsp_files)

(princ (strcat $septicadVersion "  .......  LISP/DCL Application Loaded\n"))
(princ)
