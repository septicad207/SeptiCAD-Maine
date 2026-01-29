; this program only needs to be run when SeptiCAD v5 is installed for the first time for each Windows User
; if there is more than one login for the computer, then it needs to be run once for each user when they are logged in
; it places code in the on_doc_load.lsp file in your "roaming" support folder for Bricscad
; if you install an upgraded version of Bricscad it needs to be run again
; to find the "roaming support folder for Bricscad" copy the command below without the ; and paste it into the command line.
;(strcat (getvar "roamablerootprefix") "support\\on_doc_load.lsp")

;;; If AutoCAD
	(if (= (strcase (getvar "PRODUCT")) "AUTOCAD")
		(progn
			(if (setq fileName (strcat (getvar "roamablerootprefix") "support\\ACADDOC.lsp"))
			  (progn
				; if the file exist add to the end of it, if the file does not create file
				(setq fil (open fileName "a"))

				(write-line "" fil)		
				(write-line ";;;;;; Start SeptiCAD v5 Entry ;;;;;;" fil)
				(write-line "       (if (findfile \"c:/SeptiCADv5/SeptiCADv5.lsp\")(progn" fil)
				(write-line "         (load \"c:/SeptiCADv5/SeptiCADv5.lsp\")" fil)
				(write-line "         (if (not (menugroup \"SEPTICADV5\"))(command \"_menuload\" \"c:/SeptiCADv5/SeptiCADv5-Toolbar-ACAD.cuix\"))" fil)
				(write-line "         (princ)))" fil)
				(write-line ";;;;;; End SeptiCAD v5 Entry ;;;;;;" fil)
				
				(Alert (strcat "\nToolbar and LISP Routine Loading Functions successfully added to " fileName))

				; closing fil makes the changes
				(close fil)

			        ; load SeptiCAD so it works immediately
				(load "c:/SeptiCADv5/SeptiCADv5.lsp")
				(if (not (menugroup "SEPTICADV5"))(command "_menuload" "c:/SeptiCADv5/SeptiCADv5-Toolbar-ACAD.cuix"))

			  )
			        ; if something went wrong 
				(Alert "\nToolbar and LISP Routine Loading Functions did not work.  Contact SeptiCAD for troubleshooting.")
			)
		)
	)
; end if AutoCAD


;;;; If Bricscad
	(if (= (strcase (getvar "PRODUCT")) "BRICSCAD")
		(progn
			(if (setq fileName (strcat (getvar "roamablerootprefix") "support\\on_doc_load.lsp"))
			  (progn
				; if the file exist add to the end of it, if the file does not create file
				(setq fil (open fileName "a"))

				(write-line "" fil)		
				(write-line ";;;;;; Start SeptiCAD v5 Entry ;;;;;;" fil)
				(write-line "       (if (findfile \"c:/SeptiCADv5/SeptiCADv5.lsp\")(progn" fil)
				(write-line "         (load \"c:/SeptiCADv5/SeptiCADv5.lsp\")" fil)
				(write-line "         (if (not (menugroup \"SEPTICADV5\"))(command \"_menuload\" \"c:/SeptiCADv5/SeptiCADv5-Toolbar.cui\"))" fil)
				(write-line "         (princ)))" fil)
				(write-line ";;;;;; End SeptiCAD v5 Entry ;;;;;;" fil)
				
				(Alert (strcat "\nToolbar and LISP Routine Loading Functions successfully added to " fileName))

				; closing fil makes the changes
				(close fil)

			        ; load SeptiCAD so it works immediately
				(load "c:/SeptiCADv5/SeptiCADv5.lsp")
				(if (not (menugroup "SEPTICADV5"))(command "_menuload" "c:/SeptiCADv5/SeptiCADv5-Toolbar.cui"))

			  )
			        ; if something went wrong 
				(Alert "\nToolbar and LISP Routine Loading Functions did not work.  Contact SeptiCAD for troubleshooting.")
			)
		)
	)
;;; end if Bricscad



; clean exit
; sets variables to nil
(setq fileName nil fil nil)
(princ)