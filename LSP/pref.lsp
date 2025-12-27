;;; this file includes the functions related to preferences and customization






;;;;;;;;;;;USEFUL REFERENCE CODE FOR CUSTOME SYTEM LIST INTERACTION 
;(foreach item (getCustomSystemList) (print item))
;(cdr (assoc "%X-4%" $customSysList))
;(replaceString titlePage3 "|ROWS|" "new value")

;;;; returns dotted pair list for customization file
;;;;all leading and trailing spaces and tabs are trimmed


(defun getCustomSystemList (SYS_TYPE / customizationFile flag2)

  (setq
    varList (list)
    lst (list)
    delim "%"
  )


  (setq customizationFile (strcat $septicadPathText SYS_TYPE ".TXT"))

  ; open system customization file
  (if (findfile customizationFile)
    (progn
      (if (setq fil (open customizationFile "r"))
        (progn
          ;;;--- Read the lines of data in the text file
          (while (setq a (read-line fil))
            ;;;--- Add the data to the list
            (setq varList
                  (append
                    varList
                    (list a)
                  )
            )
          )
          ;;;--- Close the file
          (close fil)
        )           ; end progn
      )             ; end if
    )               ; end progn
  )                 ; end if


; if the first character is a % consider it a special deliminator... if not then, ignore.
  (foreach item varlist
    (if (= (substr item 1 1) delim)
      (progn

        (setq flag2 (vl-string-search delim item 1))
        (setq code
              (substr item 1
                      (+ flag2 1)
              )
        )

        (setq str (substr item (+ flag2 2) (strlen item)))
        (setq lst (append lst (list (cons code (vl-string-trim "\t" (vl-string-trim " " str))))))
      )

    )
  )


;(FOREACH ITEM LST (PRINT ITEM))

  lst
)                   ; end function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun customSystem ()
  (setq tagList (list) textList (list))

  (foreach item (getCustomSystemList (car (nth 0 syslist)))
    (setq tagList (append tagList (list (car item))))
    (setq textList (append textList (list (cdr item))))
  )

  (setq lg (length tagList))

   ;(foreach item code (print item))
   ;(foreach item text (print item))

  (create_dialog)
  (run_the_dialog)
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun updateCustomSystem ()

  (foreach item (getCustomSystemList (car (nth (atoi (get_tile "SYSTEM")) sysList)))
    (setq tagList (append tagList (list (car item))))
    (setq textList (append textList (list (cdr item))))
  )

  (setq nu 0)
  (repeat (length tagList)
    (set_tile (nth nu tagList) (nth nu textList))
    (setq nu (1+ nu))
  )

  (updateCustomSystemTile (car (nth (atoi (get_tile "SYSTEM")) sysList)))

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun create_dialog ()

  (setq fname (strcat $septicadPathDCL "customSystem.dcl"))

  (setq fn (open fname "w"))

  (write-line "customSystem : dialog { label = \"Customize SeptiCAD System Notes\";" fn)

  (write-line ": row {" fn)

  (write-line ": column {" fn)
  (write-line ": boxed_column {" fn)
  (write-line "label = \"Selected a System to Customize\";" fn)
  (write-line ": text {" fn)
  (write-line "alignment = centered;" fn)
  (write-line "value = \"Selected a System to Customize\";" fn)

  (write-line "}" fn)
  (write-line ": text {" fn)
  (write-line "alignment = centered;" fn)
  (write-line "value = \"\";" fn)

  (write-line "}" fn)

  (write-line ": popup_list {" fn)
  (write-line "key = \"SYSTEM\";" fn)

  (write-line "action = \"\(updateCustomSystem\)\";" fn)
  (write-line "}" fn)
  (write-line "}" fn)
  (write-line "}" fn)

  (write-line ": column {" fn)
  (write-line ": button {" fn)
  (write-line "key = \"save\";" fn)
  (write-line "label = \"Save Changes\";" fn)
  (write-line "is_default = false;" fn)
  (write-line "is_cancel = false;" fn)
  (write-line "}" fn)

  (write-line ": button {" fn)
  (write-line "key = \"cancel\";" fn)
  (write-line "label = \"Cancel / End\";" fn)
  (write-line "is_default = false;" fn)
  (write-line "is_cancel = true;" fn)
  (write-line "}" fn)

  (write-line ": button {" fn)
  (write-line "key = \"help\";" fn)
  (write-line "label = \"Help\";" fn)
  (write-line "is_default = false;" fn)
  (write-line "is_cancel = false;" fn)
  (write-line "}" fn)
  (write-line "}" fn)

  (write-line ": column {" fn)
  (write-line ": image {" fn)
  (write-line "key = \"customize\";" fn)
  (write-line "height = 5;" fn)
  (write-line "width = 90;" fn)
  (write-line "color = dialog_background;" fn)
  (write-line "}" fn)
  (write-line "}" fn)

  (write-line "}" fn)

  (write-line ": row {" fn)
  (write-line ": boxed_column {" fn)

  (setq nu 0)

   (repeat lg  
    ; create the edit boxes
    (write-line ": edit_box {" fn)
    (setq l (strcat "\"" (nth nu tagList) "\"" ";"))
    (write-line (strcat "key = " l) fn)
    (setq l (nth nu taglist))
    (write-line (strcat "label = " "\"" l "\"" ";") fn)
    (write-line "alignment = centered; edit_width = 75; }" fn)

    (if (= nu 19)
      (progn
        (write-line "}" fn)
        (write-line ": boxed_column {" fn)
      )
    )
    ;increment the counter
    (setq nu (1+ nu))

  ) ;repeat
     
  (write-line "}}" fn); end the row and column
  (write-line "}" fn); end dialog


  (close fn); close the temp DCL file

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun run_the_dialog ()

;load the dialog file and definition
  (setq dcl_id (load_dialog fname))
  (if (not (new_dialog "customSystem" dcl_id))
    (exit)
  )                 ;if


  ;if the OK button is selected
  (action_tile "save" "(setq ddiag 1)")
  (action_tile "cancel" "(setq ddiag 2)(done_dialog)")
  (action_tile "help" "(setq ddiag 3)")

  (setq nu 0)
  (repeat lg
    (set_tile (nth nu tagList) (nth nu textList))
    (setq nu (1+ nu))
  )

  (createSysList)

  (setq tempList (list))
  (foreach item sysList
    (setq tempList (append tempList (list (cdr item))))
  )

  (start_list "SYSTEM" 3)
  (mapcar 'add_list tempList)
  (end_list)

  (updateCustomSystemTile (car (nth 0 syslist)))

	;;;--- First we need a slide name 
  (setq mySlideName (strcat $septicadPathDCL "CUSTOM-WINDOW.SLD"))

	;;;--- Second we neesytemCustomd the key to the image control 
  (setq myKey "customize")

	;;;--- Next we send the slide name and the key to the update function 
  (upDateImage mySlideName myKey)

  (action_tile "save" "(setq ddiag 1)(saveCustomSystemVars)")
  (action_tile "cancel" "(setq ddiag 2)(done_dialog)")
  (action_tile "help" "(customSystemVarsHelp)")


  ;start the dialog
  (start_dialog)

  ;unload the dialog
  (unload_dialog dcl_id)

  ;delete the temp DCL file
  (vl-file-delete fname)

)                   ;defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun saveCustomSystemVars (/ nu fil)

;tagList is already defined by the form fill function (updateCustomSystem)

  (setq fil (open (strcat $septicadPathText (car (nth (atoi (get_tile "SYSTEM")) sysList)) ".TXT") "w"))
; (setq fil (open (strcat $septicadPathText "TEST.TXT") "w"))	
  (setq nu 0)
  (repeat (length tagList)
    (write-line (strcat (nth nu tagList) " " (get_tile (nth nu tagList))) fil)
    (setq nu (1+ nu))
  )

  (close fil)
  (alert (strcat (nth (atoi (get_tile "SYSTEM")) tempList) " Information Saved"))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun customSystemVarsHelp ()
  (if (= (strcase (getvar "PRODUCT")) "BRICSCAD")
    (command "URL" (strcat $septicadPathPDF "CUSTOMSYSTEM.PDF"))
  )
  (if (= (strcase (getvar "PRODUCT")) "AUTOCAD")
    (Alert "Goto SeptiCAD Help or to http://septicad.com/PDF/CUSTOMSYSTEM.PDF")
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; this fuction will turn all tiles on
;;; then selectively turn tiles off that are not applicable
(defun updateCustomSystemTile (sys)

  (foreach item tagList
    (mode_tile item 0)
  )

  (if (and (/= sys "4x8_CONCRETE_CLUSTER")
           (/= sys "4x8_CONCRETE_TRENCH")
           (/= sys "8x4_CONCRETE_CLUSTER")
           (/= sys "8x4_CONCRETE_TRENCH")
      )
    (progn
      (mode_tile "%P2-1-H20%" 1)
      (mode_tile "%P2-2%" 1)
      (mode_tile "%P2-3%" 1)
      (mode_tile "%P3-1-H20%" 1)
      (mode_tile "%P3-2%" 1)
      (mode_tile "%P3-3%" 1)
      (mode_tile "%X-1-H20%" 1)
    )
  )

  (if (/= sys "STONE")
    (progn
      (mode_tile "%P3-6%" 1)
      (mode_tile "%P3-7%" 1)
      (mode_tile "%P3-8%" 1)
    )
  )


  (if (and (/= sys "STONE")
           (/= sys "ENVIRO")
           (/= sys "ELJEN")
           (/= sys "ELJEN_INVERTED")
           (/= sys "4x8_CONCRETE_CLUSTER")
           (/= sys "8x4_CONCRETE_CLUSTER")
           (/= sys "TRENCH2")
           (/= sys "TRENCH3")
      )
    (progn
      (mode_tile "%CE-1%" 1)
    )
  )

  (if (or (= sys "MOUNDBUSTER")
          (= sys "TRENCH2")
          (= sys "TRENCH3")
      )
    (progn
      (mode_tile "%X-4%" 1)
    )
  )




  (if (and (/= sys "STONE")
           (/= sys "ENVIRO")
           (/= sys "4x8_CONCRETE_CLUSTER")
           (/= sys "8x4_CONCRETE_CLUSTER")
           (/= sys "TRENCH2")
           (/= sys "TRENCH3")
      )
    (progn
      (mode_tile "%X-5%" 1)
    )
  )

  (if (and
        (/= sys "4x8_CONCRETE_CLUSTER")
        (/= sys "8x4_CONCRETE_CLUSTER")
      )
    (progn
      (mode_tile "%X-6%" 1)
    )
  )





  (if (and (/= sys "QUICK4_HI_CAP")
           (/= sys "QUICK4_STD")
           (/= sys "Q4_PLUS_EQ_36_LP")
           (/= sys "Q4_PLUS_HC")
           (/= sys "Q4_PLUS_STD")
           (/= sys "Q4_PLUS_STD_LP")
           (/= sys "QUICK4_EQ24LP")
           (/= sys "QUICK4_EQ36")
      )
    (progn
      (mode_tile "%P3-9%" 1)
    )
  )


  (if (and (/= sys "Q4_PLUS_EQ_36_LP")
           (/= sys "Q4_PLUS_STD_LP")
      )
    (progn
      (mode_tile "%P3-10%" 1)
    )
  )


  (if (= sys "STONE")
    (progn
      (mode_tile "%MAN-ALL-1%" 1)
      (mode_tile "%MAN-ALL-2%" 1)
      (mode_tile "%SERIAL-ALL-1%" 1)
      (mode_tile "%SERIAL-ALL-2%" 1)
      (mode_tile "%DBOX-MAN-1%" 1)
      (mode_tile "%DBOX-MAN-2%" 1)
      (mode_tile "%DBOX-SERIAL-1%" 1)
      (mode_tile "%DBOX-SERIAL-2%" 1)
      (mode_tile "%DBOX-SERIAL-3%" 1)
      (mode_tile "%DBOX-ALL-1%" 1)
    )
  )



  (if (and (/= sys "ELJEN")
           (/= sys "ELJEN_INVERTED")
           (/= sys "MOUNDBUSTER")
           (/= sys "TRENCH2")
           (/= sys "TRENCH3")
      )
    (progn
      (mode_tile "%DBOX-SERIAL-3%" 1)
      (mode_tile "%DBOX-ALL-2%" 1)
    )
  )


)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getSiteEvaluatorNames (/ nameList item)
  (setq nameList (list))
  (foreach item (getSiteEvaluatorList)
    (setq nameList (append nameList (list (nth 0 item))))
  )
  nameList
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getSiteEvaluatorList ()

  ;;;--- Set up an empty list
  (setq varList (list))   ; list of lines

  ;;;--- open spref1 and get information
  (if (findfile (strcat $septicadPathText "sPref1.txt"))

    (progn
      (if (setq fil (open (findfile (strcat $septicadPathText "sPref1.txt")) "r"))
        (progn
          ;;;--- Read the lines of data in the text file
          (while (setq a (read-line fil))
            ;;;--- Add the data to the list
            (setq varList
                  (append
                    varList
                    (list a)
                  )
            )
          )
          ;;;--- Close the file.
          (close fil)
        )           ; end progn
      )             ; end if
    )               ; end progn
  )                 ; end if

	; this is the structure of sPref1.txt
	;0	(get_tile "USERNAME")
	;1	(get_tile "SERIAL")

	;2	(get_tile "NAME")
	;3	(get_tile "NUMBER")
	;4	(get_tile "PHONE")
	;5	(get_tile "EMAIL")

	;6	(get_tile "NAME2")
	;7	(get_tile "NUMBER2")
	;8	(get_tile "PHONE2")
	;9	(get_tile "EMAIL2")

	;10	(get_tile "NAME3")
	;11	(get_tile "NUMBER3")
	;12	(get_tile "PHONE3")
	;13	(get_tile "EMAIL3")

  (setq $septicadUserName (nth 0 VarList))
  (setq $septicadSerialCode (nth 1 VarList))

       ;  always add the default user
       ;  only add other users if username contains information to do so

  (setq seList (list (list (nth 2 VarList) (nth 3 VarList) (nth 4 VarList) (nth 5 VarList))))
  (setq seList (append seList
                       (list (list (nth 6 VarList) (nth 7 VarList) (nth 8 VarList) (nth 9 VarList)))
                       (list (list (nth 10 VarList) (nth 11 VarList) (nth 12 VarList) (nth 13 VarList)))
               )
  )

; return list of list - (name phone number email)
  seList
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   TRIAL VS FULL VERSION CODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;VAR OR FUNCTION NAME		TYPE		DESCRIPITION
;$septicadUserName		string		user name entered into preferences - set by (getuservars)
;$septicadSerialCode		string		user serial code entered into preferences - set by (getuservars)
;$septicadFullVersion		int/boolean	0 or 1 (trial/full) - set by (septicadCheckID)
;$uniqueSerialNumber		integer		computer serial number set ComputerName,UserName and _PKSER or LICKEY - set by (setUniqueSerialNumber)
;
;(septicadCheckReg)		FUNCTION	refreshes relevant variables above and runs (septicadCheckID)
;(septicadCheckID)		FUNCTION	this runs the genID type code for the unlock code generator, and sets $septicadFullVersion
;
;(setUniqueSerialNumber str)	FUNCTION	sets computers unique serial number, if str is "ALERT" then the (sDemoAlert) is run
;(sDemoAlert)			FUNCTION	trial mode screen DCL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun setUniqueSerialNumber (flag / uTemp count tweak)

  (setq $uniqueSerialNumber 0)
  (setq tweak 1234)

  (setq uTemp (getenv "UserName"))
  (setq count 1)
  (while (<= count (strlen uTemp))
    (setq $uniqueSerialNumber (+ $uniqueSerialNumber (* (ascii (substr uTemp count 1)) count tweak)))
    (setq count (+ count 1))
  )
	; end while	

  (setq uTemp (getenv "ComputerName"))
  (setq count 1)
  (while (<= count (strlen uTemp))
    (setq $uniqueSerialNumber (+ $uniqueSerialNumber (* (ascii (substr uTemp count 1)) count tweak)))
    (setq count (+ count 1))
  )
	; end while

  (if (= (setq uTemp (getvar "_PKSER")) "")
    (setq uTemp (getvar "LICKEY"))
  )

  (setq count 1)
  (while (<= count (strlen uTemp))
    (setq $uniqueSerialNumber (+ $uniqueSerialNumber (* (ascii (substr uTemp count 1)) count tweak)))
    (setq count (+ count 1))
  )
	; end while


        ; if this function is being called due to trial mode, then put up demo screen with $uniqueSerialNumber
  (if (= flag "ALERT")
    (sDemoAlert)
  )

  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun septicadCheckID (/ count temp nameID 1stLetter LastLetter)

  (setq $septicadFullVersion nil)
  (if (or (= $septicadSerialCode "") (= $septicadUserName "")) (setq $septicadFullVersion 0))

  (setq uTemp $septicadUserName)
  (setq versionAdjustmentNumber 123456)
  (setq count 1)
  (setq temp nil)
  (setq nameID 0)
  (while (<= count (strlen uTemp))
    (setq temp (* (ascii (substr uTemp count 1)) (* count versionAdjustmentNumber)))
    (setq nameID (+ nameID temp))
    (setq count (+ count 1))
  )    ; end while

  (setq 1stLetter (atoi (substr (rtos nameID 2 0) 2 3)))
  (while (or (< 1stLetter 65) (> 1stLetter 90))
    (if (< 1stLetter 65) (setq 1stLetter (* 1stLetter 1.1)))
    (if (> 1stLetter 90) (setq 1stLetter (* 1stLetter 0.9)))
  )    ; end while
  (setq 1stLetter (chr (fix 1stLetter)))

  (setq LastLetter (atoi (substr (rtos nameID 2 0) 3 3)))
  (while (or (< LastLetter 65) (> LastLetter 90))
    (if (< LastLetter 65) (setq LastLetter (* LastLetter 1.1)))
    (if (> LastLetter 90) (setq LastLetter (* LastLetter 0.9)))
  )    ; end while
  (setq LastLetter (chr (fix LastLetter)))

  (if (= $septicadSerialCode (strcat 1stLetter (rtos (+ nameID $uniqueSerialNumber) 2 0) LastLetter))
    (setq $septicadFullVersion 1)
  )

  (if (/= $septicadSerialCode (strcat 1stLetter (rtos (+ nameID $uniqueSerialNumber) 2 0) LastLetter))
    (progn
      (setq $septicadFullVersion 0)
      (princ "\nStarting SeptiCAD Trial Version \n")
    )  ; progn
  )    ; if

  (princ)
)      ; end septicadCheckID
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun septicadCheckReg ()
        ; all septic uservars pulled into memory - stored in three text files
  (getuserVars)
  ; create unique serial number specific to each install
  (setUniqueSerialNumber "")

;   checks if correct serial code (aka registration code) is valid 
;	(septicadCheckID)

	; - Secret Code to Bypass Registration System
  (if (= $septicadUserName "Sweet Brown Branch")
    (setq $septicadFullVersion 1)
  )

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sDemoAlert ()
	; reset variable - used in c:septic to determine whether registration button was pressed or not
  (setq $$RegButton nil)

       ;;;--- Load the DCL file
  (setq dcl_id (load_dialog (strcat $septicadPathDCL "sdemo.dcl")))

      ;;;--- See if the dialog box is already loaded
  (if (not (new_dialog "sDemo" dcl_id))
    (progn
      (alert "The sDemo.DCL file was not found!")
      (exit)
    )
  )

      ;;;--- If an action event occurs, do this function
  (action_tile "okay" "(setq ddiag 1)(done_dialog)")
  (action_tile "register" "(setq ddiag 2)(done_dialog)")

  (set_tile "serial" (rtos $uniqueSerialNumber 2 0))


      ;;;--- Display the dialog box
  (start_dialog)


      ;;;--- Unload the dialog box
  (unload_dialog dcl_id)

      ;;;--- If the register button was pressed
  (if (= ddiag 2)
    (progn
      (Alert "To Register - Open SeptiCAD Preferences [PREF toolbar icon] and\nEnter your USER NAME and SERIAL CODE\nin the top-right corner of the Preferences window.")
      (Princ "To Register - Open SeptiCAD Preferences [PREF toolbar icon] and\nEnter your USER NAME and SERIAL CODE in the top-right corner of the Preferences window.")
      (setq $$RegButton "YES")
    )
  )

)
;;;;;;; end sDemoAlert











(defun GetUserVars (/ a fil tempList location)

  ;;;--- Set up an empty list
  (setq varList (list))  ; list of lines

  ;;;--- Open the text file
  (if (findfile (strcat $septicadPathText "sPref1.txt"))
    (progn
      (if (setq fil (open (findfile (strcat $septicadPathText "sPref1.txt")) "r"))
        (progn
          ;;;--- Read the lines of data in the text file
          (while (setq a (read-line fil))
            ;;;--- Add the data to the list
            (setq varList
                  (append
                    varList
                    (list a)
                  )
            )
          )
          ;;;--- Close the file.
          (close fil)
        )   ; end progn
      )     ; end if
    )       ; end progn
  )         ; end if
  ;;;--- Open the text file

  (if (findfile (strcat $septicadPathText "sPref2.txt"))
    (progn
      (if (setq fil (open (findfile (strcat $septicadPathText "sPref2.txt")) "r"))
        (progn
          ;;;--- Read the lines of data in the text file
          (while (setq a (read-line fil))
            ;;;--- Add the data to the list
            (setq varList
                  (append
                    varList
                    (list a)
                  )
            )
          )
          ;;;--- Close the file.
          (close fil)
        )   ; end progn
      )     ; end if
    )       ; end progn
  )         ; end if
  ;;;--- Open the text file

  (if (findfile (strcat $septicadPathText "sPref3.txt"))
    (progn
      (if (setq fil (open (findfile (strcat $septicadPathText "sPref3.txt")) "r"))
        (progn
          ;;;--- Read the lines of data in the text file
          (while (setq a (read-line fil))
            ;;;--- Add the data to the list
            (setq varList
                  (append
                    varList
                    (list a)
                  )
            )
          )
          ;;;--- Close the file.
          (close fil)
        )   ; end progn
      )     ; end if
    )       ; end progn
  )         ; end if


      ;;;--- REQUIRED --- Removes End of Line marker??
  (setq tempList (list))
  (foreach a varList
    (setq tempList (append tempList (list a)))
  )
  (setq varList tempList)

;;;;; variables stored in sPref1.txt
;0	(get_tile "USERNAME")
;1	(get_tile "SERIAL")
;2	(get_tile "NAME")
;3	(get_tile "NUMBER")
;4	(get_tile "PHONE")
;5	(get_tile "EMAIL")


  (setq $septicadUserName (nth 0 VarList))
  (setq $septicadSerialCode (nth 1 VarList))




;;;;;;;;;; skip these, as they are alternates ;;;;;;
;6	(get_tile "NAME2")
;7	(get_tile "NUMBER2")
;8	(get_tile "PHONE2")
;9	(get_tile "EMAIL2")
;10	(get_tile "NAME3")
;11	(get_tile "NUMBER3")
;12	(get_tile "PHONE3")
;13	(get_tile "EMAIL3")

;;;;; variables stored in sPref2.txt
;14	(get_tile "TEXT_SIZE_BIG")
;15	(get_tile "TEXT_SIZE_SMALL")
;16	(get_tile "TEXT_SIZE_LAT-LONG")
;17	(get_tile "TEXT_SIZE_HHE_BOT")
;18	(get_tile "TEXT_SIZE_HHE_TOP")
;19	(get_tile "TEXT_SIZE_MAP")
;20	(get_tile "SLABEL-TEXT")
;21	(get_tile "SLABEL-ARROW")
;22	(get_tile "TEXT_SIZE_MAPNOTE")
;23	(get_tile "ARROWSCALE")

;  legacy left over so i didnt need to renumber below
;	; page 1 fonts
;  (setq tsBig (atof (nth 14 varList)))
;  (setq tsSmall (atof (nth 15 varList)))
;  (setq tsLL (atof (nth 16 varList)))
;
;	; page 2/3 fonts
;  (setq tsBot (atof (nth 17 varList)))
;  (setq tsTop (atof (nth 18 varList)))

	; scale variable for model space fonts and arrow size
  (setq scaleMapText (atof (nth 19 varList)))

        ; slabel function options
  (setq sLabel-paperSpace-textSize (atof (nth 20 varList)))
  (setq sLabel-paperSpace-ArrowHeadSize (atof (nth 21 varList)))

         ; legacy compatablity
  (setq scaleProfileText scaleMapText)
  (setq scaleProfileNote (atof (nth 22 varList)))

        ;14 (get_tile "TEXT_SIZE_MAPNOTE") - assigned below
  (setq scaleArrowSize (atof (nth 23 varList)))


;;;;; variables stored in sPref3.txt
;24	(get_tile "PAGE3NOTES")
;25	(get_tile "PAGE3NOTES_LOCATION")
;26	(get_tile "PAGE3NOTES_WIDTH")
;27	(get_tile "PAGE2NOTES")
;28	(get_tile "PAGE2NOTES_LOCATION")
;29	(get_tile "PAGE2NOTES_WIDTH")

	;;;;page3NotesVars - location, % width, textScale value

  (if (= (nth 25 Varlist) "0") (setq location "TL"))
  (if (= (nth 25 Varlist) "1") (setq location "TR"))
  (if (= (nth 25 Varlist) "2") (setq location "BL"))
  (if (= (nth 25 Varlist) "3") (setq location "BR"))
  (if (= (nth 24 varList) "1") (setq page3NotesVars (list location (atof (nth 26 Varlist)) (atof (nth 22 varList)))))
  (if (= (nth 24 varList) "0") (setq page3NoteON "NO"))
  (if (= (nth 24 varList) "1") (setq page3NoteON "YES"))

	;;;;page2NotesVars - location, % width, textScale value
  (if (= (nth 28 Varlist) "0") (setq location "TL"))
  (if (= (nth 28 Varlist) "1") (setq location "TR"))
  (if (= (nth 28 Varlist) "2") (setq location "BL"))
  (if (= (nth 28 Varlist) "3") (setq location "BR"))

  (if (= (nth 27 varList) "1") (setq page2NotesVars (list location (atof (nth 29 Varlist)) (atof (nth 22 varList)))))
  (if (= (nth 27 varList) "0") (setq page2NoteON "NO"))
  (if (= (nth 27 varList) "1") (setq page2NoteON "YES"))



;;;;; variables stored in sPref3.txt
;30	(get_tile "VOLFUDGE")
;31	(get_tile "LOAMCOMP")
;32	(get_tile "SANDCOMP")
;33	(get_tile "DRAWSHOULDERS")
;34	(get_tile "CORNERLABELS")
;35	(get_tile "TRANSECT")
;36	(get_tile "TABLE")

	;;;;; Miscellaneous Stuff

	;;;; Gets values for Fudge Factor, compaction factors and subtract system volume....
  (setq volVar (list (nth 30 varList) (nth 31 varList) (nth 32 varList)))

  (if (= (nth 33 varList) "0") (setq shoulderPlan "NO"))
  (if (= (nth 33 varList) "1") (setq shoulderPlan "YES"))

  (if (= (nth 34 varList) "NULL") (SETQ $4CORNERS "NULL"))
  (if (= (nth 34 varList) "CORNERS") (SETQ $4CORNERS "CORNERS"))
  (if (= (nth 34 varList) "TABLE") (SETQ $4CORNERS "TABLE"))
  (if (= (nth 34 varList) "TABLEFILL") (SETQ $4CORNERS "TABLEFILL"))

  (if (= (nth 35 varList) "NO") (SETQ $LABELPROFILE "NO"))
  (if (= (nth 35 varList) "YES") (SETQ $LABELPROFILE "YES"))

  (if (= (nth 36 varList) "NO") (SETQ $LEVELTABLE "NO"))
  (if (= (nth 36 varList) "YES") (SETQ $LEVELTABLE "YES"))

;37	(get_tile "SYSTEM")
;38	(get_tile "SPACING")
;39	(get_tile "SPACING-DIM")


  (setq $defaultSystem (nth 37 varList))
  (setq $defaultSpacing (nth 38 varList))
  (setq $defaultSpacingDim (nth 39 varList))

;40	(get_tile "ALLCAPS")
  (if (= (nth 40 varList) "0") (setq $ALLCAPS "NO"))
  (if (= (nth 40 varList) "1") (setq $ALLCAPS "YES"))

;41	TANK ELEVATION CALC ON / OFF
  (if (= (nth 41 varList) "0") (setq $TANKELV "NO"))
  (if (= (nth 41 varList) "1") (setq $TANKELV "YES"))

;42	DIVERSION SWALE ON/OFF
  (if (= (nth 42 varList) "0") (setq $DSWALE "NO"))
  (if (= (nth 42 varList) "1") (setq $DSWALE "YES"))

;43	FILL CALCULATION NOTE PRINTED ON/OFF
  (if (= (nth 43 varList) "0") (setq $FILLNOTE "NO"))
  (if (= (nth 43 varList) "1") (setq $FILLNOTE "YES"))

;44	display swing tie dialog on contiune design
  (if (= (nth 44 varList) "0") (setq $SWING "NO"))
  (if (= (nth 44 varList) "1") (setq $SWING "YES"))

;45	display note on cross section, top left corner
  (if (= (nth 45 varList) "0") (setq $NOTES-PRO "NO"))
  (if (= (nth 45 varList) "1") (setq $NOTES-PRO "YES"))

;42	DIVERSION SWALE ON/OFF
  (if (= (nth 46 varList) "0") (setq $AUTOSCAR "NO"))
  (if (= (nth 46 varList) "1") (setq $AUTOSCAR "YES"))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







(defun c:sUser (/ slopeList f usrDia existing tempList)

	;Sets command echo off
  (SETVAR "CMDECHO" 0)

        ; check to see if user info has changed decision made at end of function
  (setq existing $septicadFullVersion)

  (PRINC "\nEditing SeptiCAD User Information............ \n")

	  ;;;--- Load the dcl file
  (setq dcl_id (load_dialog (strcat $septicadPathDCL "USERVARS.dcl")))

	  ;;;--- Load the dialog definition if it is not already loaded
  (if (not (new_dialog "USERVARS" dcl_id))
    (progn
      (alert "The uservars.DCL file could not be loaded!")
      (exit)
    )
  )
          ;;;--- Add the label names to the dialog box
  (start_list "PAGE3NOTES_LOCATION" 3)
  (mapcar 'add_list (list "Top Left" "Top Right" "Bottom Left" "Bottom Right"))
  (end_list)
          ;;;--- Add the label names to the dialog box
  (start_list "PAGE2NOTES_LOCATION" 3)
  (mapcar 'add_list (list "Top Left" "Top Right" "Bottom Left" "Bottom Right"))
  (end_list)

  (start_list "SPACING" 3)
  (mapcar 'add_list (list "Center to Center" "Edge to Edge"))
  (end_list)

  (if (= sysList nil) (createSysList))
  (setq tempList (list))
  (foreach item sysList
    (setq tempList (append tempList (list (cdr item))))
  )

  (start_list "SYSTEM" 3)
  (mapcar 'add_list tempList)
  (end_list)

  (start_list "SPACING-DIM" 3)
  (mapcar 'add_list (list "Center to Center" "Edge to Edge" "Both"))
  (end_list)

  (start_list "CORNERLABELS" 3)
  (mapcar 'add_list (list "Do Not Use" "Existing Grade Elevations on Corners" "Existing Grade Elevations Table" "Existing Grade Elevations Table & Depth of Fill"))
  (end_list)


	  ;;;--- If an action event occurs, do this function
  (action_tile "cancel" "(setq usrDia 1)(done_dialog)")
  (action_tile "okay" "(setq usrDia 2)(saveUserVars)(done_dialog)")
  (action_tile "mapLabels" "(setq usrDia 3)(done_dialog)")
  (action_tile "mapNotes" "(setq usrDia 4)(done_dialog)")
  (action_tile "systemCustom" "(setq usrDia 5)(done_dialog)")
  (action_tile "blocks" "(setq usrDia 6)(done_dialog)")
  (action_tile "hatches" "(setq usrDia 7)(done_dialog)")


	;;;--- First we need a slide name 
  (setq mySlideName (strcat $septicadPathDCL "customize.sld"))

	;;;--- Second we neesytemCustomd the key to the image control 
  (setq myKey "customize")

	;;;--- Next we send the slide name and the key to the update function 
  (upDateImage mySlideName myKey)


	; sets the users DCL up
  (resetUserDCL)

	  ;;;--- Display the dialog box
  (start_dialog)

	  ;;;--- Unload the dialog box
  (unload_dialog dcl_id)


	  ;;;--- If the user pressed the Cancel button
  (if (= usrDia 1)
    (PRINC "\nABORT - SeptiCAD User Info NOT Saved - ")
  )

	  ;;;--- If the user pressed the CONTINUE button
  (if (= usrDia 2)
    (progn


      ;;;;;;; the order of the list for output
      ;0	(get_tile "USERNAME")
      ;1	(get_tile "SERIAL")
      ;2	(get_tile "NAME")
      ;3	(get_tile "NUMBER")
      ;4	(get_tile "PHONE")
      ;5	(get_tile "EMAIL")
      ;6	(get_tile "NAME2")
      ;7	(get_tile "NUMBER2")
      ;8	(get_tile "PHONE2")
      ;9	(get_tile "EMAIL2")
      ;10	(get_tile "NAME3")
      ;11	(get_tile "NUMBER3")
      ;12	(get_tile "PHONE3")
      ;13	(get_tile "EMAIL3")

      ;14	(get_tile "TEXT_SIZE_BIG")        ;;;;;;;; cut out for new 4 page form
      ;15	(get_tile "TEXT_SIZE_SMALL")      ;;;;;;;; cut out for new 4 page form
      ;16	(get_tile "TEXT_SIZE_LAT-LONG")   ;;;;;;;; cut out for new 4 page form
      ;17	(get_tile "TEXT_SIZE_HHE_BOT")    ;;;;;;;; cut out for new 4 page form
      ;18	(get_tile "TEXT_SIZE_HHE_TOP")    ;;;;;;;; cut out for new 4 page form
      
      ;19	(get_tile "TEXT_SIZE_MAP")
      ;20	(get_tile "SLABEL-TEXT")
      ;21	(get_tile "SLABEL-ARROW")
      ;22	(get_tile "TEXT_SIZE_MAPNOTE")
      ;23	(get_tile "ARROWSCALE")
      ;	
      ;24	(get_tile "PAGE3NOTES")
      ;25	(get_tile "PAGE3NOTES_LOCATION")
      ;26	(get_tile "PAGE3NOTES_WIDTH")
      ;27	(get_tile "PAGE2NOTES")
      ;28	(get_tile "PAGE2NOTES_LOCATION")
      ;29	(get_tile "PAGE2NOTES_WIDTH")
      ;30	(get_tile "VOLFUDGE")
      ;31	(get_tile "LOAMCOMP")
      ;32	(get_tile "SANDCOMP")
      ;33	(get_tile "DRAWSHOULDERS")
      ;34	(get_tile "CORNERLABELS")**************
      ;35	(get_tile "TRANSECT")****************** different output/input types
      ;36	(get_tile "TABLE")*********************
      ;37	(get_tile "SYSTEM")
      ;38	(get_tile "SPACING")
      ;39	(get_tile "SPACING-DIM")
      ;40	(get_tile "ALLCAPS")
      ;41	(get_tile "TANKELV")
      ;42	(get_tile "DIVERT")
      ;43	(get_tile "FILLCALC")
      ;44	SWING
      ;45 	NOTES-PRO ; note on profile section
      ;46	$AUTOSCAR ; 

      ;;;--- Open the sPref1.txt
      (setq f (open (strcat $septicadPathText "sPref1.txt") "w"))
      (write-line (nth 0 newVarList) f)
      (write-line (nth 1 newVarList) f)
      (write-line (nth 2 newVarList) f)
      (write-line (nth 3 newVarList) f)
      (write-line (nth 4 newVarList) f)
      (write-line (nth 5 newVarList) f)
      (write-line (nth 6 newVarList) f)
      (write-line (nth 7 newVarList) f)
      (write-line (nth 8 newVarList) f)
      (write-line (nth 9 newVarList) f)
      (write-line (nth 10 newVarList) f)
      (write-line (nth 11 newVarList) f)
      (write-line (nth 12 newVarList) f)
      (write-line (nth 13 newVarList) f)
      (close f)

      ;;;--- Open the sPref2.txt
      (setq f (open (strcat $septicadPathText "sPref2.txt") "w"))
      (write-line (nth 14 newVarList) f)
      (write-line (nth 15 newVarList) f)
      (write-line (nth 16 newVarList) f)
      (write-line (nth 17 newVarList) f)
      (write-line (nth 18 newVarList) f)
      (write-line (nth 19 newVarList) f)
      (write-line (nth 20 newVarList) f)
      (write-line (nth 21 newVarList) f)
      (write-line (nth 22 newVarList) f)
      (write-line (nth 23 newVarList) f)
      (close f)

      ;;;--- Open the sPref3.txt
      (setq f (open (strcat $septicadPathText "sPref3.txt") "w"))
      (write-line (nth 24 newVarList) f)
      (write-line (nth 25 newVarList) f)
      (write-line (nth 26 newVarList) f)
      (write-line (nth 27 newVarList) f)
      (write-line (nth 28 newVarList) f)
      (write-line (nth 29 newVarList) f)
      (write-line (nth 30 newVarList) f)
      (write-line (nth 31 newVarList) f)
      (write-line (nth 32 newVarList) f)
      (write-line (nth 33 newVarList) f)

      (if (= (nth 34 newvarList) "0") (write-line "NULL" f))
      (if (= (nth 34 newvarList) "1") (write-line "CORNERS" f))
      (if (= (nth 34 newvarList) "2") (write-line "TABLE" f))
      (if (= (nth 34 newvarList) "3") (write-line "TABLEFILL" f))

      (if (= (nth 35 newvarList) "0") (write-line "NO" f))
      (if (= (nth 35 newvarList) "1") (write-line "YES" f))

      (if (= (nth 36 newvarList) "0") (write-line "NO" f))
      (if (= (nth 36 newvarList) "1") (write-line "YES" f))

      (write-line (nth 37 newVarList) f)
      (write-line (nth 38 newVarList) f)
      (write-line (nth 39 newVarList) f)
      (write-line (nth 40 newVarList) f)
      (write-line (nth 41 newVarList) f)
      (write-line (nth 42 newVarList) f)
      (write-line (nth 43 newVarList) f)
      (write-line (nth 44 newVarList) f)
      (write-line (nth 45 newVarList) f)
      (write-line (nth 46 newVarList) f)
      (close f)

      ;;;--- Resets Uservars in Memory
      (septicadCheckReg)

      (princ "\n\nSeptiCAD User Info Saved - ")
    )
  )


	  ;;;--- If the user pressed the mapLabels Button
  (if (= usrDia 3)
    (progn
      (sLabelCustom)
      (c:suser)
    )
  )

	  ;;;--- If the user pressed the mapNotes Button
  (if (= usrDia 4)
    (progn
      (pageNotesCustom)
      (c:suser)
    )
  )

	  ;;;--- If the user pressed the systemCustom Button
  (if (= usrDia 5)
    (progn
      (customSystem)
      (c:suser)
    )
  )

	  ;;;--- If the user pressed the sblock custom Button
  (if (= usrDia 6)
    (progn
      (sblockCustom)
      (c:suser)
    )
  )

	  ;;;--- If the user pressed the sHatch custom Button
  (if (= usrDia 7)
    (progn
      (sHatchCustom)
      (c:suser)
    )
  )


   ; Feedback to command line
  (if (/= $septicadFullVersion 1) (princ "Trial Version - UNREGISTERED"))
  (if (= $septicadFullVersion 1) (princ "Full Version - REGISTERED"))

   ; If status of Trial/Registered changed then Alert
  (if (/= existing $septicadFullVersion)
    (progn
      (if (/= $septicadFullVersion 1) (Alert "SeptiCAD Trial Version - UNREGISTERED"))
      (if (= $septicadFullVersion 1) (Alert "SeptiCAD Full Version - REGISTERED"))
    )
  )


  ;;;---- set some variables to nil
  (setq newVarList nil)

  ;;;--- Suppress the last echo for a clean exit
  (princ)

)
; END FUNCTION (sUser)

(defun saveUserVars ()
  
  ;;; (get_tile "TEXT_SIZE_MAP") repeated 5 times before it is called for real
  ;;; this is a work around for old code
  (setq newVarList (list
                     (get_tile "USERNAME")
                     (get_tile "SERIAL")
                     (get_tile "NAME")
                     (get_tile "NUMBER")
                     (get_tile "PHONE")
                     (get_tile "EMAIL")
                     (get_tile "NAME2")
                     (get_tile "NUMBER2")
                     (get_tile "PHONE2")
                     (get_tile "EMAIL2")
                     (get_tile "NAME3")
                     (get_tile "NUMBER3")
                     (get_tile "PHONE3")
                     (get_tile "EMAIL3")
                     
                     (get_tile "TEXT_SIZE_MAP")
                     (get_tile "TEXT_SIZE_MAP")
                     (get_tile "TEXT_SIZE_MAP")
                     (get_tile "TEXT_SIZE_MAP")
                     (get_tile "TEXT_SIZE_MAP")

                     (get_tile "TEXT_SIZE_MAP")
                     (get_tile "SLABEL-TEXT")
                     (get_tile "SLABEL-ARROW")
                     (get_tile "TEXT_SIZE_MAPNOTE")
                     (get_tile "ARROWSCALE")

                     (get_tile "PAGE3NOTES")
                     (get_tile "PAGE3NOTES_LOCATION")
                     (get_tile "PAGE3NOTES_WIDTH")
                     (get_tile "PAGE2NOTES")
                     (get_tile "PAGE2NOTES_LOCATION")
                     (get_tile "PAGE2NOTES_WIDTH")
                     (get_tile "VOLFUDGE")
                     (get_tile "LOAMCOMP")
                     (get_tile "SANDCOMP")
                     (get_tile "DRAWSHOULDERS")
                     (get_tile "CORNERLABELS")
                     (get_tile "TRANSECT")
                     (get_tile "TABLE")
                     (get_tile "SYSTEM")
                     (get_tile "SPACING")
                     (get_tile "SPACING-DIM")
                     (get_tile "ALLCAPS")
                     (get_tile "TANKELV")
                     (get_tile "DIVERT")
                     (get_tile "FILLCALC")
                     (get_tile "SWING")
                     (get_tile "NOTES-PRO")
                     (get_tile "AUTO-SCAR")
                   )
  )

)
; end saveUserVars


(defun resetUserDCL ()
	; check to see if registered
  (septicadCheckReg)

	; function creates a single list based on 3 text files
  (getUserVars)

  (set_tile "USERNAME" (nth 0 VarList))
  (set_tile "SERIAL" (nth 1 VarList))
  (set_tile "NAME" (nth 2 varList))
  (set_tile "NUMBER" (nth 3 varList))
  (set_tile "PHONE" (nth 4 varList))
  (set_tile "EMAIL" (nth 5 varList))

  (set_tile "NAME2" (nth 6 varList))
  (set_tile "NUMBER2" (nth 7 varList))
  (set_tile "PHONE2" (nth 8 varList))
  (set_tile "EMAIL2" (nth 9 varList))

  (set_tile "NAME3" (nth 10 varList))
  (set_tile "NUMBER3" (nth 11 varList))
  (set_tile "PHONE3" (nth 12 varList))
  (set_tile "EMAIL3" (nth 13 varList))
;
;  (set_tile "TEXT_SIZE_BIG" (nth 14 VarList))
;  (set_tile "TEXT_SIZE_SMALL" (nth 15 VarList))
;  (set_tile "TEXT_SIZE_LAT-LONG" (nth 16 VarList))
;  (set_tile "TEXT_SIZE_HHE_BOT" (nth 17 VarList))
;  (set_tile "TEXT_SIZE_HHE_TOP" (nth 18 VarList))
;  
  (set_tile "TEXT_SIZE_MAP" (nth 19 VarList))
  (set_tile "SLABEL-TEXT" (nth 20 VarList))
  (set_tile "SLABEL-ARROW" (nth 21 VarList))
  (set_tile "TEXT_SIZE_MAPNOTE" (nth 22 VarList))
  (set_tile "ARROWSCALE" (nth 23 VarList))

  (set_tile "PAGE3NOTES" (nth 24 VarList))
  (set_tile "PAGE3NOTES_LOCATION" (nth 25 VarList))
  (set_tile "PAGE3NOTES_WIDTH" (nth 26 VarList))
  (set_tile "PAGE2NOTES" (nth 27 VarList))
  (set_tile "PAGE2NOTES_LOCATION" (nth 28 VarList))
  (set_tile "PAGE2NOTES_WIDTH" (nth 29 VarList))

  (set_tile "VOLFUDGE" (nth 30 VarList))
  (set_tile "LOAMCOMP" (nth 31 VarList))
  (set_tile "SANDCOMP" (nth 32 VarList))
  (set_tile "DRAWSHOULDERS" (nth 33 VarList))

  (if (= (nth 34 varList) "NULL") (SET_TILE "CORNERLABELS" "0"))
  (if (= (nth 34 varList) "CORNERS") (SET_TILE "CORNERLABELS" "1"))
  (if (= (nth 34 varList) "TABLE") (SET_TILE "CORNERLABELS" "2"))
  (if (= (nth 34 varList) "TABLEFILL") (SET_TILE "CORNERLABELS" "3"))

  (if (= (nth 35 varList) "NO") (SET_TILE "TRANSECT" "0"))
  (if (= (nth 35 varList) "YES") (SET_TILE "TRANSECT" "1"))

  (if (= (nth 36 varList) "NO") (SET_TILE "TABLE" "0"))
  (if (= (nth 36 varList) "YES") (SET_TILE "TABLE" "1"))

  (set_tile "SYSTEM" (nth 37 VarList))
  (set_tile "SPACING" (nth 38 VarList))
  (set_tile "SPACING-DIM" (nth 39 VarList))
  (set_tile "ALLCAPS" (nth 40 VarList))
  (set_tile "TANKELV" (nth 41 VarList))
  (set_tile "DIVERT" (nth 42 VarList))
  (set_tile "FILLCALC" (nth 43 VarList))
  (set_tile "SWING" (nth 44 VarList))
  (set_tile "NOTES-PRO" (nth 45 VarList))
  (set_tile "AUTO-SCAR" (nth 46 VarList))


)           
; END resetUserDCL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
