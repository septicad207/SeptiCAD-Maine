;; this is the code for the soil log tool


(defun getSoillogDefaults (/ fil a formList count)

  (setq soilDefaultList (list))

	;;;--- Open the text file
  (if (findfile (strcat $septicadPathText "soillog_defaults.txt"))
    (progn

      (if (setq fil (open (findfile (strcat $septicadPathText "soillog_defaults.txt")) "r"))
        (progn

          ;;;--- Skip the first line        
          (read-line fil)

          ;;;--- Read the lines of data in the text file
          (while (setq a (read-line fil))
            ;;;--- Add the data to the list
            (setq soilDefaultList
                  (append
                    soilDefaultList
                    (list a)
                  )
            )
          )
          ;;;--- Close the file.
          (close fil)
        )
      )
    )
  )
	;;;--- REQUIRED --- Removes End of Line marker??
  (setq formList (list))
  (foreach a soilDefaultList
    (setq formList (append formList (list a)))
  )

  (setq soilDefaultList formList)
;(print "Soil List Defaults - On c:log START")
;                (setq count 1)
;		(foreach item soilDefaultList
;                  (print (strcat "Profile " (rtos count 2 0) " -> " item))
;                  (setq count (+ count 1))
;                )
;(print "Soil List Default - On c:log END")

)
;;;;;;;;;;;;;;;;end function


(defun soilDefaultUpdate (/ newLine tileList retList count)

  (setq tileList (list "s1a" "s1c" "s1d" "s2a" "s2c" "s2d" "s3a" "s3c" "s3d"))

  ; defines new line
  (setq newLine "")
  (foreach item tilelist
    (setq newLine (strcat newLine (get_tile item) " "))
  )
  ; removes last " "
  (setq newline (substr newline 1 (- (strlen newline) 1)))


  ; soilDefaultList with nine lists for profile 1-9
  (setq count 1)
  (setq retList (list))
  (foreach item soilDefaultList
    (progn
      (if (= (get_tile "profile") (rtos count 2 0))
        (progn
          (setq retList (append retList (list newLine)))
        )
        (progn
          (setq retList (append retList (list item)))
        )
      )
      (setq count (+ count 1))
    )
  )
  (setq soilDefaultList retList)
  (Princ (strcat "\nProfile " (get_tile "profile") " Default Set."))
  (princ)
)
;;;;;;;;


(defun saveSoilDefaults (/ count)

    ;;;--- Open the text file
  (if (findfile (strcat $septicadPathText "soillog_defaults.txt"))
    (progn

      (if (setq fil (open (findfile (strcat $septicadPathText "soillog_defaults.txt")) "w"))
        (progn

          ;;;--- Skip the first line        
          (write-line ": --- Defaults used to autofill soil log menus for Profile 1 to 9, used default button to set" fil)

          ;;;--- Read the lines of data in the text file
          (foreach item soilDefaultList
            (write-line item fil)
          )

          ;;;--- Close the file.
          (close fil)
        )
      )
    )
  )

)

;;;;;;;;;;;;;;;;;;;;;;;;

(defun soilAutoFill (/ prof altStr tileList retList readList count)

; Start big IF/ELSE
  (if (or
        (= (get_tile "profile") "1")
        (= (get_tile "profile") "2")
        (= (get_tile "profile") "3")
        (= (get_tile "profile") "4")
        (= (get_tile "profile") "5")
        (= (get_tile "profile") "6")
        (= (get_tile "profile") "7")
        (= (get_tile "profile") "8")
        (= (get_tile "profile") "9")
      )

; START BIG IF
    (progn
      ; define soilDefaultList variable with nine lists for profile 1-9
      (if (= soilDefaultList nil) (getSoillogDefaults))

      ;;;--- Setup a list to hold the selected items
      (setq retList (list))

      ;;;--- Save the list setting
      (setq readlist (nth (- (atoi (get_tile "profile")) 1) soilDefaultList))

      ;;; convert line of space deliminated values to list of values
      (setq retlist (ST_str2lst readlist " "))

      ; cycle thru tileList and set tiles with retList value
      (setq tileList (list "s1a" "s1c" "s1d" "s2a" "s2c" "s2d" "s3a" "s3c" "s3d"))

      (setq count 0)
      (foreach item tileList
        (progn
          (set_tile item (nth count retList))
          (setq count (+ count 1))
        )
      )

      ; turn set default button tiles on
      (mode_tile "default_text" 0)
      (mode_tile "default_button" 0)
      (set_tile "default_text" (strcat "Set Default Profile " (get_tile "profile")))

      ; END BIG IF
    )
    ; START ELSE
    (progn
      (princ "\nDefaults are only stored for Soil Profiles 1 to 9.  To quickly fill the form enter\nprofile that best matchs, then use other designation. (i.e.; 12, 5/12, etc...)")
      ; turn set default tiles off
      (mode_tile "default_text" 1)
      (mode_tile "default_button" 1)
      (set_tile "default_text" "Set Default Profile ???")
    )
    ; END ELSE
  )
; end big IF/ELSE


; end program
)


(defun checkMottle ()

	; if mottle1 field is entered then ungrey mottle2 and mottle2_depth
  (if (AND (/= (get_tile "mottle1") "0") (/= (get_tile "mottle1_depth") ""))
    (progn
      (mode_tile "mottle2" 0)        ; on
      (mode_tile "mottle2_depth" 0)  ; on
    )
  )

	; if mottle1 and mottle1_depth are blank
  (if (AND (= (get_tile "mottle1") "0") (= (get_tile "mottle1_depth") ""))
    (progn
      (mode_tile "mottle2" 1)        ; off
      (mode_tile "mottle2_depth" 1)  ; off
    )
  )


)  ; end checkMottle


(defun insert_line (blockName point)

 ;sets block insert unit vars
  (setBlockUnitsVars)

  (COMMAND "-insert" blockName point "1" "1" "0")

 ;reset block insert unit vars
  (resetBlockUnitsVars)

)

(defun insert_text (point1 point2 string / adjustPt)
  (vl-load-com)
 ; adjust locations of points by adjustPt
  (setq adjustPt 0.07)
  (setq point1 (polar point1 0.0 adjustPt))
  (setq point2 (polar point2 pi adjustPt))

 ; create mText entity
  (command "-mtext" point1 "J" "MC" "H" t_size point2 string "")

    ; if Bricscad v9 or higher or AutoCAD add background mask
  (if (or
        (and
          (= (strcase (getvar "PRODUCT")) "BRICSCAD")
          (> (atof (substr (getvar "_VERNUM") 1 2)) 8)
        )
          (= (strcase (getvar "PRODUCT")) "AUTOCAD")
      )
    (progn
      ; toggle background mask on
      (setq obj (vlax-ename->vla-object (entlast)))
      (vla-put-BackgroundFill obj :VLAX-TRUE)
      (vla-update obj)

      ; set background mask width
      (setq sset1 (ssadd))
      (ssadd (entlast) sset1)
      (modifySS sset1 45 1.5)       ; text mask offset
      (modifySS sset1 46 0)         ; text mask in v12 based on TL/BR - reset back to 0 to just put around text
    )
  )

)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun checkSoilLog (/ alertString)
  (setq alertString "")

  (if (= hole "") (setq alertString (strcat alertString "[Hole #] ")))

  (if (= prof "") (setq alertString (strcat alertString "[Profile] ")))

  (if (= condition "") (setq alertString (strcat alertString "[Class] ")))

  (if (= lim "") (setq alertString (strcat alertString "[Depth to Limiting Factor] ")))

  (if (/= alertString "") (alert (strcat "The " alertString "fields are required!!!")))
  (if (= alertString "") (done_dialog))

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;









(defun soil_log_draw (side / temp point t_size 
                 unitX unitY maxY 
                 XY xy4 x2y x3y x4y x2y4 x3y4 x4y4 x5y4 xy2 xy3 x2y2 x2y3 x3y2 x3y3 x4y2 x4y3 x5y3 x5y2
                 master_list
                 ref_def)


  (setq ANGDIR_DEF (GETVAR "ANGDIR"))(setvar "ANGDIR" 0)
  (SETQ ANGBASE_DEF (GETVAR "ANGBASE"))(setvar "ANGBASE" 0)

  (setBlockUnitsVars)             ;sets block insert unit vars
  (setq ex (getvar "Expert"))     ; suppresses "Block already defined. Redefined it? ALERT"	
  (setvar "EXPERT" 2)
    
  (setq t_size 0.08)                ; text size for Log
  (setq unitY 0.06850625)                 ; distance represents 1 inch on log
  (setq unitX 0.8479)                  ; distance between column
  (setq maxY 48.0)                  ; max depth of log
  
    (if (tblsearch "block" "SOILLOG_LINE")
      (setq soilLineBlock "SOILLOG_LINE")                                             ; if
      (setq soilLineBlock (strcat $septicadPathBlockRequired "SOILLOG_LINE.dwg"))     ; else
    )
      
    (if (tblsearch "block" "CHECKBOX_CHECKED")
      (setq checkBoxBlock "CHECKBOX_CHECKED")                                             ; if
      (setq checkBoxBlock (strcat $septicadPathBlockRequired "CHECKBOX_CHECKED.DWG"))     ; else
    )
  
  (setq master_list (get_HHE-200_form_hidden (getvar "CTAB")))

 
  
  (if (= side "LEFT")
     (progn
        (setq XY (car (nth 2 (ret_index "SLA_TOP_LEFT" master_list))))
        
        (if (/= hole "")  (create_mtext (ret_index "SLA1" master_list) hole))
        (if (/= org "")  (create_mtext (ret_index "SLA2" master_list) org))
        (if (/= prof "")  (create_mtext (ret_index "SLA5" master_list) prof))
        (if (/= condition "")  (create_mtext (ret_index "SLA6" master_list) condition))
        (if (/= slop "")  (create_mtext (ret_index "SLA7" master_list) slop))
        (if (/= lim "")  (create_mtext (ret_index "SLA8" master_list) (strcat lim "\"")))
        (if (/= elv "")  (create_mtext (ret_index "SLA3" master_list) elv))
        (if ex_lim (create_mtext (ret_index "SLA4" master_list) (rtos ex_lim 2 0)))        
        
        
        
        

        (if (= lim_type 1)(COMMAND "-insert" checkBoxBlock (car (nth 2 (ret_index "SLA9" master_list))) "1" "1" "0"))
        (if (= lim_type 2)(COMMAND "-insert" checkBoxBlock (car (nth 2 (ret_index "SLA10" master_list))) "1" "1" "0"))
        (if (= lim_type 3)(COMMAND "-insert" checkBoxBlock (car (nth 2 (ret_index "SLA11" master_list))) "1" "1" "0"))
        (if (= lim_type 4)(COMMAND "-insert" checkBoxBlock (car (nth 2 (ret_index "SLA12" master_list))) "1" "1" "0"))
        
        (if (= tptb 0) (COMMAND "-insert" checkBoxBlock (car (nth 2 (ret_index "SLA13" master_list))) "1" "1" "0"))
        (if (= tptb 1) (COMMAND "-insert" checkBoxBlock (car (nth 2 (ret_index "SLA14" master_list))) "1" "1" "0"))
     )
  )

  
  (if (= side "RIGHT")
     (progn
        (setq XY (car (nth 2 (ret_index "SLB_TOP_LEFT" master_list))))
        
        (if (/= hole "")  (create_mtext (ret_index "SLB1" master_list) hole))
        (if (/= org "")  (create_mtext (ret_index "SLB2" master_list) org))
        (if (/= prof "")  (create_mtext (ret_index "SLB5" master_list) prof))
        (if (/= condition "")  (create_mtext (ret_index "SLB6" master_list) condition))
        (if (/= slop "")  (create_mtext (ret_index "SLB7" master_list) slop))
        (if (/= lim "")  (create_mtext (ret_index "SLB8" master_list) (strcat lim "\"")))
        (if (/= elv "")  (create_mtext (ret_index "SLB3" master_list) elv))
        (if ex_lim (create_mtext (ret_index "SLB4" master_list) (rtos ex_lim 2 0)))       

        (if (= lim_type 1)(COMMAND "-insert" checkBoxBlock (car (nth 2 (ret_index "SLB9" master_list))) "1" "1" "0"))
        (if (= lim_type 2)(COMMAND "-insert" checkBoxBlock (car (nth 2 (ret_index "SLB10" master_list))) "1" "1" "0"))
        (if (= lim_type 3)(COMMAND "-insert" checkBoxBlock (car (nth 2 (ret_index "SLB11" master_list))) "1" "1" "0"))
        (if (= lim_type 4)(COMMAND "-insert" checkBoxBlock (car (nth 2 (ret_index "SLB12" master_list))) "1" "1" "0"))
        
        (if (= tptb 0) (COMMAND "-insert" checkBoxBlock (car (nth 2 (ret_index "SLB13" master_list))) "1" "1" "0"))
        (if (= tptb 1) (COMMAND "-insert" checkBoxBlock (car (nth 2 (ret_index "SLB14" master_list))) "1" "1" "0"))

     )
  )

  ;; now draw the soil log

  (setq x2y (polar XY 0.0 unitX))                          ; TL - Consistency
  (setq x3y (polar x2y 0.0 unitX))                         ; TL Color
  (setq x4y (polar x3y 0.0 unitX))                         ; TL Mottling

  (setq xy4 (polar xy (/ (* 3.0 pi) 2) (* maxY unitY)))    ; BL - Log/Texture
  (setq x2y4 (polar x2y (/ (* 3.0 pi) 2) (* maxY unitY)))  ; BL - Consistency
  (setq x3y4 (polar x3y (/ (* 3.0 pi) 2) (* maxY unitY)))  ; BL - Color
  (setq x4y4 (polar x4y (/ (* 3.0 pi) 2) (* maxY unitY)))  ; BL - Mottling
  (setq x5y4 (polar x4y4 0.0 unitX))                       ; BR - Log
  

  (if (= ex_lim 0)
    (progn
      (setq ref_def (polar xy (/ (* 3.0 pi) 2) (* unitY maxY)))
    )
  )
  (if (> ex_lim 0)
    (progn
      (setq ref_def (polar xy (/ (* 3.0 pi) 2) (* unitY ex_lim)))
    )
  )

  (if (and (> ex_lim 0) (<= ex_lim 46))
    (progn
      ; Bedrock or refusal ref_def set above
      (setq ref_txt (strcat ex_why " " (rtos ex_lim 2 0) " inches"))
      (COMMAND "-insert" soilLineBlock ref_def "4" "1" "0")
      (insert_text ref_def x5y4 ref_txt)

      ; set mText entity width to 0.0
      (setq sset1 (ssadd))
      (ssadd (entlast) sset1)
      (modifySS sset1 41 0.0)

    )
  )
  (if (> ex_lim 46)
    (progn
      ; Bedrock or refusal ref_def set above
      (setq ref_txt (strcat ex_why " " (rtos ex_lim 2 0) " inches"))
      (setq point (polar xy (/ (* 3.0 pi) 2) (* unitY maxY)))
      (setq point (polar point (/ pi 2.0) (* unitY 2)))
      (insert_text point x5y4 ref_txt)

      ; set mText entity width to 0.0
      (setq sset1 (ssadd))
      (ssadd (entlast) sset1)
      (modifySS sset1 41 0.0)

      ; set ref_def to bottom of page for formatting where text goes
      (setq ref_def (polar xy (/ (* 3.0 pi) 2) (* unitY maxY)))
    )
  )


  (if (or (and (= s1b 0) (= s2b 0)) (and (/= s1b 0) (/= s2b 0)))
    (progn

      ; Soil type 1
      (if (= s1b 0)
        (progn
          (if (= ex_lim 0)
            (progn
              (setq point (polar xy (/ (* 3 pi) 2) (* maxY unitY)))
              (setq point (polar point 0.0 unitX))
              (insert_text xy point s1a)
            ) 
          ) 
          (if (and (> ex_lim 0) (<= ex_lim 48))
            (progn
              (setq point (polar xy (/ (* 3 pi) 2) (* ex_lim unitY)))
              (setq point (polar point 0.0 unitX))
              (insert_text xy point s1a)
            ) 
          ) 
          (if (> ex_lim 48)
            (progn
              (setq point (polar xy (/ (* 3 pi) 2) (* maxY unitY)))
              (setq point (polar point 0.0 unitX))
              (insert_text xy point s1a)
            ) 
          ) 


        ) 
      )

      (if (/= s1b 0)
        (progn
          (setq xy2 (polar XY (/ (* 3.0 pi) 2) (* unitY s1b)))
          (setq x2y2 (polar xy2 0.0 unitX))
          (insert_text xy x2y2 s1a)
          (insert_line soilLineBlock xy2)
        )
      )

      ; Soil type 2
      (if (/= s1b 0)
        (progn
          (setq xy3 (polar xy (/ (* 3.0 pi) 2) (* unitY s2b)))
          (setq x2y3 (polar xy3 0.0 unitX))
          (insert_text xy2 x2y3 s2a)
          (insert_line soilLineBlock xy3)
        )               
      )                

      ; Soil type 3
      (if (/= s2b 0)
        (progn
          (if (< (cadr ref_def) (cadr x2y4))
            (progn
              (insert_text xy3 x2y4 s3a)
            )
          )
          (if (>= (cadr ref_def) (cadr x2y4))
            (progn
              (setq point (list (car x2y4) (cadr ref_def)))
              (insert_text xy3 point s3a)
            )
          )
        )               
      )                


      ; Soil Consistency 1
      (if (= s1b 0)
        (progn
          (if (= ex_lim 0)
            (progn
              (setq point (polar x2y (/ (* 3 pi) 2) (* maxY unitY)))
              (setq point (polar point 0.0 unitX))
              (insert_text x2y point s1c)
            )         
          )           
          (if (and (> ex_lim 0) (<= ex_lim 48))
            (progn
              (setq point (polar x2y (/ (* 3 pi) 2) (* ex_lim unitY)))
              (setq point (polar point 0.0 unitX))
              (insert_text x2y point s1c)
            )           
          )           
          (if (> ex_lim 48)
            (progn
              (setq point (polar x2y (/ (* 3 pi) 2) (* maxY unitY)))
              (setq point (polar point 0.0 unitX))
              (insert_text x2y point s1c)
            )           
          )              
        )              
      )                


      (if (/= s1b 0)
        (progn
          (setq x2y2 (polar x2y (/ (* 3.0 pi) 2) (* unitY s1b)))
          (setq x3y2 (polar x2y2 0.0 unitX))
          (insert_text x2y x3y2 s1c)
          (insert_line soilLineBlock x2y2)
        )               
      )                

      ; Soil Consistency 2
      (if (/= s1b 0)
        (progn
          (setq x2y3 (polar x2y (/ (* 3.0 pi) 2) (* unitY s2b)))
          (setq x3y3 (polar x2y3 0.0 unitX))
          (insert_text x2y2 x3y3 s2c)
          (insert_line soilLineBlock x2y3)
        )              
      )                

      ; Soil Consistency 3
      (if (/= s2b 0)
        (progn
          (if (< (cadr ref_def) (cadr x3y4))
            (progn
              (insert_text x2y3 x3y4 s3c)
            )
          )
          (if (>= (cadr ref_def) (cadr x3y4))
            (progn
              (setq point (list (car x3y4) (cadr ref_def)))
              (insert_text x2y3 point s3c)
            )
          )
        )              
      )                





      ; Soil Color 1
      (if (= s1b 0)
        (progn
          (if (= ex_lim 0)
            (progn
              (setq point (polar x3y (/ (* 3 pi) 2) (* maxY unitY)))
              (setq point (polar point 0.0 unitX))
              (insert_text x3y point s1d)
            )           
          )              
          (if (and (> ex_lim 0) (<= ex_lim 48))
            (progn
              (setq point (polar x3y (/ (* 3 pi) 2) (* ex_lim unitY)))
              (setq point (polar point 0.0 unitX))
              (insert_text x3y point s1d)
            )            
          )           
          (if (> ex_lim 48)
            (progn
              (setq point (polar x3y (/ (* 3 pi) 2) (* maxY unitY)))
              (setq point (polar point 0.0 unitX))
              (insert_text x3y point s1d)
            )            
          )             
        )                
      )
      
      (if (/= s1b 0)
        (progn
          (setq x3y2 (polar x3y (/ (* 3.0 pi) 2) (* unitY s1b)))
          (setq x4y2 (polar x3y2 0.0 unitX))
          (insert_text x3y x4y2 s1d)
          (insert_line soilLineBlock x3y2)
        )             
      )                 

      ; Soil Color 2
      (if (/= s1b 0)
        (progn
          (setq x3y3 (polar x3y (/ (* 3.0 pi) 2) (* unitY s2b)))
          (setq x4y3 (polar x3y3 0.0 unitX))
          (insert_text x3y2 x4y3 s2d)
          (insert_line soilLineBlock x3y3)
        )               
      )               

      ; Soil Color 3
      (if (/= s2b 0)
        (progn
          (if (< (cadr ref_def) (cadr x4y4))
            (progn
              (insert_text x3y3 x4y4 s3d)
            )
          )
          (if (>= (cadr ref_def) (cadr x4y4))
            (progn
              (setq point (list (car x4y4) (cadr ref_def)))
              (insert_text x3y3 point s3d)
            )
          )
        )           
      )              


    )                  
  )                  



  (if (and (/= s1b 0) (= s2b 0))
    (progn

      ; Soil type 1

      (setq xy2 (polar XY (/ (* 3.0 pi) 2) (* unitY s1b)))
      (setq x2y2 (polar xy2 0.0 unitX))
      (insert_text xy x2y2 s1a)
      (insert_line soilLineBlock xy2)

      ; Soil type 2


      (if (= ex_lim 0) (insert_text xy2 x2y4 s2a))
      (if (/= ex_lim 0) (insert_text xy2 (polar ref_def 0.0 unitX) s2a))

      ; Soil Consistency 1

      (setq x2y2 (polar x2y (/ (* 3.0 pi) 2) (* unitY s1b)))
      (setq x3y2 (polar x2y2 0.0 unitX))
      (insert_text x2y x3y2 s1c)
      (insert_line soilLineBlock x2y2)


      ; Soil Consistency 2


      (if (= ex_lim 0) (insert_text x2y2 x3y4 s2c))
      (if (/= ex_lim 0) (insert_text x2y2 (polar ref_def 0.0 (* unitX 2.0)) s2c))

      ; Soil Color 1

      (setq x3y2 (polar x3y (/ (* 3.0 pi) 2) (* unitY s1b)))
      (setq x4y2 (polar x3y2 0.0 unitX))
      (insert_text x3y x4y2 s1d)
      (insert_line soilLineBlock x3y2)


      ; Soil Color 2

      (if (= ex_lim 0) (insert_text x3y2 x4y4 s2d))
      (if (/= ex_lim 0) (insert_text x3y2 (polar ref_def 0.0 (* unitX 3.0)) s2d))


    )               
  )                  

  (if (/= mot1_d 0)
    (progn
      ; Mottle 1
      (setq x4y2 (polar x4y (/ (* 3.0 pi) 2) (* unitY mot1_d)))
      (setq x5y2 (polar x4y2 0.0 unitX))


      (if (/= mot2_d 0)  ; if Mot2 depth is entered
        (progn
          ; Mottle 2
          (setq x4y3 (polar x4y (/ (* 3.0 pi) 2) (* unitY mot2_d)))
          (setq x5y3 (polar x4y3 0.0 unitX))
          (insert_line soilLineBlock x4y3)
        )
      )

      ;;;;;Put in something about ref_def
      ; if ref def not defined 
      (if (and (= mot2_d 0) (= ex_lim 0))  ; if Mot2 depth and ex_lim not set empty
        (progn
          (setq x4y3 x4y4)
          (setq x5y3 (polar x4y3 0.0 unitX))
        )
      )

      (if (and (= mot2_d 0) (> ex_lim 0))  ; if Mot2 depth and ex_lim not set empty
        (progn
          (setq x4y3 x4y4)
          (setq x5y3 (polar ref_def 0.0 (* unitX 4.0)))
        )
      )


      (insert_text x4y2 x5y3 mot1)
      (insert_line soilLineBlock x4y2)


      ; Check to see if ref_def --- limit of excavation
      (if (AND (/= mot2_d 0) (< (cadr ref_def) (cadr x5y4)))
        (progn
          (insert_text x4y3 x5y4 mot2)
        )
      )
      (if (AND (/= mot2_d 0) (>= (cadr ref_def) (cadr x5y4)))
        (progn
          (setq point (list (car x5y3) (cadr ref_def)))
          (insert_text x4y3 point mot2)
        )
      )

    ) 
  )    


  (setvar "EXPERT" ex)
  (setvar "ANGDIR" ANGDIR_DEF)
  (SETVAR "ANGBASE" ANGBASE_DEF)

;reset block insert unit vars
  (resetBlockUnitsVars)


)    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






(defun c:log (/ ddiag fil tp a consistencyFormList consistencyList colorFormList colorList mottleList mottleFormList textureList textureFormList)
  (setvar "cmdecho" 0)


(if (or (= (getvar "CTAB") "HHE_200_PG3") (= (getvar "CTAB") "ALT_PG3"))
  (progn


  (setvar "CLAYER" "FORM_INPUT")
  (if (/= (getvar "CVPORT") 1) (command "PSPACE"))


  ; get profile 1-9 defaults
  (getSoillogDefaults)
  (if (= scaleArrowSize nil) (getuservars))

; default error control
  (setq lastErrorFunction *error*)
  (setq *error* DEFAULT_ERROR)

  (toggle-OSNAP-Vars-OFF)

; sets undo spot start 
  (command "UNDO" "BE")

  (setq ddiag nil)  ; precautionary reset ddiag

	  ;;;--- Set up an empty list
  (setq colorList (list))

	  ;;;--- Open the text file
  (if (findfile (strcat $septicadPathText "soillog_color.txt"))
    (progn

      (if (setq fil (open (findfile (strcat $septicadPathText "soillog_color.txt")) "r"))
        (progn

          ;;;--- Skip the first three lines        
          (read-line fil)
          (read-line fil)
          (read-line fil)

          ;;;--- Read the lines of data in the text file
          (while (setq a (read-line fil))
            ;;;--- Add the data to the list
            (setq colorList
                  (append
                    colorList
                    (list a)
                  )
            )
          )
          ;;;--- Close the file.
          (close fil)
        )
      )
    )
  )

  (setq colorFormList (list))
  (foreach a colorList
    (setq colorFormList (append colorFormList (list a)))
  )

	  ;;;--- Set up an empty list
  (setq consistencyList (list))

	  ;;;--- Open the text file
  (if (findfile (strcat $septicadPathText "soillog_consistency.txt"))
    (progn

      (if (setq fil (open (findfile (strcat $septicadPathText "soillog_consistency.txt")) "r"))
        (progn

          ;;;--- Skip the first three lines        
          (read-line fil)
          (read-line fil)
          (read-line fil)

          ;;;--- Read the lines of data in the text file
          (while (setq a (read-line fil))
            ;;;--- Add the data to the list
            (setq consistencyList
                  (append
                    consistencyList
                    (list a)
                  )
            )
          )
          ;;;--- Close the file.
          (close fil)
        )
      )
    )
  )

  (setq consistencyFormList (list))
  (foreach a consistencyList
    (setq consistencyFormList (append consistencyFormList (list a)))
  )

	  ;;;--- Set up an empty list
  (setq textureList (list))

	  ;;;--- Open the text file
  (if (findfile (strcat $septicadPathText "soillog_texture.txt"))
    (progn

      (if (setq fil (open (findfile (strcat $septicadPathText "soillog_texture.txt")) "r"))
        (progn

          ;;;--- Skip the first three lines        
          (read-line fil)
          (read-line fil)
          (read-line fil)

          ;;;--- Read the lines of data in the text file
          (while (setq a (read-line fil))
            ;;;--- Add the data to the list
            (setq textureList
                  (append
                    textureList
                    (list a)
                  )
            )
          )
          ;;;--- Close the file.
          (close fil)
        )
      )
    )
  )
	      ;;;--- REQUIRED --- Removes End of Line marker??
  (setq textureFormList (list))
  (foreach a textureList
    (setq textureFormList (append textureFormList (list a)))
  )

	  ;;;--- Set up an empty list
  (setq mottleList (list))

	  ;;;--- Open the text file
  (if (findfile (strcat $septicadPathText "soillog_mottle.txt"))
    (progn

      (if (setq fil (open (findfile (strcat $septicadPathText "soillog_mottle.txt")) "r"))
        (progn

          ;;;--- Skip the first three lines        
          (read-line fil)
          (read-line fil)
          (read-line fil)

          ;;;--- Read the lines of data in the text file
          (while (setq a (read-line fil))
            ;;;--- Add the data to the list
            (setq mottleList
                  (append
                    mottleList
                    (list a)
                  )
            )
          )
          ;;;--- Close the file.
          (close fil)
        )
      )
    )
  )
	      ;;;--- REQUIRED --- Removes End of Line marker??
  (setq mottleFormList (list))
  (foreach a mottleList
    (setq mottleFormList (append mottleFormList (list a)))
  )

  (setq dcl_id (load_dialog (strcat $septicadPathDCL "soillog.dcl")))
  (new_dialog "soillog" dcl_id)


  (setq tptbList (list "Test Pit" "Test Boring"))
  (start_list "tptb" 3)
  (mapcar 'add_list tptbList)
  (end_list)

  (setq limList (list "" "Groundwater" "Restrictive" "Bedrock" "Pit Depth"))
  (start_list "lim_type" 3)
  (mapcar 'add_list limList)
  (end_list)

  (setq LOEList (list "" "Limit of Excavation at" "Bedrock at" "Refusal (Firm) at" "Refusal in Stones at"))
  (start_list "ex_why" 3)
  (mapcar 'add_list LOEList)
  (end_list)


  (start_list "s1a" 3)  (mapcar 'add_list textureFormList)  (end_list)
  (start_list "s2a" 3)  (mapcar 'add_list textureFormList)  (end_list)
  (start_list "s3a" 3)  (mapcar 'add_list textureFormList)  (end_list)
  (start_list "s1c" 3)  (mapcar 'add_list consistencyFormList)  (end_list)
  (start_list "s2c" 3)  (mapcar 'add_list consistencyFormList)  (end_list)
  (start_list "s3c" 3)  (mapcar 'add_list consistencyFormList)  (end_list)
  (start_list "s1d" 3)  (mapcar 'add_list colorFormList)  (end_list)
  (start_list "s2d" 3)  (mapcar 'add_list colorFormList)  (end_list)
  (start_list "s3d" 3)  (mapcar 'add_list colorFormList)  (end_list)
  (start_list "mottle1" 3)  (mapcar 'add_list mottleFormList)  (end_list)
  (start_list "mottle2" 3)  (mapcar 'add_list mottleFormList)  (end_list)

    ;;;--- If an action event occurs, do this function
  (action_tile "log2" "(setq ddiag 3)(saveSoilVars)")
  (action_tile "log1" "(setq ddiag 2)(saveSoilVars)")
  (action_tile "cancel" "(setq ddiag 1)(done_dialog)")
  (action_tile "log2-2" "(setq ddiag 3)(setq tp 2)(saveSoilVars)")
  (action_tile "log1-2" "(setq ddiag 2)(setq tp 2)(saveSoilVars)")
  (action_tile "log2-3" "(setq ddiag 3)(setq tp 3)(saveSoilVars)")
  (action_tile "log1-3" "(setq ddiag 2)(setq tp 3)(saveSoilVars)")
  (action_tile "default_button" "(soilDefaultUpdate)")

  ;;;--- Display the dialog box
  (start_dialog)

  ;;;--- Unload the dialog box
  (unload_dialog dcl_id)

  ;;;--- If the user pressed the Cancel button
  (if (= ddiag 1)
    (PRINC "\nSoil Log Cancelled.")
  )

  ;;;--- If the user didn't pressed the Cancel button, then save defaults
  (saveSoilDefaults)


   ;;;--- If the user pressed the Left Log button
      (if (= ddiag 2)
        (progn
          ; formats the number result from the popup_list DCL tile
          ; set variables as the one chosen from the popup
          (setq s1a (nth s1a textureFormList))
          (setq s2a (nth s2a textureFormList))
          (setq s3a (nth s3a textureFormList))
          (setq s1c (nth s1c consistencyFormList))
          (setq s2c (nth s2c consistencyFormList))
          (setq s3c (nth s3c consistencyFormList))
          (setq s1d (nth s1d colorFormList))
          (setq s2d (nth s2d colorFormList))
          (setq s3d (nth s3d colorFormList))
          (setq mot1 (nth mot1 mottleFormList))
          (setq mot2 (nth mot2 mottleFormList))

          (soil_log_draw "LEFT")
        )
      )
      ;;;--- If the user pressed the Right Log button
      (if (= ddiag 3)
        (progn
          ; formats the number result from the popup_list DCL tile
          ; set variables as the one chosen from the popup
          (setq s1a (nth s1a textureFormList))
          (setq s2a (nth s2a textureFormList))
          (setq s3a (nth s3a textureFormList))
          (setq s1c (nth s1c consistencyFormList))
          (setq s2c (nth s2c consistencyFormList))
          (setq s3c (nth s3c consistencyFormList))
          (setq s1d (nth s1d colorFormList))
          (setq s2d (nth s2d colorFormList))
          (setq s3d (nth s3d colorFormList))
          (setq mot1 (nth mot1 mottleFormList))
          (setq mot2 (nth mot2 mottleFormList))

          (soil_log_draw "RIGHT")
        )
      )

      ;;; if locate test pit on page 2
      (if (= tp 2) (tpDraw "2"))

      ;;; if locate test pit on page 2
      (if (= tp 3) (tpDraw "3"))

      ; adds to number if demo
      (if (= $septicadFullVersion 0) (setq $$### (+ $$### 1)))
   


; change back to SEPTICAD layer
  (SETVAR "CLAYER" "SEPTICAD")  ; CHANGES LAYER

; sets all variables to nil
  (setq s1a nil s1b nil s1c nil s1d nil s2a nil s2b nil s2c nil s2d nil s3a nil s3c nil s3d nil mot1 nil mot2 nil mot1_d nil mot2_d nil ex_lim nil ex_why nil ex_why nil hole nil tptb nil org nil prof nil condition nil slop nil lim nil lim_type nil elv nil)

; sets undo spot end 
  (command "UNDO" "E")

  (toggle-OSNAP-Vars-RESET)

  (if (= $ALLCAPS "YES")
    (ALLCAPS (ssget "X" '((0 . "TEXT,MTEXT") (8 . "FORM_INPUT"))))
  )


  (setq *error* lastErrorFunction)

  )
  (progn (alert "Layout HHE_200_PG3 or ALT_PG3 must be on screen"))
)

  ;;;--- Suppress the last echo for a clean exit
  (princ)
)



(defun saveSoilVars ()

  (setq s1a (ATOI (get_tile "s1a")))
  (setq s1b (ATOI (get_tile "s1b")))
  (setq s1c (ATOI (get_tile "s1c")))
  (setq s1d (ATOI (get_tile "s1d")))

  (setq s2a (ATOI (get_tile "s2a")))
  (setq s2b (ATOI (get_tile "s2b")))
  (setq s2c (ATOI (get_tile "s2c")))
  (setq s2d (ATOI (get_tile "s2d")))

  (setq s3a (ATOI (get_tile "s3a")))
  (setq s3c (ATOI (get_tile "s3c")))
  (setq s3d (ATOI (get_tile "s3d")))

  (setq mot1 (ATOI (get_tile "mottle1")))
  (setq mot2 (ATOI (get_tile "mottle2")))
  (setq mot1_d (ATOI (get_tile "mottle1_depth")))
  (setq mot2_d (ATOI (get_tile "mottle2_depth")))

  (setq ex_lim (ATOF (get_tile "ex_lim")))
  (setq ex_why (atoi (get_tile "ex_why")))
  (setq ex_why (nth ex_why LOEList))

  (setq hole (get_tile "hole"))
  (setq tptb (ATOI (get_tile "tptb")))
  (setq org (get_tile "org"))
  (setq prof (get_tile "profile"))
  (setq condition (get_tile "class"))
  (setq slop (get_tile "slope"))
  (setq lim (get_tile "lim"))
  (setq lim_type (ATOI (get_tile "lim_type")))

  (setq elv (get_tile "elv"))


  (checkSoilLog)
)  ; END saveSoilVars


(defun tpDraw (page / ortho_def ex scale fontSize dt da p1 p2)

;sets block insert unit vars
  (setBlockUnitsVars)

  (toggle-OSNAP-Vars-OFF)

	; Set orthomode to OFF
  (setq ortho_DEF (getvar "ORTHOMODE"))
  (setvar "ORTHOMODE" 0)

  (setvar "CLAYER" "SEPTICAD")
  (command "LAYOUT" "S" "MODEL")

  (setq ex (getvar "Expert"))
  (setvar "Expert" 2)
	; suppresses "Block already defined. Redefined it? ALERT"

  (setq scale 1.0)
  (setScales)

  (if (= scaleArrowSize nil) (getuservars))

  (if (= page "2")
    (progn
      (setq scale (/ page2Scale 10.0))
      (setq fontSize page2TextSize)

      ; Zoom to Model Space and Center on Page 2
      (if (/= sPage2 nil) (command "ZOOM" "c" cPage2 (* sPage2 (/ (last (getvar "screensize")) 8.0))))
      (if (= sPage2 nil) (command "ZOOM" "E"))
    )
  )


  (if (= page "3")
    (progn
      (setq fontSize page3TextSize)
      (setq scale (/ page3Scale 10.0))

      ; Zoom to Model Space and Center on Page 3
      (if (/= sPage3 nil) (command "ZOOM" "c" cPage3 (* sPage3 (/ (last (getvar "screensize")) 8.0))))
      (if (= sPage3 nil) (command "ZOOM" "E"))
    )
  )


  (setq dt (getvar "dimtxt"))
  (setvar "dimtxt" fontSize)

  (setq da (getvar "dimasz"))
  (setvar "dimasz" (* fontSize scaleArrowSize))

  (setq tempPt (list 10000 0 0))
  (setq block (strcat $septicadPathBlockRequired "TP.DWG"))

  (if (= (strcase (getvar "PRODUCT")) "AUTOCAD")
    (progn
      (command "-insert" block tempPt scale scale "0")
      ; insert point
      (command "move" "L" "" tempPT (setq p1 (getpoint "\nClick Location of Test Pit-Boring")))
    )
  )

  (if (= (strcase (getvar "PRODUCT")) "BRICSCAD")
    (progn
      (command "-insert" block tempPt scale scale "0")
      ; now get insert point
      (command "move" "L" "" tempPT (setq p1 (getpoint "\nClick Location of Test Pit-Boring")))
    )
  )


  (if (= (strcase (getvar "PRODUCT")) "INTELLICAD")
    (progn
      (command "-insert" block tempPt scale scale "0")
      ; insert point
      (command "move" "L" "" tempPT (setq p1 (getpoint "\nClick Location of Test Pit-Boring")))
    )
  )

	; text leader point
  (setq p2 (polar p1 (/ (* 4 pi) 3.0) (* 5 fontSize)))

	; insert text leader if not blank
  (command "LEADER" p1 p2 "" hole "")

	; Reset System Variable
  (setvar "Expert" ex)
  (setvar "dimtxt" dt)
  (setvar "dimasz" da)
  (setvar "ORTHOMODE" ortho_DEF)
  (toggle-OSNAP-Vars-RESET)

;reset block insert unit vars
  (resetBlockUnitsVars)


)
; end function

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun classAutoFill (/ class)
  (setq class (get_tile "class"))

	; auto capitalize
  (if (= class "b") (set_tile "class" "B"))
  (if (= class "c") (set_tile "class" "C"))
  (if (= class "d") (set_tile "class" "D"))
  (if (= class "ai") (set_tile "class" "AI"))
  (if (= class "aii") (set_tile "class" "AII"))
  (if (= class "aiii") (set_tile "class" "AIII"))

  (if (OR (= class "AI") (= class "ai"))
    (progn
      (set_tile "ex_why" "Bedrock")
    )  ; end progn
  )    ; end if

  (if (OR (= class "AII") (= class "aii"))
    (progn
      (set_tile "ex_why" "Bedrock")
    )  ; end progn
  )    ; end if

  (if (OR (= class "AIII") (= class "aiii"))
    (progn
      (set_tile "ex_why" "Bedrock")
    )  ; end progn
  )    ; end if


)      ; end classAutoFill
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun checkAutoDepths (/ depth_top depth_bot)
  (setq depth_top (atoi (get_tile "DEPTH_TOP")))
  (setq depth_bot (atoi (get_tile "DEPTH_BOT")))
  (if (< depth_top 0)
    (progn
      (set_tile "DEPTH_TOP" "")
      (Alert "Must be + number - Depth to Limiting Factor at highest elevation on top of system")
    )  ; end progn
  )    ; end if

  (if (< depth_bot 0)
    (progn
      (set_tile "DEPTH_BOT" "")
      (Alert "Must be + number - Depth to Limiting Factor at highest elevation on bottom of system")
    )  ; end progn
  )    ; end if
)      ; end checkAutoDepths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;; this is legacy code used to calculate the loading rate
;;; however this has been removed for the 2025 version as
;;; the sizing rate is no longer required on hhe-200 form
;;;  (designFlow) and (loadingRate)
;(DEFUN loadingRate (/ profile)
;
;	; get profile and set to real value from list
;       (setq profile (atoi (get_tile "E8")))
;       (setq profile (nth profile E8_LIST))
;
;	; Set location of radio column for design flow sf/g/day
;       (if (= profile "1") (set_tile "SIZING" "2"))
;       (if (= profile "2") (set_tile "SIZING" "1"))
;       (if (= profile "3") (set_tile "SIZING" "1"))
;       (if (= profile "4") (set_tile "SIZING" "0"))
;       (if (= profile "5") (set_tile "SIZING" "0"))
;       (if (= profile "6") (set_tile "SIZING" "0"))
;       (if (= profile "7") (set_tile "SIZING" "1"))
;       (if (= profile "8") (set_tile "SIZING" "2"))
;       (if (= profile "9") (set_tile "SIZING" "3"))
;
;	; Calculate design flow and square footage
;       (designFlow)
;)
;; end loadingRate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;(defun designFlow (/ soilRate sf df div-cluster div-trench method divider)
;; THESE FUNCTIONS ARE GOOD FOR V3 SYSTEM BUT NOT CHECKED FOR OTHER SYSTEMS!!!!!!!!!!!!!!!!!!!!!!11
;; set these variables again because they are set to nil at the end of c:septic
;  (RUN-DIM_SYSTEM)
;
;
;
;; if endcaps are added to square feet in mapDrawChambers (if system is member of $endCapChambers) then the sf stored in these variables is added to total
;;endCapDiv-cluster - defined in mapDrawChambers
;;endCapDiv-trench - defind in mapDrawChambers
;
;; this code should be re written interms of two method
;; $df-method = "UNIT" based
;; $df-method = "LINEAR" based
;; then add on extra for encaps for Quick 4 and Q4+
;
;  (if (= SYS_TYPE "4x8_CONCRETE_CLUSTER")
;    (progn
;      (setq method "UNIT" div-cluster 64 div-trench 64)
;    )
;  )
;  (if (= SYS_TYPE "4x8_CONCRETE_TRENCH")
;    (progn
;      (setq method "UNIT" div-cluster 90 div-trench 90)
;    )
;  )
;  (if (= SYS_TYPE "8x4_CONCRETE_CLUSTER")
;    (progn
;      (setq method "UNIT" div-cluster 64 div-trench 64)
;    )
;  )
;  (if (= SYS_TYPE "8x4_CONCRETE_TRENCH")
;    (progn
;      (setq method "UNIT" div-cluster 77 div-trench 77)
;    )
;  )
;  (if (= SYS_TYPE "ADS_BIO2")
;    (progn
;      (setq method "UNIT" div-cluster 28.8 div-trench 28.8)
;    )
;  )
;  (if (= SYS_TYPE "ADS_HI_CAP")
;    (progn
;      (setq method "UNIT" div-cluster 36 div-trench 50)
;    )
;  )
;  (if (= SYS_TYPE "ADS_STD")
;    (progn
;      (setq method "UNIT" div-cluster 36 div-trench 44)
;    )
;  )
;  (if (= SYS_TYPE "ARC18")
;    (progn
;      (setq method "UNIT" div-cluster 12.5 div-trench 20)
;    )
;  )
;  (if (= SYS_TYPE "ARC24")
;    (progn
;      (setq method "UNIT" div-cluster 18.5 div-trench 30)
;    )
;  )
;  (if (= SYS_TYPE "ARC36")
;    (progn
;      (setq method "UNIT" div-cluster 29 div-trench 35)
;    )
;  )
;  (if (= SYS_TYPE "ARC36HC")
;    (progn
;      (setq method "UNIT" div-cluster 29 div-trench 40)
;    )
;  )
;  (if (= SYS_TYPE "ELJEN")
;    (progn
;      (setq method "UNIT" div-cluster 48 div-trench 48)
;    )
;  )
;  (if (= SYS_TYPE "ELJEN_INVERTED")
;    (progn
;      (setq method "UNIT" div-cluster 48 div-trench 48)
;    )
;  )
;  (if (= SYS_TYPE "ENVIRO")
;    (progn
;      (setq method "LINEAR" div-cluster 5 div-trench 5)
;    )
;  )
;  (if (= SYS_TYPE "INF_EQ24")
;    (progn
;      (setq method "UNIT" div-cluster 33.3 div-trench 33.3)
;    )
;  )
;  (if (= SYS_TYPE "INF_HI_CAP")
;    (progn
;      (setq method "UNIT" div-cluster 36 div-trench 50)
;    )
;  )
;  (if (= SYS_TYPE "INF_STD")
;    (progn
;      (setq method "UNIT" div-cluster 36 div-trench 44)
;    )
;  )
;  (if (= SYS_TYPE "MOUNDBUSTER")
;    (progn
;      (setq method "LINEAR" div-cluster 3.5 div-trench 3.5)
;    )
;  )
;  (if (= SYS_TYPE "Q4_PLUS_EQ_36_LP")
;    (progn
;      (setq method "UNIT" div-cluster 14.8 div-trench 20.8)
;    )
;  )
;  (if (= SYS_TYPE "Q4_PLUS_HC")
;    (progn
;      (setq method "UNIT" div-cluster 23.2 div-trench 32)
;    )
;  )
;  (if (= SYS_TYPE "Q4_PLUS_STD")
;    (progn
;      (setq method "UNIT" div-cluster 23.2 div-trench 28)
;    )
;  )
;  (if (= SYS_TYPE "Q4_PLUS_STD_LP")
;    (progn
;      (setq method "UNIT" div-cluster 23.2 div-trench 28)
;    )
;  )
;  (if (= SYS_TYPE "QUICK4_EQ24")
;    (progn
;      (setq method "UNIT" div-cluster 16 div-trench 16)
;    )
;  )
;  (if (= SYS_TYPE "QUICK4_EQ24LP")
;    (progn
;      (setq method "UNIT" div-cluster 10.8 div-trench 14)
;    )
;  )
;  (if (= SYS_TYPE "QUICK4_EQ36")
;    (progn
;      (setq method "UNIT" div-cluster 14.8 div-trench 20.8)
;    )
;  )
;  (if (= SYS_TYPE "QUICK4_HI_CAP")
;    (progn
;      (setq method "UNIT" div-cluster 23.2 div-trench 32)
;    )
;  )
;  (if (= SYS_TYPE "QUICK4_STD")
;    (progn
;      (setq method "UNIT" div-cluster 23.2 div-trench 28)
;    )
;  )
;  (if (= SYS_TYPE "STONE")
;    (progn
;      (setq method "UNIT" div-cluster 1 div-trench 1)
;    )
;  )
;  (if (= SYS_TYPE "TRENCH2")
;    (progn
;      (setq method "LINEAR" div-cluster 4 div-trench 4)
;    )
;  )
;  (if (= SYS_TYPE "TRENCH3")
;    (progn
;      (setq method "LINEAR" div-cluster 5 div-trench 5)
;    )
;  )
;
;  (if (= row_sep 0)
;    (setq divider div-cluster)
;    (setq divider div-trench)
;  )
;
;; Set design flow and square footage if data has been entered
;  (if (and (/= num_rows nil) (/= num_units nil))
;    (progn
;      ; get Soil Loading Rate
;      (setq soilRate (atoi (get_tile "SIZING")))
;      (if (= soilRate 0) (setq soilRate 2.6))
;      (if (= soilRate 1) (setq soilRate 3.3))
;      (if (= soilRate 2) (setq soilRate 4.1))
;      (if (= soilRate 3) (setq soilRate 5.0))
;
;      (if (= method "UNIT")
;        (progn
;          (setq sf (* num_units num_rows divider))
;          (setq df (/ sf soilRate))
;        )
;      )
;      (if (= method "LINEAR")
;        (progn
;          (setq sf (* num_units num_rows))
;          (setq df (/ (* sf divider) soilRate))
;        )
;      )
;
;      ; if system is an endcaps chamber type, then add sf for each row and redefine df
;      (if (member sys_type $endCapChambers)
;        (progn
;
;          (if (= row_sep 0)
;            (setq sf (+ sf (* endCapDiv-cluster num_rows)))
;            (setq sf (+ sf (* endCapDiv-trench num_rows)))
;          )
;          (setq df (/ sf soilRate))
;        )
;      )
;
;
;
;      ; set DCL tiles for Design Flow and Square/Linear footage
;      (set_tile "H1" (rtos df 2 0))
;      (set_tile "F10" (rtos sf 2 0))
;
;
;;	(print (strcat "design flow =" (rtos df 2 0)))
;;	(print (strcat "stonebed sf =" (rtos sf 2 0)))
;;	(print "design flow calculator has not been dbl check for accuracy")
;
;    )
;  )
;;;;;;;;;;;;;;;;; END BIG PROGN-IF (/= NUM_ROWS NIL)
;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END FUNCTION


