;; This file includes most of the functions for filling out the HHE-200 form
;; in paper space, including DCL menus.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ret_assoc (key lst / itm rtn)
  (while (setq itm (assoc key lst))
    (setq rtn (cons (cdr itm) rtn) lst (cdr (member itm lst)))
  )
  (reverse rtn)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ret_index (tag lst / item cnt str index rtn)

  (setq cnt 0)
  (setq index nil)
  (setq rtn nil)

  (foreach item lst
    (setq str (car (nth 0 item)))
    (if (wcmatch tag str)
      (setq index cnt)
    )
    (setq cnt (1+ cnt))
  )

  ; test for null case - no record found
  (if (/= index nil)
    (setq rtn (nth index lst))
  )
  rtn
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun create_checkbox (record / point)

  (setq point (car (nth 2 record)))

  (if (tblsearch "block" "CHECKBOX_CHECKED")
    (COMMAND "-insert" "CHECKBOX_CHECKED" point "1" "1" "0")  ; if
    (COMMAND "-insert" (strcat $septicadPathBlockRequired "CHECKBOX_CHECKED.DWG") point "1" "1" "0")  ; else
  )

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun create_mtext (record str / point height attach width rotation)

  (setq point (car (nth 2 record)))
  (setq height (car (nth 3 record)))
  (setq attach (car (nth 4 record)))
  (setq width (car (nth 5 record)))
  (setq rotation (car (nth 6 record)))

  (entmake (list
             (cons 0 "MTEXT")
             (cons 100 "AcDbEntity")  ; Required for all post-R12 entities.
             (cons 8 "FORM_INPUT")
             (cons 100 "AcDbMText")   ; Identifies the entity as MTEXT.
             (cons 10 point)
             (cons 1 str)
             (cons 71 attach)
             (cons 40 height)
             (cons 41 width)
             (cons 50 rotation)
           )
  )

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get_HHE-200_form_hidden (layoutName / tag ent1 cnt sset1 wbuf master_list)

  ; create a list containging a list representing each place where text or checkboxes can be inserted
  (setq master_list (list))
  (setq wbuf 0)

	; create empty selection set
  ; note layer name is hard coded
  (setq sset1 (ssadd))
  (setq sset1 (ssget "X" '((0 . "MTEXT") (8 . "PAGE_HHE-200_HIDDEN"))))
  

    ;;("ALT_PG3" "ALT_PLAN_SEPTIC" "ALT_PLAN_PROFILE" "HHE_200_PG1" "HHE_200_PG2" "HHE_200_PG3" "HHE_200_PG4" "ALT_PLAN_SITE")

    (if (= layoutName "HHE_200_PG1")(setq sset1 (ssget "X" '((0 . "MTEXT") (8 . "PAGE_HHE-200_HIDDEN") (410 . "HHE_200_PG1")))))
    (if (= layoutName "HHE_200_PG2")(setq sset1 (ssget "X" '((0 . "MTEXT") (8 . "PAGE_HHE-200_HIDDEN") (410 . "HHE_200_PG2")))))
    (if (= layoutName "HHE_200_PG3")(setq sset1 (ssget "X" '((0 . "MTEXT") (8 . "PAGE_HHE-200_HIDDEN") (410 . "HHE_200_PG3")))))
    (if (= layoutName "HHE_200_PG4")(setq sset1 (ssget "X" '((0 . "MTEXT") (8 . "PAGE_HHE-200_HIDDEN") (410 . "HHE_200_PG4")))))    
    (if (= layoutName "ALT_PG3")(setq sset1 (ssget "X" '((0 . "MTEXT") (8 . "PAGE_HHE-200_HIDDEN") (410 . "ALT_PG3")))))
    (if (= layoutName "ALT_PLAN_SITE")(setq sset1 (ssget "X" '((0 . "MTEXT") (8 . "PAGE_HHE-200_HIDDEN") (410 . "ALT_PLAN_SITE")))))
    (if (= layoutName "ALT_PLAN_SEPTIC")(setq sset1 (ssget "X" '((0 . "MTEXT") (8 . "PAGE_HHE-200_HIDDEN") (410 . "ALT_PLAN_SEPTIC")))))
    (if (= layoutName "ALT_PLAN_PROFILE")(setq sset1 (ssget "X" '((0 . "MTEXT") (8 . "PAGE_HHE-200_HIDDEN") (410 . "ALT_PLAN_PROFILE")))))    
  

     ; create while loop to go through each item in selection set
  (setq cnt 0)

  (while (< cnt (sslength sset1))
    (setq ent1 (entget (ssname sset1 cnt)))

    (setq master_list (append master_list
                              (list (list
                                      (ret_assoc 1 ent1)    ;; tag
                                      (ret_assoc 410 ent1)  ;; layout
                                      (ret_assoc 10 ent1)   ;; point
                                      (ret_assoc 40 ent1)   ;; height
                                      (ret_assoc 71 ent1)   ;; attach
                                      (ret_assoc 41 ent1)   ;; width
                                      (ret_assoc 50 ent1)   ;; rotation
                                    )
                              )
                      )
    )
    (setq cnt (1+ cnt))
  )
   ;(master_list_print master_list)
  master_list
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun master_list_print (lst / item)
  (foreach item lst
    (princ "\n")
    (princ (car (nth 0 item)))(princ "  *  ")
    (princ (car (nth 1 item)))(princ "  *  ")
    (princ (car (nth 2 item)))(princ "  *  ")
    (princ (car (nth 3 item)))(princ "  *  ")
    (princ (car (nth 4 item)))(princ "  *  ")
    (princ (car (nth 5 item)))
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get_HHE-200_form_hidden_check_boxes (/ field)

  (setq field (list
                "B1" "B2" "B5" "B6" "B7" "B8" "B9"
                "C1" "C2" "C3" "C4" "C5" "C6" "C7" "C8" "C9"
                "D1" "D2" "D3" "D4" "D7" "D8" "D11" "D13"  
                "E1" "E2" "E3" "E4" "E5" "E6" "E7" "E8" "E9" "E10" "E11" "E12"                
                "G2" "G3"
                "H1" "H2"
                "I1" "I2" "I3" "I4"
                "J1" "J2" "J3" "J4" "J3A" "J3B" "J3C" "J3D"  
                "K1" "K2" "K3" "K4" "K5"
                "L1" "L2" "L3" "L4"
                "M1" "M2" "M3"
                "N1" "N2" "N3" "N4" "N5" "N7" "N8"
                "O2" "O3" "O5"
                "Q1D" "Q2D"
              )
  )
  field
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get_HHE-200_form_hidden_text_fields (/ field)
  (setq field
        (list
          "A1" "A2" "A3" "A4" "A5" "A6" "A7" "A8" "A9" "A10" "A11" "A12" "A13"
          "B3" "B4"
          "G1"
          "L1A" "L2A" "L3A" "L4A"
          "E1A" "E2A" "E3A" "E4A" "E7A" "E7B" "E7C" "E8A" "E10A"
          "Q1A" "Q1B" "Q1C"  "Q2A" "Q2B" "Q2C"
          "K6"
          "D5" "D6" "D9" "D10" "D12"
          "O1" "O4"
          "J4A" "J5"
          "N6"
          "M4"
          "S1" "S2" "S3" "S4" "S5" "S6" "S7"
          "P1" "P2" "P3" "P4" "P5"
          "R1"
        )
  )
  field
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -----------------------------------------------------------------;;
;; ---------------------  HHE_200_PG1 ------------------------------;;
;; -----------------------------------------------------------------;;
(defun septicad_print_page1 (checkbox_fields text_fields / layout_name item record buf)
  
  (setq layout_name "HHE_200_PG1")

  (if (member layout_name (layoutList))
    (progn

      (if (/= (getvar "CTAB") layout_name) (COMMAND "LAYOUT" "S" layout_name))
      (if (/= (getvar "CVPORT") 1) (command "PSPACE"))

    ;; ("HHE_200_PG1" "HHE_200_PG2" "HHE_200_PG3" "HHE_200_PG4" "ALT_PG3" "ALT_PLAN_SITE" "ALT_PLAN_SEPTIC" "ALT_PLAN_PROFILE")
    (setq master_list (get_HHE-200_form_hidden "HHE_200_PG1"))

      ;;;;;;;;;;;;;;;;;;;; mtexts ;;;;;;;;;;;;;;;;;;;;;;;

      (foreach item text_fields
        (setq record (ret_index item master_list))
        (if (= (car (nth 1 record)) layout_name)
          (progn
            (setq buf (cdr (assoc item pg1Data)))
            (if (/= buf "") (create_mtext record (cdr (assoc item pg1Data))))
          )
        )
      )

      ;;;;;;;;;;;;;;;;;;;; mtexts ;;;;;;;;;;;;;;;;;;;;;;;       

      ;;;;;;;;;;;;;;;;;;;; check boxes ;;;;;;;;;;;;;;;;;;;;;;;
      
 
      (foreach item checkbox_fields
        (setq record (ret_index item master_list))
        (if (= (car (nth 1 record)) layout_name)
          (progn
            (setq buf (cdr (assoc item pg1Data)))
            (if (= buf 1) (create_checkbox record))
          )
        )
      )

      ;;;;;;;;;;;;;;;;;;;; check boxes ;;;;;;;;;;;;;;;;;;;;;;; 

      (if (= $ALLCAPS "YES")
        (ALLCAPS (ssget "X" '((0 . "MTEXT") (8 . "FORM_INPUT"))))
      )  
  
    )
  )

) ; end defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -----------------------------------------------------------------;;
;; ---------------------  HHE_200_PG2 ------------------------------;;
;; -----------------------------------------------------------------;;
(defun septicad_print_page2 (checkbox_fields text_fields / layout_name item record buf buf1 buf2)
  
      (setq layout_name "HHE_200_PG2")

  (if (member layout_name (layoutList))
    (progn
      
      (if (/= (getvar "CTAB") layout_name) (COMMAND "LAYOUT" "S" layout_name))
      (if (/= (getvar "CVPORT") 1) (command "PSPACE"))

    ;; ("HHE_200_PG1" "HHE_200_PG2" "HHE_200_PG3" "HHE_200_PG4" "ALT_PG3" "ALT_PLAN_SITE" "ALT_PLAN_SEPTIC" "ALT_PLAN_PROFILE")
    (setq master_list (get_HHE-200_form_hidden "HHE_200_PG2"))

      ;;;;;;;;;;;;;;;;;;;; mtexts ;;;;;;;;;;;;;;;;;;;;;;;

      (foreach item text_fields
        (setq record (ret_index item master_list))
        ;;(print (car(nth 1 record)))
        (if (= (car (nth 1 record)) layout_name)
          (progn
            (setq buf (cdr (assoc item pg1Data)))
            (if (/= buf "") (create_mtext record (cdr (assoc item pg1Data))))

          )
        )
      )

      ; top right hand header to fill out
      (setq record (ret_index "HEADER_OWNER" master_list))
      (setq buf (cdr (assoc "A6" pg1Data)))
      (if (/= buf "") (create_mtext record buf))

      (setq record (ret_index "HEADER_ADDRESS" master_list))
      (setq buf1 (cdr (assoc "A1" pg1Data)))
      (setq buf2 (cdr (assoc "A2" pg1Data)))
      (if (/= buf1 "")(setq buf2 (strcat buf1 " " buf2)))
      (if (/= buf2 "") (create_mtext record buf2))


      ;;;;;;;;;;;;;;;;;;;; mtexts ;;;;;;;;;;;;;;;;;;;;;;;       

      ;;;;;;;;;;;;;;;;;;;; check boxes ;;;;;;;;;;;;;;;;;;;;;;;

      (foreach item checkbox_fields
        (setq record (ret_index item master_list))
        (if (= (car (nth 1 record)) layout_name)
          (progn
            (setq buf (cdr (assoc item pg1Data)))
            (if (= buf 1) (create_checkbox record))
          )
        )
      )
      ;;;;;;;;;;;;;;;;;;;; check boxes ;;;;;;;;;;;;;;;;;;;;;;;

      (if (= $ALLCAPS "YES")
        (ALLCAPS (ssget "X" '((0 . "MTEXT") (8 . "FORM_INPUT"))))
      )
    )
  )
) ; end defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -----------------------------------------------------------------;;
;; ---------------------  HHE_200_PG3 ------------------------------;;
;; -----------------------------------------------------------------;;
(defun septicad_print_page3 (checkbox_fields text_fields / layout_name item record buf buf1 buf2)
  
      (setq layout_name "HHE_200_PG3")

  (if (member layout_name (layoutList))
    (progn

      (if (/= (getvar "CTAB") layout_name) (COMMAND "LAYOUT" "S" layout_name))
      (if (/= (getvar "CVPORT") 1) (command "PSPACE"))
    
    (setq master_list (get_HHE-200_form_hidden layout_name))

      ;;;;;;;;;;;;;;;;;;;; mtexts ;;;;;;;;;;;;;;;;;;;;;;;
      ; top right hand header to fill out
      (setq record (ret_index "HEADER_OWNER" master_list))
      (setq buf (cdr (assoc "A6" pg1Data)))
      (if (/= buf "") (create_mtext record buf))

      (setq record (ret_index "HEADER_ADDRESS" master_list))
      (setq buf1 (cdr (assoc "A1" pg1Data)))
      (setq buf2 (cdr (assoc "A2" pg1Data)))
      (if (/= buf1 "")(setq buf2 (strcat buf1 " " buf2)))
      (if (/= buf2 "") (create_mtext record buf2))


      ;;;;;;;;;;;;;;;;;;;; mtexts ;;;;;;;;;;;;;;;;;;;;;;;  


      ;;;;;;;;;;;;;;;;;;;; scales ;;;;;;;;;;;;;;;;;;;;;;;
      
      (setq buf (rtos (* sPage2 10) 2 0))

      (setq record (ret_index "SC1" master_list))
      (create_mtext record buf)
      (xDataTag (entlast) "PAGE2_SCALE" 1000 "SEPTICAD")     

      (setq record (ret_index "SC2" master_list))
      (create_mtext record buf)
      (xDataTag (entlast) "PAGE2_SCALE" 1000 "SEPTICAD") 
      
      ;;;;;;;;;;;;;;;;;;;; scales ;;;;;;;;;;;;;;;;;;;;;;;

      (if (= $ALLCAPS "YES")
        (ALLCAPS (ssget "X" '((0 . "MTEXT") (8 . "FORM_INPUT"))))
      )
    )
  )
) ; end defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -----------------------------------------------------------------;;
;; ---------------------  HHE_200_PG4 ------------------------------;;
;; -----------------------------------------------------------------;;
(defun septicad_print_page4 (checkbox_fields text_fields /
                            U1_Ftemp U2_Ftemp D1_Ftemp D2_Ftemp 
                            record buf buf1 buf2 uprec
                            topOfFill topOfDevice bottomOfDevice otherElv)
    
      (setq layout_name "HHE_200_PG4")

  (if (member layout_name (layoutList))
    (progn
      
      (if (/= (getvar "CTAB") layout_name) (COMMAND "LAYOUT" "S" layout_name))
      (if (/= (getvar "CVPORT") 1) (command "PSPACE"))

      (setq master_list (get_HHE-200_form_hidden layout_name))

      ;;;;;;;;;;;;;;;;;;;; mtexts ;;;;;;;;;;;;;;;;;;;;;;;
      ; top right hand header to fill out
      (setq record (ret_index "HEADER_OWNER" master_list))
      (setq buf (cdr (assoc "A6" pg1Data)))
      (if (/= buf "") (create_mtext record buf))

      (setq record (ret_index "HEADER_ADDRESS" master_list))
      (setq buf1 (cdr (assoc "A1" pg1Data)))
      (setq buf2 (cdr (assoc "A2" pg1Data)))
      (if (/= buf1 "")(setq buf2 (strcat buf1 " " buf2)))
      (if (/= buf2 "") (create_mtext record buf2))

      ;------- ERP section                        
      (setq record (ret_index "X1" master_list))
      (setq buf (cdr (assoc "X1" pg1Data)))
      (if (/= buf "") (create_mtext record buf))

      (setq record (ret_index "X2" master_list))
      (setq buf (cdr (assoc "X2" pg1Data)))
      (if (/= buf "") (create_mtext record buf))
      ;;;;;;;;;;;;;;;;;;;; mtexts ;;;;;;;;;;;;;;;;;;;;;;;  


      ;;;;;;;;;;;; scales insert and xData Tag ;;;;;;;;;;
      ;;;-------map 
      (setq buf (rtos (* sPage3 10) 2 0))

      (create_mtext (ret_index "SC3" master_list) buf)
      (xDataTag (entlast) "PAGE3_SCALE" 1000 "SEPTICAD")  
          
      (create_mtext (ret_index "SC4" master_list) buf)
      (xDataTag (entlast) "PAGE3_SCALE" 1000 "SEPTICAD") 
          
      ;;--------- profile H and V
      (setq buf (rtos profileScale 2 0))

      (create_mtext (ret_index "SC5" master_list) buf)
      (xDataTag (entlast) "PROFILE_SCALE" 1000 "SEPTICAD")    
          
      (create_mtext (ret_index "SC6" master_list) buf)
      (xDataTag (entlast) "PROFILE_SCALE" 1000 "SEPTICAD")     
      
      ;;;;;;;;;;;; scales insert and xData Tag ;;;;;;;;;;    
      
      
      ;;;;;;;; backfill requirements  ;;;;;;;;;;;;; ;;;;;
      
      ;;;; if the fill is less than zero set text at zero
      (if (<= U1_F 0.0)(setq U1_Ftemp 0)(setq U1_Ftemp U1_F))
      (if (<= U2_F 0.0)(setq U2_Ftemp 0)(setq U2_Ftemp U2_F))
      (if (<= D1_F 0.0)(setq D1_Ftemp 0)(setq D1_Ftemp D1_F))
      (if (<= D2_F 0.0)(setq D2_Ftemp 0)(setq D2_Ftemp D2_F))

      (if (= crossSection "LEFT")
        (progn
          (SETQ buf1 (STRCAT (RTOS U1_Ftemp 2 0) "\""))
          (SETQ buf2 (STRCAT (RTOS D1_Ftemp 2 0) "\""))
        )
      )
          
      (if (= crossSection "RIGHT")
        (progn
          (SETQ buf1 (STRCAT (RTOS U2_Ftemp 2 0) "\""))
          (SETQ buf2 (STRCAT (RTOS D2_Ftemp 2 0) "\""))
        )
      )
          
      (create_mtext (ret_index "V1" master_list) buf1) 
      (create_mtext (ret_index "V2" master_list) buf2) 
      ;;;;;;;; backfill requirements  ;;;;;;;;;;;;; ;;;;;      



    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;;;;; START CONSTRUCTiON ELEVATIONS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


      ; DETERMINE TOP OF FILL, CORRECTING FOR SLOPE IF SLOPING 3% IS USED
    (if (/= (LAST fill_top) 0)
      (progn
        (if (= (/ (LAST (POLAR fill_top angleBot (/ (/ chamber_W 2.0) (cos angleBot)))) (fix (LAST (POLAR fill_top angleBot (/ (/ chamber_W 2.0) (cos angleBot)))))) 1) (setq uprec 0))
        (if (/= (/ (LAST (POLAR fill_top angleBot (/ (/ chamber_W 2.0) (cos angleBot)))) (fix (LAST (POLAR fill_top angleBot (/ (/ chamber_W 2.0) (cos angleBot)))))) 1) (setq uprec 1))
      )
      ;else
      (setq uprec 0)
    )
       (if (= slopeType "SLOPING3")
         (SETQ topOfFill (STRCAT (RTOS (LAST (POLAR fill_top angleBot (/ (/ chamber_W 2.0) (cos angleBot)))) 2 UPREC) "\""))
         (SETQ topOfFill (STRCAT (RTOS (LAST fill_top) 2 0) "\""))
       )
     ;;; topOfFill is now set

     ;;; top of device or stone
       (if (= (/ (+ (LAST row_start) CHAMBER_H) (fix (+ (LAST row_start) CHAMBER_H))) 1) (setq uprec 0))
       (if (/= (/ (+ (LAST row_start) CHAMBER_H) (fix (+ (LAST row_start) CHAMBER_H))) 1) (setq uprec 1))
       (SETQ topOfDevice (+ (LAST row_start) CHAMBER_H))
       ; perforated pipe elevation
       (if (or (= sys_type "STONE") (= sys_type "TRENCH2") (= sys_type "TRENCH3"))
           (SETQ topOfDevice (- topOfDevice 1))
       )
       (SETQ topOfDevice (STRCAT (RTOS topOfDevice 2 UPREC) "\""))

     ;;; bottom of device or stone
        (if (/= (LAST row_start) 0)
          (progn
            (if (= (/ (LAST row_start) (fix (LAST row_start))) 1) (setq uprec 0))
            (if (/= (/ (LAST row_start) (fix (LAST row_start))) 1) (setq uprec 1))
          )
          ;else
          (setq uprec 0)
        )
       (SETQ bottomOfDevice (STRCAT (RTOS (LAST row_start) 2 UPREC) "\""))


          (create_mtext (ret_index "W1" master_list) topOfFill)
          (create_mtext (ret_index "W2" master_list) topOfDevice)           
          (create_mtext (ret_index "W3" master_list) bottomOfDevice)           


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;;;;; END CONSTRUCTiON ELEVATIONS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      
      (if (= $ALLCAPS "YES")
        (ALLCAPS (ssget "X" '((0 . "MTEXT") (8 . "FORM_INPUT"))))
      )
  
    )
  )
) ; end defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -----------------------------------------------------------------;;
;; ---------------------  ALT_PG3  ------------------------------;;
;; -----------------------------------------------------------------;;
(defun septicad_print_alt_page3 (checkbox_fields text_fields /
                            U1_Ftemp U2_Ftemp D1_Ftemp D2_Ftemp 
                            record buf buf1 buf2 uprec
                            topOfFill topOfDevice bottomOfDevice otherElv)
    
      (setq layout_name "ALT_PG3")

  (if (member layout_name (layoutList))
    (progn
      
      (if (/= (getvar "CTAB") layout_name) (COMMAND "LAYOUT" "S" layout_name))
      (if (/= (getvar "CVPORT") 1) (command "PSPACE"))

      (setq master_list (get_HHE-200_form_hidden layout_name))

      ;;;;;;;;;;;;;;;;;;;; mtexts ;;;;;;;;;;;;;;;;;;;;;;;
      ; top right hand header to fill out
      (setq record (ret_index "HEADER_OWNER" master_list))
      (setq buf (cdr (assoc "A6" pg1Data)))
      (if (/= buf "") (create_mtext record buf))

      (setq record (ret_index "HEADER_ADDRESS" master_list))
      (setq buf1 (cdr (assoc "A1" pg1Data)))
      (setq buf2 (cdr (assoc "A2" pg1Data)))
      (if (/= buf1 "")(setq buf2 (strcat buf1 " " buf2)))
      (if (/= buf2 "") (create_mtext record buf2))

      ;------- ERP section                        
      (setq record (ret_index "X1" master_list))
      (setq buf (cdr (assoc "X1" pg1Data)))
      (if (/= buf "") (create_mtext record buf))

      (setq record (ret_index "X2" master_list))
      (setq buf (cdr (assoc "X2" pg1Data)))
      (if (/= buf "") (create_mtext record buf))
      ;;;;;;;;;;;;;;;;;;;; mtexts ;;;;;;;;;;;;;;;;;;;;;;;  

     
      ;;;;;;;; backfill requirements  ;;;;;;;;;;;;; ;;;;;
      
      ;;;; if the fill is less than zero set text at zero
      (if (<= U1_F 0.0)(setq U1_Ftemp 0)(setq U1_Ftemp U1_F))
      (if (<= U2_F 0.0)(setq U2_Ftemp 0)(setq U2_Ftemp U2_F))
      (if (<= D1_F 0.0)(setq D1_Ftemp 0)(setq D1_Ftemp D1_F))
      (if (<= D2_F 0.0)(setq D2_Ftemp 0)(setq D2_Ftemp D2_F))

      (if (= crossSection "LEFT")
        (progn
          (SETQ buf1 (STRCAT (RTOS U1_Ftemp 2 0) "\""))
          (SETQ buf2 (STRCAT (RTOS D1_Ftemp 2 0) "\""))
        )
      )
          
      (if (= crossSection "RIGHT")
        (progn
          (SETQ buf1 (STRCAT (RTOS U2_Ftemp 2 0) "\""))
          (SETQ buf2 (STRCAT (RTOS D2_Ftemp 2 0) "\""))
        )
      )
          
      (create_mtext (ret_index "V1" master_list) buf1) 
      (create_mtext (ret_index "V2" master_list) buf2) 
      ;;;;;;;; backfill requirements  ;;;;;;;;;;;;; ;;;;;      



    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;;;;; START CONSTRUCTiON ELEVATIONS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


      ; DETERMINE TOP OF FILL, CORRECTING FOR SLOPE IF SLOPING 3% IS USED
    (if (/= (LAST fill_top) 0)
      (progn
        (if (= (/ (LAST (POLAR fill_top angleBot (/ (/ chamber_W 2.0) (cos angleBot)))) (fix (LAST (POLAR fill_top angleBot (/ (/ chamber_W 2.0) (cos angleBot)))))) 1) (setq uprec 0))
        (if (/= (/ (LAST (POLAR fill_top angleBot (/ (/ chamber_W 2.0) (cos angleBot)))) (fix (LAST (POLAR fill_top angleBot (/ (/ chamber_W 2.0) (cos angleBot)))))) 1) (setq uprec 1))
      )
      ;else
      (setq uprec 0)
    )
       (if (= slopeType "SLOPING3")
         (SETQ topOfFill (STRCAT (RTOS (LAST (POLAR fill_top angleBot (/ (/ chamber_W 2.0) (cos angleBot)))) 2 UPREC) "\""))
         (SETQ topOfFill (STRCAT (RTOS (LAST fill_top) 2 0) "\""))
       )
     ;;; topOfFill is now set

     ;;; top of device or stone
       (if (= (/ (+ (LAST row_start) CHAMBER_H) (fix (+ (LAST row_start) CHAMBER_H))) 1) (setq uprec 0))
       (if (/= (/ (+ (LAST row_start) CHAMBER_H) (fix (+ (LAST row_start) CHAMBER_H))) 1) (setq uprec 1))
       (SETQ topOfDevice (+ (LAST row_start) CHAMBER_H))
       ; perforated pipe elevation
       (if (or (= sys_type "STONE") (= sys_type "TRENCH2") (= sys_type "TRENCH3"))
           (SETQ topOfDevice (- topOfDevice 1))
       )
       (SETQ topOfDevice (STRCAT (RTOS topOfDevice 2 UPREC) "\""))

     ;;; bottom of device or stone
        (if (/= (LAST row_start) 0)
          (progn
            (if (= (/ (LAST row_start) (fix (LAST row_start))) 1) (setq uprec 0))
            (if (/= (/ (LAST row_start) (fix (LAST row_start))) 1) (setq uprec 1))
          )
          ;else
          (setq uprec 0)
        )
       (SETQ bottomOfDevice (STRCAT (RTOS (LAST row_start) 2 UPREC) "\""))


          (create_mtext (ret_index "W1" master_list) topOfFill)
          (create_mtext (ret_index "W2" master_list) topOfDevice)           
          (create_mtext (ret_index "W3" master_list) bottomOfDevice)           


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;;;;; END CONSTRUCTiON ELEVATIONS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      
      (if (= $ALLCAPS "YES")
        (ALLCAPS (ssget "X" '((0 . "MTEXT") (8 . "FORM_INPUT"))))
      )
  
    )
  )
  
) ; end defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;"ALT_PLAN_SEPTIC" "ALT_PLAN_PROFILE" "ALT_PLAN_SITE"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -----------------------------------------------------------------;;
;; ---------------------  ALT_PLAN_SITE ------------------------------;;
;; -----------------------------------------------------------------;;
(defun septicad_print_alt_site (checkbox_fields text_fields / layout_name item record buf buf1 buf2)
  
      (setq layout_name "ALT_PLAN_SITE")

  (if (member layout_name (layoutList))
    (progn
      
      (if (/= (getvar "CTAB") layout_name) (COMMAND "LAYOUT" "S" layout_name))
      (if (/= (getvar "CVPORT") 1) (command "PSPACE"))
    
    (setq master_list (get_HHE-200_form_hidden layout_name))

      ;;;;;;;;;;;;;;;;;;;; mtexts ;;;;;;;;;;;;;;;;;;;;;;;
      ; top right hand header to fill out
      (setq record (ret_index "HEADER_OWNER" master_list))
      (setq buf (cdr (assoc "A6" pg1Data)))
      (if (/= buf "") (create_mtext record buf))

      (setq record (ret_index "HEADER_ADDRESS" master_list))
      (setq buf1 (cdr (assoc "A1" pg1Data)))
      (setq buf2 (cdr (assoc "A2" pg1Data)))
      (if (/= buf1 "")(setq buf2 (strcat buf1 " " buf2)))
      (if (/= buf2 "") (create_mtext record buf2))


      ;;;;;;;;;;;;;;;;;;;; mtexts ;;;;;;;;;;;;;;;;;;;;;;;  


      ;;;;;;;;;;;;;;;;;;;; scales ;;;;;;;;;;;;;;;;;;;;;;;
      
      (setq buf (rtos (* sPage2 10) 2 0))

      (setq record (ret_index "SC1" master_list))
      (if record (create_mtext record buf))
      (xDataTag (entlast) "PAGE2_SCALE" 1000 "SEPTICAD")     

      (setq record (ret_index "SC2" master_list))
      (if record (create_mtext record buf))
      (xDataTag (entlast) "PAGE2_SCALE" 1000 "SEPTICAD") 
      
      ;;;;;;;;;;;;;;;;;;;; scales ;;;;;;;;;;;;;;;;;;;;;;;

      (if (= $ALLCAPS "YES")
        (ALLCAPS (ssget "X" '((0 . "MTEXT") (8 . "FORM_INPUT"))))
      )
    )
  )
  
) ; end defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;"ALT_PLAN_SEPTIC" "ALT_PLAN_PROFILE" "ALT_PLAN_SITE"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -----------------------------------------------------------------;;
;; ---------------------  ALT_PLAN_SEPTIC ------------------------------;;
;; -----------------------------------------------------------------;;
(defun septicad_print_alt_septic (checkbox_fields text_fields / layout_name item record buf buf1 buf2)
  
      (setq layout_name "ALT_PLAN_SEPTIC")

  (if (member layout_name (layoutList))
    (progn
      
      (if (/= (getvar "CTAB") layout_name) (COMMAND "LAYOUT" "S" layout_name))
      (if (/= (getvar "CVPORT") 1) (command "PSPACE"))
    
    (setq master_list (get_HHE-200_form_hidden layout_name))

      ;;;;;;;;;;;;;;;;;;;; mtexts ;;;;;;;;;;;;;;;;;;;;;;;
      ; top right hand header to fill out
      (setq record (ret_index "HEADER_OWNER" master_list))
      (setq buf (cdr (assoc "A6" pg1Data)))
      (if (/= buf "") (create_mtext record buf))

      (setq record (ret_index "HEADER_ADDRESS" master_list))
      (setq buf1 (cdr (assoc "A1" pg1Data)))
      (setq buf2 (cdr (assoc "A2" pg1Data)))
      (if (/= buf1 "")(setq buf2 (strcat buf1 " " buf2)))
      (if (/= buf2 "") (create_mtext record buf2))


      ;;;;;;;;;;;;;;;;;;;; mtexts ;;;;;;;;;;;;;;;;;;;;;;;  


      ;;;;;;;;;;;;;;;;;;;; scales ;;;;;;;;;;;;;;;;;;;;;;;
      
      (setq buf (rtos (* sPage3 10) 2 0))

      (setq record (ret_index "SC3" master_list))
      (if record (create_mtext record buf))
      (xDataTag (entlast) "PAGE3_SCALE" 1000 "SEPTICAD")     

      (setq record (ret_index "SC4" master_list))
      (if record (create_mtext record buf))
      (xDataTag (entlast) "PAGE3_SCALE" 1000 "SEPTICAD") 
      
      ;;;;;;;;;;;;;;;;;;;; scales ;;;;;;;;;;;;;;;;;;;;;;;

      (if (= $ALLCAPS "YES")
        (ALLCAPS (ssget "X" '((0 . "MTEXT") (8 . "FORM_INPUT"))))
      )
    )
  )
) ; end defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;"ALT_PLAN_SEPTIC" "ALT_PLAN_PROFILE" "ALT_PLAN_SITE"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -----------------------------------------------------------------;;
;; ---------------------  ALT_PLAN_PROFILE ------------------------------;;
;; -----------------------------------------------------------------;;
(defun septicad_print_alt_profile (checkbox_fields text_fields / layout_name item record buf buf1 buf2)
  
      (setq layout_name "ALT_PLAN_PROFILE")

  (if (member layout_name (layoutList))
    (progn

      (if (/= (getvar "CTAB") layout_name) (COMMAND "LAYOUT" "S" layout_name))
      (if (/= (getvar "CVPORT") 1) (command "PSPACE"))
    
    (setq master_list (get_HHE-200_form_hidden layout_name))

      ;;;;;;;;;;;;;;;;;;;; mtexts ;;;;;;;;;;;;;;;;;;;;;;;
      ; top right hand header to fill out
      (setq record (ret_index "HEADER_OWNER" master_list))
      (setq buf (cdr (assoc "A6" pg1Data)))
      (if (/= buf "") (create_mtext record buf))

      (setq record (ret_index "HEADER_ADDRESS" master_list))
      (setq buf1 (cdr (assoc "A1" pg1Data)))
      (setq buf2 (cdr (assoc "A2" pg1Data)))
      (if (/= buf1 "")(setq buf2 (strcat buf1 " " buf2)))
      (if (/= buf2 "") (create_mtext record buf2))


      ;;;;;;;;;;;;;;;;;;;; mtexts ;;;;;;;;;;;;;;;;;;;;;;;  


      ;;;;;;;;;;;;;;;;;;;; scales ;;;;;;;;;;;;;;;;;;;;;;;
      
      (setq buf (rtos profileScale 2 0))

      (setq record (ret_index "SC5" master_list))
      (if record (create_mtext record buf))
      (xDataTag (entlast) "PROFILE_SCALE" 1000 "SEPTICAD")     

      (setq record (ret_index "SC6" master_list))
      (if record (create_mtext record buf))
      (xDataTag (entlast) "PROFILE_SCALE" 1000 "SEPTICAD") 
      
      ;;;;;;;;;;;;;;;;;;;; scales ;;;;;;;;;;;;;;;;;;;;;;;

      (if (= $ALLCAPS "YES")
        (ALLCAPS (ssget "X" '((0 . "MTEXT") (8 . "FORM_INPUT"))))
      )
    )
  )
) ; end defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
































;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:justPage1 (/ temp CMDECHO_DEF EXPERT_DEF ANGDIR_DEF ANGBASE_DEF)


  (toggle-OSNAP-Vars-OFF)
  (setBlockUnitsVars)
  (SETQ CMDECHO_DEF (GETVAR "CMDECHO")) (setvar "CMDECHO" 0)
  (setq EXPERT_DEF (getvar "EXPERT")) (setvar "EXPERT" 2)
  (setq ANGDIR_DEF (GETVAR "ANGDIR")) (setvar "ANGDIR" 0)
  (SETQ ANGBASE_DEF (GETVAR "ANGBASE")) (setvar "ANGBASE" 0)

      
      (HHE-200_PAGE1A_DCL)
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (if (member "HHE_200_PG1" (layoutList))
        (progn
          
          (setq checkbox_fields (get_HHE-200_form_hidden_check_boxes))
          (setq text_fields (get_HHE-200_form_hidden_text_fields))
   
          (setvar "CLAYER" "FORM_INPUT")
      
          (command "UNDO" "BE")

          (princ "Printing to Form.....")
          
          (septicad_print_page1 checkbox_fields text_fields)(princ "HHE-200 Page 1..")
          (septicad_print_page2 checkbox_fields text_fields)(princ "Page 2..")

      (if (= $ALLCAPS "YES")
        (ALLCAPS (ssget "X" '((0 . "MTEXT") (8 . "FORM_INPUT"))))
      )


          (princ "Done\n")
            ; sets undo spot end 
          (command "UNDO" "E")
        )
      )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (resetBlockUnitsVars)
  (toggle-OSNAP-Vars-RESET)
  (SETVAR "CMDECHO" CMDECHO_DEF)
  (setvar "EXPERT" EXPERT_DEF)
  (setvar "ANGDIR" ANGDIR_DEF)
  (SETVAR "ANGBASE" ANGBASE_DEF)


  (princ)

)
; end function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

















(defun HHE-200_PAGE1A_DCL (/ ddiag)

  (PRINC "\nEntering Information for Page 1 HHE-200.....")

;;;; Load user variables
  (getUserVars)

  ; variable is reset in (HHE-200_PAGE1C_DCL_SAVEVARS)
  (setq pg1Data (list))

 ;;;--- Load the dcl file
  (setq dcl_id (load_dialog (strcat $septicadPathDCL "PAGE1A.dcl")))

  ;;;--- Load the dialog definition if it is not already loaded
  (if (not (new_dialog "PAGE1A" dcl_id))
    (progn
      (alert "The DCL file could not be loaded!")
      (exit)
    )
  )


  (start_list "DESIGNER" 3)
  (mapcar 'add_list (getSiteEvaluatorNames))
  (end_list)

  ;;;--- If an action event occurs, do this function
  (action_tile "accept" "(setq ddiag 2)(HHE-200_PAGE1A_DCL_SAVEVARS)(done_dialog)")
  (action_tile "cancel" "(setq ddiag 1)(done_dialog)")

  (PG1a_Reset_Form)

  ;;;--- Display the dialog box
  (start_dialog)

  ;;;--- Unload the dialog box
  (unload_dialog dcl_id)

  ;;;--- If the user pressed the Cancel button
  (if (= ddiag 1)
    (progn
      (Princ "\nCancelled Continue Design")
      (exit)
    )
  )

  ;;;--- If the user pressed the Okay button
  (if (= ddiag 2)
    (HHE-200_PAGE1B_DCL)
  )

  (princ)

)
; END HHE-200_PAGE1A_DCL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun HHE-200_PAGE1B_DCL (/ ddiag waterList)

  ;;;--- Load the dcl file
  (setq dcl_id (load_dialog (strcat $septicadPathDCL "PAGE1B.dcl")))

  ;;;--- Load the dialog definition if it is not already loaded
  (if (not (new_dialog "PAGE1B" dcl_id))
    (progn
      (alert "The DCL file could not be loaded!")
      (exit)
    )
  )

  ;;;--- If an action event occurs, do this function
  (action_tile "accept" "(setq ddiag 2)(HHE-200_PAGE1B_DCL_SAVEVARS)(done_dialog)")
  (action_tile "cancel" "(setq ddiag 1)(done_dialog)")
  (action_tile "back" "(setq ddiag 3)(HHE-200_PAGE1B_DCL_SAVEVARS)(done_dialog)")

  (PG1b_Reset_Form)

  ;;;--- Display the dialog box
  (start_dialog)

  ;;;--- Unload the dialog box
  (unload_dialog dcl_id)

  (if (= ddiag 1)
    (progn
      (Princ "\nCancelled Continue Design")
      (exit)
    )
  )

  ;;;--- If the user pressed the Okay button
  (if (= ddiag 2)
    (progn
      (HHE-200_PAGE1C_DCL)
    )
  )
  ;;;--- If the user pressed the back button
  (if (= ddiag 3)
    (HHE-200_PAGE1A_DCL)
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun HHE-200_PAGE1C_DCL (/ ddiag lst1 lst2 lst3)

  ;;;--- Load the dcl file
  (setq dcl_id (load_dialog (strcat $septicadPathDCL "PAGE1C.dcl")))
  (setq lst1 (list "sq. ft." "lin. ft."))
  (setq lst2 (list "Not Required" "May be Required" "Required"))
  (setq lst3 (list "Table 4A [Dwelling Unit(s)]" "Table 4C [other facilities]" "Table 4G [meter readings]"))

  ;;;--- Load the dialog definition if it is not already loaded
  (if (not (new_dialog "PAGE1C" dcl_id))
    (progn
      (alert "The DCL file could not be loaded!")
      (exit)
    )
  )

  (start_list "SIZETYPE" 3)  ; sq.ft. or lin. ft.
  (mapcar 'add_list lst1)
  (end_list)

  (start_list "PUMP" 3)      ; Pump
  (mapcar 'add_list lst2)
  (end_list)

  (start_list "BASEDON" 3)   ; design flow calculation type
  (mapcar 'add_list lst3)
  (end_list)

  ;;;--- If an action event occurs, do this function
  (action_tile "accept" "(setq ddiag 2)(HHE-200_PAGE1C_DCL_SAVEVARS)(done_dialog)")
  (action_tile "cancel" "(setq ddiag 1)(done_dialog)")
  (action_tile "back" "(setq ddiag 3)(HHE-200_PAGE1C_DCL_SAVEVARS)(done_dialog)")

  (PG1c_Reset_Form)

   ;;;--- on load set Disposal Field Type section
  (fieldType)

  ;;;--- Display the dialog box
  (start_dialog)

  ;;;--- Unload the dialog box
  (unload_dialog dcl_id)

  (if (= ddiag 1)
    (progn
      (Princ "\nCancelled Continue Design")
      (exit)
    )
  )

  ;;;--- If the user pressed the Okay button
  (if (= ddiag 2)
    (princ "DONE\n")
  )

  ;;;--- If the user pressed the Back button
  (if (= ddiag 3)
    (HHE-200_PAGE1B_DCL)
  )


)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun changeDCLcheckbox (tile tileChange)
  (if (= (get_tile tile) "1")
    (set_tile tileChange "1")
  )
  (if (= (get_tile tile) "0")
    (set_tile tileChange "0")
  )
)

(defun changeExpansionRadio ()
  (if (= (get_tile "B5") "0")
    (progn
      (set_tile "B6" "0")
      (set_tile "B7" "0")
    )
  )
)
(defun changeConcreteTankRadio ()
  (if (= (get_tile "D1") "0")
    (progn
      (set_tile "D7" "0")
      (set_tile "D8" "0")
    )
  )
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun HHE-200_PAGE1A_DCL_SAVEVARS ()

; create two global lists that all use of a back button
  (setq $hhe-200page1A-tile (list "I1" "I2" "I3" "I4" "DESIGNER" "A1" "A2" "A3" "A4" "A5" "A6" "A7" "A8" "A9" "A10" "A11" "A12" "A13" "B1" "B2" "B3" "B4" "B5" "B6" "B7" "B8" "B9" "G1" "G2" "G3" "H1" "H2"))

  ;; DESIGNER pulldown is excluded 
  (setq checkbox_fields (list "I1" "I2" "I3" "I4" "B1" "B2" "B5" "B6" "B7" "B8" "B9" "G2" "G3" "H1" "H2"))
  (setq text_fields     (list "A1" "A2" "A3" "A4" "A5" "A6" "A7" "A8" "A9" "A10" "A11" "A12" "A13" "B3" "B4" "G1"))
  

  ; for refilling the form when going back and forth thru the three windows (A, B, C)
  (setq $hhe-200page1A-val (list))
  (foreach item $hhe-200page1A-tile
    (setq $hhe-200page1A-val (append $hhe-200page1A-val (list (get_tile item))))
  )

  ; ADD ALL DATA TO A DOTTED PAIR LIST - pg1DataB

  (setq $designer (get_tile "DESIGNER"))
  (setq seInfo (nth (atoi $designer) (getSiteEvaluatorList)))

; ADD ALL DATA TO A DOTTED PAIR LIST - pg1DataB
  (setq pg1DataA (list))

  (setq pg1DataA (append (list (cons "SIG_SE_NAME" (nth 0 seInfo))) pg1DataA))  ;name
  (setq pg1DataA (append (list (cons "SIG_SE_NUM" (nth 1 seInfo))) pg1DataA))  ;number
  (setq pg1DataA (append (list (cons "SIG_SE_PHONE" (nth 2 seInfo))) pg1DataA))  ;phone
  (setq pg1DataA (append (list (cons "SIG_SE_EMAIL" (nth 3 seInfo))) pg1DataA))  ;email     

  ;; SAVE TEXT ENTRY DATA INPUT
  (foreach item text_fields
      (setq pg1DataA (append (list (cons item (get_tile item))) pg1DataA))
  )
  
  ;; SAVE CHECKBOX OR RADIO INPUT
  (foreach item checkbox_fields
    (setq pg1DataA (append (list (cons item (ATOI (get_tile item)))) pg1DataA))
  )


)      
; END HHE-200_PAGE1A_DCL_SAVEVARS



(defun HHE-200_PAGE1B_DCL_SAVEVARS ()

; create two global lists that all use of a back button
  (setq $hhe-200page1B-val (list))
  
  (setq $hhe-200page1B-tile (list "C1" "C2" "C3" "C4" "C5" "C6" "C7" "C8" "C9" 
                                  "L1" "L1A" "L2" "L2A" "L3" "L3A" "L4" "L4A" 
                                   
                                  "E1" "E2" "E3" "E4" "E5" "E6" "E7" "E8" "E9" "E10" "E11" "E12"
                                  "E1A" "E2A" "E3A" "E4A" "E7A" "E7B" "E7C" "E8A" "E10A"
                                  
                                  "Q1A" "Q1B" "Q1C" "Q1D" "Q2A" "Q2B" "Q2C" "Q2D"
                                  "K1" "K2" "K3" "K4" "K5" "K6"))


  (setq checkbox_fields (list "C1" "C2" "C3" "C4" "C5" "C6" "C7" "C8" "C9" 
                              "L1" "L2" "L3" "L4" 
                               
                              "E1" "E2" "E3" "E4" "E5" "E6" "E7" "E8" "E9" "E10" "E11" "E12"
                              "K1" "K2" "K3" "K4" "K5"
                              "Q1D" "Q2D"))
                              
  (setq text_fields     (list "L1A" "L2A" "L3A" "L4A" 
                              "E1A" "E2A" "E3A" "E4A" "E7A" "E7B" "E7C" "E8A" "E10A"
                              "Q1A" "Q1B" "Q1C"  "Q2A" "Q2B" "Q2C"
                              "K6"))


  ; for refilling the form when going back and forth thru the three windows (A, B, C)
  (foreach item $hhe-200page1B-tile
    (setq $hhe-200page1B-val (append $hhe-200page1B-val (list (get_tile item))))
  )

; ADD ALL DATA TO A DOTTED PAIR LIST - pg1DataB
  (setq pg1DataB (list))
  
  ;; SAVE TEXT ENTRY DATA INPUT
  (foreach item text_fields
      (setq pg1DataB (append (list (cons item (get_tile item))) pg1DataB))
  )
  
  ;; SAVE CHECKBOX OR RADIO INPUT
  (foreach item checkbox_fields
    (setq pg1DataB (append (list (cons item (ATOI (get_tile item)))) pg1DataB))
  )


)      ; END HHE-200_PAGE1B_DCL_SAVEVARS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun HHE-200_PAGE1C_DCL_SAVEVARS (/ strLong strLat lst4 lst5 pull_val )

; create two global lists that all use of a back button

(setq $hhe-200page1C-val (list))

(setq $hhe-200page1C-tile
      (list "D1" "D2" "D3" "D4" "D5" "D6" "D7" "D8" "D9" "D10" "D11" "D12" "D13"
            "P1" "P2" "P3" "P4" "P5"
            "O1" "O4" 
            "J1" "J2" "J3" "J4" "J5" "J3A" "J3B" "J3C" "J3D" "J4A"
            "BASEDON" "SIZETYPE" "PUMP"
            "N1" "N2" "N3" "N4" "N5" "N6" "N7" "N8"
            "M4"
            "S1" "S2" "S3" "S4" "S5" "S6" "S7"
            "X1" "X2"
            "R1"
      )
)

(foreach item $hhe-200page1C-tile
  (setq $hhe-200page1C-val (append $hhe-200page1C-val (list (get_tile item))))
)


(setq checkbox_fields
      (list "D1" "D2" "D3" "D4" "D7" "D8" "D11" "D13"
            "J1" "J2" "J3" "J4" "J3A" "J3B" "J3C" "J3D"
            "N1" "N2" "N3" "N4" "N5" "N7" "N8"
      )
)

(setq text_fields
      (list "D5" "D6" "D9" "D10" "D12"
            "P1" "P2" "P3" "P4" "P5"
            "O1" "O4"
            "J4A" "J5"
            "N6"
            "M4"
            "S1" "S2" "S3" "S4" "S5" "S6" "S7"
            "X1" "X2"
            "R1"
      )
)

  ;; pulldowns are dealt with individually
   ; BASEDON,SIZETYPE,PUMP


; ADD ALL DATA TO A DOTTED PAIR LIST - pg1DataB
  (setq pg1DataC (list))
  
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ; PULL DOWN MENUS
   ; start
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   ;; CHECKBOXES linked to pulldown
   ;; BASEDON SIZETYPE PUMP
  
    ;;(start_list "SIZETYPE" 3)  ; sq.ft. or lin. ft.
    ;; J6 J7
    
    (setq pull_val (ATOI (get_tile "SIZETYPE")))
  
    (if (= pull_val 0)
      (progn
         (setq pg1DataC (append (list (cons "J6" 1)) pg1DataC))
         (setq pg1DataC (append (list (cons "J7" 0)) pg1DataC))
      )
    )
    (if (= pull_val 1)
      (progn
         (setq pg1DataC (append (list (cons "J6" 0)) pg1DataC))
         (setq pg1DataC (append (list (cons "J7" 1)) pg1DataC))
      )
    )  
  
  ;;(start_list "PUMP" 3)      ; Pump
  ;;M1 M2 M3
    (setq pull_val (ATOI (get_tile "PUMP")))
    (if (= pull_val 0)
      (progn
         (setq pg1DataC (append (list (cons "M1" 1)) pg1DataC))
         (setq pg1DataC (append (list (cons "M2" 0)) pg1DataC))
         (setq pg1DataC (append (list (cons "M3" 0)) pg1DataC))         
      )
    )

    (if (= pull_val 1)
      (progn
         (setq pg1DataC (append (list (cons "M1" 0)) pg1DataC))
         (setq pg1DataC (append (list (cons "M2" 1)) pg1DataC))
         (setq pg1DataC (append (list (cons "M3" 0)) pg1DataC))         
      )
    )
    
    (if (= pull_val 2)
      (progn
         (setq pg1DataC (append (list (cons "M1" 0)) pg1DataC))
         (setq pg1DataC (append (list (cons "M2" 0)) pg1DataC))
         (setq pg1DataC (append (list (cons "M3" 1)) pg1DataC))         
      )
    )



  ;;(start_list "BASEDON" 3)   ; design flow calculation type
  ;;O2 O3 O5

    (setq pull_val (ATOI (get_tile "BASEDON")))
    (if (= pull_val 0)
      (progn
         (setq pg1DataC (append (list (cons "O2" 1)) pg1DataC))
         (setq pg1DataC (append (list (cons "O3" 0)) pg1DataC))
         (setq pg1DataC (append (list (cons "O5" 0)) pg1DataC))         
      )
    )

    (if (= pull_val 1)
      (progn
         (setq pg1DataC (append (list (cons "O2" 0)) pg1DataC))
         (setq pg1DataC (append (list (cons "O3" 1)) pg1DataC))
         (setq pg1DataC (append (list (cons "O5" 0)) pg1DataC))         
      )
    )
    
    (if (= pull_val 2)
      (progn
         (setq pg1DataC (append (list (cons "O2" 0)) pg1DataC))
         (setq pg1DataC (append (list (cons "O3" 0)) pg1DataC))
         (setq pg1DataC (append (list (cons "O5" 1)) pg1DataC))         
      )
    )


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ; PULL DOWN MENUS
   ; END
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; SAVE TEXT ENTRY DATA INPUT
  (foreach item text_fields
      (setq pg1DataC (append (list (cons item (get_tile item))) pg1DataC))
  )
  
  ;; SAVE CHECKBOX OR RADIO INPUT
  (foreach item checkbox_fields
    (setq pg1DataC (append (list (cons item (ATOI (get_tile item)))) pg1DataC))
  )

  ; merges into once big list
  (setq pg1Data (append pg1DataA pg1DataB))
  (setq pg1Data (append pg1Data pg1DataC))

  ; for debuggging
;  (princ "\n----- Page 1A\n")(foreach item pg1DataA (princ "\t")(print item))
;  (princ "\n----- Page 1B\n")(foreach item pg1DataB (princ "\t")(print item))
;  (princ "\n----- Page 1C\n")(foreach item pg1DataC (princ "\t")(print item))


)
; END HHE-200_PAGE1C_DCL_SAVEVARS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;








(defun PG1a_Reset_Form ()
  (if (/= $hhe-200page1A-tile nil)
    (progn
      (setq count 0)
      (foreach item $hhe-200page1A-val
        (progn
          (set_tile (nth count $hhe-200page1A-tile) item)
          (setq count (+ count 1))
        )
      )
    )
  )
)


(defun PG1b_Reset_Form ()
  (if (/= $hhe-200page1B-tile nil)
    (progn
      (setq count 0)
      (foreach item $hhe-200page1B-val
        (progn
          (set_tile (nth count $hhe-200page1B-tile) item)
          (setq count (+ count 1))
        )
      )
    )
  )
)

(defun PG1c_Reset_Form ()
  (if (/= $hhe-200page1C-tile nil)
    (progn
      (setq count 0)
      (foreach item $hhe-200page1C-val
        (progn
          (set_tile (nth count $hhe-200page1C-tile) item)
          (setq count (+ count 1))
        )
      )
    )
  )
)

























;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; FILLS IN CHECK BOXES IN THE DCL FORM FOR DISPOSAL FIELD TYPE AND SIZE
; DESIGN FLOW IS NOT CALCULATED HERE
(defun fieldType ()


;; "J1" "J2" "J3" "J4" "J3A" "J3B" "J3C" "J3D"
;; "J4A" 

;; "J5"


     ; reset tiles to uncheck or blank before setting according to system
  (foreach item (list "J1" "J2" "J3" "J4" "J3A" "J3B" "J3C" "J3D")
    (set_tile item "0")
  )
  (set_tile "J4A" "")
  
    ; set sf sizing in checkboxes near where stone bed eq sf or lf are written
  (set_tile "SIZETYPE" "0")

  (if (= sys_type "STONE") (set_tile "J1" "1"))

  (if (OR (= sys_type "TRENCH2")
          (= sys_type "TRENCH3")
      )
    (PROGN
      (set_tile "SIZETYPE" "1")
      (set_tile "J2" "1")
    )
  )

    ; if cluster concrete chambers
  (if (or (= SYS_TYPE "4x8_CONCRETE_CLUSTER") (= SYS_TYPE "8x4_CONCRETE_CLUSTER"))
    (progn
      ; check proprietary device box
      (set_tile "J3" "1")
      ; check cluster
      (set_tile "J3A" "1")
      ; if h20 load set that other wise set regular load
      (if (= $H20-RATED "YES")
        (set_tile "J3D" "1")
        (set_tile "J3C" "1")
      )
    )
  )


   ; if ordinary chambers
  (if (or (= SYS_TYPE "4x8_CONCRETE_TRENCH")
          (= SYS_TYPE "8x4_CONCRETE_TRENCH")
          (= SYS_TYPE "ADS_HI_CAP")
          (= SYS_TYPE "ELJEN")
          (= SYS_TYPE "ELJEN_INVERTED")
          (= SYS_TYPE "ADS_BIO2")
          (= SYS_TYPE "QUICK4_EQ24")
          (= SYS_TYPE "QUICK4_HI_CAP")
          (= SYS_TYPE "QUICK4_STD")
          (= SYS_TYPE "ADS_STD")
          (= SYS_TYPE "INF_STD")
          (= SYS_TYPE "INF_HI_CAP")
          (= SYS_TYPE "INF_EQ24")
          (= SYS_TYPE "ARC18")
          (= SYS_TYPE "ARC24")
          (= SYS_TYPE "ARC36HC")
          (= SYS_TYPE "ARC36")
          (= SYS_TYPE "Q4_PLUS_EQ_36_LP")
          (= SYS_TYPE "Q4_PLUS_HC")
          (= SYS_TYPE "Q4_PLUS_STD")
          (= SYS_TYPE "Q4_PLUS_STD_LP")
          (= SYS_TYPE "QUICK4_EQ24LP")
          (= SYS_TYPE "QUICK4_EQ36")
      )

    (progn
      ; check proprietary device box
      (set_tile "J3" "1")

      ;set sf sizing
      (set_tile "SIZETYPE" "0")

      ; if h20 load set that other wise set regular load
      (if (= $H20-RATED "YES")
        (set_tile "J3D" "1")
        (set_tile "J3C" "1")
      )

      ; if row_sep = 0 then check cluster.
      (if (= row_sep 0) (set_tile "J3A" "1"))
    )
  )



    ;if linear loading CHAMBER TYPE
  (if (= SYS_TYPE "ENVIRO")
    (progn
      ; check proprietary device box
      (set_tile "J3" "1")
      ; linear feet
      (set_tile "SIZETYPE" "1")
      ; linear
      (set_tile "J3B" "1")
    )
  )
  
      ;if linear loading CHAMBER TYPE
  (if (= SYS_TYPE "MOUNDBUSTER")
    (progn
      ; check proprietary device box
      (set_tile "J3" "1")
      ; linear feet
      (set_tile "SIZETYPE" "1")
      ; linear
      (set_tile "J3B" "1")
    )
  )

)
;;;;;;;;;;;;;;;;;;;;;;;;;

