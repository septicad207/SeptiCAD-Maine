;;; this file is for the main design window and the tools that print to model space

;; for development. it reloads septicad
(defun c:sss ()
  (load $septicadPathLSP)
)

;;;;;;;;; List of SYS_TYPE keywords used for different systems
;STONE
;TRENCH2
;TRENCH3
;ADS_STD
;ADS_HI_CAP
;ADS_BIO2
;ARC18
;ARC36
;ARC36HC
;INF_STD
;INF_HI_CAP
;INF_EQ24
;QUICK4_STD
;QUICK4_HI_CAP
;QUICK4_EQ24
;ELJEN
;ELJEN_INVERTED
;ENVIRO
;MOUNDBUSTER - converted to maine fuji pipe
;4x8_CONCRETE_CLUSTER
;8x4_CONCRETE_CLUSTER
;4x8_CONCRETE_TRENCH
;8x4_CONCRETE_TRENCH
---------------------------------------------------------------------------------
;STONE...........................Stone Bed"
;TRENCH2.........................Stone Trench (2 feet wide
;TRENCH3.........................Stone Trench (3 feet wide	
;ADS_STD.........................ADS BioDiffuser Standard
;ADS_HI_CAP......................ADS BioDiffuser Hi-Capacity
;ADS_BIO2........................ADS BioDiffuser Narrow [Bio 2]
;ARC18.......................ADS Arc 18
;ARC36.......................ADS Arc 36
;ARC36HC.....................ADS Arc 36 Hi-Capacity
;INF_STD.........................Infiltrator Standard
;INF_HI_CAP......................Infiltrator Hi-Capacity
;INF_EQ24........................Infiltrator EQ24
;QUICK4_STD......................Infiltrator Quick4 Standard
;QUICK4_HI_CAP...................Infiltrator Quick4 Hi-Capacity
;QUICK4_EQ24.....................Infiltrator Quick4 EQ 24
;ELJEN...........................Eljen GSF Geotextile Sand Filter
;ELJEN_INVERTED..................Eljen GSF Geotextile Sand Filter (Tranverse
;ENVIRO..........................Enviro-Septic Pipe
;4x8_CONCRETE_CLUSTER............4' x 8' Concrete Chamber [Cluster]
;8x4_CONCRETE_CLUSTER............8' x 4' Concrete Chamber [Cluster]
;4x8_CONCRETE_TRENCH.............4' x 8' Concrete Chamber [Trench]
;8x4_CONCRETE_TRENCH.............8' x 4' Concrete Chamber [Trench]

; (serialALL string)          -> Serial all rows w/ d-box and 2 labels (d-box and serial connection) - string = "TR" or "TL"
; (manifoldALL string)         -> Manifold all rows w/out d-box and 2 labels (manifold top and manifold bottom) - string = "TR" or "TL"
; (manifoldMULTI list)        -> Creates manifold based on list = e.g. (2 2 2 2 2)
;                                draws manifolds and creates list of inlet points inletListLEFT & inletListRIGHT
; (serialMULTI list string)   -> Creates serial piping based a sequence and side
; (dBoxTOP string)            -> Creates d-box piping if on uphill side of system based on side of system 
;                                draws lines based on inlet point list-> inletListLEFT & inletListRIGHT
; (dboxCENTER string)         -> Creates d-box piping if on center-left/right side of system - string = "CR" or "CL"
;                               if <=5 outlets required then center single outlet close to field
;                               if >5 outlets required then center double outlet close to field
; (dboxALL string)            -> Puts D-box on TL, TR, CL or CR
; (sDistribution)             -> main DCL for Pipe Layout (checkDistributionPipeDialog) (pipeSaveVars)
; (doDistributionPipes)       -> calls (sDistribution) via (continueSeptic) asks if okay or redo.  if reod then -> undo 1 step and (sDistribution)
; (c:sPipe)                   -> zoom to page 3 then (doDistributionPipes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun doDistributionPipes (/ ddiag dcl_id)

  (setq ANGDIR_DEF (GETVAR "ANGDIR"))
  (setvar "ANGDIR" 0)
  (SETQ ANGBASE_DEF (GETVAR "ANGBASE"))
  (setvar "ANGBASE" 0)

  (sDistribution)

   ; Ask if piping is okay or redo
       ;;;--- Load the DCL file
  (setq dcl_id (load_dialog (strcat $septicadPathDCL "pipeLayoutCheck.dcl")))

      ;;;--- See if the dialog box is already loaded
  (if (not (new_dialog "PIPELAYOUTCHECK" dcl_id))
    (progn
      (alert "The pipeLayoutCheck.dcl file was not found!")
      (exit)
    )
  )

      ;;;--- If an action event occurs, do this function
  (action_tile "okay" "(setq ddiag 1)(done_dialog)")
  (action_tile "redo" "(setq ddiag 2)(done_dialog)")

      ;;;--- Display the dialog box
  (start_dialog)

      ;;;--- Unload the dialog box
  (unload_dialog dcl_id)

   ; if okay do nothing
   ; if redo
  (if (= ddiag 2)
    (progn
      (command "UNDO" "1")
      (doDistributionPipes)
    )
  )

  (setvar "ANGDIR" ANGDIR_DEF)
  (SETVAR "ANGBASE" ANGBASE_DEF)

)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; START (c:sDistribution) function suite - DCL function for distribution pipe layout creation
;;;
(defun c:sPipe ()
  (if (/= cPage3 nil) (command "ZOOM" "c" cPage3 (* sPage3 (/ (last (getvar "screensize")) 10.0))))
  (if (/= pipePtListRIGHT nil)
    (doDistributionPipes)
    (princ "\nError-> List of Chamber Inlets not Found.")
  )
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun sDistribution (/ typeList sideList tempList eljenSerialStr moundbusterStr)

  
  
  ;;;;;;;;;; Start Subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            (defun pipeSaveVars (/ retList flag count item readlist)
              (setq pipeType (get_tile "TYPE"))
              (setq pipeSequence nil)

              (if (= (get_tile "SIDE") "0") (setq pipeSide "TL"))
              (if (= (get_tile "SIDE") "1") (setq pipeSide "CL"))
              (if (= (get_tile "SIDE") "2") (setq pipeSide "TR"))
              (if (= (get_tile "SIDE") "3") (setq pipeSide "CR"))

              (setq $pipeLastInputList (list pipeType (get_tile "SIDE") (get_tile "SEQ")))

            ; get sequence string and turn into list of numbers
              (setq readList (get_tile "SEQ"))

              ;;-- If begins with white space than remove it
              (setq flag nil)
              (while (= flag nil)
                (if (= (substr readlist 1 1) " ")
                  (setq readList (substr readlist 2 (- (strlen readList) 1)))
                  ; else
                  (setq flag 1)
                )
              )

              ;;-- Turn string into list of #'s
              (setq count 1)
              (setq retList (list))
              (while (setq item (read readlist))
                (setq retList (append retList (list item)))
                (while
                  (and
                    (/= " " (substr readlist count 1))
                    (/= "" (substr readlist count 1))
                  )
                  (setq count (1+ count))
                )
                (setq readlist (substr readlist count))
              )

              (setq pipeSequence retList)

               ; check to see if pipe sequence = num rows
              (if (OR (= pipeType "2") (= pipeType "4"))
                (progn
                  ;;-- determine # of chambers entered
                  (setq count 0)
                  (foreach item pipeSequence
                    (setq count (+ count item))
                  )
                  ;;-- COMPARE input to NUM_ROWS and alert user if input incorrect!
                  ; if BAD
                  (if (/= count num_rows)
                    (alert (strcat "The total # of Rows in the Sequence [# Rows = " (rtos count 2 0) "] is not consistent with the system design [# Rows = " (rtos num_rows 2 0) "]"))
                    (done_dialog)
                  )
                )
                (progn
                  (done_dialog)
                )
              )


            )
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            (defun checkDistributionPipeDialog (/ pipeSide)
            ; pipeType
            ; "0" = dbox all rows
            ; "1" = serial all rows
            ; "2" = serial with dbox
            ; "3" = manifold all rows
            ; "4" = manifold with dbox

              (setq pipeType (get_tile "TYPE"))
              (if (= (get_tile "SIDE") "0") (setq pipeSide "TL"))
              (if (= (get_tile "SIDE") "1") (setq pipeSide "CL"))
              (if (= (get_tile "SIDE") "2") (setq pipeSide "TR"))
              (if (= (get_tile "SIDE") "3") (setq pipeSide "CR"))

              (if (= pipeType "0")
                (progn
                  (if (= pipeSide "TL") (setq slideName (strcat $septicadPathDCL "PIPE-DBOX-ALL-TL.SLD")))
                  (if (= pipeSide "CL") (setq slideName (strcat $septicadPathDCL "PIPE-DBOX-ALL-CL.SLD")))
                  (if (= pipeSide "TR") (setq slideName (strcat $septicadPathDCL "PIPE-DBOX-ALL-TR.SLD")))
                  (if (= pipeSide "CR") (setq slideName (strcat $septicadPathDCL "PIPE-DBOX-ALL-CR.SLD")))
                  (mode_tile "SEQ" 1)
                )
              )

              (if (= pipeType "1")
                (progn
                  (if (= pipeSide "TL") (setq slideName (strcat $septicadPathDCL "PIPE-SERIAL-ALL-L.SLD")))
                  (if (= pipeSide "CL") (setq slideName (strcat $septicadPathDCL "PIPE-SERIAL-ALL-L.SLD")))
                  (if (= pipeSide "TR") (setq slideName (strcat $septicadPathDCL "PIPE-SERIAL-ALL-R.SLD")))
                  (if (= pipeSide "CR") (setq slideName (strcat $septicadPathDCL "PIPE-SERIAL-ALL-R.SLD")))
                  (if (= pipeSide "CL")
                    (progn
                      (set_tile "SIDE" "0") (Princ "\nCenter Left Not Allowed - Set to Top Left")
                    )
                  )
                  (if (= pipeSide "CR")
                    (progn
                      (set_tile "SIDE" "2") (Princ "\nCenter Right Not Allowed - Set to Top Right")
                    )
                  )
                  (mode_tile "SEQ" 1)
                )
              )


              (if (= pipeType "2")
                (progn
                  (if (= pipeSide "TL") (setq slideName (strcat $septicadPathDCL "PIPE-SERIAL-DBOX-TL.SLD")))
                  (if (= pipeSide "CL") (setq slideName (strcat $septicadPathDCL "PIPE-SERIAL-DBOX-CL.SLD")))
                  (if (= pipeSide "TR") (setq slideName (strcat $septicadPathDCL "PIPE-SERIAL-DBOX-TR.SLD")))
                  (if (= pipeSide "CR") (setq slideName (strcat $septicadPathDCL "PIPE-SERIAL-DBOX-CR.SLD")))
                  (mode_tile "SEQ" 0)
                )
              )

              (if (= pipeType "3")
                (progn
                  (if (= pipeSide "TL") (setq slideName (strcat $septicadPathDCL "PIPE-MANIFOLD-ALL-L.SLD")))
                  (if (= pipeSide "CL") (setq slideName (strcat $septicadPathDCL "PIPE-MANIFOLD-ALL-L.SLD")))
                  (if (= pipeSide "TR") (setq slideName (strcat $septicadPathDCL "PIPE-MANIFOLD-ALL-R.SLD")))
                  (if (= pipeSide "CR") (setq slideName (strcat $septicadPathDCL "PIPE-MANIFOLD-ALL-R.SLD")))
                  (if (= pipeSide "TL")
                    (progn
                      (set_tile "SIDE" "1") (Princ "\nTop Left Not Allowed - Set to Center Left")
                    )
                  )
                  (if (= pipeSide "TR")
                    (progn
                      (set_tile "SIDE" "3") (Princ "\nTop Right Not Allowed - Set to Center Right")
                    )
                  )
                  (mode_tile "SEQ" 1)
                )
              )

              (if (= pipeType "4")
                (progn
                  (if (= pipeSide "TL") (setq slideName (strcat $septicadPathDCL "PIPE-MANIFOLD-DBOX-TL.SLD")))
                  (if (= pipeSide "CL") (setq slideName (strcat $septicadPathDCL "PIPE-MANIFOLD-DBOX-CL.SLD")))
                  (if (= pipeSide "TR") (setq slideName (strcat $septicadPathDCL "PIPE-MANIFOLD-DBOX-TR.SLD")))
                  (if (= pipeSide "CR") (setq slideName (strcat $septicadPathDCL "PIPE-MANIFOLD-DBOX-CR.SLD")))
                  (mode_tile "SEQ" 0)
                )
              )

              (upDateImage slideName "IMAGE")

            )

            ;;;
            ;;; END (c:sDistribution) - Main DCL function for distribution pipe layout creation
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;









            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;; START (serialMULTI) - SERIAL FOR SEQUENCE OF ROWS
            ;;; does not draw pipe if 1 is entered in sequence
            (defun serialMULTI (sequence side / count2 count item)

              (setq offset 1)
              (setq inletListRIGHT (list))
              (setq inletListLEFT (list))
              (setq pipeList (list))

              (if (= (substr sys_type 5 16) "CONCRETE_CLUSTER")
                (setq offset (+ offset (/ stonebeside 12.0)))
              )
              (if (= (substr sys_type 5 15) "CONCRETE_TRENCH")
                (setq offset (+ offset 1))
              )

                    ; create inletListRIGHT and inletListLEFT - used by D-box functions
              (setq count 0)
              (foreach item sequence
                ; create inletList point foreach side... D-box function knows which one to use
                (setq inletListRIGHT (append inletListRIGHT (list (nth count pipePtListRIGHT))))
                (setq inletListLEFT (append inletListLEFT (list (nth count pipePtListLEFT))))
                (repeat item
                  (setq count (+ count 1))
                )
              )
                    ; end foreach defining pipe inlets

              (if (OR (= side "TL") (= side "CL"))
                (progn
                  (setq count 0 count2 0)  ; row counter
                  (foreach item sequence
                    ; START IF (> ITEM 1)
                    (if (> item 1)
                      (progn
                        (setq count2 0)    ; group counter
                        (repeat item
                          (if (/= (+ count2 1) item)
                            (progn
                              ; Distribution Starts on LEFT SIDE
                              ; if count2 is EVEN then do pipe on right side
                              (if (= (fix (/ count2 2.0)) (/ count2 2.0))
                                (progn
                                  (command "PLINE"
                                           (nth count pipePtListRIGHT)
                                           (polar (nth count pipePtListRIGHT) %RIGHT offset)
                                           (polar (nth (+ count 1) pipePtListRIGHT) %RIGHT offset)
                                           (nth (+ count 1) pipePtListRIGHT)
                                           ""
                                  )
                                )
                              )
                              ; if count2 id ODD the do pipe on left side
                              (if (/= (fix (/ count2 2.0)) (/ count2 2.0))
                                (progn
                                  (command "PLINE"
                                           (nth count pipePtListLEFT)
                                           (polar (nth count pipePtListLEFT) %LEFT offset)
                                           (polar (nth (+ count 1) pipePtListLEFT) %LEFT offset)
                                           (nth (+ count 1) pipePtListLEFT)
                                           ""
                                  )
                                )
                              )
                            )
                          )
                          ; end if not last row 
                          (setq count2 (+ count2 1))
                          (setq count (+ count 1))
                        )
                        ; end repeat
                      )
                    )
                    ; END IF (> ITEM 1)

                    ; START IF (= ITEM 1)
                    (if (= item 1)
                      (setq count (+ count 1))
                    )
                    ; END IF (= ITEM 1)               

                    (setq count2 (+ count2 1))
                  )
                )
              )
                   ; end if (= side "CL")

              (if (or (= side "TR") (= side "CR"))
                (progn
                  (setq count 0)           ; row counter
                  (foreach item sequence
                    ; START IF (> ITEM 1)
                    (if (> item 1)
                      (progn
                        (setq count2 0)    ; group counter
                        (repeat item
                          (if (/= (+ count2 1) item)
                            (progn
                              ; Distribution Starts on RIGHT SIDE
                              ; if count2 is ODD then do pipe on right side
                              (if (/= (fix (/ count2 2.0)) (/ count2 2.0))
                                (progn
                                  (command "PLINE"
                                           (nth count pipePtListRIGHT)
                                           (polar (nth count pipePtListRIGHT) %RIGHT offset)
                                           (polar (nth (+ count 1) pipePtListRIGHT) %RIGHT offset)
                                           (nth (+ count 1) pipePtListRIGHT)
                                           ""
                                  )
                                )
                              )
                              ; if count2 id EVEN the do pipe on left side
                              (if (= (fix (/ count2 2.0)) (/ count2 2.0))
                                (progn
                                  (command "PLINE"
                                           (nth count pipePtListLEFT)
                                           (polar (nth count pipePtListLEFT) %LEFT offset)
                                           (polar (nth (+ count 1) pipePtListLEFT) %LEFT offset)
                                           (nth (+ count 1) pipePtListLEFT)
                                           ""
                                  )
                                )
                              )
                            )
                          )
                          ; end if not last row 
                          (setq count2 (+ count2 1))
                          (setq count (+ count 1))
                        )
                        ; end repeat
                      )
                    )
                    ; END IF (> ITEM 1)

                    ; START IF (= ITEM 1)
                    (if (= item 1)
                      (setq count (+ count 1))
                    )
                    ; END IF (= ITEM 1)               

                    (setq count2 (+ count2 1))
                  )
                )
              )
                   ; end if (= side "CR")

            )
            ;;; END (serialMULTI) - SERIAL FOR SEQUENCE OF ROWS
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;; START (dboxALL) - Draws dbox all
            (defun dboxALL (location)
              (setq inletListLEFT pipePtListLEFT)
              (setq inletListRIGHT pipePtListRIGHT)

              (if (= location "TR") (dboxTop "TR"))
              (if (= location "TL") (dboxTop "TL"))
              (if (= location "CL") (dboxCENTER "CL"))
              (if (= location "CR") (dboxCENTER "CR"))

              ; Prints Label for D-Box and Pipe Label
              (SETQ DIMASZ_DEF (getvar "DIMASZ"))
              (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
              (SETVAR "DIMASZ" (* page3TextSize scaleArrowSize))
              (SETVAR "DIMTXT" page3TextSize)

              (if (or (= location "TR") (= location "CR"))
                (progn
                  (setq arrowPt (polar p0 %RIGHT (* dBoxLength 0.5)))
                  (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %RIGHT (* page3TextSize 4.0)))
                  (COMMAND "LEADER" arrowPt textPt "" (cdr (assoc "%DBOX-ALL-1%" $customSysList)) "")
                )
              )
              (if (or (= location "TL") (= location "CL"))
                (progn
                  (setq arrowPt (polar p0 %LEFT (* dBoxLength 0.5)))
                  (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %LEFT (* page3TextSize 4.0)))
                  (COMMAND "LEADER" arrowPt textPt "" (cdr (assoc "%DBOX-ALL-1%" $customSysList)) "")
                )
              )


              (if (member sys_type $endCapChambers)
                (progn
                  (if (or (= location "TR") (= location "CR"))
                    (progn
                      (setq arrowPt (nth 0 pipePtListLEFT))
                      (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %LEFT (* page3TextSize 4.0)))
                      (COMMAND "LEADER" arrowPt textPt "" endCapNote "")
                    )
                  )
                  (if (or (= location "TL") (= location "CL"))
                    (progn
                      (setq arrowPt (nth 0 pipePtListRIGHT))
                      (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %RIGHT (* page3TextSize 4.0)))
                      (COMMAND "LEADER" arrowPt textPt "" endCapNote "")
                    )
                  )

                )
              )


                    ; add endcap note for certain systems
              (if (or (= SYS_TYPE "MOUNDBUSTER")
                      (= SYS_TYPE "ELJEN")
                      (= SYS_TYPE "ELJEN_INVERTED")
                      (= SYS_TYPE "TRENCH2")
                      (= SYS_TYPE "TRENCH3")
                  )
                (progn

                  (IF (or (= location "TR") (= location "CR"))
                    (progn
                      (setq arrowPt (nth 0 inletListLEFT))
                      (setq TextPt (polar arrowPt %LEFT (* page3TextSize 4.0)))
                    )
                  )
                  (if (or (= location "TL") (= location "CL"))
                    (progn
                      (setq arrowPt (nth 0 inletListRIGHT))
                      (setq TextPt (polar arrowPt %RIGHT (* page3TextSize 4.0)))
                    )
                  )

                  (addEndCapNote arrowPt TextPt (cdr (assoc "%DBOX-ALL-2%" $customSysList)))
                )
              )


              (SETVAR "DIMTXT" DIMTXT_DEF)
              (SETVAR "DIMASZ" DIMASZ_DEF)


            )
            ;;; END (dboxALL) - Draws dbox all
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;; START (dboxCENTER) - DRAWS DBOX WHEN ON CENTER SIDE OF SYSTEM
            ;;; works with list: inletListLEFT & inletListRIGHT
            ;;; syntax: (dboxCENTER "CL") or "CR"
            (defun dboxCENTER (location)
              ; use different dbox based if >5 outlets
              (if (<= (length inletListLEFT) 5) (setq dBoxWidth 1))
              (if (> (length inletListLEFT) 5) (setq dBoxWidth 1.5))

              (setq dBoxLength (+ 0.5 (* 0.25 (length inletListLEFT))))
              (setq pipeOffset 1)

              (setq offset pipeoffset)
              (setq dBoxPipeOffset 0.5)

              (if (AND
                    (= pipeType "2")
                    (= (substr sys_type 5 16) "CONCRETE_CLUSTER")
                  )
                (setq pipeOffset (+ pipeOffset (/ stonebeside 12.0)))
              )
              (if (AND
                    (= pipeType "2")
                    (= (substr sys_type 5 15) "CONCRETE_TRENCH")
                  )
                (setq pipeOffset (+ pipeOffset 1))
              )

              (IF (= dBoxWidth 1)
                (progn
                  (IF (= location "CL")
                    (progn
                      ; create point for D-box rectangle (dbox inlet = p0) (corners = p1-p4)
                      (setq startPt (fix (/ (length inletListLEFT) 2.0)))
                      (setq p0 (polar (nth startPt inletListLEFT) %LEFT pipeOffset))
                      (setq p1 (polar p0 %UP (/ dBoxWidth 2.0)))
                      (setq p2 (polar p1 %DOWN dboxWidth))
                      (setq p3 (polar p2 %LEFT dBoxLength))
                      (setq p4 (polar p1 %LEFT dBoxLength))
                      (command "PLINE" p0 (nth startPt inletListLEFT) "")
                      (command "PLINE" p1 p2 p3 p4 p1 "")

                      ; draw pipes on uphill side of system
                      (setq count (- startPt 1))
                      (setq dBoxInlet (polar p1 %LEFT dBoxPipeOffset))
                      (while (>= count 0)
                        (setq chamberInlet (nth count inletListLEFT))
                        (setq chamberInlet2 (polar chamberInlet %LEFT 1))
                        (setq dBoxInlet2 (polar dBoxInlet %UP 1))
                        (setq intPt (lineIntersection dBoxInlet dBoxInlet2 chamberInlet chamberInlet2))
                        (command "PLINE" dboxInlet intPt chamberInlet "")
                        (setq dBoxInlet (polar dBoxInlet %LEFT dBoxPipeOffset))
                        (setq count (- count 1))
                      )
                      ; draw pipes on downhill side of system
                      (setq count (+ startPt 1))
                      (setq dBoxInlet (polar p2 %LEFT dBoxPipeOffset))
                      (while (<= count (- (length inletListLEFT) 1))
                        (setq chamberInlet (nth count inletListLEFT))
                        (setq chamberInlet2 (polar chamberInlet %LEFT 1))
                        (setq dBoxInlet2 (polar dBoxInlet %DOWN 1))
                        (setq intPt (lineIntersection dBoxInlet dBoxInlet2 chamberInlet chamberInlet2))
                        (command "PLINE" dboxInlet intPt chamberInlet "")
                        (setq dBoxInlet (polar dBoxInlet %LEFT dBoxPipeOffset))
                        (setq count (+ count 1))
                      )

                      ; Draw inlet pipe to D-box
                      (command "PLINE" (polar p0 %LEFT dBoxLength) (polar p0 %LEFT (+ dBoxLength 5)) (polar p0 (- %LEFT 0.5) (+ dBoxLength 10)) "")

                    )
                  )
                  ; END IF "CL"
                  (IF (= location "CR")
                    (progn
                      ; create point for D-box rectangle (dbox inlet = p0) (corners = p1-p4)
                      (setq startPt (fix (/ (length inletListRIGHT) 2.0)))
                      (setq p0 (polar (nth startPt inletListRIGHT) %RIGHT pipeOffset))
                      (setq p1 (polar p0 %UP (/ dBoxWidth 2.0)))
                      (setq p2 (polar p1 %DOWN dboxWidth))
                      (setq p3 (polar p2 %RIGHT dBoxLength))
                      (setq p4 (polar p1 %RIGHT dBoxLength))
                      (command "PLINE" p0 (nth startPt inletListRIGHT) "")
                      (command "PLINE" p1 p2 p3 p4 p1 "")

                      ; draw pipes on uphill side of system
                      (setq count (- startPt 1))
                      (setq dBoxInlet (polar p1 %RIGHT dBoxPipeOffset))
                      (while (>= count 0)
                        (setq chamberInlet (nth count inletListRIGHT))
                        (setq chamberInlet2 (polar chamberInlet %RIGHT 1))
                        (setq dBoxInlet2 (polar dBoxInlet %UP 1))
                        (setq intPt (lineIntersection dBoxInlet dBoxInlet2 chamberInlet chamberInlet2))
                        (command "PLINE" dboxInlet intPt chamberInlet "")
                        (setq dBoxInlet (polar dBoxInlet %RIGHT dBoxPipeOffset))
                        (setq count (- count 1))
                      )
                      ; draw pipes on downhill side of system
                      (setq count (+ startPt 1))
                      (setq dBoxInlet (polar p2 %RIGHT dBoxPipeOffset))
                      (while (<= count (- (length inletListRIGHT) 1))
                        (setq chamberInlet (nth count inletListRIGHT))
                        (setq chamberInlet2 (polar chamberInlet %LEFT 1))
                        (setq dBoxInlet2 (polar dBoxInlet %DOWN 1))
                        (setq intPt (lineIntersection dBoxInlet dBoxInlet2 chamberInlet chamberInlet2))
                        (command "PLINE" dboxInlet intPt chamberInlet "")
                        (setq dBoxInlet (polar dBoxInlet %RIGHT dBoxPipeOffset))
                        (setq count (+ count 1))
                      )
                      ; Draw inlet pipe to D-box
                      (command "PLINE" (polar p0 %RIGHT dBoxLength) (polar p0 %RIGHT (+ dBoxLength 5)) (polar p0 (- %RIGHT 0.5) (+ dBoxLength 10)) "")
                    )
                  )
                  ; END IF "CR"
                )
              )

              (IF (= dBoxWidth 1.5)
                (progn
                  (setq pipeOffset 2)
                  (IF (= location "CL")
                    (progn
                      ; create point for D-box rectangle (dbox inlet = p0) (corners = p1-p4)
                      (setq startPt (fix (/ (length inletListLEFT) 2.0)))
                      (setq p0 (polar (nth startPt inletListLEFT) %LEFT pipeOffset))
                      (setq p0 (polar p0 %UP (/ (distance (nth startPt inletListLEFT) (nth (- startPt 1) inletListLEFT)) 2.0)))
                      (setq p1 (polar p0 %UP (/ dBoxWidth 2.0)))
                      (setq p2 (polar p1 %DOWN dboxWidth))
                      (setq p3 (polar p2 %LEFT dBoxLength))
                      (setq p4 (polar p1 %LEFT dBoxLength))

                      ; draw the d-box
                      (command "PLINE" p1 p2 p3 p4 "CLOSE")

                      ; draw two pipes from center of D-box
                      (setq a1 (polar p0 %UP (/ dBoxPipeOffset 2.0)))
                      (setq a1a (polar a1 %RIGHT (/ pipeOffset 2.0)))
                      (setq a1b (polar a1a %UP (- (/ (distance (nth startPt inletListLEFT) (nth (- startPt 1) inletListLEFT)) 2.0) (/ dBoxPipeOffset 2.0))))
                      (setq a2 (polar p0 %DOWN (/ dBoxPipeOffset 2.0)))
                      (setq a2a (polar a2 %RIGHT (/ pipeOffset 2.0)))
                      (setq a2b (polar a2a %DOWN (- (/ (distance (nth startPt inletListLEFT) (nth (- startPt 1) inletListLEFT)) 2.0) (/ dBoxPipeOffset 2.0))))

                      (command "PLINE" a1 a1a a1b (nth (- startPt 1) inletListLEFT) "")
                      (command "PLINE" a2 a2a a2b (nth startPt inletListLEFT) "")

                      ; draw pipes on uphill side of system
                      (setq count (- startPt 2))
                      (setq dBoxInlet (polar p1 %LEFT dBoxPipeOffset))
                      (while (>= count 0)
                        (setq chamberInlet (nth count inletListLEFT))
                        (setq chamberInlet2 (polar chamberInlet %RIGHT 1))
                        (setq dBoxInlet2 (polar dBoxInlet %UP 1))
                        (setq intPt (lineIntersection dBoxInlet dBoxInlet2 chamberInlet chamberInlet2))
                        (command "PLINE" dboxInlet intPt chamberInlet "")
                        (setq dBoxInlet (polar dBoxInlet %LEFT dBoxPipeOffset))
                        (setq count (- count 1))
                      )
                      ; draw pipes on downhill side of system
                      (setq count (+ startPt 1))
                      (setq dBoxInlet (polar p2 %LEFT dBoxPipeOffset))
                      (while (<= count (- (length inletListLEFT) 1))
                        (setq chamberInlet (nth count inletListLEFT))
                        (setq chamberInlet2 (polar chamberInlet %RIGHT 1))
                        (setq dBoxInlet2 (polar dBoxInlet %UP 1))
                        (setq intPt (lineIntersection dBoxInlet dBoxInlet2 chamberInlet chamberInlet2))
                        (command "PLINE" dboxInlet intPt chamberInlet "")
                        (setq dBoxInlet (polar dBoxInlet %LEFT dBoxPipeOffset))
                        (setq count (+ count 1))
                      )
                      ; Draw inlet pipe to D-box
                      (command "PLINE" (polar p0 %LEFT dBoxLength) (polar p0 %LEFT (+ dBoxLength 5)) (polar p0 (- %LEFT 0.5) (+ dBoxLength 10)) "")
                    )
                  )
                  ; END IF "CL"
                  (IF (= location "CR")
                    (progn
                      ; create point for D-box rectangle (dbox inlet = p0) (corners = p1-p4)
                      (setq startPt (fix (/ (length inletListRIGHT) 2.0)))
                      (setq p0 (polar (nth startPt inletListRIGHT) %RIGHT pipeOffset))
                      (setq p0 (polar p0 %UP (/ (distance (nth startPt inletListRIGHT) (nth (- startPt 1) inletListRIGHT)) 2.0)))
                      (setq p1 (polar p0 %UP (/ dBoxWidth 2.0)))
                      (setq p2 (polar p1 %DOWN dboxWidth))
                      (setq p3 (polar p2 %RIGHT dBoxLength))
                      (setq p4 (polar p1 %RIGHT dBoxLength))

                      ; draw the d-box
                      (command "PLINE" p1 p2 p3 p4 "CLOSE")

                      ; draw two pipes from center of D-box
                      (setq a1 (polar p0 %UP (/ dBoxPipeOffset 2.0)))
                      (setq a1a (polar a1 %LEFT (/ pipeOffset 2.0)))
                      (setq a1b (polar a1a %UP (- (/ (distance (nth startPt inletListRIGHT) (nth (- startPt 1) inletListRIGHT)) 2.0) (/ dBoxPipeOffset 2.0))))
                      (setq a2 (polar p0 %DOWN (/ dBoxPipeOffset 2.0)))
                      (setq a2a (polar a2 %LEFT (/ pipeOffset 2.0)))
                      (setq a2b (polar a2a %DOWN (- (/ (distance (nth startPt inletListRIGHT) (nth (- startPt 1) inletListRIGHT)) 2.0) (/ dBoxPipeOffset 2.0))))

                      (command "PLINE" a1 a1a a1b (nth (- startPt 1) inletListRIGHT) "")
                      (command "PLINE" a2 a2a a2b (nth startPt inletListRIGHT) "")

                      ; draw pipes on uphill side of system
                      (setq count (- startPt 2))
                      (setq dBoxInlet (polar p1 %RIGHT dBoxPipeOffset))
                      (while (>= count 0)
                        (setq chamberInlet (nth count inletListRIGHT))
                        (setq chamberInlet2 (polar chamberInlet %RIGHT 1))
                        (setq dBoxInlet2 (polar dBoxInlet %UP 1))
                        (setq intPt (lineIntersection dBoxInlet dBoxInlet2 chamberInlet chamberInlet2))
                        (command "PLINE" dboxInlet intPt chamberInlet "")
                        (setq dBoxInlet (polar dBoxInlet %RIGHT dBoxPipeOffset))
                        (setq count (- count 1))
                      )
                      ; draw pipes on downhill side of system
                      (setq count (+ startPt 1))
                      (setq dBoxInlet (polar p2 %RIGHT dBoxPipeOffset))
                      (while (<= count (- (length inletListRIGHT) 1))
                        (setq chamberInlet (nth count inletListRIGHT))
                        (setq chamberInlet2 (polar chamberInlet %RIGHT 1))
                        (setq dBoxInlet2 (polar dBoxInlet %UP 1))
                        (setq intPt (lineIntersection dBoxInlet dBoxInlet2 chamberInlet chamberInlet2))
                        (command "PLINE" dboxInlet intPt chamberInlet "")
                        (setq dBoxInlet (polar dBoxInlet %RIGHT dBoxPipeOffset))
                        (setq count (+ count 1))
                      )
                      ; Draw inlet pipe to D-box
                      (command "PLINE" (polar p0 %RIGHT dBoxLength) (polar p0 %RIGHT (+ dBoxLength 5)) (polar p0 (+ %RIGHT 0.5) (+ dBoxLength 10)) "")
                    )
                  )
                  ; END IF "CR"
                )
              )

                    ; add endcap note for certain systems
              (if (or (= SYS_TYPE "MOUNDBUSTER")
                      (= SYS_TYPE "ELJEN")
                      (= SYS_TYPE "ELJEN_INVERTED")
                      (= SYS_TYPE "TRENCH2")
                      (= SYS_TYPE "TRENCH3")
                  )
                (progn

                  (IF (= location "CR")
                    (progn
                      (setq arrowPt (nth 0 inletListLEFT))
                      (setq TextPt (polar arrowPt %LEFT (* page3TextSize 4.0)))
                    )
                  )
                  (IF (= location "CL")
                    (progn
                      (setq arrowPt (nth 0 inletListRIGHT))
                      (setq TextPt (polar arrowPt %RIGHT (* page3TextSize 4.0)))
                    )
                  )

                  (addEndCapNote arrowPt TextPt (cdr (assoc "%DBOX-SERIAL-3%" $customSysList)))
                )
              )

            )
            ;;; END (dboxCENTER) - DRAWS DBOX WHEN ON CENTER SIDE OF SYSTEM
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


            (defun addEndcapNote (arrowPt textPt txt)
              (SETQ DIMASZ_DEF (getvar "DIMASZ"))
              (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
              (SETVAR "DIMASZ" (* page3TextSize scaleArrowSize))
              (SETVAR "DIMTXT" page3TextSize)

              (COMMAND "LEADER" arrowPt textPt "" txt "")

              (SETVAR "DIMTXT" DIMTXT_DEF)
              (SETVAR "DIMASZ" DIMASZ_DEF)

            )




            ; START (dBoxPipeLabels-serial) - Label D-Box Serial
            (defun dBoxPipeLabels-serial (location)

              ; Prints Label for D-Box and Pipe Label
              (SETQ DIMASZ_DEF (getvar "DIMASZ"))
              (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
              (SETVAR "DIMASZ" (* page3TextSize scaleArrowSize))
              (SETVAR "DIMTXT" page3TextSize)

              (IF (= location "TL")
                (progn
                  (setq arrowPt (polar p0 %LEFT (* dBoxLength 0.5)))
                  (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %LEFT (* page3TextSize 4.0)))
                  (COMMAND "LEADER" arrowPt textPt "" (cdr (assoc "%DBOX-SERIAL-1%" $customSysList)) "")

                  (setq arrowPt (polar (polar (nth 0 inletListRIGHT) %DOWN (/ row_sep 12.0)) %RIGHT offset))
                  (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %RIGHT (* page3TextSize 4.0)))
                  (COMMAND "LEADER" arrowPt textPt "" (cdr (assoc "%DBOX-SERIAL-2%" $customSysList)) "")
                )
              )
              (IF (= location "TR")
                (progn
                  (setq arrowPt (polar p0 %RIGHT (* dBoxLength 0.5)))
                  (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %RIGHT (* page3TextSize 4.0)))
                  (COMMAND "LEADER" arrowPt textPt "" (cdr (assoc "%DBOX-SERIAL-1%" $customSysList)) "")

                  (setq arrowPt (polar (polar (nth 0 inletListLEFT) %DOWN (/ row_sep 12.0)) %LEFT offset))
                  (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %LEFT (* page3TextSize 4.0)))
                  (COMMAND "LEADER" arrowPt textPt "" (cdr (assoc "%DBOX-SERIAL-2%" $customSysList)) "")
                )
              )

              (IF (= location "CR")
                (progn
                  (setq arrowPt (polar p0 %RIGHT (* dBoxLength 0.5)))
                  (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %RIGHT (* page3TextSize 4.0)))
                  (COMMAND "LEADER" arrowPt textPt "" (cdr (assoc "%DBOX-SERIAL-1%" $customSysList)) "")

                  (setq arrowPt (polar (polar (nth 0 inletListLEFT) %DOWN (/ row_sep 12.0)) %LEFT offset))
                  (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %LEFT (* page3TextSize 4.0)))
                  (COMMAND "LEADER" arrowPt textPt "" (cdr (assoc "%DBOX-SERIAL-2%" $customSysList)) "")
                )
              )
              (IF (= location "CL")
                (progn
                  (setq arrowPt (polar p0 %LEFT (* dBoxLength 0.5)))
                  (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %LEFT (* page3TextSize 4.0)))
                  (COMMAND "LEADER" arrowPt textPt "" (cdr (assoc "%DBOX-SERIAL-1%" $customSysList)) "")

                  (setq arrowPt (polar (polar (nth 0 inletListRIGHT) %DOWN (/ row_sep 12.0)) %RIGHT offset))
                  (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %RIGHT (* page3TextSize 4.0)))
                  (COMMAND "LEADER" arrowPt textPt "" (cdr (assoc "%DBOX-SERIAL-2%" $customSysList)) "")
                )
              )


                    ; add endcap note for certain systems
              (if (or (= SYS_TYPE "MOUNDBUSTER")
                      (= SYS_TYPE "ELJEN")
                      (= SYS_TYPE "ELJEN_INVERTED")
                      (= SYS_TYPE "TRENCH2")
                      (= SYS_TYPE "TRENCH3")
                  )
                (progn

                  (IF (= location "TR")
                    (progn
                      (setq arrowPt (last inletListLEFT))
                      (setq TextPt (polar arrowPt %LEFT (* page3TextSize 4.0)))
                    )
                  )
                  (IF (= location "TL")
                    (progn
                      (setq arrowPt (last inletListRIGHT))
                      (setq TextPt (polar arrowPt %RIGHT (* page3TextSize 4.0)))
                    )
                  )

                  (addEndCapNote arrowPt TextPt (cdr (assoc "%DBOX-SERIAL-3%" $customSysList)))
                )
              )




              (if (member sys_type $endCapChambers)
                (progn
                  (if (or (= location "TR") (= location "CR"))
                    (progn
                      (setq arrowPt (nth 0 pipePtListLEFT))
                      (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %LEFT (* page3TextSize 4.0)))
                      (COMMAND "LEADER" arrowPt textPt "" endCapNote "")
                    )
                  )
                  (if (or (= location "TL") (= location "CL"))
                    (progn
                      (setq arrowPt (nth 0 pipePtListRIGHT))
                      (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %RIGHT (* page3TextSize 4.0)))
                      (COMMAND "LEADER" arrowPt textPt "" endCapNote "")
                    )
                  )

                )
              )














              (SETVAR "DIMTXT" DIMTXT_DEF)
              (SETVAR "DIMASZ" DIMASZ_DEF)

            )
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;;






            ; START (dBoxPipeLabels-manifold) - Label D-Box Manifold
            (defun dBoxPipeLabels-manifold (location)

              ; Prints Label for D-Box and Pipe Label
              (SETQ DIMASZ_DEF (getvar "DIMASZ"))
              (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
              (SETVAR "DIMASZ" (* page3TextSize scaleArrowSize))
              (SETVAR "DIMTXT" page3TextSize)

              (IF (= location "TL")
                (progn
                  (setq arrowPt (polar p0 %LEFT (* dBoxLength 0.5)))
                  (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %LEFT (* page3TextSize 4.0)))
                  (COMMAND "LEADER" arrowPt textPt "" (cdr (assoc "%DBOX-MAN-1%" $customSysList)) "")

                  (setq arrowPt (nth 0 inletListRIGHT))
                  (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %RIGHT (* page3TextSize 4.0)))
                  (COMMAND "LEADER" arrowPt textPt "" (cdr (assoc "%DBOX-MAN-2%" $customSysList)) "")
                )
              )
              (IF (= location "TR")
                (progn
                  (setq arrowPt (polar p0 %RIGHT (* dBoxLength 0.5)))
                  (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %RIGHT (* page3TextSize 4.0)))
                  (COMMAND "LEADER" arrowPt textPt "" (cdr (assoc "%DBOX-MAN-1%" $customSysList)) "")

                  (setq arrowPt (nth 0 inletListLEFT))
                  (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %LEFT (* page3TextSize 4.0)))
                  (COMMAND "LEADER" arrowPt textPt "" (cdr (assoc "%DBOX-MAN-2%" $customSysList)) "")
                )
              )

              (IF (= location "CR")
                (progn
                  (setq arrowPt (polar p0 %RIGHT (* dBoxLength 0.5)))
                  (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %RIGHT (* page3TextSize 4.0)))
                  (COMMAND "LEADER" arrowPt textPt "" (cdr (assoc "%DBOX-MAN-1%" $customSysList)) "")

                  (setq arrowPt (polar (nth 0 inletListLEFT) %DOWN (/ row_sep 12.0)))
                  (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %LEFT (* page3TextSize 4.0)))
                  (COMMAND "LEADER" arrowPt textPt "" (cdr (assoc "%DBOX-MAN-2%" $customSysList)) "")
                )
              )
              (IF (= location "CL")
                (progn
                  (setq arrowPt (polar p0 %LEFT (* dBoxLength 0.5)))
                  (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %LEFT (* page3TextSize 4.0)))
                  (COMMAND "LEADER" arrowPt textPt "" (cdr (assoc "%DBOX-MAN-1%" $customSysList)) "")

                  (setq arrowPt (polar (nth 0 inletListRIGHT) %DOWN (/ row_sep 12.0)))
                  (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %RIGHT (* page3TextSize 4.0)))
                  (COMMAND "LEADER" arrowPt textPt "" (cdr (assoc "%DBOX-MAN-2%" $customSysList)) "")
                )
              )



              (if (member sys_type $endCapChambers)
                (progn
                  (if (or (= location "TR") (= location "CR"))
                    (progn
                      (setq arrowPt (nth 0 pipePtListLEFT))
                      (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %LEFT (* page3TextSize 4.0)))
                      (COMMAND "LEADER" arrowPt textPt "" endCapNote "")
                    )
                  )
                  (if (or (= location "TL") (= location "CL"))
                    (progn
                      (setq arrowPt (nth 0 pipePtListRIGHT))
                      (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %RIGHT (* page3TextSize 4.0)))
                      (COMMAND "LEADER" arrowPt textPt "" endCapNote "")
                    )
                  )

                )
              )





              (SETVAR "DIMTXT" DIMTXT_DEF)
              (SETVAR "DIMASZ" DIMASZ_DEF)

            )
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;; START (dboxTOP) - DRAWS DBOX WHEN ON UPHILL SIDE OF SYSTEM
            ;;; works with list: inletListLEFT & inletListRIGHT
            ;;; syntax: (dboxTop "TL") or "TR"
            (defun dboxTOP (location / pipeOffset dBoxPipeOffset)
              (setq dBoxLength (* 0.5 (length inletListLEFT)))
              (setq dBoxWidth 1)
              (setq pipeOffset 1)

              (setq dBoxPipeOffset 0.5)

              (if (= (substr sys_type 5 16) "CONCRETE_CLUSTER")
                (progn
                  (setq pipeoffset (+ pipeoffset (/ (/ stonebeside 12.0) 2.0)))
                )
              )
              (if (= (substr sys_type 5 15) "CONCRETE_TRENCH")
                (progn
                  (setq pipeoffset (+ pipeoffset 0.5))
                )
              )

              (setq offset pipeoffset)

              (IF (= location "TL")
                (progn
                  ; create point for D-box rectangle (dbox inlet = p0) (corners = p1-p4)
                  (setq p0 (polar (nth 0 inletListLEFT) %LEFT pipeOffset))
                  (setq p1 (polar p0 %UP (/ dBoxWidth 2.0)))
                  (setq p2 (polar p1 %DOWN dboxWidth))
                  (setq p3 (polar p2 %LEFT dBoxLength))
                  (setq p4 (polar p1 %LEFT dBoxLength))
                  (command "PLINE" p0 (nth 0 inletListLEFT) "")
                  (command "PLINE" p1 p2 p3 p4 p1 "")

                  (setq count 1)
                  (setq dBoxInlet (polar p2 %LEFT dBoxPipeOffset))
                  (repeat (- (length inletListLEFT) 1)
                    (setq chamberInlet (nth count inletListLEFT))
                    (setq chamberInlet2 (polar chamberInlet %LEFT 1))
                    (setq dBoxInlet2 (polar dBoxInlet %DOWN 1))
                    (setq intPt (lineIntersection dBoxInlet dBoxInlet2 chamberInlet chamberInlet2))
                    (command "PLINE" dboxInlet intPt chamberInlet "")

                    (setq dBoxInlet (polar dBoxInlet %LEFT dBoxPipeOffset))
                    (setq count (+ count 1))
                  )

                  ; Draw inlet pipe to D-box
                  (command "PLINE" (polar p0 %LEFT dBoxLength) (polar p0 %LEFT (+ dBoxLength 5)) (polar p0 (- %LEFT 0.5) (+ dBoxLength 10)) "")

                )
              )
              ; end if location = "TL"

              (IF (= location "TR")
                (progn
                  ; create point for D-box rectangle (dbox inlet = p0) (corners = p1-p4)
                  (setq p0 (polar (nth 0 inletListRIGHT) %RIGHT pipeOffset))
                  (setq p1 (polar p0 %UP (/ dBoxWidth 2.0)))
                  (setq p2 (polar p1 %DOWN dboxWidth))
                  (setq p3 (polar p2 %RIGHT dBoxLength))
                  (setq p4 (polar p1 %RIGHT dBoxLength))
                  (command "PLINE" p0 (nth 0 inletListRIGHT) "")
                  (command "PLINE" p1 p2 p3 p4 p1 "")

                  (setq count 1)
                  (setq dBoxInlet (polar p2 %RIGHT dBoxPipeOffset))
                  (repeat (- (length inletListRIGHT) 1)
                    (setq chamberInlet (nth count inletListRIGHT))
                    (setq chamberInlet2 (polar chamberInlet %LEFT 1))
                    (setq dBoxInlet2 (polar dBoxInlet %DOWN 1))
                    (setq intPt (lineIntersection dBoxInlet dBoxInlet2 chamberInlet chamberInlet2))
                    (command "PLINE" dboxInlet intPt chamberInlet "")

                    (setq dBoxInlet (polar dBoxInlet %RIGHT dBoxPipeOffset))
                    (setq count (+ count 1))
                  )

                  ; Draw inlet pipe to D-box
                  (command "PLINE" (polar p0 %RIGHT dBoxLength) (polar p0 %RIGHT (+ dBoxLength 5)) (polar p0 (+ %RIGHT 0.5) (+ dBoxLength 10)) "")

                )
              )
              ; end if location = "TR"


                    ; add endcap note for certain systems
              (if (or (= SYS_TYPE "MOUNDBUSTER")
                      (= SYS_TYPE "ELJEN")
                      (= SYS_TYPE "ELJEN_INVERTED")
                      (= SYS_TYPE "TRENCH2")
                      (= SYS_TYPE "TRENCH3")
                  )
                (progn

                  (IF (= location "TR")
                    (progn
                      (setq arrowPt (last inletListLEFT))
                      (setq TextPt (polar arrowPt %LEFT (* page3TextSize 4.0)))
                    )
                  )
                  (IF (= location "TL")
                    (progn
                      (setq arrowPt (last inletListRIGHT))
                      (setq TextPt (polar arrowPt %RIGHT (* page3TextSize 4.0)))
                    )
                  )

                  (addEndCapNote arrowPt TextPt (cdr (assoc "%DBOX-SERIAL-3%" $customSysList)))
                )
              )
            )
            ;;; END (dboxTOP) - DRAWS DBOX WHEN ON UPHILL SIDE OF SYSTEM
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;; START (manifoldMULTI) - MANIFOLD FOR SEQUENCE OF ROWS
            ;;; does not draw pipe if 1 is entered in sequence
            (defun manifoldMULTI (sequence / count2 count item)

              (setq offset 1)

              (if (= (substr sys_type 5 16) "CONCRETE_CLUSTER")
                (setq offset (+ offset (/ stonebeside 12.0)))
              )
              (if (= (substr sys_type 5 15) "CONCRETE_TRENCH")
                (setq offset (+ offset 1))
              )


              (setq inletListRIGHT (list))
              (setq inletListLEFT (list))

              (setq count 0)
              (foreach item sequence
                (setq count2 1)
                (repeat item
                  ; if more than 1 do this, else it must be 1
                  (if (AND (< count2 item) (AND (/= (nth count pipePtListRIGHT) nil) (/= (nth (+ count 1) pipePtListRIGHT) nil)))
                    (progn
                      ; create manifold lines
                      (command "PLINE"
                               (nth count pipePtListRIGHT)
                               (polar (nth count pipePtListRIGHT) %RIGHT offset)
                               (polar (nth (+ count 1) pipePtListRIGHT) %RIGHT offset)
                               (nth (+ count 1) pipePtListRIGHT)
                               ""
                      )
                      (command "PLINE"
                               (nth count pipePtListLEFT)
                               (polar (nth count pipePtListLEFT) %LEFT offset)
                               (polar (nth (+ count 1) pipePtListLEFT) %LEFT offset)
                               (nth (+ count 1) pipePtListLEFT)
                               ""
                      )

                      ; create inletList for pipe inlets on manifold 
                      (if (= count2 1)
                        (progn
                          (setq distbtw (distance (polar (nth count pipePtListRIGHT) %RIGHT offset) (polar (nth (+ count 1) pipePtListRIGHT) %RIGHT offset)))
                          (setq inletPt (polar (polar (nth count pipePtListRIGHT) %RIGHT offset) %DOWN (/ distbtw 2.0)))
                          (setq inletListRIGHT (append inletListRIGHT (list inletPt)))
                          (setq distbtw (distance (polar (nth count pipePtListLEFT) %LEFT offset) (polar (nth (+ count 1) pipePtListLEFT) %LEFT offset)))
                          (setq inletPt (polar (polar (nth count pipePtListLEFT) %LEFT offset) %DOWN (/ distbtw 2.0)))
                          (setq inletListLEFT (append inletListLEFT (list inletPt)))
                        )
                      )

                      (setq count2 (+ 1 count2))
                    )
                    ; end if more than 1, start if 1
                  )
                  ; end if 

                  (if (= item 1)
                    (progn
                      ; create stub pipes
                      (command "PLINE"
                               (nth count pipePtListRIGHT)
                               (polar (nth count pipePtListRIGHT) %RIGHT offset)
                               ""
                      )
                      (command "PLINE"
                               (nth count pipePtListLEFT)
                               (polar (nth count pipePtListLEFT) %LEFT offset)
                               ""
                      )
                      ; add point to inlet lists
                      (setq inletPt (polar (nth count pipePtListRIGHT) %RIGHT offset))
                      (setq inletListRIGHT (append inletListRIGHT (list inletPt)))
                      (setq inletPt (polar (nth count pipePtListLEFT) %LEFT offset))
                      (setq inletListLEFT (append inletListLEFT (list inletPt)))
                    )
                  )
                  ; end if 1


                  (setq count (+ count 1))
                )
                ; end repeat
              )
                    ; end foreach



            )
            ;;; END (manifoldMULTI) - MANIFOLD FOR SEQUENCE OF ROWS
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;; START (manifoldALL) - MANIFOLD ALL ROWS
            (defun manifoldALL (dboxLocation / inletPt)

              (setq offset 1)

              (if (= (substr sys_type 5 16) "CONCRETE_CLUSTER")
                (setq offset (+ offset (/ stonebeside 12.0)))
              )
              (if (= (substr sys_type 5 15) "CONCRETE_TRENCH")
                (setq offset (+ offset 1))
              )

            ;;;;;;;;;; Draw RIGHT Manifold
              (setq count 0)
              (while (< count (- (length pipePtListRIGHT) 1))
                (if (AND (/= (nth count pipePtListRIGHT) nil) (/= (nth (+ count 1) pipePtListRIGHT) nil))
                  (progn
                    (command "PLINE"
                             (nth count pipePtListRIGHT)
                             (polar (nth count pipePtListRIGHT) %RIGHT offset)
                             (polar (nth (+ count 1) pipePtListRIGHT) %RIGHT offset)
                             (nth (+ count 1) pipePtListRIGHT)
                             ""
                    )
                  )
                )
                (setq count (+ count 1))
              )
                    ; end while

            ;;;;;;;;;; Draw LEFT Manifold
              (setq count 0)
              (while (< count (- (length pipePtListLEFT) 1))
                (if (AND (/= (nth count pipePtListLEFT) nil) (/= (nth (+ count 1) pipePtListLEFT) nil))
                  (progn
                    (command "PLINE"
                             (nth count pipePtListLEFT)
                             (polar (nth count pipePtListLEFT) %LEFT offset)
                             (polar (nth (+ count 1) pipePtListLEFT) %LEFT offset)
                             (nth (+ count 1) pipePtListLEFT)
                             ""
                    )
                  )
                )
                (setq count (+ count 1))
              )
                    ; end while

            ;;;;;;;;;; Manifold inlet alway in middle of manifold... shy to upslope if odd rows, exact middle if even rows
              (if (OR (= dboxLocation "TL") (= dboxLocation "CL"))
                (progn
                  ; list item for pipe start point
                  (setq inletPt (fix (- (/ num_rows 2) 1)))
                  (setq inletPt (polar (nth inletPt pipePtListLEFT) %DOWN (/ (distance (nth inletPt pipePtListLEFT) (nth (+ inletPt 1) pipePtListLEFT)) 2.0)))
                  (setq inletPt (polar inletPt %LEFT offset))
                  (command "PLINE" inletPt (polar inletPt %LEFT 5.0) (polar inletPt (- %LEFT 0.5) 10.0) "")

                  ; point on distribution side
                  (setq arrowPt1 (polar (nth 0 pipePtListLEFT) %DOWN (* (distance (car pipePtListLEFT) (last pipePtListLEFT)) 0.20)))
                  (setq arrowPt1 (polar arrowPt1 %LEFT offset))
                  (setq textPt1 (polar (polar arrowPt1 %UP (* page3TextSize 4.0)) %LEFT (* page3TextSize 4.0)))
                  ; point on far side
                  (setq arrowPt2 (polar (nth 0 pipePtListRIGHT) %DOWN (* (distance (car pipePtListRIGHT) (last pipePtListRIGHT)) 0.20)))
                  (setq arrowPt2 (polar arrowPt2 %RIGHT offset))
                  (setq textPt2 (polar (polar arrowPt2 %UP (* page3TextSize 4.0)) %RIGHT (* page3TextSize 4.0)))
                )
              )
              (if (OR (= dboxLocation "TR") (= dboxLocation "CR"))
                (progn
                  ; list item for pipe start point
                  (setq inletPt (fix (- (/ num_rows 2.0) 1)))
                  (setq inletPt (polar (nth inletPt pipePtListRIGHT) %DOWN (/ (distance (nth inletPt pipePtListRIGHT) (nth (+ inletPt 1) pipePtListRIGHT)) 2.0)))
                  (setq inletPt (polar inletPt %RIGHT offset))
                  (command "PLINE" inletPt (polar inletPt %RIGHT 5.0) (polar inletPt (- %RIGHT 0.5) 10.0) "")

                  ; point on distribution side
                  (setq arrowPt2 (polar (nth 0 pipePtListLEFT) %DOWN (* (distance (car pipePtListLEFT) (last pipePtListLEFT)) 0.20)))
                  (setq arrowPt2 (polar arrowPt2 %LEFT offset))
                  (setq textPt2 (polar (polar arrowPt2 %UP (* page3TextSize 4.0)) %LEFT (* page3TextSize 4.0)))

                  ; point on far side
                  (setq arrowPt1 (polar (nth 0 pipePtListRIGHT) %DOWN (* (distance (car pipePtListRIGHT) (last pipePtListRIGHT)) 0.20)))
                  (setq arrowPt1 (polar arrowPt1 %RIGHT offset))
                  (setq textPt1 (polar (polar arrowPt1 %UP (* page3TextSize 4.0)) %RIGHT (* page3TextSize 4.0)))


                )
              )

                    ; Prints Label for D-Box and Pipe Label
              (SETQ DIMASZ_DEF (getvar "DIMASZ"))
              (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
              (SETVAR "DIMASZ" (* page3TextSize scaleArrowSize))
              (SETVAR "DIMTXT" page3TextSize)


              ; sets the pipe text for each system type
                    ; no stone
                    ; closeTxt is the side closes to distribution
                    ; farTxt is the side farthest to distribution

              (COMMAND "LEADER" arrowPt1 textPt1 "" (cdr (assoc "%MAN-ALL-1%" $customSysList)) "")
              (COMMAND "LEADER" arrowPt2 textPt2 "" (cdr (assoc "%MAN-ALL-2%" $customSysList)) "")





              (if (member sys_type $endCapChambers)
                (progn
                  (if (or (= dboxLocation "TR") (= dboxLocation "CR"))
                    (progn
                      (setq arrowPt (nth 0 pipePtListLEFT))
                      (setq textPt (polar (polar arrowPt %UP (* page3TextSize 5.0)) %LEFT (* page3TextSize 5.0)))
                      (COMMAND "LEADER" arrowPt textPt "" endCapNote "")
                    )
                  )
                  (if (or (= dboxLocation "TL") (= dboxLocation "CL"))
                    (progn
                      (setq arrowPt (nth 0 pipePtListRIGHT))
                      (setq textPt (polar (polar arrowPt %UP (* page3TextSize 5.0)) %RIGHT (* page3TextSize 5.0)))
                      (COMMAND "LEADER" arrowPt textPt "" endCapNote "")
                    )
                  )

                )
              )


              (SETVAR "DIMTXT" DIMTXT_DEF)
              (SETVAR "DIMASZ" DIMASZ_DEF)



            )
            ;;; END (manifoldALL) - MANIFOLD ALL ROWS
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ; START (serialALL) SERIAL ALL ROWS
            ; function draws serial all rows using pipePtListLeft and pipePtListRight variables
            ; inserts 1'x1' d-box and label for d-box and serial connectiong pipe
            (defun serialALL (dboxLocation / count pipeList dboxLocation offset inletPt DIMASZ_DEF DIMTXT_DEF p1 p2 p3 p4 arrowPt textPt pipeList inletPt1 inletPt2 inletPt3)

              (setq pipeList (list))

              (setq offset 1)


              (if (= (substr sys_type 5 16) "CONCRETE_CLUSTER")
                (setq offset (+ offset (/ stonebeside 12.0)))
              )
              (if (= (substr sys_type 5 15) "CONCRETE_TRENCH")
                (setq offset (+ offset 1))
              )

                 ;;;;;;;;;;;;;Start if TL;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              (if (or (= dboxLocation "TL") (= dboxLocation "CL"))
                (progn
                  (setq count 0)
                  (while (< count num_rows)
                    (setq PipeList (append pipeList (list (nth count pipePtListRight))))
                    (setq PipeList (append pipeList (list (nth (+ count 1) pipePtListRight))))
                    (setq PipeList (append pipeList (list (nth (+ count 1) pipePtListLeft))))
                    (setq PipeList (append pipeList (list (nth (+ count 2) pipePtListLeft))))
                    (setq count (+ count 2))
                  )

                  (setq count 0)
                  (while (< count (- (length pipeList) 1))
                    (if (AND (/= (nth count pipeList) nil) (/= (nth (+ count 1) pipeList) nil))
                      (progn
                        (command "PLINE"
                                 (nth count pipeList)
                                 (polar (nth count pipeList) %RIGHT offset)
                                 (polar (nth (+ count 1) pipeList) %RIGHT offset)
                                 (nth (+ count 1) pipeList)
                                 ""
                        )
                      )
                    )

                    (if (AND (nth (+ count 2) pipeList) (nth (+ count 3) pipeList))
                      (progn
                        (command "PLINE"
                                 (nth (+ count 2) pipeList)
                                 (polar (nth (+ count 2) pipeList) %LEFT offset)
                                 (polar (nth (+ count 3) pipeList) %LEFT offset)
                                 (nth (+ count 3) pipeList)
                                 ""
                        )
                      )
                    )
                    (setq count (+ count 4))
                  )
                  ; end while

                  ; draw 1' x 1' D-box with pipes 1.5 feet %RIGHT of the chambers
                  (setq inletPt1 (nth 0 pipePtListLEFT))
                  (setq inletPt2 (polar inletPt1 %LEFT 1.5))
                  (setq p1 (polar inletPt2 %DOWN 0.5))
                  (setq p2 (polar inletPt2 %UP 0.5))
                  (setq p3 (polar p2 %LEFT 1))
                  (setq p4 (polar p1 %LEFT 1))
                  (setq inletPt3 (polar p4 %UP 0.5))

                  (command "PLINE" inletPt1 inletPt2 "")
                  (command "PLINE" p1 p2 p3 p4 "CLOSE")
                  (command "PLINE" inletPt3 (polar inletPt3 %LEFT 5.0) (polar inletPt3 (- %LEFT 0.5) 10.0) "")

                  ; Prints Label for D-Box and Pipe Label
                  (SETQ DIMASZ_DEF (getvar "DIMASZ"))
                  (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
                  (SETVAR "DIMASZ" (* page3TextSize scaleArrowSize))
                  (SETVAR "DIMTXT" page3TextSize)

                  (setq arrowPt (polar p1 (angle p1 p3) (/ (distance p1 p3) 2.0)))
                  (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %LEFT (* page3TextSize 4.0)))
                  (COMMAND "LEADER" arrowPt textPt "" (cdr (assoc "%SERIAL-ALL-1%" $customSysList)) "")

                  (setq arrowPt (polar (polar (nth 0 pipeList) %DOWN (/ (distance (nth 0 pipeList) (nth 1 pipeList)) 2.0)) %RIGHT offset))
                  (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %RIGHT (* page3TextSize 4.0)))
                  (COMMAND "LEADER" arrowPt textPt "" (cdr (assoc "%SERIAL-ALL-2%" $customSysList)) "")


                  (if (member sys_type $endCapChambers)
                    (progn
                      (setq arrowPt (nth 0 pipePtListRIGHT))
                      (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %RIGHT (* page3TextSize 4.0)))
                      (COMMAND "LEADER" arrowPt textPt "" endCapNote "")
                    )
                  )




                  (SETVAR "DIMTXT" DIMTXT_DEF)
                  (SETVAR "DIMASZ" DIMASZ_DEF)


                )
              )
                 ;;;;;;;;;;;;;end if TL;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


                 ;;;;;;;;;;;;;Start if TR;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              (if (or (= dboxLocation "TR") (= dboxLocation "CR"))
                (progn
                  (setq count 0)
                  (while (< count num_rows)
                    (setq PipeList (append pipeList (list (nth count pipePtListLeft))))
                    (setq PipeList (append pipeList (list (nth (+ count 1) pipePtListLeft))))
                    (setq PipeList (append pipeList (list (nth (+ count 1) pipePtListRight))))
                    (setq PipeList (append pipeList (list (nth (+ count 2) pipePtListRight))))
                    (setq count (+ count 2))
                  )

                  (setq count 0)
                  (while (< count (- (length pipeList) 1))
                    (if (AND (/= (nth count pipeList) nil) (/= (nth (+ count 1) pipeList) nil))
                      (progn
                        (command "PLINE"
                                 (nth count pipeList)
                                 (polar (nth count pipeList) %LEFT offset)
                                 (polar (nth (+ count 1) pipeList) %LEFT offset)
                                 (nth (+ count 1) pipeList)
                                 ""
                        )
                      )
                    )

                    (if (AND (nth (+ count 2) pipeList) (nth (+ count 3) pipeList))
                      (progn
                        (command "PLINE"
                                 (nth (+ count 2) pipeList)
                                 (polar (nth (+ count 2) pipeList) %RIGHT offset)
                                 (polar (nth (+ count 3) pipeList) %RIGHT offset)
                                 (nth (+ count 3) pipeList)
                                 ""
                        )
                      )
                    )
                    (setq count (+ count 4))
                  )
                  ; end while

                  ; draw 1' x 1' D-box with pipes 1.5 feet %RIGHT of the chambers
                  (setq inletPt1 (nth 0 pipePtListRIGHT))
                  (setq inletPt2 (polar inletPt1 %RIGHT 1.5))
                  (setq p1 (polar inletPt2 %DOWN 0.5))
                  (setq p2 (polar inletPt2 %UP 0.5))
                  (setq p3 (polar p2 %RIGHT 1))
                  (setq p4 (polar p1 %RIGHT 1))
                  (setq inletPt3 (polar p4 %UP 0.5))

                  (command "PLINE" inletPt1 inletPt2 "")
                  (command "PLINE" p1 p2 p3 p4 "CLOSE")
                  (command "PLINE" inletPt3 (polar inletPt3 %RIGHT 5.0) (polar inletPt3 (- %RIGHT 0.5) 10.0) "")

                  ; Prints Label for D-Box and Pipe Label
                  (SETQ DIMASZ_DEF (getvar "DIMASZ"))
                  (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
                  (SETVAR "DIMASZ" (* page3TextSize scaleArrowSize))
                  (SETVAR "DIMTXT" page3TextSize)

                  (setq arrowPt (polar p1 (angle p1 p3) (/ (distance p1 p3) 2.0)))
                  (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %RIGHT (* page3TextSize 4.0)))
                  (COMMAND "LEADER" arrowPt textPt "" (cdr (assoc "%SERIAL-ALL-1%" $customSysList)) "")

                  (setq arrowPt (polar (polar (nth 0 pipeList) %DOWN (/ (distance (nth 0 pipeList) (nth 1 pipeList)) 2.0)) %LEFT offset))
                  (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %LEFT (* page3TextSize 4.0)))
                  (COMMAND "LEADER" arrowPt textPt "" (cdr (assoc "%SERIAL-ALL-2%" $customSysList)) "")



                  (if (member sys_type $endCapChambers)
                    (progn
                      (setq arrowPt (nth 0 pipePtListLEFT))
                      (setq textPt (polar (polar arrowPt %UP (* page3TextSize 4.0)) %LEFT (* page3TextSize 4.0)))
                      (COMMAND "LEADER" arrowPt textPt "" endCapNote "")

                    )
                  )




                  (SETVAR "DIMTXT" DIMTXT_DEF)
                  (SETVAR "DIMASZ" DIMASZ_DEF)

                )
              )
                 ;;;;;;;;;;;;;end if TR;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


            )
            ; END (serialALL)
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  
  
  
  
  
  
  
  
  ;;;;;;;;;; End Subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
    (command "UNDO" "BE")

  (toggle-OSNAP-Vars-OFF)

       ;;;--- Load the DCL file
  (setq dcl_id (load_dialog (strcat $septicadPathDCL "pipelayout.dcl")))

      ;;;--- See if the dialog box is already loaded
  (if (not (new_dialog "PIPELAYOUT" dcl_id))
    (progn
      (alert "The pipelayout.dcl file was not found!")
      (exit)
    )
  )

      ;;;--- If an action event occurs, do this function
  (action_tile "okay" "(setq ddiag 1)(pipeSaveVars)")
  (action_tile "ignore" "(setq ddiag 2)(done_dialog)")


  (setq typeList (list "D-Box All Rows" "Serial All Rows" "Serial with D-Box" "Manifold All Rows" "Manifold with D-Box"))
  (start_list "TYPE" 3)
  (mapcar 'add_list typeList)
  (end_list)

  (setq sideList (list "Top Left" "Center Left" "Top Right" "Center Right"))
  (start_list "SIDE" 3)
  (mapcar 'add_list sideList)
  (end_list)

  (if isShelf
    (progn
      (setq tempStr "")
      (foreach item shelfList
        (setq tempStr (strcat tempStr (rtos item 2 0) " "))
      )
      (setq tempStr (substr tempStr 1 (- (strlen tempStr) 1)))
      (set_tile "SEQ" tempStr)
    )
  )

  (if $pipeLastInputList
    (progn
      (set_tile "TYPE" (nth 0 $pipeLastInputList))
      (set_tile "SIDE" (nth 1 $pipeLastInputList))
      (set_tile "SEQ" (nth 2 $pipeLastInputList))
    )
  )

    ; function changes DCL tile image based on pull downs
  (checkDistributionPipeDialog)

      ;;;--- Display the dialog box
  (start_dialog)

      ;;;--- Unload the dialog box
  (unload_dialog dcl_id)

  (setq eljenSerialStr "Serial Distribution with Eljen GSF requires special coupler at end of each serial row or OVERFLOW PIPES.  Designer MUST manually adjust piping and notes manually as required.\nConsult Maine Eljen In-Drain GSF Design Manual.  For more information go to: \n     http://www.indrain.com/\n     or call (207) 894-7141")
  (setq moundbusterStr "Slight changes to distribution manifold at the manifold inlet are required\nConsult Moundbuster Design and Installation Manual")



  (if (= ddiag 1)
    (progn

      ; dboxall rows 
      (if (= pipeType "0")
        (dboxAll pipeSide)
      )
      ; serial all rows 
      (if (= pipeType "1")
        (progn
          (serialAll pipeSide)
          (if (or (= sys_type "ELJEN_INVERTED") (= sys_type "ELJEN"))
            (alert eljenSerialStr)
          )
        )
      )
      ; serial with d-box 
      (if (= pipeType "2")
        (progn
          (serialMULTI pipeSequence pipeSide)
          (if (= pipeSide "TR") (dboxTOP "TR"))
          (if (= pipeSide "TL") (dboxTOP "TL"))
          (if (= pipeSide "CR") (dboxCENTER "CR"))
          (if (= pipeSide "CL") (dboxCENTER "CL"))
          (dBoxPipeLabels-serial pipeSide)
          (if (or (= sys_type "ELJEN_INVERTED") (= sys_type "ELJEN"))
            (alert eljenSerialStr)
          )
        )
      )
      ; manifold all rows
      (if (= pipeType "3")
        (progn
          (manifoldALL pipeSide)
          (if (= sys_type "MOUNDBUSTER")
            (alert moundbusterStr)
          )
        )
      )

      ; manifold multi all rows
      (if (= pipeType "4")
        (progn
          (manifoldMULTI pipeSequence)
          (if (= pipeSide "TR") (dboxTOP "TR"))
          (if (= pipeSide "TL") (dboxTOP "TL"))
          (if (= pipeSide "CR") (dboxCENTER "CR"))
          (if (= pipeSide "CL") (dboxCENTER "CL"))
          (dBoxPipeLabels-manifold pipeSide)
          (if (= sys_type "MOUNDBUSTER")
            (alert moundbusterStr)
          )
        )
      )

      (if (= $ALLCAPS "YES")
        (ALLCAPS (ssget "X" '((0 . "TEXT,MTEXT") (8 . "SEPTICAD"))))
      )

    )
  )

  (toggle-OSNAP-Vars-RESET)
  (command "UNDO" "E")
  (princ)
  
  
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapDrawChambers (pt1 pt2 pt3 pt4 / count ptLeftOrig)



  ;;;;;;;;;; Start Subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
    

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;Draw crossover pipes for Eljen GSF if >40' long.  One cross over pipe per 40' length
        (defun mapDrawEljenCrossOver (ptTop ptBot / p1 p2 numCrossOverPipes offsetLength offset count DIMASZ_DEF DIMTXT_DEF)

          (SETQ DIMASZ_DEF (getvar "DIMASZ"))
          (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
          (SETVAR "DIMASZ" (* page3TextSize scaleArrowSize))
          (SETVAR "DIMTXT" page3TextSize)


                ; sys_L, sys_W, chamber_H, etc are in feet
          (setq numCrossOverPipes (fix (/ sys_L 40.0)))
          (setq count 0 offsetLength (/ sys_L (+ 1 numCrossOverPipes)))

          (if (= (fix (/ num_units 2.0))
                 (/ num_units 2.0)
              )
            (progn
              (setq offsetLength (+ offsetLength (/ chamber_L 2.0)))
            )
          )
          (setq offset offsetLength)

          (if (= isShelf "YES")
            (progn
              (while (< count numCrossOverPipes)
                (setq p1 (polar ptTop %DOWN (/ chamber_W 2.0)))
                (setq p1 (polar p1 %RIGHT offset))
                (setq p2 (polar ptBot %UP (+ (/ chamber_W 2.0) row_sep)))
                (setq p2 (polar p2 %RIGHT offset))
                (COMMAND "PLINE" p1 p2 "")

                ; adjust offset and count
                (setq offset (+ offset offsetLength))
                (setq count (+ count 1))
              )

              ; ONLY label first crossoverpipe
              (if (/= label "LABEL-NO")
                (progn
                  (setq TEXT_PT1 (polar p1 (angle p1 p2) (/ (+ chamber_W row_sep) 2.0)))
                  (setq TEXT_PT2 (polar TEXT_PT1 (/ pi 12.0) (* page3TextSize 12.0)))
                  (COMMAND "LEADER" TEXT_PT1 TEXT_PT2 "" "Non-Perforated Cross Over Pipe" "")
                  (setq label "LABEL-NO")
                )
              )
            )
          )

          (if (/= isShelf "YES")
            (progn

              (while (< count numCrossOverPipes)
                (setq p1 (polar c_u1 %DOWN (/ chamber_W 2.0)))
                (setq p1 (polar p1 %RIGHT offset))
                (setq p2 (polar c_d1 %UP (/ chamber_W 2.0)))
                (setq p2 (polar p2 %RIGHT offset))
                (COMMAND "PLINE" p1 p2 "")

                ; adjust offset and count
                (setq offset (+ offset offsetLength))
                (setq count (+ count 1))
              )

              ; ONLY label first crossoverpipe
              (if (/= label "LABEL-NO")
                (progn
                  (setq TEXT_PT1 (polar p1 (angle p1 p2) (/ (+ chamber_W row_sep) 2.0)))
                  (setq TEXT_PT2 (polar TEXT_PT1 (/ pi 12.0) (* page3TextSize 12.0)))
                  (COMMAND "LEADER" TEXT_PT1 TEXT_PT2 "" "Non-Perforated Cross Over Pipe" "")
                  (setq label "LABEL-NO")
                )
              )

            )
          )

          (SETVAR "DIMTXT" DIMTXT_DEF)
          (SETVAR "DIMASZ" DIMASZ_DEF)
        )
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ; draws pipe to connect current row with previous row
        (defun chamberInletListDefault (/ pipeOffset leftTop rightTop rightBot)

          ; Points for Left Side Chamber Center
          (setq leftTop (polar c_U1 %DOWN (/ chamber_w 2.0)))
          (setq leftBot (polar leftTop %DOWN (+ chamber_W row_sep)))

          ; Points for Right Side Chamber Center
          (setq rightTop (polar leftTop %RIGHT sys_L))
          (setq rightBot (polar leftBot %RIGHT sys_L))

          (setq pipeOffset 1)
          (setq count 1)

          (while (< count num_rows)
            ; add point to point list
            (setq pipePtListLeft (append pipePtListLeft (list leftTop)))
            (setq pipePtListRight (append pipePtListRight (list rightTop)))

            (setq leftTop (polar leftTop %DOWN (+ chamber_w row_sep)))
            (setq leftBot (polar leftTop %DOWN (+ chamber_W row_sep)))

            (setq rightTop (polar leftTop %RIGHT sys_L))
            (setq rightBot (polar leftBot %RIGHT sys_L))
            (setq count (+ count 1))
          )

        ; add last row center point to point lists
          (setq pipePtListLeft (append pipePtListLeft (list leftTop)))
          (setq pipePtListRight (append pipePtListRight (list rightTop)))

        )
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ; Creates inlet point on left and right
        (defun chamberInletListShelf (leftBot / pipeOffset)

          (setq pipeOffset 1)

          ; Points for Left Side Chamber Center
          (setq leftBot (polar leftBot %DOWN (/ chamber_w 2.0)))
          (setq leftTop (polar leftBot %UP (+ chamber_W row_sep)))

          ; Points for Right Side Chamber Center
          (setq rightBot (polar leftBot %RIGHT sys_L))
          (setq rightTop (polar leftTop %RIGHT sys_L))

          ; Draw pipes

          (setq pipePtListLeft (append pipePtListLeft (list leftBot)))
          (setq pipePtListRight (append pipePtListRight (list rightBot)))

        )
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;; End Subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (setq EljenCrossOverLabel "")
  (setq pipePtListLeft (list))
  (setq pipePtListRight (list))



;;;; ask for or set the endcap size and label string
  (if (member sys_type $endCapChambers)
    (progn

;endCapLen
;endCapDiv-cluster
;endCapDiv-trench
;endCapNote
;;; systems with only one option
      (if (= SYS_TYPE "Q4_PLUS_HC")
        (progn
          (setq endCapLen 1.51666666666667 endCapDiv-cluster 14.6 endCapDiv-trench 14.6 endCapNote (cdr (assoc "%P3-9%" $customSysList)))
        )
      )
      (if (= SYS_TYPE "Q4_PLUS_STD")
        (progn
          (setq endCapLen 1.51666666666667 endCapDiv-cluster 14.6 endCapDiv-trench 14.6 endCapNote (cdr (assoc "%P3-9%" $customSysList)))
        )
      )
      (if (= SYS_TYPE "QUICK4_EQ24")
        (progn
          (setq endCapLen 0.9583333333 endCapDiv-cluster 7.52 endCapDiv-trench 7.52 endCapNote (cdr (assoc "%P3-9%" $customSysList)))
        )
      )
      (if (= SYS_TYPE "QUICK4_EQ24LP")
        (progn
          (setq endCapLen 0.375 endCapDiv-cluster 0 endCapDiv-trench 0 endCapNote (cdr (assoc "%P3-9%" $customSysList)))
        )
      )
      (if (= SYS_TYPE "QUICK4_EQ36")
        (progn
          (setq endCapLen 0.958333333333333 endCapDiv-cluster 6.4 endCapDiv-trench 6.4 endCapNote (cdr (assoc "%P3-9%" $customSysList)))
        )
      )

      (if (= SYS_TYPE "QUICK4_HI_CAP")
        (progn
          (setq endCapLen 1.208333333 endCapDiv-cluster 13.97 endCapDiv-trench 19.28 endCapNote (cdr (assoc "%P3-9%" $customSysList)))
        )
      )
      (if (= SYS_TYPE "QUICK4_STD")
        (progn
          (setq endCapLen 1.125 endCapDiv-cluster 12.81 endCapDiv-trench 15.47 endCapNote (cdr (assoc "%P3-9%" $customSysList)))
        )
      )

      ; FOR THESE TWO SYSTEMS THERE IS A CHOICE
      (IF (OR
            (= SYS_TYPE "Q4_PLUS_STD_LP")
            (= SYS_TYPE "Q4_PLUS_EQ_36_LP")
          )
        (PROGN
          ;;; START DCL DIALOG
          (setq dcl_id (load_dialog (strcat $septicadPathDCL "ENDCAP.dcl")))

          ;;;--- See if the dialog box is already loaded
          (if (not (new_dialog "ENDCAP" dcl_id))
            (progn
              (alert "The ENDCAP.DCL file was not found!")
              (exit)
            )
          )

          ;;;--- If an action event occurs, do this function
               (action_tile "cancel" "(setq ddiag 1)(done_dialog)")
               (action_tile "left" "(setq ddiag 2)(done_dialog)")
               (action_tile "right" "(setq ddiag 3)(done_dialog)")

               (set_tile "TEXT" "Select Endcap Type to Use")


          ;;;--- Display the dialog box
               (start_dialog)

          ;;;--- Unload the dialog box
               (unload_dialog dcl_id)

          ;;;--- If the cancel button was pressed
          (if (= ddiag 1)
            (progn
              (Print "Quick4 Plus 8 Endcap used as default.")
              ;  Quick4 Plus 8 Endcap (4.5" long, 1 sf each)
              (setq endCapLen 0.375 endCapDiv-cluster 2 endCapDiv-trench 2 endCapNote (cdr (assoc "%P3-9%" $customSysList)))
            )
          )

          ;;;--- If the "left" button was pressed
          (if (= ddiag 2)
            (PROGN
              ;  Quick4 Plus 8 Endcap (4.5" long, 1 sf each)
               (setq endCapLen 0.375 endCapDiv-cluster 2 endCapDiv-trench 2 endCapNote (cdr (assoc "%P3-9%" $customSysList)))
            )
          )


          ;;;--- If the "right" button was pressed
          (if (= ddiag 3)
            (PROGN
              ;  IF Quick4 Plus All-in-One 8 Endcap (13.3" long, 2.9 sf each) 
               (setq endCapLen 1.10833333333333 endCapDiv-cluster 5.8 endCapDiv-trench 5.8 endCapNote (cdr (assoc "%P3-10%" $customSysList)))
            )
          )

          ;;; END DCL DIALOG

        )
      )  ; END IF


    )
  )




; if concrete chamber then define shelfList
  (if (= (substr sys_type 5 16) "CONCRETE_CLUSTER")
    (progn
      (setq count 0)
      (while (< count num_rows)
        (setq shelfList (append shelfList (list 1)))
        (setq count (+ count 1))
      )
    )
  )

  (foreach shelf shelfList
    (progn
      (setq count 0)
      (setq ptLeftOrig pt4)
      (while (< count shelf)
        (setq ptLeft pt4)
        (mapDrawRow pt1 pt2 pt3 pt4)
        (if (and (= isShelf "YES") (> count 0))
          (progn
            ; add first row pipe point to list first 
            (if (= count 1)
              (progn
                (setq leftBot (polar ptLeft %DOWN (/ chamber_w 2.0)))
                (setq leftTop (polar leftBot %UP (+ chamber_W row_sep)))
                (setq rightBot (polar leftBot %RIGHT sys_L))
                (setq rightTop (polar leftTop %RIGHT sys_L))
                (setq pipePtListLeft (append pipePtListLeft (list leftTop)))
                (setq pipePtListRight (append pipePtListRight (list rightTop)))
              )
            )
            (chamberInletListShelf ptLeft)
          )
        )


        (setq pt1 (polar pt1 %DOWN (+ chamber_W row_sep)))
        (setq pt2 (polar pt2 %DOWN (+ chamber_W row_sep)))
        (setq pt3 (polar pt3 %DOWN (+ chamber_W row_sep)))
        (setq pt4 (polar pt4 %DOWN (+ chamber_W row_sep)))

        (setq count (+ count 1))
      )

      ; if eljen, system is longer than 40 feet and system is flat
      (if (and (OR (= SYS_TYPE "ELJEN") (= SYS_TYPE "ELJEN_INVERTED"))
               (>= sys_L 40.0)
               (= (cdr row_start) (cdr row_end))
          )
        (mapDrawEljenCrossOver ptLeftOrig pt4)
      )

      (if (= isShelf "YES")
        (progn
          ; adjust point to next shelf starting point
          (setq pt1 (polar pt1 %DOWN (- (/ shelf_sep 12.0) row_sep)))
          (setq pt2 (polar pt2 %DOWN (- (/ shelf_sep 12.0) row_sep)))
          (setq pt3 (polar pt3 %DOWN (- (/ shelf_sep 12.0) row_sep)))
          (setq pt4 (polar pt4 %DOWN (- (/ shelf_sep 12.0) row_sep)))
        )
      )

    )
  )

  (if (/= isShelf "YES") (chamberInletListDefault))

    ; if cluster concrete then need to move each pipePtListRIGHT point by 2x stonebeside
  (if (= (substr sys_type 5 16) "CONCRETE_CLUSTER")
    (progn
      (setq temp (list))
      (foreach item pipePtListRIGHT
        (setq temp (append temp (list (polar item %LEFT (* (/ stonebeside 12.0) 2.0)))))
      )
      (setq pipePtListRIGHT temp)
    )
  )

    ; if cluster concrete then need to move each pipePtListRIGHT point by 2 feet
  (if (= (substr sys_type 5 15) "CONCRETE_TRENCH")
    (progn
      (setq temp (list))
      (foreach item pipePtListRIGHT
        (setq temp (append temp (list (polar item %LEFT 2.0))))
      )
      (setq pipePtListRIGHT temp)
    )
  )


  (if (member sys_type $endCapChambers)
    (progn

      (setq temp (list))
      (foreach item pipePtListRIGHT
        (setq temp (append temp (list (polar item %RIGHT endCapLen))))
      )
      (setq pipePtListRIGHT temp)
      (setq temp (list))
      (foreach item pipePtListLEFT
        (setq temp (append temp (list (polar item %LEFT endCapLen))))
      )
      (setq pipePtListLEFT temp)

    )
  )

  (setq label nil)




)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun stoneChamberChoose (/ ddiag)

   ;;;;START
    ;;;;; CHOOSE TO PUT STONE AROUND CHAMBERS
       ;;;--- Load the DCL file
  (setq dcl_id (load_dialog (strcat $septicadPathDCL "stoneChamber.dcl")))

      ;;;--- See if the dialog box is already loaded
  (if (not (new_dialog "STONECHAMBER" dcl_id))
    (progn
      (alert "The stoneChamber.DCL file was not found!")
      (exit)
    )
  )

      ;;;--- If an action event occurs, do this function
  (action_tile "cancel" "(setq ddiag 1)(done_dialog)")
  (action_tile "yes" "(setq ddiag 2)(done_dialog)")
  (action_tile "no" "(setq ddiag 3)(done_dialog)")

      ;;;--- Display the dialog box
  (start_dialog)

      ;;;--- Unload the dialog box
  (unload_dialog dcl_id)

      ;;;--- If the cancel button was pressed
  (if (= ddiag 1)
    (progn
      (Princ "\nNo stone beside chambers used as default.")
      (setq $stoneChamber "NO")
    )
  )

      ;;;--- If the "yes" button was pressed
  (if (= ddiag 2)
    (setq $stoneChamber "YES")
  )

      ;;;--- If the "no" button was pressed
  (if (= ddiag 3)
    (setq $stoneChamber "NO")
  )
   ;;;; END
   ;;;;; CHOOSE TO PUT STONE AROUND CHAMBERS
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun H20-RatedChoose (/ ddiag)

   ;;;;START
    ;;;;; CHOOSE TO PUT STONE AROUND CHAMBERS
       ;;;--- Load the DCL file
  (setq dcl_id (load_dialog (strcat $septicadPathDCL "H20-CHOOSE.dcl")))

      ;;;--- See if the dialog box is already loaded
  (if (not (new_dialog "H20" dcl_id))
    (progn
      (alert "The H20-CHOOSE.DCL file was not found!")
      (exit)
    )
  )

      ;;;--- If an action event occurs, do this function
  (action_tile "CANCEL" "(setq ddiag 1)(done_dialog)")
  (action_tile "H20" "(setq ddiag 2)(done_dialog)")
  (action_tile "STANDARD" "(setq ddiag 3)(done_dialog)")

      ;;;--- Display the dialog box
  (start_dialog)

      ;;;--- Unload the dialog box
  (unload_dialog dcl_id)

      ;;;--- If the cancel button was pressed
  (if (= ddiag 1)
    (progn
      (Princ "\nRegular (non-H-20) Load Rated Chambers Used By Default")
      (setq $H20-RATED "NO")
    )
  )

      ;;;--- If the "yes" button was pressed
  (if (= ddiag 2)
    (setq $H20-RATED "YES")
  )

      ;;;--- If the "no" button was pressed
  (if (= ddiag 3)
    (setq $H20-RATED "NO")
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun makeBlock (blockName sset1 basePoint / ex)
  ; CALL -> (makeBlock "BLOCKNAME" (ssget) (getpoint))
  (setq ex (getvar "Expert"))
  (setvar "Expert" 2)
  ;sets block insert unit vars
  (setBlockUnitsVars)

  (command "_-BLOCK" blockName basePoint sset1 "")
  (command "_-INSERT" blockName basePoint "1" "1" "0")

   ;reset block insert unit vars
  (resetBlockUnitsVars)
  (setvar "Expert" ex)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun PRO_FILL_ERROR (msg / altstr)

       (if (AND
             (AND (= doCompound "NO")
                  (>= (angle slope_bot slope_top) 3.11160)
             )
                (= slopeType "SLOPING3")
           )
           ; IF probably involved with SLOPING3
         (progn
           (alert "Final Grade Type is Sloping 3% and Ground surface slope is < 3%.  Up slope fill does not intersect the ground surface.")
         )

         ; ELSE probably not involved with SLOPING3
         (progn
           (setq altstr "SeptiCAD ERROR\n\nMost likely the Fill extensions DO NOT intersect the ground surface.\n\nCheck elevation data or use a different Final Grade Type")
           (setq altstr (strcat altstr "\n\nGround surface slope beneath system footprint is " (rtos (* (/ (- (last slope_top) (last slope_bot)) sys_W) 100) 2 0) "%"))
           (alert altStr)
         )
       )


       (command "UNDO" "E")
       (command "UNDO" "1")

   ; generate log file
       (septiLog)

   ; reset error function	
       (setq *error* lastErrorFunction)
       (princ)

)
; end PRO_FILL_ERROR


(defun PRO_FILL (/ SH_U SH_D SH_U_PT SH_D_PT PT1 ANG1 FILL_ANG_DEG fillDraw temp systemSlope 3upRight 3upLeft 3downRight 3downLeft 2upLeft slopeDownExtend slopeUpExtend up down left right)

;;;;;;; Sets Current Layer
       (SETVAR "CLAYER" "SEPTICAD")

; taken out because it is already called in (main_drawing)
;	(toggle-OSNAP-Vars-OFF)

       (setq lastErrorFunction *error*)
       (setq *error* PRO_FILL_ERROR)

;;;--- Set default directions
       (setq down (/ (* 3.0 pi) 2.0))
       (setq up (/ pi 2.0))
       (setq right 0.0)
       (setq left pi)

       (setq 3upRight 0.0299910)
; 3% up - angle right
       (setq 3upLeft 3.11160)
       (setq 3downRight 6.25319)
       (setq 3downLeft 3.17158)
       (setq 2upLeft 3.12160)

	; Calculate the System Slope Based on ROW_START and ROW_END
       (setq systemSlope 0.00)
  (if (/= sys_type "STONE")
    (setq systemSlope (/ (- (last row_start) (last row_end)) sys_W))
  )


	; Used to do Refill form
       (setq autoDesign nil)

	; dummy var for working inside the if-then
       (setq temp slopeType)


;;;;;;;;;;;;;;;;;;;;;; START IF AUTO DESIGN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	; choose which slopeType to do - SLOPING or CROWN
  (if (= slopeType "AUTODESIGN")
    (progn
      ; for refill form
      (setq autoDesign "YES")

      ;;;-- Determine whether a 3% crown is required	
      (if (< systemSlope 0.03) (setq temp "CROWN"))


;;;;;;;;;;;;;;;;;;;;
      (if (>= systemSlope 0.03)
        (PROGN

          (setq temp "SLOPING")
          (IF (<= (- (+ (last row_start) chamber_H fill_above) (LAST SLOPE_TOP)) 4.0)
            (progn

              (if (< (angle slope_bot slope_top) 3upLeft) (setq temp "AUTO-SLOPING3"))
              ;(if (>= (angle slope_bot slope_top) 3upLeft)(setq temp "SLOPING"))
            )
          )

        )
      )
;;;;;;;;;;;;;;;;;;;



    )
  )


	; if temp was set then change to temp and let functions below work
       (if (= temp "AUTO-SLOPING3") (setq slopeType "SLOPING3"))
       (if (= temp "CROWN") (setq slopeType "CROWN"))
       (if (= temp "SLOPING") (setq slopeType "SLOPING"))
       (if (= temp "SLOPING3") (setq slopeType "SLOPING3"))


;;;;;;;;;;;;;;;;;;;;;; END IF AUTO DESIGN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






  (if (= slopeType "CROWN")
    (progn

      (SETQ fill_top (polar (POLAR row_start up (+ chamber_H fill_above)) left stonebeside))
      (SETQ fill_bot (POLAR fill_top right sys_w))
      ; angle of fill (GLOBAL VARIABLES)
      (SETQ angleBot (angle fill_top fill_bot))
      (SETQ angleTop (angle fill_bot fill_top))
    )
  )



  (if (= slopeType "SLOPING")
    (progn

      ;(SETQ fill_bot (POLAR row_end left (/ chamber_W 2.0)))
      ;(SETQ fill_bot (POLAR fill_bot up (+ chamber_H fill_above)))
      (SETQ fill_bot (polar (POLAR row_end up (+ chamber_H fill_above)) right stonebeside))

      (if (= (nth 0 shelfList) 1)
        (progn
          (SETQ fill_top (POLAR row_start right (/ chamber_W 2.0)))
          (SETQ fill_top (POLAR fill_top up (+ chamber_H fill_above)))
        )
      )

      (if (/= (nth 0 shelfList) 1)
        (progn
          (SETQ fill_top (POLAR row_start right (+ topShelfWidth (/ chamber_W 2.0))))
          (SETQ fill_top (POLAR fill_top up (+ chamber_H fill_above)))
        )
      )

      ; angle of fill (GLOBAL VARIABLES)
      (SETQ angleBot (angle fill_top fill_bot))
      (SETQ angleTop (angle fill_bot fill_top))

      ; move top of fill to edge of system - based on angle of system
      (SETQ fill_top (POLAR fill_top pi (+ stonebeside (/ chamber_W 2.0))))
;	(SETQ fill_bot (POLAR fill_bot angleBot (/ (/ chamber_W 2.0) (cos angleBot))))

      ; if top of system is buried then set fill_top to slope_top... 
      ; slope bottom is already set at minimum.
      (IF (>= (LAST SLOPE_TOP) (last fill_top)) (setq fill_top slope_top))

;(command "PLINE" row_start fill_top fill_bot row_end "")

    )
  )

  (if (= slopeType "SLOPING3")
    (progn

      (if (/= temp "AUTO-SLOPING3")
        (progn
          (setq UP_PRO_SLOPE 3.11160)
          (setq UP_SLOPE 3.11160)
        )
      )

      (if (<= systemSlope 0.03)
        (progn
          ; if compound grades not used when systemSlope <3% and groundsurface slope <3% then problem will occur
          (if (AND (= doCompound "NO")
                   (>= (angle slope_bot slope_top) 3upLeft)
              )
            (progn
              (Alert "Using 3% and System Slope of system is <= 3% - Upslope Fill Extension Will NOT intersect - CANCEL DESIGN")
              (exit)
            )
          )



          (SETQ fill_bot (POLAR row_end up (+ chamber_H fill_above)))
;		(SETQ fill_bot (POLAR row_end left (/ chamber_W 2.0)))
;		(SETQ fill_bot (POLAR fill_bot up (+ chamber_H fill_above)))

          (setq distFill (/ (- sys_w chamber_W) (ABS (cos 3upLeft))))

          (setq fill_top (polar fill_bot 3upLeft distFill))
        )
      )


      (if (> systemSlope 0.03)
        (progn
          (SETQ fill_bot (POLAR row_end left (/ chamber_W 2.0)))
          (SETQ fill_bot (POLAR fill_bot up (+ chamber_H fill_above)))

          (SETQ fill_top (POLAR row_start right (/ chamber_W 2.0)))
          (SETQ fill_top (POLAR fill_top up (+ chamber_H fill_above)))
        )
      )


      (IF (>= (LAST SLOPE_TOP) (last fill_top)) (setq fill_top slope_top))


      ; angle of fill (GLOBAL VARIABLES)
      (SETQ angleBot (angle fill_top fill_bot))
      (SETQ angleTop (angle fill_bot fill_top))

      (IF (< (LAST SLOPE_TOP) (last fill_top))
        (progn
          ; move top of fill to edge of system - based on angle of system
          (SETQ fill_top (POLAR fill_top (- angleTop pi) (/ (/ chamber_W 2.0) (cos angleTop))))
          (SETQ fill_bot (POLAR fill_bot angleBot (/ (/ chamber_W 2.0) (cos angleBot))))
        )
      )


;(command "PLINE" row_start fill_top fill_bot row_end "")

    )
  )



  (if (= isShelf "YES")
    (progn
      (if (= (nth 0 shelfList) 1)
        (progn
          (if (/= slopeType "SLOPING3") (SETQ FILL_TOP_SH (POLAR FILL_TOP left UP_SHOULDER)))
          (if (= slopeType "SLOPING3") (SETQ FILL_TOP_SH (POLAR FILL_TOP 3upLeft (abs (/ UP_SHOULDER (COS 3upLeft))))))
          (if (= slopeType "CROWN") (SETQ FILL_TOP_SH (POLAR FILL_TOP 3downLeft (abs (/ UP_SHOULDER (COS 3downLeft))))))

          (if (/= slopeType "CROWN") (SETQ FILL_BOT_SH (POLAR FILL_BOT angleBot (/ DOWN_SHOULDER (COS angleBot)))))
          (if (= slopeType "CROWN") (SETQ FILL_BOT_SH (POLAR FILL_BOT 3downRight (/ DOWN_SHOULDER (COS 3downRight)))))
        )
      )

      (if (/= (nth 0 shelfList) 1)
        (progn
          (if (/= slopeType "SLOPING3") (SETQ FILL_TOP_SH (POLAR FILL_TOP left (+ topShelfWidth UP_SHOULDER))))
          (if (= slopeType "SLOPING3") (SETQ FILL_TOP_SH (POLAR FILL_TOP 3upLeft (abs (/ UP_SHOULDER (COS 3upLeft))))))
          (if (= slopeType "CROWN") (SETQ FILL_TOP_SH (POLAR FILL_TOP 3downLeft (abs (/ UP_SHOULDER (COS 3downLeft))))))

          (if (/= slopeType "CROWN") (SETQ FILL_BOT_SH (POLAR FILL_BOT angleBot (/ DOWN_SHOULDER (COS angleBot)))))
          (if (= slopeType "CROWN") (SETQ FILL_BOT_SH (POLAR FILL_BOT 3downRight (/ DOWN_SHOULDER (COS 3downRight)))))
        )
      )
    )
  )

  (if (/= isShelf "YES")
    (progn
      (if (/= slopeType "SLOPING3") (SETQ FILL_TOP_SH (POLAR FILL_TOP left UP_SHOULDER)))
      (if (= slopeType "SLOPING3") (SETQ FILL_TOP_SH (POLAR FILL_TOP 3upLeft (abs (/ UP_SHOULDER (COS 3upLeft))))))
      (if (= slopeType "CROWN") (SETQ FILL_TOP_SH (POLAR FILL_TOP 3downLeft (abs (/ UP_SHOULDER (COS 3downLeft))))))

      (if (/= slopeType "CROWN") (SETQ FILL_BOT_SH (POLAR FILL_BOT angleBot (/ DOWN_SHOULDER (COS angleBot)))))
      (if (= slopeType "CROWN") (SETQ FILL_BOT_SH (POLAR FILL_BOT 3downRight (/ DOWN_SHOULDER (COS 3downRight)))))
    )
  )

  (if (= slopeType "CROWN")
    (progn

      (line_inter FILL_TOP (polar FILL_TOP 3upRight 10.0) FILL_BOT (polar FILL_BOT 3upLeft 10.0))
      (SETQ crown_pt XY_INT)

    )
  )




;#######################################################################
;;;;;;;;;;;;;;;;;START DRAWING EXISTING SLOPE;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; START extend the top slope line this much from intersection if not existing point (e.g. u1u2) is okay
       (setq extendSlope 120.0)
       (setq fillDraw "NO")
       (setq doBoth 0)


;;;;;;DRAW LEFT HAND SIDE CROSS SECTION

  (if (or (= doBoth 1) (= crossSection "LEFT"))
    (progn
      (fillCalculate u1u2 u1u1 (list 0.0 U1) (list sys_w D1) d1d1 d1d2 fill_top_sh (POLAR FILL_TOP_SH UP_PRO_SLOPE 100) fill_bot_sh (POLAR FILL_BOT_SH DOWN_SLOPE 100))

      ; fillCalculate returns global variables botint & topint
      (SETQ DOWN_INT botint)
      (SETQ UP_INT topint)

      ; if intersection is within shoulder top 
      (if (/= slopeType "CROWN")
        (progn
          (if (and (/= slopeType "SLOPING3") (and (< (car topint) (car fill_top)) (>= (car topint) (car fill_top_sh))))
            (progn
              (setq slopeType "SLOPING3")
              (setq fill_top_sh (polar fill_top (angle fill_bot fill_top) 0.0001))
              (Princ "\nSmall uphill fill extension - using Final Grade Type: Sloping 3%")
              (pro_fill)
            )
          )
        )
      )

      ; adjust end of slope based on grade spot given or fill intersection
      ; adjust TOP
      (if (<= (car up_int) (car u1u2))
        (progn
          (setq TOP up_int)
          (setq TOP (polar TOP (ANGLE u1u1 u1u2) extendSlope))
        )
      )


      (if (> (car up_int) (car u1u2))
        (progn
          (setq TOP u1u2)
          (if (< (* (- (car u1u2) (car up_int)) -1) extendSlope)
            (setq TOP (polar TOP (ANGLE u1u1 u1u2) extendSlope))
          )
        )
      )


      ; adjust BOTTOM
      (if (>= (car down_int) (car d1d2))
        (progn
          (setq BOTTOM down_int)
          (setq BOTTOM (polar BOTTOM (angle d1d1 d1d2) extendSlope))
        )
      )

      (if (< (car down_int) (car d1d2))
        (progn
          (setq BOTTOM d1d2)
          (if (<= (- (car d1d2) (car down_int)) extendSlope)
            (setq BOTTOM (polar down_int (angle d1d1 d1d2) extendSlope))
          )
        )
      )

      ; Draw Ground Surface Line
      (command "pline" TOP u1u2 u1u1 (list (car slope_top) u1) (list (car slope_bot) d1) d1d1 d1d2 BOTTOM "")

      (setq sset1 (ssadd))
      ; if not primary cross section then make gray (252)
      (if (= crossSection "RIGHT")
        (progn
          (setq sset1 (ssadd))
          (ssadd (entlast) sset1)
          (setq ang (* (angle d1d1 d1d2) (/ 180.0 pi)))
          (command "text" "J" "BC" d1d2 profileTextSize ang "Existing Grade Surface on Opposite Side")
          (ssadd (entlast) sset1)
          (modifySS sset1 62 252)
          (makeBlock "GroundSurfaceOppositeSide" sset1 BOTTOM)
        )
      )


    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;DRAW RIGHT HAND SIDE CROSS SECTION


  (if (or (= doBoth 1) (= crossSection "RIGHT"))
    (progn
      ; if actually drawing left cross section then reset at end 
      (setq leftBotint botint)
      (setq leftTopint topint)

      (fillCalculate u2u2 u2u1 (list 0.0 U2) (list sys_w D2) d2d1 d2d2 fill_top_sh (POLAR FILL_TOP_SH UP_PRO_SLOPE 100) fill_bot_sh (POLAR FILL_BOT_SH DOWN_SLOPE 100))

      ; fillCalculate returns global variables botint & topint
      (SETQ DOWN_INT botint)
      (SETQ UP_INT topint)

      ; if intersection is within shoulder top 

      (if (/= slopeType "CROWN")
        (progn
          (if (and (/= slopeType "SLOPING3") (and (< (car topint) (car fill_top)) (>= (car topint) (car fill_top_sh))))
            (progn
              (setq slopeType "SLOPING3")
              (setq fill_top_sh (polar fill_top (angle fill_bot fill_top) 0.0001))
              (Princ "\nSmall uphill fill extension - using Final Grade Type: Sloping 3%")
              (pro_fill)
            )
          )
        )
      )


      ; adjust end of slope based on grade spot given or fill intersection
      ; adjust TOP
      (if (<= (car up_int) (car u2u2))
        (progn
          (setq TOP up_int)
          (setq TOP (polar TOP (ANGLE u2u1 u2u2) extendSlope))
        )
      )

      (if (> (car up_int) (car u2u2))
        (progn
          (setq TOP u2u2)
          (if (< (* (- (car u2u2) (car up_int)) -1) extendSlope)
            (setq TOP (polar TOP (ANGLE u2u1 u2u2) extendSlope))
          )
        )
      )

      ; adjust BOTTOM
      (if (>= (car down_int) (car d2d2))
        (progn
          (setq BOTTOM down_int)
          (setq BOTTOM (polar BOTTOM (angle d2d1 d2d2) extendSlope))
        )
      )

      (if (< (car down_int) (car d2d2))
        (progn
          (setq BOTTOM d2d2)
          (if (<= (- (car d2d2) (car down_int)) extendSlope)
            (setq BOTTOM (polar down_int (angle d2d1 d2d2) extendSlope))
          )
        )
      )

      ; Draw slope Line
      (command "pline" TOP u2u2 u2u1 (list (car slope_top) u2) (list (car slope_bot) d2) d2d1 d2d2 BOTTOM "")

      (setq sset1 (ssadd))
      ; if not primary cross section then make gray 252
      (if (= crossSection "LEFT")
        (progn
          (setq sset1 (ssadd))
          (ssadd (entlast) sset1)
          (setq ang (* (angle d2d1 d2d2) (/ 180.0 pi)))
          (command "text" "J" "BC" d2d2 profileTextSize ang "Existing Grade Surface on Opposite Side")
          (ssadd (entlast) sset1)
          (modifySS sset1 62 252)
          (makeBlock "GroundSurfaceOppositeSide" sset1 BOTTOM)
        )
      )


      ; if actually drawing left cross section then reset at end 
      (if (= crossSection "LEFT")
        (progn
          (setq botint leftBotint)
          (setq topint leftTopint)
          (SETQ DOWN_INT botint)
          (SETQ UP_INT topint)
        )
      )


    )
  )



;;; END extend the top slope line this much from intersection if not existing point (e.g. u1u2) is okay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Label grades on compound slopes if applicable

  (if (= crossSection "LEFT")
    (progn

      (if (/= (rtos (angle u1u2 u1u1) 2 4) (rtos (angle u1u1 slope_top) 2 4))
        (printGrade u1u2 u1u1 profileTextSize "B" " % Grade")
      )

      (if (/= (rtos (angle u1u1 slope_top) 2 4) (rtos (angle slope_top slope_bot) 2 4))
        (printGrade u1u1 slope_top profileTextSize "B" " % Grade")
      )

      (if (/= (rtos (angle slope_bot d1d1) 2 4) (rtos (angle slope_top slope_bot) 2 4))
        (progn
          (printGrade slope_bot d1d1 profileTextSize "B" " % Grade")
        )
      )

      (if (/= (rtos (angle d1d1 d1d2) 2 4) (rtos (angle slope_bot d1d1) 2 4))
        (printGrade d1d1 d1d2 profileTextSize "B" " % Grade")
      )
    )
  )


  (if (= crossSection "RIGHT")
    (progn

      (if (/= (rtos (angle u2u2 u2u1) 2 4) (rtos (angle u2u1 slope_top) 2 4))
        (printGrade u2u2 u2u1 profileTextSize "B" " % Grade")
      )

      (if (/= (rtos (angle u2u1 slope_top) 2 4) (rtos (angle slope_top slope_bot) 2 4))
        (printGrade u2u1 slope_top profileTextSize "B" " % Grade")
      )


      (if (/= (rtos (angle slope_bot d2d1) 2 4) (rtos (angle slope_top slope_bot) 2 4))
        (printGrade slope_bot d2d1 profileTextSize "B" " % Grade")
      )

      (if (/= (rtos (angle d2d1 d2d2) 2 4) (rtos (angle slope_bot d2d1) 2 4))
        (printGrade d2d1 d2d2 profileTextSize "B" " % Grade")
      )
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       #######################################################################
;;;;;;;;;;;;;;;;;END DRAWING EXISTING SLOPE;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; START: If intersection of fill is within the top shoulder.... then the shoulder is adjusted

       (IF (AND (< (LAST slope_top) (LAST fill_top)) (< (LAST slope_bot) (LAST fill_bot)))
         (progn

           (if (and (< (car topint) (car fill_top)) (>= (car topint) (car fill_top_sh)))
             (progn
               ; adjust fill shoulder
               ;(setq fill_top_sh (polar fill_top (angle fill_bot fill_top) 0.0001))
               (setq fill_top_sh fill_top)
               (if (/= slopeType "CROWN") (COMMAND "PLINE" UP_INT FILL_TOP FILL_BOT FILL_BOT_SH DOWN_INT ""))
               (if (= slopeType "CROWN") (COMMAND "PLINE" UP_INT crown_pt FILL_BOT_SH DOWN_INT ""))
             )
           )

           (if (and (< (car topint) (car fill_top)) (< (car topint) (car fill_top_sh)))
             (progn
               (if (/= slopeType "CROWN") (COMMAND "PLINE" UP_INT FILL_TOP_SH FILL_TOP FILL_BOT FILL_BOT_SH DOWN_INT ""))
               (if (= slopeType "CROWN") (COMMAND "PLINE" UP_INT FILL_TOP_SH crown_pt FILL_BOT_SH DOWN_INT ""))
             )
           )


           ; draw all the fill
           (setq fillDraw "YES")
         )
       )
;;; END 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




  (if (= slopeType "CROWN")
    (progn

      ; IF BOTH TOP AND BOTTOM ARE BURIED
      (IF (AND (>= (LAST slope_top) (LAST fill_top)) (>= (LAST slope_bot) (LAST fill_bot)))
        ; FUNCTION CALLS A DCL WINDOW TO CHOOSE FINAL-EXISTING OR 3% CROWN
          (auto-crown)
      )

      ; IF FILL_TOP IS AT GRADE OR ABOVE AND FILL_BOT IS BELOW GRADE
      (IF (AND (>= (LAST slope_top) (LAST fill_top)) (< (LAST slope_bot) (LAST fill_bot)))
        (progn
          (LINE_INTER crown_pt (polar crown_pt 3downLeft 10.0) slope_top slope_bot)
          (setq UP_INT xy_int)
          (COMMAND "PLINE" up_int crown_pt FILL_BOT_SH DOWN_INT "")
          ; draw bottom of fill, SLOPING3 used
          (setq fillDraw "BOTTOM")
        )
      )
    )
  )


  (if (/= slopeType "CROWN")
    (progn

      ; IF FILL_TOP IS AT GRADE OR ABOVE AND FILL_BOT IS BELOW GRADE
      (IF (AND (>= (LAST slope_top) (LAST fill_top)) (< (LAST slope_bot) (LAST fill_bot)))
        (progn
          (LINE_INTER fill_bot (polar fill_bot 3upLeft 10.0) slope_top slope_bot)
          (setq UP_INT xy_int)
          (IF (>= (LAST SLOPE_TOP) (last fill_top)) (setq up_int fill_top))
          (COMMAND "PLINE" up_int FILL_BOT_SH DOWN_INT "")
          ; draw bottom of fill, SLOPING3 used
          (setq fillDraw "BOTTOM")
        )
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;Print existing grade or final grade info. Input can be modified by (auto-crown) function via the crownFlat variable

  (if (= crownFlat nil)
    (progn

      ; do normal if above ground
      (IF (AND (< (LAST slope_top) (LAST fill_top)) (< (LAST slope_bot) (LAST fill_bot)))
        (progn
;			(IF (> (LAST SLOPE_TOP) (last row_start))
;				(printGrade slope_bot (polar slope_bot (angle slope_top slope_bot) (* profileTextSize 12.0)) profileTextSize "T" " % Existing Grade")
;			)
;			
;			(IF (<= (LAST SLOPE_TOP) (last row_start))
;				(printGrade slope_top slope_bot profileTextSize "B" " % Existing Grade")
;			)
          (printGrade slope_bot (polar slope_bot (angle slope_top slope_bot) (* profileTextSize 12.0)) profileTextSize "T" " % Existing Grade")
        )
      )

      ; if below ground and not 3% crown then print this
      (IF (AND (>= (LAST slope_top) (LAST fill_top)) (>= (LAST slope_bot) (LAST fill_bot)))
          (printGrade slope_top slope_bot profileTextSize "T" " % Existing/Final Grade")
      )


    )
  )

  (if (= crownFlat 1)
    (printGrade slope_top slope_bot profileTextSize "T" " % Existing/Final Grade")
  )

; reset crownFlat variable to nil
       (setq crownFlat nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	; if fillDraw is either YES or BOTTOM label and top of the fill
       (if (AND (/= slopeType "CROWN") (/= fillDraw "NO")) (printGrade fill_top fill_bot profileTextSize "T" " % Final Grade"))
       (if (AND (= slopeType "CROWN") (/= fillDraw "NO")) (command "text" "J" "BC" crown_pt profileTextSize "0" "3% Crown"))


  (if (= fillDraw "YES")
    (progn

      (if (= slopeType "SLOPING3")
        (printGrade (polar fill_top (angle fill_top up_int) (* profileTextSize 5)) fill_top profileTextSize "T" " % Final Grade")
      )

      (if (/= slopeType "SLOPING3")
        (progn
          ;-------Print uphill shoulder--------
          (SETQ SH_U (RTOS UP_SHOULDER 3 2))
          (SETQ SH_U (STRCAT SH_U " Shoulder"))
          (SETQ SH_U_PT (POLAR FILL_TOP_SH (/ PI 2) (* (/ profileScale 5.0) 1)))
          (if (= slopeType "CROWN") (SETQ SH_u_PT (POLAR sh_u_pt (/ PI 2) (* (/ profileScale 5.0) 1))))
          (COMMAND "TEXT" "J" "BL" SH_U_PT profileTextSize 0 SH_U)

          ;-------Print downhill shoulder--------
          (SETQ SH_D_PT (POLAR FILL_BOT_SH (/ PI 2) (/ profileTextSize 2.0)))
          (if (= slopeType "CROWN") (SETQ SH_D_PT (POLAR sh_d_pt (/ PI 2) (* (/ profileScale 5.0) 1))))

          (if (/= slopeType "CROWN")
            (progn
              (setq ang1 (* (angle fill_bot fill_bot_sh) (/ 180.0 pi)))
              (setq SH_D_PT (polar fill_bot (angle fill_bot fill_bot_sh) (/ (distance fill_bot fill_bot_sh) 2.0)))
              (setq sh_d_pt (polar sh_d_pt (/ pi 2.0) (/ profileTextSize 2.0)))
            )
          )

          (SETQ SH_D (RTOS DOWN_SHOULDER 3 2))
          (SETQ SH_D (STRCAT SH_D " Shoulder"))


          (if (= slopeType "CROWN") (COMMAND "TEXT" "J" "BR" SH_D_PT profileTextSize 0 SH_D))
          (if (/= slopeType "CROWN") (command "text" "J" "BC" SH_D_PT profileTextSize ang1 SH_D))

        )
      )
    )
  )


	; if fillDraw is BOTTOM or YES
  (if (/= fillDraw "NO")
    (progn
      ; PRINTS THE GRADE OF THE SLOPE USED ON THE DOWN-HILL FILL EXTENSION
      ; CONVERTS RADIAN TO DEGREES FOR "TEXT" COMMAND
      (SETQ ANG1 (* (ANGLE FILL_BOT_SH DOWN_INT) (/ 180 PI)))

      ; CREATES INSERT POINT
      (SETQ PT1 (POLAR FILL_BOT_SH DOWN_SLOPE (/ (distance down_int fill_bot_sh) 3.0)))

      (SETQ PT1 (POLAR PT1 (/ PI 2.0) 2.5))

      ;DOWN_SLOPE_TXT WAS DETEMINED IN THE BEGINNING OF main_drawing
      (COMMAND "TEXT" "J" "BC" PT1 profileTextSize ANG1 DOWN_SLOPE_TXT)

      ;;;-;;;
      (if (>= (last fill_top) (last slope_top))
        (progn

          (if (/= slopeType "SLOPING3")
            (progn
              ; CONVERTS RADIAN TO DEGREES FOR "TEXT" COMMAND
              (SETQ ANG1 (* (- (ANGLE FILL_TOP_SH UP_INT) pi) (/ 180 PI)))

              ; CREATES INSERT POINT
              (SETQ PT1 (POLAR FILL_TOP_SH UP_PRO_SLOPE (/ (distance up_int fill_top_sh) 3.0)))
              (SETQ PT1 (POLAR PT1 (/ PI 2.0) 2.5))

              ;UP_SLOPE_TXT WAS DETEMINED IN THE BEGINNING OF main_drawing
              (if (/= fillDraw "BOTTOM") (COMMAND "TEXT" "J" "BC" PT1 profileTextSize ANG1 UP_SLOPE_TXT))
            )
          )

          (if (= slopeType "SLOPING3")
            (progn
              (SETQ ANG1 (* (ANGLE UP_INT FILL_TOP) (/ 180 PI)))

              ; CREATES INSERT POINT
              (SETQ PT1 (POLAR FILL_TOP_SH UP_PRO_SLOPE 24.0))
              (SETQ PT1 (POLAR PT1 (/ PI 2.0) 2.5))

            )
          )


        )
      )
      ;;;-;;;

    )
  )


;;;;;;  if FillDraw = NO   -and-   fill_top is above slope_top   -and-   fill_bot is below slope_top    THEN
;;;;;;  nothing has printed to the screen yet.  The valid slopeTypes are SLOPING or SLOPING3 Assume the fillType of always SLOPING

       (if (and
             (and (= fillDraw "NO") (> (last fill_top) (last slope_top)))
             (<= (last fill_bot) (last slope_bot))
           )
         (progn

           (setq fill_bot slope_bot)
           (command "PLINE" up_int fill_top_sh fill_top fill_bot "")

           ; label top tapered slope
           (SETQ ANG1 (* (- (ANGLE FILL_TOP_SH UP_INT) pi) (/ 180 PI)))
           (SETQ PT1 (POLAR FILL_TOP_SH UP_PRO_SLOPE (/ (distance up_int fill_top_sh) 2.0)))
           (SETQ PT1 (POLAR PT1 (/ PI 2.0) 2.5))
           (COMMAND "TEXT" "J" "BC" PT1 profileTextSize ANG1 UP_SLOPE_TXT)

           ; label top grade
           (printGrade fill_top fill_bot profileTextSize "T" " % Final Grade")




         )
       )

; taken out because it is already called in (main_drawing)
;	(toggle-OSNAP-Vars-RESET)
       (setq *error* lastErrorFunction)

)
; END PRO_FILL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;  draws upslope, downslope and bed with dimensions on bottom of cross section
; right and left arrowheads are kept at the drawing margin, if fill extension intersection
; with the ground surface is off the page
(defun SCALE_BOT (/ DIST_BOT V1 V2 V3 V4 UTXT DTXT DIMASZ_DEF DIMTXT_DEF SYS_W_TXT rightSideSys leftSideSys cProfile cProfileBotCenter cProfileLeftCenter cProfileRightCenter)

;;;--- Set default directions
       (setq down (/ (* 3.0 pi) 2.0))
       (setq up (/ pi 2.0))
       (setq right 0.0)
       (setq left pi)

       (setq leftSideSys (polar row_start left stonebeside))
       (setq rightSideSys (polar row_end right stonebeside))

       (setq cProfile (cProfileGet))

        ; set at just above the bottom of the margin so arrowheads print fully
       (setq cProfileBotCenter (polar cProfile down (* 22.8 profileScale)))
        ; left and right margin points
       (setq cProfileLeftCenter (polar cProfile left (* 48 profileScale)))
       (setq cProfileRightCenter (polar cProfile right (* 48 profileScale)))

        ; directly below downslope side of bed
       (setq v2 (list (car rightSideSys) (last cProfileBotCenter)))

	; downslope intersection
       (setq v1 (polar v2 right (ABS (- (CAR DOWN_INT) (CAR rightSideSys)))))

	; if v1 is off the right side of the page then bring it back on the page
  (if (> (car v1) (car cProfileRightCenter))
    (setq v1 (list (car cProfileRightCenter) (last v1)))
  )

	; upslope side of bed
       (setq v3 (polar v2 left sys_w))

	; upslope intersection
       (setq v4 (polar v3 left (ABS (- (CAR UP_INT) (CAR leftSideSys)))))
	; if v4 is off the left side of the page then bring it back on the page
  (if (< (car v4) (car cProfileLeftCenter))
    (setq v4 (list (car cProfileLeftCenter) (last v4)))
  )

       (SETQ DIMASZ_DEF (getvar "DIMASZ"))
       (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
       (SETVAR "DIMASZ" (* profileTextSize scaleArrowSize 1.3))
       (SETVAR "DIMTXT" (* profileTextSize 1.3))

	;DRAWS DIMENSIONS on bottom

	;;;--- DOWNHILL
  (if (> (last fill_bot) (last slope_bot))
    (COMMAND "DIM" "hor" V1 V2 V2 (STRCAT (rtos (/ (ABS (- (CAR SLOPE_BOT) (CAR DOWN_INT))) 12.0) 2 0) "'") "EXIT")
  )

	;;;--- SYSTEM
       (COMMAND "DIM" "hor" V2 V3 V3 (rtos sys_w 3 2) "EXIT")

	;;;--- UPHILL
  (if (> (last fill_top) (last slope_top))
    (COMMAND "DIM" "hor" V4 V3 V3 (STRCAT (rtos (/ (ABS (- (CAR SLOPE_TOP) (CAR UP_INT))) 12.0) 2 0) "'") "EXIT")
  )

	;REST DEFAULT DIM SETTINGS
       (SETVAR "DIMTXT" DIMTXT_DEF)
       (SETVAR "DIMASZ" DIMASZ_DEF)
)












(defun fillCalculate (topSlopeB topSlopeA topSystem botSystem botSlopeA botSlopeB topFill topGrade botFill botGrade)

  (setq topInt nil botInt nil botFillLength nil topFillLength nil)

  (if (< (angle topSystem topSlopeA) (angle topFill topGrade))
    (progn
      (line_inter topSystem topSlopeA topFill topGrade)

      (if (AND (>= (car xy_int) (car topSlopeA)) (<= (car xy_int) (car topSystem)))  ; if intersection is based on first extension line
        (progn
          (setq topFillLength (ABS (- (car xy_int) (car topSystem))))  ; length of fill
          (setq topInt xy_int)
        )  ; end progn
      )    ; end if
    )      ; progn
  )        ; if


  (if (= topInt nil)
    (progn
      (if (< (angle topSlopeA topSlopeB) (angle topFill topGrade))
        (progn
          (line_inter topSlopeB topSlopeA topFill topGrade)
          (setq topFillLength (ABS (- (car xy_int) (car topSystem))))  ; length of fill
          (setq topInt xy_int)
        )  ; progn
      )    ; if
    )      ; progn
  )        ; if


  (if (< (angle botGrade botFill) (angle botSlopeA botSystem))
    (progn

      (line_inter botSystem botSlopeA botFill botGrade)

      ; if intersection is based on first extension line
      (if (AND (<= (car xy_int) (car botSlopeA)) (>= (car xy_int) (car botSystem)))
        (progn
          (setq botFillLength (ABS (- (car xy_int) (car botSystem))))  ; length of fill
          (setq botInt xy_int)
        )  ; end progn
      )    ; end if
    )      ; progn
  )        ; if


  (if (= botInt nil)
    (progn
      (if (< (angle botGrade botFill) (angle botSlopeB botSlopeA))
        (progn
          (line_inter botSlopeA botSlopeB botFill botGrade)
          (setq botFillLength (ABS (- (car xy_int) (car botSystem))))  ; length of fill
          (setq botInt xy_int)
        )  ; progn
      )    ; if
    )      ; progn
  )        ; if

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
















        
        











































;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (lineIntersection a1 a2 b1 b2) -> returns intersection point even if the segments don't intersect
; (setq intPt (lineIntersection a1 a2 b1 b2))
; Function extend points defining two lines to near infinity and calculates the intersection
(defun lineIntersection (a1 a2 b1 b2 / bigNumber)
  (setq bigNumber 999999999999)
  (setq a1 (polar a1 (angle a2 a1) bigNumber))
  (setq a2 (polar a2 (angle a1 a2) bigNumber))
  (setq b1 (polar b1 (angle b2 b1) bigNumber))
  (setq b2 (polar b2 (angle b1 b2) bigNumber))
  (inters a1 a2 b1 b2)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; legacy function - first try could remove it easily... so ignore it now
;;;; XY_INT POINT IS GLOBAL
(defun LINE_INTER (P1 P2 P3 P4)
       (SETQ XY_INT (lineIntersection P1 P2 P3 P4))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MAP_FILL_EXT (/ leftFill rightFill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Profile A - fill length calculation
       (fillCalculate u1u2 u1u1 (list 0.0 U1) (list sys_w D1) d1d1 d1d2 fill_top_sh (POLAR FILL_TOP_SH UP_PRO_SLOPE 100) fill_bot_sh (POLAR FILL_BOT_SH DOWN_SLOPE 100))
       (setq U1_U topFillLength)
       (setq D1_D botFillLength)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Profile B - fill length calculation
       (fillCalculate u2u2 u2u1 (list 0.0 U2) (list sys_w D2) d2d1 d2d2 fill_top_sh (POLAR FILL_TOP_SH UP_PRO_SLOPE 100) fill_bot_sh (POLAR FILL_BOT_SH DOWN_SLOPE 100))
       (setq U2_U topFillLength)
       (setq D2_D botFillLength)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Profile C - fill length calculation
;; NOTICE how fill_top OR fill_bot are used instead of fill_top_sh OR fill_bot_sh
	;;;;; Correct leftFill and rightFill based on topFill
       (setq leftFill (list (* left_shoulder -1) (last fill_top)))
       (setq rightFill (list (+ right_shoulder sys_L) (last fill_top)))

       (fillCalculate U1L2 U1L1 (list 0.0 U1) (list sys_L U2) U2R1 U2R2 leftFill (POLAR leftFill LEFT_SLOPE 100) rightFill (POLAR rightFill RIGHT_SLOPE 100))

       (setq U1_L topFillLength)
       (setq U2_R botFillLength)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Profile d - fill length calculation
	;;;;; Correct leftFill and rightFill based on topFill
       (setq leftFill (list (* left_shoulder -1) (last fill_bot)))
       (setq rightFill (list (+ right_shoulder sys_L) (last fill_bot)))

       (fillCalculate D1L2 D1L1 (list 0.0 D1) (list sys_L D2) D2R1 D2R2 leftFill (POLAR leftFill LEFT_SLOPE 100) rightFill (POLAR rightFill RIGHT_SLOPE 100))

       (setq D1_L topFillLength)
       (setq D2_R botFillLength)


;;;;; all 8 fill extension should be set now


;;;---  If the System corner is buried - Fill extension set to shoulder width 
       (if (<= U1_F 0.0) (SETQ U1_U UP_SHOULDER U1_L LEFT_SHOULDER))
       (if (<= U2_F 0.0) (SETQ U2_U UP_SHOULDER U2_R RIGHT_SHOULDER))
       (if (<= D1_F 0.0) (SETQ D1_D DOWN_SHOULDER D1_L LEFT_SHOULDER))
       (if (<= D2_F 0.0) (SETQ D2_D DOWN_SHOULDER D2_R RIGHT_SHOULDER))

)
; END MAP_FILL_EXT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:4cross (/ leftFill rightFill mDist pntLst up down left right pt1 pt2 ang textPT ANGDIR_DEF ANGBASE_DEF ORTHO_DEF blockName ex)



  ;;;;;;;;;; Start Subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
          (defun movePL (pntLst moveX moveY)
            (setq pLList (list))
            (command "PLINE")

            (foreach item pntLst
              (setq point (polar item (/ pi 2.0) moveY))
              (setq point (polar point 0.0 moveX))
              (setq PLList (append PLList (list point)))
              (command point)
            )

            ; end Pline function
            (command "")

          )
          ; end movePL function


          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ; move each point in a list of lists by x and y distances specified  used in 4cross
          (defun moveChamberPtListPoints (pntList moveX moveY / newList temp item shelf point)
            (setq newList (list) temp (list))

            (foreach shelf pntList
              (foreach item shelf
                (setq point (polar item (/ pi 2.0) moveY))
                (setq point (polar point 0.0 moveX))
                (setq temp (append temp (list point)))
              )
              (setq newList (append newList (list temp)))
              (setq temp (list))
            )
            newList
          )
          ;;;;;;;;;;;; end function

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; inserts block as point specified in a list
          (defun 4cross-chambers (pntList / newList item shelf point cnt attr)

            (setq newList (list))

            (foreach shelf pntList
              (foreach item shelf
                (setq newList (append newList (list item)))
              )
            )


            (if (or (= sys_type "MOUNDBUSTER") (= sys_type "8x4_CONCRETE_CLUSTER") (= sys_type "4x8_CONCRETE_CLUSTER"))
              (setq attr "NO")
              (setq attr "YES")
            )

            (setq cnt 1)
            (foreach item newList
              (if (= attr "NO") (COMMAND "INSERT" $chamberBlock item 1 1 0))
              (if (= attr "YES") (COMMAND "INSERT" $chamberBlock item 1 1 0 (rtos cnt 2 0)))
              (setq cnt (1+ cnt))
            )

             ; add other stuff around chambers for certain systems.
            (if (or (= SYS_TYPE "ENVIRO") (= SYS_TYPE "GEOFLOW")) (drawChamberSandEnviro pntList))
            (if (or (= SYS_TYPE "ELJEN") (= SYS_TYPE "ELJEN_INVERTED")) (drawChamberSandEljen pntList))
            (if (or (= SYS_TYPE "8x4_CONCRETE_CLUSTER") (= SYS_TYPE "4x8_CONCRETE_CLUSTER"))
              (progn
                (if (OR (/= stoneBeside 0) (/= stoneBelow 0))
                  (clusterConcreteStoneHatch (nth 0 newList) chamber_W)
                )
              )
            )

          )
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (defun 4crossStone (BL W / BR TL TR up down left right)

            ;;;;--- Set default directions
            (setq down (/ (* 3.0 pi) 2.0))
            (setq up (/ pi 2.0))
            (setq right 0.0)
            (setq left pi)

            (setq BR (polar BL right W))
            (setq TL (polar BL up chamber_H))
            (setq TR (polar BR up chamber_H))

            (command "_.PLINE" BL BR TR TL BL "")

            ;;;; insert the left and right stone bed blocks
            (COMMAND "_.insert" (strcat $septicadPathChambers "STONE_LEFT.dwg") BL "1" "1" "0")
            (COMMAND "_.insert" (strcat $septicadPathChambers "STONE_RIGHT.dwg") BR "1" "1" "0")

          )
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;; End Subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




  (if (/= u1u2 nil)
    (progn
      ;sets block insert unit vars
      (setBlockUnitsVars)

      ;;;;--- Set default directions
      (setq down (/ (* 3.0 pi) 2.0))
      (setq up (/ pi 2.0))
      (setq right 0.0)
      (setq left pi)
      (setq extendSlope 120)

      (setq ANGDIR_DEF (GETVAR "ANGDIR"))
      (setvar "ANGDIR" 0)

      (SETQ ANGBASE_DEF (GETVAR "ANGBASE"))
      (setvar "ANGBASE" 0)

      (toggle-OSNAP-Vars-OFF)

      (setq ORTHO_DEF (getvar "ORTHOMODE"))
      (setvar "ORTHOMODE" 0)

      (COMMAND "LAYOUT" "S" "MODEL")  ; Change to Model Layout

      ; Final Grade and Existing Grade List of Points saved in svolume
      ;$Cross-A-FGrade
      ;$Cross-A-EGrade
      ;$Cross-B-FGrade
      ;$Cross-B-EGrade
      ;$Cross-C-FGrade
      ;$Cross-C-EGrade
      ;$Cross-D-FGrade
      ;$Cross-D-EGrade

      ;chamberPtList
;	(setq cnt 0)(princ "\n")
;	(foreach item chamberptlist (setq cnt (1+ cnt)) (princ cnt) (princ " -> ") (princ item) (princ "\n"))

      ;;;;;;;;;;;;;;;;;;;;;; Cross-Section A-A' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (setq mDist (* 1.2
                     (distance
                       (car $Cross-A-FGrade)
                       (last $Cross-A-FGrade)
                     )
                  )
      )

      (movePL $Cross-A-FGrade mDist 0)
      (movePL $Cross-A-EGrade mDist 0)

      (command "text" "J" "C" (polar (car PLlist) up (* profileTextSize 2)) (* profileTextSize 3) 0 "A")
      (command "text" "J" "C" (polar (last PLlist) up (* profileTextSize 2)) (* profileTextSize 3) 0 "A'")

      ; insert block defining cross sections
      (setq ex (getvar "Expert"))
      (setvar "Expert" 2)  ; suppresses "Block already defined. Redefined it? ALERT"

      (setq blockName (strcat $septicadPathBlockRequired "4CROSS.DWG"))
      (COMMAND "_.INSERT" blockName (polar (nth 0 PLlist) up 100) "1" "1" "0")

      (setvar "EXPERT" ex)

      ; INSERT CHAMBERS/STONEBED
      (if (/= SYS_TYPE "STONE")
        (progn
          (setq chamberPtList-4cross (moveChamberPtListPoints chamberPtList mdist 0))
          (4cross-chambers chamberPtList-4cross)
        )
      )

      (if (= SYS_TYPE "STONE")
        (4crossStone (polar row_start right mdist) sys_W)
      )

;	(setq cnt 0)(princ "\n")
;	(foreach item chamberPtList-4cross (setq cnt (1+ cnt)) (princ cnt) (princ " -> ") (princ item) (princ "\n"))

      ;;;;;;;;;;;;;;;;;;;;;; Cross-Section B-B' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (setq mDist (+ mDist (* 1.2
                              (distance
                                (car $Cross-A-FGrade)
                                (last $Cross-A-FGrade)
                              )
                           )
                  )
      )

      (movePL $Cross-B-FGrade mDist 0)
      (movePL $Cross-B-EGrade mDist 0)

      (command "text" "J" "C" (polar (car PLlist) up (* profileTextSize 2)) (* profileTextSize 3) 0 "B")
      (command "text" "J" "C" (polar (last PLlist) up (* profileTextSize 2)) (* profileTextSize 3) 0 "B'")

      (if (/= SYS_TYPE "STONE")
        (progn
          (setq chamberPtList-4cross (moveChamberPtListPoints chamberPtList mdist 0))
          (4cross-chambers chamberPtList-4cross)
        )
      )

      (if (= SYS_TYPE "STONE")
        (4crossStone (polar row_start right mdist) sys_W)
      )

      ;;;;;;;;;;;;;;;;;;;;;; Cross-Section C-C' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (setq mDist (+ mDist (* 1.2
                              (distance
                                (car $Cross-B-FGrade)
                                (last $Cross-B-FGrade)
                              )
                           )
                  )
      )

      (movePL $Cross-C-FGrade mDist 0)
      (movePL $Cross-C-EGrade mDist 0)

      (command "text" "J" "C" (polar (car PLlist) up (* profileTextSize 2)) (* profileTextSize 3) 0 "C")
      (command "text" "J" "C" (polar (last PLlist) up (* profileTextSize 2)) (* profileTextSize 3) 0 "C'")



      (if (AND (/= sys_type "8x4_CONCRETE_CLUSTER") (/= SYS_TYPE "STONE") (/= sys_type "4x8_CONCRETE_CLUSTER"))
        (progn
          ; get points for drawing rectangle box representing First row and last row	
          (setq newList (list))
          (foreach shelf chamberPtList
            (foreach item shelf
              (setq newList (append newList (list item)))
            )
          )
          (setq firstRowBL (list 0.0 (cadr (car newList))))       ; used in c-c'
          (setq firstRowBR (polar firstRowBL right sys_L))        ; used in C-c'
          (setq lastRowBL (list 0.0 (cadr (last newList))))       ; used in d-d'
          (setq lastRowBR (polar lastRowBL right sys_L))          ; used in d-d'

          ; draw Row 1 on c-c' and label it
          (movePL (list
                    firstRowBL
                    firstRowBR
                    (polar firstRowBR up chamber_H)
                    (polar firstRowBL up chamber_H)
                    firstRowBL
                  )
                  mDist 0
          )
          (setq txtPt (polar (polar firstRowBL up (/ chamber_H 2.0))
                             right
                             (+ (/ (distance (polar firstRowBL up chamber_H) (polar firstRowBR up chamber_H)) 2.0) mdist)
                      )
          )

          (COMMAND "._MTEXT" txtPt "J" "MC" "H" profiletextSize txtPt "ROW 1" "")
        )
      )
      (if (OR (= sys_type "8x4_CONCRETE_CLUSTER") (= sys_type "4x8_CONCRETE_CLUSTER"))
        (progn
          ; get points for drawing rectangle box representing First row and last row	
          (setq newList (list))
          (foreach shelf chamberPtList
            (foreach item shelf
              (setq newList (append newList (list item)))
            )
          )
          (setq firstRowBL (list 0.0 (cadr (car newList))))       ; used in c-c'
          (setq firstRowBR (polar firstRowBL right sys_L))        ; used in C-c'
          (setq firstRowBL (polar firstRowBL right stonebeside))  ; used in C-c'
          (setq firstRowBR (polar firstRowBR left stonebeside))   ; used in C-c'

          (setq lastRowBL (list 0.0 (cadr (last newList))))       ; used in d-d'
          (setq lastRowBR (polar lastRowBL right sys_L))          ; used in d-d'
          (setq lastRowBL (polar lastRowBL right stonebeside))    ; used in C-c'
          (setq lastRowBR (polar lastRowBR left stonebeside))     ; used in C-c'

          ; draw Row 1 on c-c' and label it
          (movePL (list
                    firstRowBL
                    firstRowBR
                    (polar firstRowBR up chamber_H)
                    (polar firstRowBL up chamber_H)
                    firstRowBL
                  )
                  mDist 0
          )
          (setq txtPt (polar (polar firstRowBL up (/ chamber_H 2.0))
                             right
                             (+ (/ (distance (polar firstRowBL up chamber_H) (polar firstRowBR up chamber_H)) 2.0) mdist)
                      )
          )

          (COMMAND "._MTEXT" txtPt "J" "MC" "H" profiletextSize txtPt "ROW 1" "")
          (clusterConcreteStoneHatch (polar firstRowBL right mdist) chamber_L)
        )
      )

      (if (= SYS_TYPE "STONE")
        (4crossStone (polar row_start right mdist) sys_L)
      )


      ;;;;;;;;;;;;;;;;;;;;;; Cross-Section D-D' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (setq mDist (+ mDist (* 1.2
                              (distance
                                (car $Cross-C-FGrade)
                                (last $Cross-C-FGrade)
                              )
                           )
                  )
      )

      (movePL $Cross-D-FGrade mDist 0)
      (movePL $Cross-D-EGrade mDist 0)

      (command "text" "J" "C" (polar (car PLlist) up (* profileTextSize 2)) (* profileTextSize 3) 0 "D")
      (command "text" "J" "C" (polar (last PLlist) up (* profileTextSize 2)) (* profileTextSize 3) 0 "D'")

      (if (AND (/= sys_type "8x4_CONCRETE_CLUSTER") (/= SYS_TYPE "STONE") (/= sys_type "4x8_CONCRETE_CLUSTER"))
        (progn
          ; draw Last on d-d' and label it
          (movePL (list
                    lastRowBL
                    lastRowBR
                    (polar lastRowBR up chamber_H)
                    (polar lastRowBL up chamber_H)
                    lastRowBL
                  )
                  mDist 0
          )
          (setq txtPt (polar (polar lastRowBL up (/ chamber_H 2.0))
                             right
                             (+ (/ (distance (polar lastRowBL up chamber_H) (polar lastRowBR up chamber_H)) 2.0) mdist)
                      )
          )

          (COMMAND "._MTEXT" txtPt "J" "MC" "H" profiletextSize txtPt (strcat "ROW " (rtos num_rows 2 0)) "")
        )
      )
      (if (OR (= sys_type "8x4_CONCRETE_CLUSTER") (= sys_type "4x8_CONCRETE_CLUSTER"))
        (progn
          ; draw Last on d-d' and label it
          (movePL (list
                    lastRowBL
                    lastRowBR
                    (polar lastRowBR up chamber_H)
                    (polar lastRowBL up chamber_H)
                    lastRowBL
                  )
                  mDist 0
          )
          (setq txtPt (polar (polar lastRowBL up (/ chamber_H 2.0))
                             right
                             (+ (/ (distance (polar lastRowBL up chamber_H) (polar lastRowBR up chamber_H)) 2.0) mdist)
                      )
          )

          (COMMAND "._MTEXT" txtPt "J" "MC" "H" profiletextSize txtPt (strcat "ROW " (rtos num_rows 2 0)) "")
          (clusterConcreteStoneHatch (polar lastRowBL right mdist) chamber_L)
        )
      )

      (if (= SYS_TYPE "STONE")
        (4crossStone (polar row_start right mdist) sys_L)
      )

      (setvar "ANGDIR" ANGDIR_DEF)
      (SETVAR "ANGBASE" ANGBASE_DEF)
      (setvar "ORTHOMODE" ORTHO_DEF)
      (toggle-OSNAP-Vars-RESET)

      ; zoom extents
      (command "ZOOM" "E")

      ; reset variable to NIL
      (setq PLlist nil)

      ;reset block insert unit vars
      (resetBlockUnitsVars)

    )
    (alert "4cross only works when information is in memory - after design - it does not work when you reopen a saved file")
  )

  (princ)


)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MAP_FILL (/ p1 p2 p3 p4 p5 p6 p7 p8 textPoint DIMASZ_DEF DIMTXT_DEF lineWidth lineType sset1)

	; set linewidth and linetype of fill extension
  (setq lineType "DASHED2")
  (setq lineWidth 70)
	; create empty selection set
  (setq sset1 (ssadd))

	; the points for drawing the fill extension
	;     p2       p3
	;  p1 ----------  p4
	;     |        |
	;  p8 ----------  p5
	;     p7      p6
	;

  (setq p1 (polar c_u1 %LEFT (/ u1_L 12.0)))
  (setq p2 (polar c_u1 %UP (/ u1_U 12.0)))
  (setq p3 (polar c_u2 %UP (/ u2_U 12.0)))
  (setq p4 (polar c_u2 %RIGHT (/ u2_R 12.0)))
  (setq p5 (polar c_d2 %RIGHT (/ d2_R 12.0)))
  (setq p6 (polar c_d2 %DOWN (/ d2_d 12.0)))
  (setq p7 (polar c_d1 %DOWN (/ d1_d 12.0)))
  (setq p8 (polar c_d1 %LEFT (/ d1_L 12.0)))


; Draws Fill Extension and add each entity to the selection set

  (if (OR (> U1_F 0) (> D1_F 0))
    (PROGN
      (COMMAND "line" p1 p8 "")
      (ssadd (entlast) sset1)
    )
  )


  (if (OR (> U1_F 0) (> U2_F 0))
    (PROGN
      (COMMAND "line" p2 p3 "")
      (ssadd (entlast) sset1)
    )
  )

  (if (OR (> U2_F 0) (> D2_F 0))
    (PROGN
      (COMMAND "line" p4 p5 "")
      (ssadd (entlast) sset1)
    )
  )

  (if (OR (> D1_F 0) (> D2_F 0))
    (PROGN
      (COMMAND "line" p6 p7 "")
      (ssadd (entlast) sset1)
    )
  )


  (if (> D1_F 0)
    (PROGN
      (COMMAND "spline" p7 p8 "" p6 p1)
      (ssadd (entlast) sset1)
    )
  )

  (if (> U1_F 0)
    (PROGN
      (COMMAND "spline" p1 p2 "" p8 p3)
      (ssadd (entlast) sset1)
    )
  )

  (if (> U2_F 0)
    (PROGN
      (COMMAND "spline" p3 p4 "" p2 p5)
      (ssadd (entlast) sset1)
    )
  )

  (if (> D2_F 0)
    (PROGN
      (COMMAND "spline" p5 p6 "" p4 p7)
      (ssadd (entlast) sset1)
    )
  )
	; Modify all entities in selection set
  (modifySS sset1 6 lineType)
  (modifySS sset1 370 lineWidth)


; Prints "Toe of Fill" near P7
  (SETQ DIMASZ_DEF (getvar "DIMASZ"))
  (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
  (SETVAR "DIMASZ" (* page3TextSize scaleArrowSize))
  (SETVAR "DIMTXT" page3TextSize)

  (setq textPoint (polar P7 %DOWN (* page3TextSize 4.0)))

  (setq str (cdr (assoc "%P3-4%" $customSysList)))
  (COMMAND "LEADER" P7 textPoint "" str "")


  (SETVAR "DIMTXT" DIMTXT_DEF)
  (SETVAR "DIMASZ" DIMASZ_DEF)

)
; END MAP FILL







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun crossLabel (/ p1 p2 p1a p2a proPtA proPtB selSet1 ex sset1)

        ;sets block insert unit vars
  (setBlockUnitsVars)

  (setq sset1 (ssadd))

  (setq scaleVar 2.5)

  (if (= crossSection "LEFT") (setq p1 (polar c_u1 %RIGHT 2.0)))
  (if (= crossSection "RIGHT") (setq p1 (polar c_u2 %LEFT 2.0)))
  (setq p1 (polar p1 %UP (* page3textSize scaleVar)))


  (if (= crossSection "LEFT") (setq p2 (polar c_d1 %RIGHT 2.0)))
  (if (= crossSection "RIGHT") (setq p2 (polar c_d2 %LEFT 2.0)))
  (setq p2 (polar p2 %DOWN (* page3textSize scaleVar)))

  (setq p1a (polar p1 %RIGHT scaleVar))
  (setq p2a (polar p2 %RIGHT scaleVar))

  (command "PLINE" p1a p2a "")
  (ssadd (entlast) sset1)

       ; 1 = yes
  (if (= %RotateSystem 1)
    (progn
      (setq blockRotation (- 90 BEARING))
    )
    (setq blockRotation 0)
  )

  (setq ex (getvar "Expert"))
  (setvar "Expert" 2)
  (command "_-insert" (strcat $septicadPathBlockRequired "CROSS_SECTION.DWG") p1a (/ page3scale 10.0) (/ page3scale 10.0) blockRotation)
  (ssadd (entlast) sset1)
  (command "_-insert" (strcat $septicadPathBlockRequired "CROSS_SECTION.DWG") p2a (/ page3scale 10.0) (/ page3scale 10.0) blockRotation)
  (ssadd (entlast) sset1)
  (setvar "Expert" ex)


  (if (= %RotateSystem 1)
    (progn

      (if (OR (>= bearing 315) (< bearing 45))
        (progn
          (setq ptA (polar p1a %UP (* (/ page3scale 10.0) 1.1)))
          (setq ptB (polar p2a %DOWN (* (/ page3scale 10.0) 1.1)))
          (COMMAND "-MTEXT" pta "J" "MR" "H" (* page3textSize 1.5) ptA "A" "")
          (ssadd (entlast) sset1)
          (COMMAND "-MTEXT" ptB "J" "ML" "H" (* page3textSize 1.5) ptB "B" "")
          (ssadd (entlast) sset1)
        )
      )

      (if (AND (>= bearing 45) (< bearing 135))
        (progn
          (setq ptA (polar p1a %LEFT (* (/ page3scale 10.0) 1.1)))
          (setq ptB (polar p2a %LEFT (* (/ page3scale 10.0) 1.1)))
          (COMMAND "-MTEXT" pta "J" "MR" "H" (* page3textSize 1.5) ptA "A" "")
          (ssadd (entlast) sset1)
          (COMMAND "-MTEXT" ptB "J" "MR" "H" (* page3textSize 1.5) ptB "B" "")
          (ssadd (entlast) sset1)
        )
      )
      (if (AND (>= bearing 135) (< bearing 225))
        (progn
          (setq ptA (polar p1a %UP (* (/ page3scale 10.0) 1.1)))
          (setq ptB (polar p2a %DOWN (* (/ page3scale 10.0) 1.1)))
          (COMMAND "-MTEXT" pta "J" "ML" "H" (* page3textSize 1.5) ptA "A" "")
          (ssadd (entlast) sset1)
          (COMMAND "-MTEXT" ptB "J" "MR" "H" (* page3textSize 1.5) ptB "B" "")
          (ssadd (entlast) sset1)
        )
      )
      (if (AND (>= bearing 225) (< bearing 315))
        (progn
          (setq ptA (polar p1a %LEFT (* (/ page3scale 10.0) 1.1)))
          (setq ptB (polar p2a %LEFT (* (/ page3scale 10.0) 1.1)))
          (COMMAND "-MTEXT" pta "J" "ML" "H" (* page3textSize 1.5) ptA "A" "")
          (ssadd (entlast) sset1)
          (COMMAND "-MTEXT" ptB "J" "ML" "H" (* page3textSize 1.5) ptB "B" "")
          (ssadd (entlast) sset1)
        )
      )

    )
  )




  (if (= %RotateSystem 0)
    (progn
      (COMMAND "-MTEXT" (polar p1a %RIGHT (* (/ page3scale 10.0) 1.1)) "J" "ML" "H" (* page3textSize 1.5) (polar p1a right (* (/ page3scale 10.0) 1.1)) "A" "")
      (ssadd (entlast) sset1)
      (COMMAND "-MTEXT" (polar p2a %RIGHT (* (/ page3scale 10.0) 1.1)) "J" "ML" "H" (* page3textSize 1.5) (polar p2a right (* (/ page3scale 10.0) 1.1)) "B" "")
      (ssadd (entlast) sset1)
    )
  )



  (COMMAND "-GROUP" "CREATE" "*" "" sset1 "")

        ; CREATING CROSS SECTION LABEL
  (SETQ proPtA (polar row_start (/ PI 2.0) (* profileScale 13)))
  (SETQ proPtA (polar proPtA PI (* page3textSize scaleVar 12.0)))
  (SETQ proPtB (polar (list (car row_end) (cadr row_start)) (/ PI 2.0) (* profileScale 13)))
  (SETQ proPtB (polar proPtB 0.0 (* page3textSize scaleVar 12.0)))

  (IF (or (or (= sys_type "8x4_CONCRETE_CLUSTER") (= SYS_TYPE "STONE")) (= sys_type "4x8_CONCRETE_CLUSTER"))
    (PROGN
      (SETQ proPtA (polar proPtA (/ PI 2.0) (* profileScale 2)))
      (SETQ proPtB (polar proPtB (/ PI 2.0) (* profileScale 2)))
    )
  )

  (command "text" "J" "MC" proPtA (* profileTextSize 2) 0 "A")
  (command "text" "J" "MC" proPtB (* profileTextSize 2) 0 "B")

;reset block insert unit vars
  (resetBlockUnitsVars)

)
; end crossLabel



(defun STEP_CAL (/ TEMP1 TEMP2 LIM_BOT_CH)

  (if (or (= SYS_TYPE "4x8_CONCRETE_CLUSTER") (= SYS_TYPE "8x4_CONCRETE_CLUSTER"))
    (progn
      (SETQ STEP 0)
      (SETQ ROW_START (LIST 0.0 DEPTH_STEP))
    )
  )

  (if (= sys_type "STONE")
    (progn
      (SETQ STEP 0)
      (SETQ ROW_START (LIST 0.0 DEPTH_STEP))
    )
  )


  (if (and (and (/= sys_type "STONE") (/= SYS_TYPE "4x8_CONCRETE_CLUSTER")) (/= SYS_TYPE "8x4_CONCRETE_CLUSTER"))
    (progn
      ; IF AUTO-DESIGN
      (if (= STEP_TYPE "AUTO")
        (progn

          (setq lim_top (list 0.0 DEPTH_TOP))
          (setq lim_bot (list sys_w DEPTH_BOT))

          (SETQ TEMP1 (ANGLE LIM_BOT LIM_TOP))
          (SETQ TEMP1 (- PI TEMP1))
          (SETQ TEMP2 (ABS (/ CHAMBER_W (COS TEMP1))))
          (SETQ LIM_BOT_CH (POLAR LIM_BOT (ANGLE LIM_BOT LIM_TOP) TEMP2))


          (if (> NUM_ROWS 1)
            (progn
              (SETQ STEP (/ (- (LAST LIM_BOT_CH) (LAST LIM_TOP)) (- NUM_ROWS 1)))
              (SETQ STEP (FIX STEP))
              (SETQ STEP (* STEP -1))
            )
          )

          (if (= NUM_ROWS 1)  ; if one row
            (progn
              (SETQ STEP 0.0)
            )
          )
          (SETQ ROW_START (POLAR LIM_TOP (/ PI 2) LIM_SEP))


        )
      )
      ; END IF (STEP_TYPE = "AUTO") ----AUTO-DESIGN

      ; If FORCE STEP
      (if (= STEP_TYPE "FORCE")
        (progn
          ;#### ASSIGNS AN ELEVATION TO TOP AND BOTTOM LIMITING FACTORS
          (SETQ STEP STEP_SIZE)

          ;#### CREATES THE POSITION POINT FOR THE BOTTOM LEFT CORNER OF ROW 1
          (SETQ ROW_START (LIST 0.0 DEPTH_STEP))

        )
      )
      ; END IF (STEP_TYPE = "FORCE") ----FORCE STEP



      ;;;; start if >20% 
      (if (> (/ step (+ chamber_w row_sep)) 0.20)
        (progn
          (alert (strcat
                   "                                                                     SeptiCAD WARNING\n\n"
                   "System slope exceeds 20%.  Current design is for " (rtos num_rows 2 0) " rows with " (rtos step 2 0) "\" step drop per row ["
                   (rtos (* 100 (/ step (+ chamber_w row_sep))) 2 2)
                   "%]\n\nSystem slope will be exactly 20% if the step drop per row is "
                   (rtos (* 0.20 (+ chamber_w row_sep)) 2 2)
                   "\"."
                   "\n\nNOTE:  For non-whole number step drops use the FORCE-STEP calculation method (i.e. "
                   (rtos (* 0.20 (+ chamber_w row_sep)) 2 2) "\" step drop per row)"
                 )
          )
        )
      )
      ;;;; end if >20%
    )
  )
)           ; end STEP_CAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main_drawing (/ elv_top elv_bot ex currentLayer)

  ; Set to eliminate block related dialog
       (setq ex (getvar "Expert"))
       (setvar "Expert" 2)
       (setq $chamberBlock nil)

       (COMMAND "LAYOUT" "S" "MODEL")
       (setq currentLayer (getvar "CLAYER"))
       (SETVAR "CLAYER" "SEPTICAD")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;%:%:%:%:%:%:%:%:%:%:%:%:%:%:%:%:%:%:%:%:%:%:
; 1 = ON, 0 = OFF in main savevars


  (if (= %RotateSystem 1)
    (progn
      (SETQ BEARING_RADIANS (- (/ pi 2.0) (* (/ BEARING 360.0) 2.0 pi)))
      (setq %LEFT (+ pi BEARING_RADIANS))
      (setq %UP (+ (/ pi 2.0) BEARING_RADIANS))
      (setq %DOWN (+ (/ (* 3.0 pi) 2.0) BEARING_RADIANS))
      (setq %RIGHT (+ 0.0 BEARING_RADIANS))

    )
  )
       (if (= %RotateSystem 0)
         (progn
           (setq %LEFT pi)
           (setq %UP (/ pi 2.0))
           (setq %DOWN (/ (* 3.0 pi) 2.0))
           (setq %RIGHT 0.0)
         )
       )
;%:%:%:%:%:%:%:%:%:%:%:%:%:%:%:%:%:%:%:%:%:%:


       ; base point for drawing - top left corner
       (setq page2BaseMap (list (* 12.0 page2scale) 800.0))

       ; base point for drawing - top left corner
       (setq page3BaseMap '(0.0 800.0))

       (SETVAR "CLAYER" "SEPTICAD")

       (setq lastErrorFunction *error*)
       (setq *error* DEFAULT_ERROR)

     ; Set Variables that are required before (main_drawing)     
      (COND
        ((= DOWN_SLOPE "0") (SETQ DOWN_SLOPE_TXT "3:1 Grade"))
      )
      (COND
        ((= DOWN_SLOPE "1") (SETQ DOWN_SLOPE_TXT "4:1 Grade"))
      )
      (COND
        ((= DOWN_SLOPE "2") (SETQ DOWN_SLOPE_TXT "5:1 Grade"))
      )
      (COND
        ((= DOWN_SLOPE "3") (SETQ DOWN_SLOPE_TXT "6:1 Grade"))
      )

      (COND
        ((= DOWN_SLOPE "0") (SETQ DOWN_SLOPE (ANGLE '(0 1) '(3 0))))
      )
      (COND
        ((= DOWN_SLOPE "1") (SETQ DOWN_SLOPE (ANGLE '(0 1) '(4 0))))
      )
      (COND
        ((= DOWN_SLOPE "2") (SETQ DOWN_SLOPE (ANGLE '(0 1) '(5 0))))
      )
      (COND
        ((= DOWN_SLOPE "3") (SETQ DOWN_SLOPE (ANGLE '(0 1) '(6 0))))
      )

      (COND
        ((= LEFT_SLOPE "0") (SETQ LEFT_SLOPE (ANGLE '(0 1) '(-3 0))))
      )
      (COND
        ((= LEFT_SLOPE "1") (SETQ LEFT_SLOPE (ANGLE '(0 1) '(-4 0))))
      )
      (COND
        ((= LEFT_SLOPE "2") (SETQ LEFT_SLOPE (ANGLE '(0 1) '(-5 0))))
      )
      (COND
        ((= LEFT_SLOPE "3") (SETQ LEFT_SLOPE (ANGLE '(0 1) '(-6 0))))
      )

      (COND
        ((= RIGHT_SLOPE "0") (SETQ RIGHT_SLOPE (ANGLE '(0 1) '(3 0))))
      )
      (COND
        ((= RIGHT_SLOPE "1") (SETQ RIGHT_SLOPE (ANGLE '(0 1) '(4 0))))
      )
      (COND
        ((= RIGHT_SLOPE "2") (SETQ RIGHT_SLOPE (ANGLE '(0 1) '(5 0))))
      )
      (COND
        ((= RIGHT_SLOPE "3") (SETQ RIGHT_SLOPE (ANGLE '(0 1) '(6 0))))
      )

      (COND
        ((= UP_SLOPE "0") (SETQ UP_SLOPE_TXT "3:1 Grade"))
      )
      (COND
        ((= UP_SLOPE "1") (SETQ UP_SLOPE_TXT "4:1 Grade"))
      )
      (COND
        ((= UP_SLOPE "2") (SETQ UP_SLOPE_TXT "5:1 Grade"))
      )
      (COND
        ((= UP_SLOPE "3") (SETQ UP_SLOPE_TXT "6:1 Grade"))
      )

      (COND
        ((= UP_SLOPE "0") (SETQ UP_PRO_SLOPE (ANGLE '(0 1) '(-3 0))))
      )
      (COND
        ((= UP_SLOPE "1") (SETQ UP_PRO_SLOPE (ANGLE '(0 1) '(-4 0))))
      )
      (COND
        ((= UP_SLOPE "2") (SETQ UP_PRO_SLOPE (ANGLE '(0 1) '(-5 0))))
      )
      (COND
        ((= UP_SLOPE "3") (SETQ UP_PRO_SLOPE (ANGLE '(0 1) '(-6 0))))
      )

      (COND
        ((= UP_SLOPE "0") (SETQ UP_SLOPE (ANGLE '(0 1) '(3 0))))
      )
      (COND
        ((= UP_SLOPE "1") (SETQ UP_SLOPE (ANGLE '(0 1) '(4 0))))
      )
      (COND
        ((= UP_SLOPE "2") (SETQ UP_SLOPE (ANGLE '(0 1) '(5 0))))
      )
      (COND
        ((= UP_SLOPE "3") (SETQ UP_SLOPE (ANGLE '(0 1) '(6 0))))
      )

    ; High Capacity or Standard Chambers - Choose with or without stone beside
       (if (or (= SYS_TYPE "ADS_HI_CAP")
               (= SYS_TYPE "QUICK4_HI_CAP")
               (= SYS_TYPE "QUICK4_STD")
               (= SYS_TYPE "Q4_PLUS_HC")
               (= SYS_TYPE "Q4_PLUS_STD")
               (= SYS_TYPE "ARC36HC")
               (= SYS_TYPE "ARC36")
               (= SYS_TYPE "ADS_STD")
               (= SYS_TYPE "INF_STD")
               (= SYS_TYPE "INF_HI_CAP")
           )
         (progn
           ; call dialog to ask yes or no question sets $stoneChamber = "YES" or "NO"
           (stoneChamberChoose)

           (if (= SYS_TYPE "ADS_HI_CAP")
             (progn
               (if (= $stoneChamber "YES")
                 (progn
                   (setq $chamberBlock (STRCAT $septicadPathChambers "ADS_HI_CAP_STONE.dwg")) (PRO_SHELF $chamberBlock)
                 )
               )
               (if (= $stoneChamber "NO")
                 (progn
                   (setq $chamberBlock (STRCAT $septicadPathChambers "ADS_HI_CAP.dwg")) (PRO_SHELF $chamberBlock)
                 )
               )
             )
           )
           (if (= SYS_TYPE "QUICK4_HI_CAP")
             (progn
               (if (= $stoneChamber "YES")
                 (progn
                   (setq $chamberBlock (STRCAT $septicadPathChambers "QUICK4_HI_CAP_STONE.dwg")) (PRO_SHELF $chamberBlock)
                 )
               )
               (if (= $stoneChamber "NO")
                 (progn
                   (setq $chamberBlock (STRCAT $septicadPathChambers "QUICK4_HI_CAP.dwg")) (PRO_SHELF $chamberBlock)
                 )
               )
             )
           )
           (if (= SYS_TYPE "QUICK4_STD")
             (progn
               (if (= $stoneChamber "YES")
                 (progn
                   (setq $chamberBlock (STRCAT $septicadPathChambers "QUICK4_STD_STONE.dwg")) (PRO_SHELF $chamberBlock)
                 )
               )
               (if (= $stoneChamber "NO")
                 (progn
                   (setq $chamberBlock (STRCAT $septicadPathChambers "QUICK4_STD.dwg")) (PRO_SHELF $chamberBlock)
                 )
               )
             )
           )
           (if (= SYS_TYPE "Q4_PLUS_HC")
             (progn
               (if (= $stoneChamber "YES")
                 (progn
                   (setq $chamberBlock (STRCAT $septicadPathChambers "Q4_PLUS_HC_STONE.dwg")) (PRO_SHELF $chamberBlock)
                 )
               )
               (if (= $stoneChamber "NO")
                 (progn
                   (setq $chamberBlock (STRCAT $septicadPathChambers "Q4_PLUS_HC.dwg")) (PRO_SHELF $chamberBlock)
                 )
               )
             )
           )
           (if (= SYS_TYPE "Q4_PLUS_STD")
             (progn
               (if (= $stoneChamber "YES")
                 (progn
                   (setq $chamberBlock (STRCAT $septicadPathChambers "Q4_PLUS_STD_STONE.dwg")) (PRO_SHELF $chamberBlock)
                 )
               )
               (if (= $stoneChamber "NO")
                 (progn
                   (setq $chamberBlock (STRCAT $septicadPathChambers "Q4_PLUS_STD.dwg")) (PRO_SHELF $chamberBlock)
                 )
               )
             )
           )

           (if (= SYS_TYPE "ARC36HC")
             (progn
               (if (= $stoneChamber "YES")
                 (progn
                   (setq $chamberBlock (STRCAT $septicadPathChambers "ARC36HC_STONE.dwg")) (PRO_SHELF $chamberBlock)
                 )
               )
               (if (= $stoneChamber "NO")
                 (progn
                   (setq $chamberBlock (STRCAT $septicadPathChambers "ARC36HC.dwg")) (PRO_SHELF $chamberBlock)
                 )
               )
             )
           )

           (if (= SYS_TYPE "ARC36")
             (progn
               (if (= $stoneChamber "YES")
                 (progn
                   (setq $chamberBlock (STRCAT $septicadPathChambers "ARC36_STONE.dwg")) (PRO_SHELF $chamberBlock)
                 )
               )
               (if (= $stoneChamber "NO")
                 (progn
                   (setq $chamberBlock (STRCAT $septicadPathChambers "ARC36.dwg")) (PRO_SHELF $chamberBlock)
                 )
               )
             )
           )

           (if (= SYS_TYPE "ADS_STD")
             (progn
               (if (= $stoneChamber "YES")
                 (progn
                   (setq $chamberBlock (STRCAT $septicadPathChambers "ADS_STD_STONE.dwg")) (PRO_SHELF $chamberBlock)
                 )
               )
               (if (= $stoneChamber "NO")
                 (progn
                   (setq $chamberBlock (STRCAT $septicadPathChambers "ADS_STD.dwg")) (PRO_SHELF $chamberBlock)
                 )
               )
             )
           )
           (if (= SYS_TYPE "INF_STD")
             (progn
               (if (= $stoneChamber "YES")
                 (progn
                   (setq $chamberBlock (STRCAT $septicadPathChambers "INF_STD_STONE.dwg")) (PRO_SHELF $chamberBlock)
                 )
               )
               (if (= $stoneChamber "NO")
                 (progn
                   (setq $chamberBlock (STRCAT $septicadPathChambers "INF_STD.dwg")) (PRO_SHELF $chamberBlock)
                 )
               )
             )
           )

           (if (= SYS_TYPE "INF_HI_CAP")
             (progn
               (if (= $stoneChamber "YES")
                 (progn
                   (setq $chamberBlock (STRCAT $septicadPathChambers "INF_HI_CAP_STONE.dwg")) (PRO_SHELF $chamberBlock)
                 )
               )
               (if (= $stoneChamber "NO")
                 (progn
                   (setq $chamberBlock (STRCAT $septicadPathChambers "INF_HI_CAP.dwg")) (PRO_SHELF $chamberBlock)
                 )
               )
             )
           )
         )
       )

  ; Concrete - Choose wether H-20 Load Rated or Not
  ; call dialog to ask yes or no question sets $H20-RATED = "YES" or "NO"
       (SETQ $H20-RATED NIL)
  (if (= (substr sys_type 5 8) "CONCRETE")
    (H20-RatedChoose)
  )

  ; Set to eliminate block related dialog
       (setq ex (getvar "Expert"))
       (setvar "Expert" 2)


  ; Send these to legacy functions v1
  (COND
    ((= sys_type "STONE") (PRO_STONE))
  )
  (COND
    ((= SYS_TYPE "4x8_CONCRETE_CLUSTER") (PRO_4x8_CONCRETE_CLUSTER))
  )
  (COND
    ((= SYS_TYPE "8x4_CONCRETE_CLUSTER") (PRO_8x4_CONCRETE_CLUSTER))
  )

  ; Send these to new functions v2 - CHAMBERS WITH STONE BESIDE CHOICE ABOVE
  (COND
    ((= SYS_TYPE "ELJEN") (setq $chamberBlock (STRCAT $septicadPathChambers "ELJEN.DWG")) (PRO_SHELF $chamberBlock))
  )
  (COND
    ((= SYS_TYPE "ELJEN_INVERTED") (setq $chamberBlock (STRCAT $septicadPathChambers "ELJEN_INVERTED.DWG")) (PRO_SHELF $chamberBlock))
  )
  (COND
    ((= SYS_TYPE "ENVIRO") (setq $chamberBlock (STRCAT $septicadPathChambers "ENVIRO.DWG")) (PRO_SHELF $chamberBlock))
  )
  (COND
    ((= SYS_TYPE "ADS_BIO2") (setq $chamberBlock (STRCAT $septicadPathChambers "ADS_BIO2.DWG")) (PRO_SHELF $chamberBlock))
  )
  (COND
    ((= SYS_TYPE "QUICK4_EQ24") (setq $chamberBlock (STRCAT $septicadPathChambers "QUICK4_EQ24.DWG")) (PRO_SHELF $chamberBlock))
  )
  (COND
    ((= SYS_TYPE "MOUNDBUSTER") (setq $chamberBlock (STRCAT $septicadPathChambers "MOUNDBUSTER.DWG")) (PRO_SHELF $chamberBlock))
  )
  (COND
    ((= SYS_TYPE "8x4_CONCRETE_TRENCH") (setq $chamberBlock (STRCAT $septicadPathChambers "8x4_CONCRETE_TRENCH.DWG")) (PRO_SHELF $chamberBlock))
  )
  (COND
    ((= SYS_TYPE "4x8_CONCRETE_TRENCH") (setq $chamberBlock (STRCAT $septicadPathChambers "4x8_CONCRETE_TRENCH.DWG")) (PRO_SHELF $chamberBlock))
  )
  (COND
    ((= SYS_TYPE "ARC18") (setq $chamberBlock (STRCAT $septicadPathChambers "ARC18.DWG")) (PRO_SHELF $chamberBlock))
  )
  (COND
    ((= SYS_TYPE "INF_EQ24") (setq $chamberBlock (STRCAT $septicadPathChambers "INF_EQ24.DWG")) (PRO_SHELF $chamberBlock))
  )
  (COND
    ((= SYS_TYPE "TRENCH2") (setq $chamberBlock (STRCAT $septicadPathChambers "TRENCH2.DWG")) (PRO_SHELF $chamberBlock))
  )
  (COND
    ((= SYS_TYPE "TRENCH3") (setq $chamberBlock (STRCAT $septicadPathChambers "TRENCH3.DWG")) (PRO_SHELF $chamberBlock))
  )

  (COND
    ((= SYS_TYPE "ARC24") (setq $chamberBlock (STRCAT $septicadPathChambers "ARC24.DWG")) (PRO_SHELF $chamberBlock))
  )
  (COND
    ((= SYS_TYPE "Q4_PLUS_EQ_36_LP") (setq $chamberBlock (STRCAT $septicadPathChambers "Q4_PLUS_EQ_36_LP.DWG")) (PRO_SHELF $chamberBlock))
  )
  (COND
    ((= SYS_TYPE "Q4_PLUS_STD_LP") (setq $chamberBlock (STRCAT $septicadPathChambers "Q4_PLUS_STD_LP.DWG")) (PRO_SHELF $chamberBlock))
  )
  (COND
    ((= SYS_TYPE "QUICK4_EQ24LP") (setq $chamberBlock (STRCAT $septicadPathChambers "QUICK4_EQ24LP.DWG")) (PRO_SHELF $chamberBlock))
  )
  (COND
    ((= SYS_TYPE "QUICK4_EQ36") (setq $chamberBlock (STRCAT $septicadPathChambers "QUICK4_EQ36.DWG")) (PRO_SHELF $chamberBlock))
  )




  ; Reset variable
       (setvar "EXPERT" ex)


   ; IF AUTO-DESIGN
;   (if (= STEP_TYPE "AUTO")
;      (progn
;        (command "pline" lim_top lim_bot "")
;
;        (setq ang (angle lim_top lim_bot))
;        (setq insertPoint (polar lim_top ang (/ (distance lim_top lim_bot) 2.0)))
;        (setq insertPoint (polar insertPoint (- ang (/ pi 2.0)) 1.5))
;        (command "text" "J" "TC" insertPoint profileTextSize (* ang (/ 180.0 pi)) "Limiting Factor")
;      )
;   )  

       (if (= SYS_TYPE "ENVIRO") (drawChamberSandEnviro chamberptlist))
       (if (or (= SYS_TYPE "ELJEN") (= SYS_TYPE "ELJEN_INVERTED")) (drawChamberSandEljen chamberptlist))


       (PRO_FILL)

       (SETQ U1_F (* -1 (- U1 (LAST FILL_TOP))))
       (SETQ U2_F (* -1 (- U2 (LAST FILL_TOP))))
       (SETQ D1_F (* -1 (- D1 (LAST FILL_BOT))))
       (SETQ D2_F (* -1 (- D2 (LAST FILL_BOT))))



; calculate fill extensions lengths
       (MAP_FILL_EXT)

; Draw Plan View, various specialty versions, then default plastic chamber type


  (if (= sys_type "STONE")
    (STONE_DRAW page3BaseMap sys_L sys_W pipeSpacing shStone)
  )

  (if (or (= sys_type "MOUNDBUSTER") (= SYS_TYPE "ENVIRO") (= SYS_TYPE "TRENCH2") (= SYS_TYPE "TRENCH3"))
    (progn
      (setq temp num_units)
      (setq num_units 1)
      ; sys_L SUBSTITUTED FOR chamber_L
      (CHAMBER_DRAW page3BaseMap chamber_W sys_L sys_L sys_W row_sep)
      (setq num_units temp)
    )
  )




   ; ALL THE REST
       (if (and (/= sys_type "MOUNDBUSTER")
                (/= sys_type "STONE")
                (/= SYS_TYPE "ENVIRO")
                (/= SYS_TYPE "TRENCH2")
                (/= SYS_TYPE "TRENCH3")
           )
         (CHAMBER_DRAW page3BaseMap chamber_W chamber_L sys_L sys_W row_sep)
       )


    ; create elevation table
  (if (and (/= sys_type "4x8_CONCRETE_CLUSTER") (/= sys_type "8x4_CONCRETE_CLUSTER") (/= sys_type "STONE"))
    (progn
      (COND
        ((and (= $LEVELTABLE "NO") (= (isLevelSystem) "NO")) (PRO_TABLE_SHELF))
      )
      (COND
        ((and (= $LEVELTABLE "NO") (= (isLevelSystem) "YES")) (princ "\nElevation Table not printed because user does not want Elevation Table on level system to be printed\n"))
      )
      (COND
        ((and (= $LEVELTABLE "YES") (= (isLevelSystem) "NO")) (PRO_TABLE_SHELF))
      )
      (COND
        ((and (= $LEVELTABLE "YES") (= (isLevelSystem) "YES")) (PRO_TABLE_SHELF))
      )
    )
  )


	; draw scale on bottom of cross section
       (SCALE_BOT)


	; insert profile note
       (if (= $NOTES-PRO "YES") (profileNote))


	; complete volume calculation and print to cross section, pass either yes or no to print note on cross section 
       (sVolume $FILLNOTE)


	; label crosssection A and B on Page 3 Plan/Crosssection
       (IF (= $LABELPROFILE "YES") (crossLabel))

        ; draw shoulders, corner labels and elevation table
       (if (= shoulderPlan "YES") (shoulders))
       (if (= $4CORNERS "CORNERS") (elevationCorners))
       (if (= $4CORNERS "TABLE") (elevationTable))
  (if (= $4CORNERS "TABLEFILL")
    (progn
      (elevationTable)
      (DepthOfFill)
    )
  )


  (if (= $DSWALE "YES")
    (dswale "INLINE")
  )

  (if (= $AUTOSCAR "YES")
    (c:scarAuto)
  )



       (COMMAND "REGEN")
       (SETVAR "CLAYER" currentLayer)
       (setq *error* lastErrorFunction)
       (setvar "Expert" ex)

















)
;;;; END main_drawing
;;;;;;;;;;;;;;;;;;;;


          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (defun shoulders (/ lineType sset1 p1 p2 p1a p2a DIMASZ_DEF DIMTXT_DEF chamd pa pb pc pd pe pf pg ph)

            ; create empty selection set
            (setq sset1 (ssadd))
            (setq lineType "DASHED2")

            (setq p1 (polar C_U1 %UP (/ UP_shoulder 12.0)))
            (setq p1 (polar p1 %LEFT (/ LEFT_shoulder 12.0)))
            (setq p2 (polar C_D2 %DOWN (/ DOWN_shoulder 12.0)))
            (setq p2 (polar p2 %RIGHT (/ RIGHT_shoulder 12.0)))

            (setq p1a (polar p1 %RIGHT (+ (/ sys_L 12.0) (/ LEFT_shoulder 12.0) (/ RIGHT_shoulder 12.0))))
            (setq p2a (polar p2 %LEFT (+ (/ sys_L 12.0) (/ LEFT_shoulder 12.0) (/ RIGHT_shoulder 12.0))))
              ; corners to chamfer
              ;;;; p1     p1a
              ;;;; p2a    p2


            ; create 8 points for chamfered poly line
            (setq chamd 1.0)
            (setq pa (polar p1 %RIGHT chamd))
            (setq pb (polar p1a %LEFT chamd))
            (setq pc (polar p1a %DOWN chamd))
            (setq pd (polar p2 %UP chamd))
            (setq pe (polar p2 %LEFT chamd))
            (setq pf (polar p2a %RIGHT chamd))
            (setq pg (polar p2a %UP chamd))
            (setq ph (polar p1 %DOWN chamd))
            (command "pline" pa pb pc pd pe pf pg ph pa "")

            ; add to selection set and apply a line type
            (ssadd (entlast) sset1)
            (modifySS sset1 6 lineType)

            (SETQ DIMASZ_DEF (getvar "DIMASZ"))
            (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
            (SETVAR "DIMASZ" (* page3TextSize scaleArrowSize))
            (SETVAR "DIMTXT" page3TextSize)

            (setq str (cdr (assoc "%P3-5%" $customSysList)))

            (if (= %RotateSystem 1)
              (progn
                (if (OR (>= bearing 315) (< bearing 45))
                  (progn
                    (setq arrowPoint pa)
                    (setq textPoint (polar (polar p1 %RIGHT (* 0.1 (distance c_u1 c_u2))) %UP (* page3TextSize 5.0)))
                  )
                )
                (if (AND (>= bearing 45) (< bearing 135))
                  (progn
                    (setq arrowPoint (polar p1 %down (* 0.4 (distance c_u1 c_d1))))
                    (setq textPoint (polar (polar p1 %LEFT (* 0.4 (distance c_u1 c_u2))) %DOWN (* page3TextSize 5.0)))
                  )
                )
                (if (AND (>= bearing 135) (< bearing 225))
                  (progn
                    (setq arrowPoint (polar pd %UP (* (distance pd pc) 0.5)))
                    (setq textPoint (polar (polar p2 %RIGHT (* 0.4 (distance c_u1 c_u2))) %UP (* page3TextSize 5.0)))
                  )
                )
                (if (AND (>= bearing 225) (< bearing 315))
                  (progn
                    (setq arrowPoint pb)
                    (setq textPoint (polar (polar p1a %RIGHT (* 0.4 (distance c_u1 c_u2))) %UP (* page3TextSize 5.0)))
                  )
                )
                (COMMAND "LEADER" arrowPoint textPoint "" str "")
              )
            )





            (if (= %RotateSystem 0)
              (progn
                (if (= sys_type "STONE")
                  (COMMAND "LEADER" (polar p1 %RIGHT (* 0.4 (distance c_u1 c_u2))) (polar (polar p1 %RIGHT (* 0.4 (distance c_u1 c_u2))) (/ (* 5.0 PI) 12.0) (* page3TextSize 5.0)) "" str "")
                  (COMMAND "LEADER" (polar p1 %RIGHT (* 0.4 (distance c_u1 c_u2))) (polar (polar p1 %RIGHT (* 0.4 (distance c_u1 c_u2))) (/ (* 7.0 PI) 12.0) (* page3TextSize 5.0)) "" str "")
                )
              )
            )

            (SETVAR "DIMTXT" DIMTXT_DEF)
            (SETVAR "DIMASZ" DIMASZ_DEF)

          )
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun createSysList ()

; version 4 systems
;"STONE"
;"ENVIRO"
;"ELJEN"
;"ELJEN_INVERTED"
;"ADS_HI_CAP"
;"ADS_BIO2"
;
;"QUICK4_HI_CAP"
;"QUICK4_EQ24"
;
;"4x8_CONCRETE_CLUSTER"
;"8x4_CONCRETE_CLUSTER"
;
;"4x8_CONCRETE_TRENCH"
;"8x4_CONCRETE_TRENCH"
;
;"MOUNDBUSTER"

  (setq sysList (list))
	; Create list with a KEY.Description format in list sysList

  (setq sysList (append sysList (list (cons "MOUNDBUSTER" "Maine Fuji Pipe"))))

  (setq sysList (append sysList (list (cons "STONE" "Stone Bed"))))
  (setq sysList (append sysList (list (cons "TRENCH2" "Stone Trench (2 feet wide)"))))
  (setq sysList (append sysList (list (cons "TRENCH3" "Stone Trench (3 feet wide)"))))

  (setq sysList (append sysList (list (cons "ELJEN" "Eljen GSF Geotextile Sand Filter"))))
  (setq sysList (append sysList (list (cons "ELJEN_INVERTED" "Eljen GSF Geotextile Sand Filter (Tranverse)"))))

  (setq sysList (append sysList (list (cons "ENVIRO" "Enviro-Septic Pipe"))))

  (setq sysList (append sysList (list (cons "4x8_CONCRETE_CLUSTER" "4' x 8' Long Concrete Chamber [Cluster]"))))
  (setq sysList (append sysList (list (cons "8x4_CONCRETE_CLUSTER" "8' x 4' Long Concrete Chamber [Cluster]"))))

  (setq sysList (append sysList (list (cons "4x8_CONCRETE_TRENCH" "4' x 8' Long Concrete Chamber [Trench]"))))
  (setq sysList (append sysList (list (cons "8x4_CONCRETE_TRENCH" "8' x 4' Long Concrete Chamber [Trench]"))))


  (setq sysList (append sysList (list (cons "ADS_STD" "ADS BioDiffuser Standard"))))
  (setq sysList (append sysList (list (cons "ADS_HI_CAP" "ADS BioDiffuser Hi-Capacity"))))
  (setq sysList (append sysList (list (cons "ADS_BIO2" "ADS BioDiffuser Narrow [Bio 2]"))))

  (setq sysList (append sysList (list (cons "ARC18" "ADS Arc 18"))))
  (setq sysList (append sysList (list (cons "ARC24" "ADS Arc 24"))))
  (setq sysList (append sysList (list (cons "ARC36" "ADS Arc 36"))))
  (setq sysList (append sysList (list (cons "ARC36HC" "ADS Arc 36 Hi-Capacity"))))

  (setq sysList (append sysList (list (cons "INF_STD" "Infiltrator Standard"))))
  (setq sysList (append sysList (list (cons "INF_HI_CAP" "Infiltrator Hi-Capacity"))))
  (setq sysList (append sysList (list (cons "INF_EQ24" "Infiltrator EQ24"))))

  (setq sysList (append sysList (list (cons "QUICK4_STD" "Infiltrator Quick4 Standard"))))
  (setq sysList (append sysList (list (cons "QUICK4_HI_CAP" "Infiltrator Quick4 Hi-Capacity"))))
  (setq sysList (append sysList (list (cons "QUICK4_EQ24" "Infiltrator Quick4 EQ 24"))))
  (setq sysList (append sysList (list (cons "QUICK4_EQ24LP" "Infiltrator Quick4 EQ 24 Low Profile"))))
  (setq sysList (append sysList (list (cons "QUICK4_EQ36" "Infiltrator Quick4 EQ 36"))))

  (setq sysList (append sysList (list (cons "Q4_PLUS_STD" "Infiltrator Quick4 Plus Standard"))))
  (setq sysList (append sysList (list (cons "Q4_PLUS_STD_LP" "Infiltrator Quick4 Plus Standard Low Profile"))))
  (setq sysList (append sysList (list (cons "Q4_PLUS_HC" "Infiltrator Quick4 Plus High Capacity"))))
  (setq sysList (append sysList (list (cons "Q4_PLUS_EQ_36_LP" "Infiltrator Quick4 Plus EQ 36 Low Profile"))))






	; list of chambers with end cap that are plotted and included in system sizing calculations
  (setq $endCapChambers (list
                          "Q4_PLUS_EQ_36_LP"
                          "Q4_PLUS_HC"
                          "Q4_PLUS_STD"
                          "Q4_PLUS_STD_LP"
                          "QUICK4_EQ24"
                          "QUICK4_EQ24LP"
                          "QUICK4_EQ36"
                          "QUICK4_HI_CAP"
                          "QUICK4_STD"
                        )
  )


)
; createSysList











;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun C:SEPTIC (/ choice str)

 ; IF LAYOUT IS NOT FOUND
  (if (NOT (tblsearch "layer" "FORM_HHE-200"))
    (progn

      ; IF TEMPLATE FOUND and in bricscad THEN OPEN IT
      (if (and
            (findfile (strcat $septicadPathRoot "SeptiCAD-Template.dwt"))
            (= (strcase (getvar "PRODUCT")) "BRICSCAD")
          )
        (progn

          ;;; START DCL DIALOG
          (setq dcl_id (load_dialog (strcat $septicadPathDCL "TEMPLATE.dcl")))

          ;;;--- See if the dialog box is already loaded
          (if (not (new_dialog "TEMPLATE" dcl_id))
            (progn
              (alert "The TEMPLATE.DCL file was not found!")
              (exit)
            )
          )

          ;;;--- If an action event occurs, do this function
          (action_tile "NEW" "(setq ddiag 1)(done_dialog)")
          (action_tile "CANCEL" "(setq ddiag 2)(done_dialog)")

          ;;;--- Display the dialog box
          (start_dialog)

          ;;;--- Unload the dialog box
          (unload_dialog dcl_id)

          ;;;--- If the "NEW" button was pressed
          (if (= ddiag 1) (setq choice "NEW"))

          ;;;--- If the "CANCEL" button was pressed
          (if (= ddiag 2) (setq choice "CANCEL"))

          ;;; END DCL DIALOG
          ;;; choice is now set to NEW or CANCEL




          (if (= choice "NEW")
            (command "new" (strcat $septicadPathRoot "SeptiCAD-Template.dwt"))
          )

          (if (= choice "CANCEL")
            (progn
              (setq str (strcat "A SeptiCAD template file is required.  The default template should be located here -> " (strcat $septicadPathRoot "SeptiCAD-Template.dwt")))
              (princ str)
              (ALERT str)
              (exit)
            )
          )

        )
      )
      ; END-> IF TEMPLATE FOUND THEN OPEN IT OR INSERT IT 

      ; IF TEMPLATE FOUND and in ACAD THEN tell user to open septicad file
      (if (and
            (findfile (strcat $septicadPathRoot "SeptiCAD-Template.dwt"))
            (= (strcase (getvar "PRODUCT")) "AUTOCAD")
          )
        (progn

          (setq str (strcat "A SeptiCAD template file is required.  The default template is located here -> " (strcat $septicadPathRoot "SeptiCAD-Template.dwg")))
          (princ str)
          (ALERT str)
          (exit)

        )
      )

    )
  )
 ; END->  IF LAYOUT IS NOT FOUND


; IF SEPTICAD TEMPLATE FILE IS OPEN THEN THESE COMMANDS FOLLOW
  (if (= $septicadFullVersion nil) (septicadCheckReg))

  (if (/= $septicadFullVersion 1)
    (progn
      (setUniqueSerialNumber "ALERT")
    )
  )

  (if (/= $$RegButton "YES") (maindesign))
  (if (= $$RegButton "YES") (c:suser))

  (princ)
)
; end c:septic

(defun mainDesign (/ ATTDIA_DEFAULT NGDIR_DEF ANGBASE_DEF DIMASZ_DEF DIMTXT_DEF ortho_DEF this slopeList tempList ddiag)





  ;;;;;;;;;; Start Subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 
           (defun mainHelp ()
            (if (/= (strcase (getvar "PRODUCT")) "AUTOCAD")
              (command "URL" (strcat $septicadPathHTML "MAIN.HTML"))
            )
            (if (= (strcase (getvar "PRODUCT")) "AUTOCAD")
              (ACADbrowser (strcat $septicadPathHTML "MAIN.HTML"))
            )
          )

          (defun mainHelpElevations ()
            (if (/= (strcase (getvar "PRODUCT")) "AUTOCAD")
              (command "URL" (strcat "file:///" $septicadPathPDF "ELEVATIONS.pdf"))
            )
            (if (= (strcase (getvar "PRODUCT")) "AUTOCAD")
              (ACADbrowser (strcat $septicadPathPDF "ELEVATIONS.pdf"))
            )
          )
 
  
          (defun shelf-help ()
            (if (= (strcase (getvar "PRODUCT")) "BRICSCAD")
              (command "URL" (strcat $septicadPathPDF "SHELF.PDF"))
            )
            (if (= (strcase (getvar "PRODUCT")) "AUTOCAD")
              (ACADbrowser (strcat $septicadPathPDF "SHELF.pdf"))
            )
          )
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (defun lf-help (slideNumber)

                 ;;;--- Load the DCL file
            (setq dcl_id (load_dialog (strcat $septicadPathDCL "LIMITING-FACTOR-HELP.dcl")))

                ;;;--- See if the dialog box is already loaded
            (if (not (new_dialog "LIMITING_FACTOR_HELP" dcl_id))
              (progn
                (alert "The LIMITING-FACTOR-HELP.DCL file was not found!")
                (exit)
              )
            )

                 ; choose which slide to show based on the button pressed
            (if (= slideNumber 0) (setq slideName (strcat $septicadPathDCL "LIMITING-FACTOR-HELP-AUTO.SLD")))
            (if (= slideNumber 1) (setq slideName (strcat $septicadPathDCL "LIMITING-FACTOR-HELP-FORCE.SLD")))

            ;;;;;--------------- in LSP after new_dialog
            ;;;--- First we need a slide name 
            (setq mySlideName slideName)

            ;;;--- Second we need the key to the image control 
            (setq myKey "image")

            ;;;--- Next we send the slide name and the key to the update function 
            (upDateImage mySlideName myKey)

                ;;;--- If an action event occurs, do this function
            (action_tile "cancel" "(done_dialog)")

                ;;;--- Display the dialog box
            (start_dialog)

                ;;;--- Unload the dialog box
            (unload_dialog dcl_id)

            (princ)

          )
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



            ;;;;;;;;;;Septic.DCL function;;;;;;;;;;
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            (defun ifForceAutoDCL ()

              (setq SYS_TYPE (car (nth (atoi (get_tile "SYSTEM")) sysList)))

                  ; if Auto-Step
              (if (= (get_tile "CALC_METHOD") "0")
                (progn
                  (mode_tile "LIM_SEP" 0)
                  (mode_tile "DEPTH_TOP" 0)
                  (mode_tile "DEPTH_BOT" 0)
                  (mode_tile "DEPTH_STEP" 1)
                  (mode_tile "STEP_SIZE" 1)
                )
              )

                  ; if Force-Step
              (if (= (get_tile "CALC_METHOD") "1")
                (progn
                  (mode_tile "LIM_SEP" 1)
                  (mode_tile "DEPTH_TOP" 1)
                  (mode_tile "DEPTH_BOT" 1)
                  (mode_tile "STEP_SIZE" 1)
                  (mode_tile "DEPTH_STEP" 0)

                  (if (AND (/= sys_type "STONE") (/= sys_type "4x8_CONCRETE_CLUSTER") (/= sys_type "8x4_CONCRETE_CLUSTER"))
                    (mode_tile "STEP_SIZE" 0)
                  )

                  (set_tile "SHELF_TOGGLE" "0")
                  (ifShelfDCL)

                )
              )
            )
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;;;;;;;;;Septic.DCL function;;;;;;;;;;
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            (defun ifShelfDCL ()

                  ; if NO Shelfs
              (if (= (get_tile "SHELF_TOGGLE") "0")
                (progn
                  (mode_tile "SHELF_SEQUENCE" 1)
                  (mode_tile "SHELF_SEP" 1)
                )
              )

                  ; if YES Shelfs
              (if (= (get_tile "SHELF_TOGGLE") "1")
                (progn
                  (mode_tile "SHELF_SEQUENCE" 0)
                  (mode_tile "SHELF_SEP" 0)
                  (set_tile "CALC_METHOD" "0")
                  (ifForceAutoDCL)
                )
              )
            )
            ;;;;;;;;;;;;;;;;




            (defun ifSlopeing3-DCL ()
              (if (= (get_tile "SLOPING3") "1")
                (progn
                  (set_tile "UP_SLOPE" "4")
                  (mode_tile "UP_SLOPE" 1)
                  (mode_tile "UP_WIDTH" 1)
                )
                (progn
                  (mode_tile "UP_SLOPE" 0)
                  (mode_tile "UP_WIDTH" 0)
                  (set_tile "UP_SLOPE" "1")
                )
              )


            )


            (defun ifUP_SLOPE-DCL ()

              (if (= (get_tile "UP_SLOPE") "4")
                (progn
                  (set_tile "UP_SLOPE" "4")
                  (set_tile "AUTODESIGN" "0")
                  (set_tile "CROWN" "0")
                  (set_tile "SLOPING" "0")
                  (set_tile "SLOPING3" "1")
                )
              )

              (if (/= (get_tile "SLOPING3") "1")
                (progn
                  (mode_tile "UP_SLOPE" 0)
                  (mode_tile "UP_WIDTH" 0)
                )
              )

            )


            (defun inputLast ()


               ; if this is true than this is a re-opened document
              (if (and (= u1 nil) (tblsearch "block" "page3_guide"))
                (progn
                  (mainDesignCreateTileList)
                  (xDataSystemStringLoad)
                )
              )

               ; if this is true than this is a re-opened document with some function called before hand.
              (if (and (= $septicadMainDesign-tile nil) (tblsearch "block" "page3_guide"))
                (progn
                  (mainDesignCreateTileList)
                  (xDataSystemStringLoad)
                )
              )

              (if $septicadMainDesign-tile
                (progn
                  (setq count 0)
                  (foreach tile $septicadMainDesign-tile
                    (progn
                      (set_tile tile (nth count $septicadMainDesign))
                      (setq count (+ count 1))
                    )
                  )

                  ; designer is not saved, this is a work around to autoload designer if your just messing around and do a design after continue design
                  (if $designer (set_tile "DESIGNER" $designer))

                  (ifSystemAutoFillDCL)
                  (ifShelfDCL)
                  (ifUP_SLOPE-DCL)
                  (ifSlopeing3-DCL)
                  (ifForceAutoDCL)

                )
                ;   (alert " Information not in Memory ")
              )

            )
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




            ;;;;; SEPTIC.DCL  function 
            (defun inputReset ()

              (set_tile "DESIGNER" "0")

              (set_tile "SYSTEM" $defaultSystem)

              (set_tile "U1" "-50")
              (set_tile "U2" "-50")
              (set_tile "D1" "-65")
              (set_tile "D2" "-70")

              (set_tile "FILL_ABOVE" "8")
              (set_tile "NUM_ROWS" "")
              (set_tile "NUM_UNITS" "")
              (set_tile "ROW_SEP" "")
              (set_tile "BEARING" "")

              (set_tile "DOWN_SLOPE" "1")
              (set_tile "UP_SLOPE" "1")
              (set_tile "LEFT_SLOPE" "1")
              (set_tile "RIGHT_SLOPE" "1")

              (set_tile "LEFT_WIDTH" "36")
              (set_tile "RIGHT_WIDTH" "36")
              (set_tile "UP_WIDTH" "36")
              (set_tile "DOWN_WIDTH" "36")

              (set_tile "CALC_METHOD" "0")
              (set_tile "STEP_SIZE" "")
              (set_tile "DEPTH_STEP" "")

              (set_tile "DEPTH_TOP" "")
              (set_tile "DEPTH_BOT" "")
              (set_tile "LIM_SEP" "")

              (set_tile "AUTODESIGN" "1")

              (set_tile "COMPOUND" "0")
              (set_tile "AUTO" "1")

              (set_tile "STONE_BELOW" "")
              (set_tile "STONE_BESIDE" "")

              (set_tile "SHELF_SEP" "")
              (set_tile "SHELF_TOGGLE" "0")
              (set_tile "SHELF_SEQUENCE" "")

              (ifSystemAutoFillDCL)
              (ifShelfDCL)
              (ifUP_SLOPE-DCL)
              (ifSlopeing3-DCL)
              (ifForceAutoDCL)

            )
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            (defun ifSystemAutoFillDCL (/ system text count)

              (setq system (car (nth (atoi (get_tile "SYSTEM")) sysList)))

               ; start if to end
              (if (= $septicadFullVersion 0)
                (progn
                  ;find pulldown list number for "ELJEN_INVERTED"
                  (set_tile "SYSTEM" (demoSystem-func))
                  (setq system "ELJEN_INVERTED")
                  (alert "DEMO MODE - Reverse Type B-Eljen Indrains AVAILABLE ONLY")
                )
              )

              (if (/= system "STONE")
                (progn
                  (if (= $defaultSpacing "0") (set_tile "ROW_SEP_TEXT" "Center to Center Row Spacing [in]-> "))
                  (if (= $defaultSpacing "1")
                    (progn
                      (if (= (substr system 5 15) "CONCRETE_TRENCH")
                        (set_tile "ROW_SEP_TEXT" "Edge to Edge Stone Spacing [in]-> ")
                        (set_tile "ROW_SEP_TEXT" "Edge to Edge Row Spacing [in]-> ")
                      )
                    )
                  )


                  (COND
                    ((= system "ENVIRO") (setq text "Total Length of System [ft]-> "))
                  )
                  (COND
                    ((= system "ADS_HI_CAP") (setq text "# of Units per Row [6'-4\"] long]-> "))
                  )
                  (COND
                    ((= system "ELJEN") (setq text "# of Units per Row [4' long]-> "))
                  )
                  (COND
                    ((= system "ELJEN_INVERTED") (setq text "# of Units per Row [3' long]-> "))
                  )
                  (COND
                    ((= system "ADS_BIO2") (setq text "# of Units per Row [7'-3\" long]-> "))
                  )
                  (COND
                    ((= system "QUICK4_EQ24") (setq text "# of Units per Row [4' long]-> "))
                  )
                  (COND
                    ((= system "QUICK4_HI_CAP") (setq text "# of Units per Row [4' long]-> "))
                  )
                  (COND
                    ((= system "4x8_CONCRETE_CLUSTER") (setq text "# of Units per Row [8' long]-> "))
                  )
                  (COND
                    ((= system "8x4_CONCRETE_CLUSTER") (setq text "# of Units per Row [4' long]-> "))
                  )
                  (COND
                    ((= system "4x8_CONCRETE_TRENCH") (setq text "# of Units per Row [8' long]-> "))
                  )
                  (COND
                    ((= system "8x4_CONCRETE_TRENCH") (setq text "# of Units per Row [4' long]-> "))
                  )
                  (COND
                    ((= system "MOUNDBUSTER") (setq text "Length of Perforated Pipes [ft] -> "))
                  )

                  (COND
                    ((= system "QUICK4_STD") (setq text "# of Units per Row [4' long]-> "))
                  )
                  (COND
                    ((= system "ARC18") (setq text "# of Units per Row [5' long]-> "))
                  )
                  (COND
                    ((= system "ARC36HC") (setq text "# of Units per Row [5' long]-> "))
                  )
                  (COND
                    ((= system "ARC36") (setq text "# of Units per Row [5' long]-> "))
                  )
                  (COND
                    ((= system "ADS_STD") (setq text "# of Units per Row [6'-4\"] long]-> "))
                  )

                  (COND
                    ((= system "TRENCH2") (setq text "Length of Trench [ft]-> "))
                  )
                  (COND
                    ((= system "TRENCH3") (setq text "Length of Trench [ft]-> "))
                  )

                  (COND
                    ((= system "INF_STD") (setq text "# of Units per Row [6'-3\"] long]-> "))
                  )
                  (COND
                    ((= system "INF_HI_CAP") (setq text "# of Units per Row [6'-3\"] long]-> "))
                  )
                  (COND
                    ((= system "INF_EQ24") (setq text "# of Units per Row [8'-4\"] long]-> "))
                  )


                  (COND
                    ((= system "ARC24") (setq text "# of Units per Row [5' long]-> "))
                  )

                  (COND
                    ((= system "Q4_PLUS_EQ_36_LP") (setq text "# of Units per Row [4' long]-> "))
                  )
                  (COND
                    ((= system "Q4_PLUS_HC") (setq text "# of Units per Row [4' long]-> "))
                  )
                  (COND
                    ((= system "Q4_PLUS_STD") (setq text "# of Units per Row [4' long]-> "))
                  )
                  (COND
                    ((= system "Q4_PLUS_STD_LP") (setq text "# of Units per Row [4' long]-> "))
                  )

                  (COND
                    ((= system "QUICK4_EQ24LP") (setq text "# of Units per Row [4' long]-> "))
                  )
                  (COND
                    ((= system "QUICK4_EQ36") (setq text "# of Units per Row [4' long]-> "))
                  )



                  (set_tile "NUM_UNITS_TEXT" text)

                  (set_tile "NUM_ROWS_TEXT" "Number of Rows -> ")

                  ;(set_tile "CALC_METHOD" "0")
                  (if (= system "4x8_CONCRETE_CLUSTER") (set_tile "CALC_METHOD" "1"))
                  (if (= system "8x4_CONCRETE_CLUSTER") (set_tile "CALC_METHOD" "1"))

                  (mode_tile "ROW_SEP" 0)
                  (mode_tile "STEP_SIZE" 0)

                  (ifForceAutoDCL)

                  (if (OR (= system "4x8_CONCRETE_CLUSTER") (= system "8x4_CONCRETE_CLUSTER"))
                    (progn
                      (mode_tile "ROW_SEP" 1)
                      (mode_tile "SHELF_TOGGLE" 1)
                    )
                    (mode_tile "SHELF_TOGGLE" 0)
                  )


                  (if (OR (= system "4x8_CONCRETE_CLUSTER") (= system "8x4_CONCRETE_CLUSTER")) (mode_tile "STONE_BELOW" 0))
                  (if (AND (/= system "4x8_CONCRETE_CLUSTER") (/= system "8x4_CONCRETE_CLUSTER"))
                    (progn
                      (set_tile "STONE_BELOW" "")
                      (mode_tile "STONE_BELOW" 1)
                    )
                  )

                  (if (OR (= system "4x8_CONCRETE_CLUSTER") (= system "8x4_CONCRETE_CLUSTER")) (mode_tile "STONE_BESIDE" 0))
                  (if (AND (/= system "4x8_CONCRETE_CLUSTER") (/= system "8x4_CONCRETE_CLUSTER"))
                    (progn
                      (set_tile "STONE_BESIDE" "")
                      (mode_tile "STONE_BESIDE" 1)
                    )
                  )


                  (if (OR (= system "4x8_CONCRETE_CLUSTER") (= system "8x4_CONCRETE_CLUSTER")) (mode_tile "STEP_SIZE" 1))

                  (set_tile "DEPTH_STEP_TEXT" "Bottom of Row #1 Elevation (e.g. -55\")->")

                  (ifShelfDCL)
                )
              )


              (if (= system "STONE")
                (progn
                  (set_tile "NUM_UNITS_TEXT" "Length of Stone Bed [ft]-> ")
                  (set_tile "NUM_ROWS_TEXT" "Width of Stone Bed [ft]-> ")
                  (set_tile "CALC_METHOD" "1")
                  (set_tile "STEP_SIZE" "")
                  (mode_tile "ROW_SEP" 1)
                  (mode_tile "SHELF_TOGGLE" 1)
                  (ifForceAutoDCL)
                  (mode_tile "STEP_SIZE" 1)
                  (mode_tile "STONE_BELOW" 1)

                  (set_tile "DEPTH_STEP_TEXT" "Bottom of Stone Elevation (e.g. -55\")->")
                )
              )

              (princ)
            )
            ; end ifSystemAutoFillDCL
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;











            (defun mainDesignCreateTileList ()
              (setq $septicadMainDesign-tile (list "ROTATESYSTEM" "SYSTEM" "NUM_ROWS" "NUM_UNITS" "ROW_SEP" "U1" "U2" "D1" "D2" "COMPOUND" "FILL_ABOVE" "BEARING" "CALC_METHOD" "DEPTH_TOP" "DEPTH_BOT" "STEP_SIZE" "LIM_SEP" "DEPTH_STEP" "STONE_BESIDE" "STONE_BELOW" "SHELF_SEP" "SHELF_TOGGLE" "SHELF_SEQUENCE" "DOWN_SLOPE" "UP_SLOPE" "LEFT_SLOPE" "RIGHT_SLOPE" "LEFT_WIDTH" "RIGHT_WIDTH" "UP_WIDTH" "DOWN_WIDTH" "AUTO" "LEFT" "RIGHT" "AUTODESIGN" "CROWN" "SLOPING" "SLOPING3"))
            )

            (defun mainDesignSaveVars ()

              (mainDesignCreateTileList)

              (setq $septicadMainDesign (list))

              (foreach item $septicadMainDesign-tile
                (setq $septicadMainDesign (append $septicadMainDesign (list (get_tile item))))
              )

              (setq %RotateSystem (atoi (get_tile "ROTATESYSTEM")))


              (setq $stoneChamber nil)
              (setq stoneBeside 0)
              (setq stoneBelow 0)
              (setq row_sep 0)
              (setq shelf_sep 0)
              (setq shelfList nil)
              (setq elevationMethod "AUTO")
              (setq isShelf nil)
              (setq row_sep-DCL "")

              (setq NUM_ROWS (ATOI (get_tile "NUM_ROWS")))
              (setq NUM_UNITS (ATOF (get_tile "NUM_UNITS")))

              (setq SYS_TYPE (car (nth (atoi (get_tile "SYSTEM")) sysList)))

              (setq U1 (ATOI (get_tile "U1")))
              (setq U2 (ATOI (get_tile "U2")))
              (setq D1 (ATOI (get_tile "D1")))
              (setq D2 (ATOI (get_tile "D2")))

              (setq FILL_ABOVE (ATOI (get_tile "FILL_ABOVE")))
              (setq BEARING (ATOI (get_tile "BEARING")))

                 ;DESIGN TYPE
              (setq STEP_TYPE (get_tile "CALC_METHOD"))
              (if (= STEP_TYPE "0") (setq step_type "AUTO"))
              (if (= STEP_TYPE "1") (setq step_type "FORCE"))
              (if (= STEP_TYPE "1") (setq elevationMethod "FORCE"))

                 ;FOR AUTO-DESIGN
              (setq DEPTH_TOP (ATOF (get_tile "DEPTH_TOP")))
              (setq DEPTH_BOT (ATOF (get_tile "DEPTH_BOT")))

                 ;FOR FORCE STEP
              (setq STEP_SIZE (ATOF (get_tile "STEP_SIZE")))
              (setq DEPTH_STEP (ATOF (get_tile "DEPTH_STEP")))


              (if (= (substr sys_type 5 16) "CONCRETE_CLUSTER")
                (progn
                  (setq stoneBeside (atoi (get_tile "STONE_BESIDE")))
                  (setq stoneBelow (atoi (get_tile "STONE_BELOW")))
                  (setq row_sep 0.0)
                )
              )




              (setq SHELF_SEP (ATOI (get_tile "SHELF_SEP")))

              (setq shelfToggle (get_tile "SHELF_TOGGLE"))

                 ; IF NO SHELF then Set SHELF_SEP to 0
              (if (= shelfToggle "0") (setq shelf_sep 0))


              (if (and (/= (substr sys_type 5 16) "CONCRETE_CLUSTER") (/= sys_type "STONE"))
                (progn
                  ;- getting shelf list -;
                  ; if shelfs
                  (if (= shelfToggle "1")
                    (progn
                      ;;-- Get data from DCL
                      (setq readList (get_tile "SHELF_SEQUENCE"))

                      ;;-- If begins with white space than remove it
                      (setq flag nil)
                      (while (= flag nil)
                        (if (= (substr readlist 1 1) " ")
                          (setq readList (substr readlist 2 (- (strlen readList) 1)))
                          ; else
                          (setq flag 1)
                        )
                      )

                      ;;-- Turn string into list of #'s
                      (setq count 1)
                      (setq retList (list))
                      (while (setq item (read readlist))
                        (setq retList (append retList (list item)))
                        (while
                          (and
                            (/= " " (substr readlist count 1))
                            (/= "" (substr readlist count 1))
                          )
                          (setq count (1+ count))
                        )
                        (setq readlist (substr readlist count))
                      )

                      (setq shelfList retList)
                    )
                  )
                  ; if NO shelfs then set variable to 1 1 1 1 1 1 etc
                  (if (= shelfToggle "0")
                    (progn
                      (setq count 0)
                      (setq shelfList (list))
                      (while (< count num_rows)
                        (setq shelfList (append shelfList (list 1)))
                        (setq count (1+ count))
                      )
                    )
                  )

                  ; check to see if there are shelfs or not

                  (foreach item shelfList
                    (if (> item 1) (setq isShelf "YES"))
                  )

                )
              )



              (if (and (= shelfToggle "0") (= step_type "FORCE"))
                (setq elevationMethod "FORCE")
              )

                ;- getting shelf list -;

              (setq LIM_SEP (ATOI (get_tile "LIM_SEP")))

              (setq DOWN_SLOPE (get_tile "DOWN_SLOPE"))
              (setq UP_SLOPE (get_tile "UP_SLOPE"))
              (setq LEFT_SLOPE (get_tile "LEFT_SLOPE"))
              (setq RIGHT_SLOPE (get_tile "RIGHT_SLOPE"))

              (setq LEFT_SHOULDER (atof (get_tile "LEFT_WIDTH")))
              (setq RIGHT_SHOULDER (atof (get_tile "RIGHT_WIDTH")))
              (setq UP_SHOULDER (atof (get_tile "UP_WIDTH")))
              (setq DOWN_SHOULDER (atof (get_tile "DOWN_WIDTH")))

              (if (= (atoi (get_tile "AUTO")) 1) (setq crossSection "AUTO"))
              (if (= (atoi (get_tile "LEFT")) 1) (setq crossSection "LEFT"))
              (if (= (atoi (get_tile "RIGHT")) 1) (setq crossSection "RIGHT"))

              (if (= (atoi (get_tile "AUTODESIGN")) 1) (setq slopeType "AUTODESIGN"))
              (if (= (atoi (get_tile "CROWN")) 1) (setq slopeType "CROWN"))
              (if (= (atoi (get_tile "SLOPING")) 1) (setq slopeType "SLOPING"))
              (if (= (atoi (get_tile "SLOPING3")) 1) (setq slopeType "SLOPING3"))

              (if (= (get_tile "COMPOUND") "1") (setq doCompound "YES"))
              (if (= (get_tile "COMPOUND") "0") (setq doCompound "NO"))




              (RUN-DIM_SYSTEM)

              (setq row_sep-DCL (get_tile "ROW_SEP"))

              (if (and (/= (substr sys_type 5 16) "CONCRETE_CLUSTER") (/= sys_type "STONE"))
                (progn
                  (if (= $defaultSpacing "0")
                    (progn
                      (setq row_sep (- (ATOI row_sep-DCL) chamber_W))
                    )
                  )
                  (if (= $defaultSpacing "1")
                    (progn
                      (setq row_sep (ATOI row_sep-DCL))
                    )
                  )

                )
              )
              (RUN-DIM_SYSTEM)



              (setq errorStr (mainDesignErrorCheck))

              ;;;;;----- START If Errors are Found - Alert User with info
              (if (/= errorStr "")
                (progn
                  (Alert (strcat "INPUT IS INVALID - The following parameters need to be modified:\n" errorStr))
                )
              )
              ;;;;;----- END If Errors are Found - Alert User with info

              ;;;;;----- START IF No Error are Found - Set some variables and Send to (SepticPreview)
              (if (= errorStr "")
                (progn

                  ; end SEPTIC.DCL
                  (done_dialog)

                  ; Run system dimension function
                  (RUN-DIM_SYSTEM)

                  (if (= crossSection "LEFT")
                    (progn
                      (setq ELV_TOP U1)
                      (setq ELV_BOT D1)
                    )
                  )

                  (if (= crossSection "RIGHT")
                    (progn
                      (setq ELV_TOP U2)
                      (setq ELV_BOT D2)
                    )
                  )

                  ; START if AUTO
                  (if (= crossSection "AUTO")
                    (progn
                      (if (= u1 u2)
                        (progn
                          (setq ELV_TOP U1)
                          (setq ELV_BOT D1)
                          (princ "\nLeft-side Cross Section [default]- Up-slope Corners at Same Elevation")
                          (setq crossSection "LEFT")
                        )
                      )

                      (if (> u1 u2)
                        (progn
                          (setq ELV_TOP U1)
                          (setq ELV_BOT D1)
                          (princ "\nLeft-side Cross Section - Up-slope Left Corner is Highest")
                          (setq crossSection "LEFT")
                        )
                      )

                      (if (< u1 u2)
                        (progn
                          (setq ELV_TOP U2)
                          (setq ELV_BOT D2)
                          (princ "\nRight-side Cross Section - Up-slope Right Corner is Highest")
                          (setq crossSection "RIGHT")
                        )
                      )
                    )
                  )
                  ; END if AUTO

                  (SETQ SLOPE_TOP (LIST 0.0 ELV_TOP))
                  (SETQ SLOPE_BOT (LIST SYS_W ELV_BOT))

                  ; Call Step_Cal for legacy function v1
                  (STEP_CAL)
                )
              )
              ;;;;;----- END IF No Error are Found - Set some variables and Send to (SepticPreview)


                ;  IMPORTS THE CUSTOM SYSTEM NOTES FOR THE SYSTEM SELECTED
                ; CREATES GLOBAL VARAIBLE
              (setq $customSysList (getCustomSystemList SYS_TYPE))


            )           ; end mainDesignSaveVars




            ; START - function create list of error strings -  errorStr - if errors are found
            (defun mainDesignErrorCheck (/ errorStr)

              (SETQ errorStr "")

              (if (> BEARING 360) (setq errorStr (strcat errorStr "Bearing must be between 0 and 360.\n")))
              (if (< BEARING 0) (setq errorStr (strcat errorStr "Bearing must be between 0 and 360.\n")))

               ; if center to center spacing -> $defaultSpacing "0"
               ; if edge to edge spacing -> $defaultSpacing "1"
              (if
                (and (= $defaultSpacing "0")
                     (/= (substr sys_type 5 16) "CONCRETE_CLUSTER")
                     (/= sys_type "STONE")
                )
                (progn
                  (if (< (ATOI (get_tile "ROW_SEP")) chamber_w)
                    (setq errorStr (strcat errorStr "When using Center-to-Center Row Spacing - Row Separation [in inches] must be greater than chamber width\n"))
                  )
                )
              )


              (IF (OR (= sys_type "8x4_CONCRETE_CLUSTER") (= sys_type "4x8_CONCRETE_CLUSTER"))
                (progn
                  ; function in DCL tile for "NUM_ROWS" makes sure width is >=10 and multiples of 5
                  (if (= num_units 0) (setq errorStr (strcat errorStr "The number of ROWS must be greater than zero\n")))
                  (if (= num_rows 0) (setq errorStr (strcat errorStr "The number of UNITS must be greater than zero\n")))
                  (if (= (get_tile "DEPTH_STEP") "") (setq errorStr (strcat errorStr "The BASE OF STONE must be given\n")))
                  (if (= (get_tile "STONE_BESIDE") "") (setq errorStr (strcat errorStr "The width of STONE BESIDE the chambers must be given\n")))
                  (if (= (get_tile "STONE_BELOW") "") (setq errorStr (strcat errorStr "The thickness of STONE BELOW the chambers must be given\n")))
                )
              )

              (IF (= sys_type "STONE")
                (progn
                  ; function in DCL tile for "NUM_ROWS" makes sure width is >=10 and multiples of 5
                  (if (= num_units 0) (setq errorStr (strcat errorStr "A stone bed LENGTH of 0 feet is invalid\n")))
                  (if (= (get_tile "DEPTH_STEP") "") (setq errorStr (strcat errorStr "The BASE OF STONE must be given\n")))
                  (if (/= (rem NUM_ROWS 5) 0) (setq errorStr (strcat errorStr "Width of Stone Bed must be in 5' increments [e.g. 10, 25, 45]\n")))
                  (if (= NUM_ROWS 5) (setq errorStr (strcat errorStr "Stone Bed must be at least 10' wide\n")))
                )
              )


               ; if a Chamber style system
              (if (and (/= (substr sys_type 5 16) "CONCRETE_CLUSTER") (/= sys_type "STONE"))
                (progn

                  (if (= num_units 0) (setq errorStr (strcat errorStr "The number of ROWS must be greater than zero\n")))
                  (if (= num_rows 0) (setq errorStr (strcat errorStr "The number of UNITS must be greater than zero\n")))
                  (if (= (GET_TILE "row_sep") "") (setq errorStr (strcat errorStr "The row SEPARATION DISTANCE must be given\n")))

                  ; check AUTO-STEP
                  (if (= step_type "AUTO")
                    (progn
                      (if (= (get_tile "DEPTH_TOP") "") (setq errorStr (strcat errorStr "The UP-SLOPE ELEVATION OF THE LIMITING FACTOR must be given\n")))
                      (if (= (get_tile "DEPTH_BOT") "") (setq errorStr (strcat errorStr "The DOWN-SLOPE ELEVATION OF THE LIMITING FACTOR must be given\n")))
                      (if (= (get_tile "LIM_SEP") "") (setq errorStr (strcat errorStr "A SEPARATION FROM LIMITING FACTOR must be given (i.e. 12, 18, 24)\n")))
                    )
                  )

                  ; check FORCE-STEP
                  (if (= step_type "FORCE")
                    (progn
                      (if (= (get_tile "DEPTH_STEP") "") (setq errorStr (strcat errorStr "The BASE OF CHAMBER elevation must be given\n")))
                      (if (< STEP_SIZE 0) (setq errorStr (strcat errorStr "A positive + ELEVATION DROP BETWEEN ROWS must be entered\n")))
                    )
                  )

                  ; check SHELF
                  (if (= shelfToggle "1")
                    (progn

                      ;;-- determine # of chambers entered
                      (setq count 0)
                      (foreach item shelfList
                        (setq count (+ count item))
                      )

                      ;;-- COMPARE input to NUM_ROWS and alert user if input incorrect!
                      ; if BAD
                      (if (/= count num_rows)
                        (setq errorStr (strcat errorStr "The total # of Rows in the Shelf Sequence [# Rows = " (rtos count 2 0) "] is not consistent with the system design [# Rows = " (rtos num_rows 2 0) "]\n"))
                      )

                    )
                  )

                )
              )
               ; end if chamber style system

              (if (= (get_tile "U1") "") (setq errorStr (strcat errorStr "An up-slope left CORNER ELEVATION number be given\n")))
              (if (= (get_tile "U2") "") (setq errorStr (strcat errorStr "An up-slope right CORNER ELEVATION number be given\n")))
              (if (= (get_tile "D1") "") (setq errorStr (strcat errorStr "An down-slope left CORNER ELEVATION number be given\n")))
              (if (= (get_tile "D2") "") (setq errorStr (strcat errorStr "An down-slope right CORNER ELEVATION number be given\n")))
              (if (= (get_tile "LEFT_WIDTH") "") (setq errorStr (strcat errorStr "A left SHOULDER WIDTH number be given\n")))
              (if (= (get_tile "RIGHT_WIDTH") "") (setq errorStr (strcat errorStr "A right SHOULDER WIDTH number be given\n")))
              (if (= (get_tile "UP_WIDTH") "") (setq errorStr (strcat errorStr "A up-slope SHOULDER WIDTH number be given\n")))
              (if (= (get_tile "DOWN_WIDTH") "") (setq errorStr (strcat errorStr "A down-slope SHOULDER WIDTH number be given\n")))



              errorStr
            )
            ; end mainDesignErrorCheck ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;








  ;;;;;;;;;; Start Subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
            (defun showGradeInput (tile1 key1 key2)
            (if (> (atof (get_tile tile1)) 0.0)  ; if tile has data then do this
              (progn
                (mode_tile key1 0)
                (mode_tile key2 0)
              )  ; end progn
            )
)
              
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            (defun resetGrades ()

              (createTileListsCompoundGrade)

              (foreach item tileListLength
                (set_tile item "")
              )
              (foreach item tileListElevation
                (set_tile item "")
              )

            )
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            (defun refillGrades ()

              (createTileListsCompoundGrade)

              (setq count 0)
              (if (/= gradeLength nil)
                (progn
                  (while (/= count 16)
                    (if (/= (nth count gradeLength) "")
                      (progn
                        (set_tile (nth count tileListLength) (nth count gradeLength))
                        (set_tile (nth count tileListElevation) (nth count gradeElevation))
                      )
                    )

                    (setq count (+ count 1))
                  )


                )
              )

              ; returns message if no data in memory
              (if (= gradeLength nil) (Alert "No data in memory"))


            )
            ; end refill grade 

            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            (defun createTileListsCompoundGrade ()

              (setq tileListLength
                    (list
                      "U1U2L" "U1U1L" "D1D1L" "D1D2L"
                      "U2U2L" "U2U1L" "D2D1L" "D2D2L"
                      "U1L2L" "U1L1L" "U2R1L" "U2R2L"
                      "D1L2L" "D1L1L" "D2R1L" "D2R2L"
                    )
              )

              (setq tileListElevation
                    (list
                      "U1U2E" "U1U1E" "D1D1E" "D1D2E"
                      "U2U2E" "U2U1E" "D2D1E" "D2D2E"
                      "U1L2E" "U1L1E" "U2R1E" "U2R2E"
                      "D1L2E" "D1L1E" "D2R1E" "D2R2E"
                    )
              )

            )
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  
  
              
                  (defun saveNullGrades ()

                    (createTileListsCompoundGrade)

                    (setq gradeLength (list))
                    (foreach item tileListLength
                      (setq gradeLength
                            (append
                              gradeLength
                              (list "")
                            )
                      )
                    )        ; end foreach


                    (setq gradeElevation (list))
                    (foreach item tileListElevation
                      (setq gradeElevation
                            (append
                              gradeElevation
                              (list "")
                            )
                      )
                    )        ; end foreach

                  )          ; end saveNullGrades

        
        


                  (defun saveGrades ()

                    (createTileListsCompoundGrade)

                    (setq gradeLength (list))
                    (foreach item tileListLength
                      (setq gradeLength
                            (append
                              gradeLength
                              (list (get_tile item))
                            )
                      )
                    )        ; end foreach

                    (setq gradeElevation (list))
                    (foreach item tileListElevation
                      (setq gradeElevation
                            (append
                              gradeElevation
                              (list (get_tile item))
                            )
                      )
                    )        ; end foreach

                  )          ; end saveGrades




      ;; this function is called by maindesign and compound grades
              (defun convertGrades ()

                (setq U1U1 nil U1U2 nil U1L1 nil U1L2 nil D1D1 nil D1D2 nil D1L1 nil D1L2 nil)
                (setq U2U1 nil U2U2 nil U2R1 nil U2R2 nil D2D1 nil D2D2 nil D2R1 nil D2R2 nil)

                (if (/= (atof (nth 0 gradeLength)) 0.0)
                  (setq U1U2 (list (* (atof  (nth 0 gradeLength)) -12.0) (atof (nth 0 gradeElevation))))
                )        ; end if

                (if (/= (atof (nth 1 gradeLength)) 0.0)
                  (setq U1U1 (list (* (atof (nth 1 gradeLength)) -12.0) (atof (nth 1 gradeElevation))))
                )        ; end if

                (if (/= (atof (nth 2 gradeLength)) 0.0)
                  (setq D1D1 (list (+ (* (atof (nth 2 gradeLength)) 12.0) sys_w) (atof (nth 2 gradeElevation))))
                )        ; end if

                (if (/= (atof (nth 3 gradeLength)) 0.0)
                  (setq D1D2 (list (+ (* (atof (nth 3 gradeLength)) 12.0) sys_w) (atof (nth 3 gradeElevation))))
                )        ; end if


                (if (/= (atof (nth 4 gradeLength)) 0.0)
                  (setq U2U2 (list (* (atof (nth 4 gradeLength)) -12.0) (atof (nth 4 gradeElevation))))
                )        ; end if

                (if (/= (atof (nth 5 gradeLength)) 0.0)
                  (setq U2U1 (list (* (atof (nth 5 gradeLength)) -12.0) (atof (nth 5 gradeElevation))))
                )        ; end if

                (if (/= (atof (nth 6 gradeLength)) 0.0)
                  (setq D2D1 (list (+ (* (atof (nth 6 gradeLength)) 12.0) sys_w) (atof (nth 6 gradeElevation))))
                )        ; end if

                (if (/= (atof (nth 7 gradeLength)) 0.0)
                  (setq D2D2 (list (+ (* (atof (nth 7 gradeLength)) 12.0) sys_w) (atof (nth 7 gradeElevation))))
                )        ; end if


                (if (/= (atof (nth 8 gradeLength)) 0.0)
                  (setq U1L2 (list (* (atof (nth 8 gradeLength)) -12.0) (atof (nth 8 gradeElevation))))
                )        ; end if

                (if (/= (atof (nth 9 gradeLength)) 0.0)
                  (setq U1L1 (list (* (atof (nth 9 gradeLength)) -12.0) (atof (nth 9 gradeElevation))))
                )        ; end if

                (if (/= (atof (nth 10 gradeLength)) 0.0)
                  (setq U2R1 (list (+ (* (atof (nth 10 gradeLength)) 12.0) sys_L) (atof (nth 10 gradeElevation))))
                )        ; end if

                (if (/= (atof (nth 11 gradeLength)) 0.0)
                  (setq U2R2 (list (+ (* (atof (nth 11 gradeLength)) 12.0) sys_L) (atof (nth 11 gradeElevation))))
                )        ; end if

                (if (/= (atof (nth 12 gradeLength)) 0.0)
                  (setq D1L2 (list (* (atof (nth 12 gradeLength)) -12.0) (atof (nth 12 gradeElevation))))
                )        ; end if

                (if (/= (atof (nth 13 gradeLength)) 0.0)
                  (setq D1L1 (list (* (atof (nth 13 gradeLength)) -12.0) (atof (nth 13 gradeElevation))))
                )        ; end if

                (if (/= (atof (nth 14 gradeLength)) 0.0)
                  (setq D2R1 (list (+ (* (atof (nth 14 gradeLength)) 12.0) sys_L) (atof (nth 14 gradeElevation))))
                )        ; end if

                (if (/= (atof (nth 15 gradeLength)) 0.0)
                  (setq D2R2 (list (+ (* (atof (nth 15 gradeLength)) 12.0) sys_L) (atof (nth 15 gradeElevation))))
                )        ; end if

                (if (= D2D1 nil) (setq D2D1 (polar (list sys_W D2) (angle (list 0.0 U2) (list sys_w D2)) 36.0)))
                (if (= D2D2 nil) (setq D2D2 (polar D2D1 (angle (list sys_w D2) D2D1) 36.0)))


                (if (= D2R1 nil) (setq D2R1 (polar (list sys_L D2) (angle (list 0.0 D1) (list sys_L D2)) 36.0)))
                (if (= D2R2 nil) (setq D2R2 (polar D2R1 (angle (list sys_L D2) D2R1) 36.0)))

                (if (= U2U1 nil) (setq U2U1 (polar (list 0.0 U2) (angle (list sys_W D2) (list 0.0 U2)) 36.0)))
                (if (= U2U2 nil) (setq U2U2 (polar U2U1 (angle (list 0.0 U2) U2U1) 36.0)))

                (if (= U2R1 nil) (setq U2R1 (polar (list sys_L U2) (angle (list 0.0 U1) (list sys_L U2)) 36.0)))
                (if (= U2R2 nil) (setq U2R2 (polar U2R1 (angle (list sys_L U2) U2R1) 36.0)))

                (if (= U1U1 nil) (setq U1U1 (polar (list 0.0 U1) (angle (list sys_W D1) (list 0.0 U1)) 36.0)))
                (if (= U1U2 nil) (setq U1U2 (polar U1U1 (angle (list 0.0 U1) U1U1) 36.0)))

                (if (= U1L1 nil) (setq U1L1 (polar (list 0.0 U1) (angle (list sys_L U2) (list 0.0 U1)) 36.0)))
                (if (= U1L2 nil) (setq U1L2 (polar U1L1 (angle (list 0.0 U1) U1L1) 36.0)))

                (if (= D1D1 nil) (setq D1D1 (polar (list sys_W D1) (angle (list 0.0 U1) (list sys_w D1)) 36.0)))
                (if (= D1D2 nil) (setq D1D2 (polar D1D1 (angle (list sys_w D1) D1D1) 36.0)))

                (if (= D1L1 nil) (setq D1L1 (polar (list 0.0 D1) (angle (list sys_L D2) (list 0.0 D1)) 36.0)))
                (if (= D1L2 nil) (setq D1L2 (polar D1L1 (angle (list 0.0 D1) D1L1) 36.0)))

        )

        
            (defun compoundGrade (/ ddiag mySlideName myKey)


              (setq ddiag nil)


                   ;;;--- Load the DCL file
              (setq dcl_id (load_dialog (strcat $septicadPathDCL "compoundGrade.dcl")))

                  ;;;--- See if the dialog box is already loaded
              (if (not (new_dialog "compoundGrade" dcl_id))
                (progn
                  (alert "The compoundGrade.DCL file was not found!")
                  (exit)
                )
              )

              ;;;;;--------------- in LSP after new_dialog
              ;;;--- First we need a slide name 
              (setq mySlideName (strcat $septicadPathDCL "field_compound.sld"))

              ;;;--- Second we need the key to the image control 
              (setq myKey "field")

              ;;;--- Next we send the slide name and the key to the update function 
              (upDateImage mySlideName myKey)


                  ;;;--- If an action event occurs, do this function
              (action_tile "cancel" "(setq ddiag 1)(done_dialog)")
              (action_tile "okay" "(saveGrades)(setq ddiag 2)(done_dialog)")
              (action_tile "help" "(mainHelp)")
              (action_tile "reset" "(resetGrades)")

                  ; if existing data is present refill automatically
              (if (/= gradeLength nil) (refillGrades))

                  ;;;--- Display the dialog box
              (start_dialog)

                  ;;;--- Unload the dialog box
              (unload_dialog dcl_id)


                  ;;;--- If the cancel button was pressed
              (if (= ddiag 1)
                (progn
                  ;;;--- Set an exit message
                  (Print "...compoundGrade Skipped/Cancelled - Design will be completed with 4 corner elevations...")
                  (convertGrades)
                )
              )

                  ;;;--- If the "Okay" button was pressed
              (if (= ddiag 2)
                (convertGrades)
              )
              (princ)


            )
            ; end compoundGrade


;;;;;;;;;; End Subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	;Sets command echo off
  (SETVAR "CMDECHO" 0)

  (setq preSepticadErrorFunction *error*)

  (PRINC (strcat "\n" $septicadVersion " Starting"))

  (getUserVars)

  (setq ddiag nil)

  (toggle-OSNAP-Vars-OFF)

	; Set orthomode to OFF
  (setq ortho_DEF (getvar "ORTHOMODE"))
  (setvar "ORTHOMODE" 0)

  (setq ANGDIR_DEF (GETVAR "ANGDIR"))
  (setvar "ANGDIR" 0)

  (SETQ ANGBASE_DEF (GETVAR "ANGBASE"))
  (setvar "ANGBASE" 0)

	; set so block inserts with attributes are forced to the command line 
  (setq ATTDIA_DEFAULT (getvar "ATTDIA"))
  (setvar "ATTDIA" 0)

	; sets undo spot
  (command "UNDO" "BE")

	; LISP variables are preserved from drawing to drawing in each session
  (setvar "LispInit" 0)

  (COMMAND "LAYOUT" "S" "MODEL")

	;;;-- list of chamber elevations - must be nil for redesigns and going back and forth between shelfing and not
  (setq shelfElvList nil
        topShelfWidth 0
  )


  (setq slopeList (list "3:1 Grade" "4:1 Grade" "5:1 Grade" "6:1 Grade" "3% Grade - Tapered into Slope"))
  (setq calcList (list "Auto-Step" "Force-Step"))
  (setq shelf-selection-List (list "No Shelfs, Step each Row" "Create Shelfs"))

  (createSysList)

  (setq tempList (list))
  (foreach item sysList
    (setq tempList (append tempList (list (cdr item))))
  )
		; end foreach



  (if (= (strcase (getvar "PRODUCT")) "AUTOCAD")
    (setq dcl_id (load_dialog (strcat $septicadPathDCL "SEPTIC-ACAD.dcl")))
    (setq dcl_id (load_dialog (strcat $septicadPathDCL "SEPTIC-BCAD.dcl")))
  )

  ;;;--- Load the dialog definition if it is not already loaded
  (if (not (new_dialog "SEPTIC" dcl_id))
    (progn
      (alert "The DCL file could not be loaded!")
      (exit)
    )
  )



  (start_list "SHELF_TOGGLE" 3)
  (mapcar 'add_list shelf-selection-List)
  (end_list)

  (start_list "CALC_METHOD" 3)
  (mapcar 'add_list calcList)
  (end_list)

  (start_list "SYSTEM" 3)
  (mapcar 'add_list tempList)
  (end_list)

  (start_list "DOWN_SLOPE" 3)
  (mapcar 'add_list slopeList)
  (end_list)

  (start_list "UP_SLOPE" 3)
  (mapcar 'add_list slopeList)
  (end_list)

  (start_list "LEFT_SLOPE" 3)
  (mapcar 'add_list slopeList)
  (end_list)

  (start_list "RIGHT_SLOPE" 3)
  (mapcar 'add_list slopeList)
  (end_list)


	;;;;;--------------- in LSP after new_dialog
	;;;--- First we need a slide name 
  (setq mySlideName (strcat $septicadPathDCL "field_small.sld"))

	;;;--- Second we need the key to the image control 
  (setq myKey "field")

	;;;--- Next we send the slide name and the key to the update function 
  (upDateImage mySlideName myKey)


  (updateImage (strcat $septicadPathDCL "MAIN-WINDOW.sld") "titlebar")


  ;;;--- If an action event occurs, do this function
  (action_tile "designonly" "(setq ddiag 2)(mainDesignSaveVars)")
  (action_tile "cancel" "(setq ddiag 1)(done_dialog)")
  (action_tile "reset" "(inputReset)")
  (action_tile "help" "(mainHelp)")
  (action_tile "elv_help" "(mainHelpElevations)")
  (action_tile "autostep_help" "(lf-help 0)")
  (action_tile "forcestep_help" "(lf-help 1)")
  (action_tile "SHELF_HELP" "(shelf-help)")


  (inputLast)

   ; IF demo set to Eljen Inverted
  (if (= $septicadFullVersion 0)
    (progn
      (set_tile "SYSTEM" (demoSystem-func))
      (ifSystemAutoFillDCL)
    )
    (progn
      (if (AND (= U1 nil) (= slopeType nil))
        (progn
          (set_tile "SYSTEM" $defaultSystem)
          (ifSystemAutoFillDCL)
        )
      )
    )
  )


  ;;;--- Display the dialog box
  (start_dialog)

  ;;;--- Unload the dialog box
  (unload_dialog dcl_id)

  ;;;--- If the user pressed the Cancel button
  (if (= ddiag 1)
    (PRINC "Cancelled Design.")
  )

  (if (= ddiag 2)
    (progn
      (septicPreview)
      (if (and (= ddiag 2) (= previewDesign 1))
        (progn

          (if (= doCompound "YES") (compoundGrade))
          (if (= doCompound "NO")
            (progn
              ; creates empty list
              (saveNullGrades)
              ; creates all elevation points
              (convertGrades)
            )
          )
          (setScales)
          ; Starts Septic Design Application

          (main_drawing)

          (if (and (/= (substr sys_type 5 16) "CONCRETE_CLUSTER") (/= sys_type "STONE"))
            (progn
              ; arrow and text sizes set in (PRO_LABEL)
              (PRO_LABEL)
            )
            (progn
              (if (/= (substr sys_type 5 16) "CONCRETE_CLUSTER")
                (progn
                  ;;;--- SET DEFAULT ARROWHEAD/TEXT SIZES
                  (SETQ DIMASZ_DEF (GETVAR "DIMASZ"))
                  (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
                  (SETVAR "DIMTXT" profileTextSize)
                  (SETVAR "DIMASZ" (* profileTextSize scaleArrowSize))

                  (PRO_LABEL_FILL_ABOVE)

                  (SETVAR "DIMASZ" DIMASZ_DEF)
                  (SETVAR "DIMTXT" DIMTXT_DEF)
                )
              )
            )
          )



          ; Insert the boxes in mspace and scale vports in pspace 
          (boxModel)

          (SETVAR "CLAYER" "SEPTICAD")
          (COMMAND "LAYOUT" "S" "MODEL")

          (proBearing)

          (command "zoom" "e")
          ; ends undo Groups 
          (command "UNDO" "E")

          (PRINC "\n\nSeptiCAD Design ONLY is Complete...\n")
          (PRINC "Use the 'Continue Design' tool to insert swings ties, design notes and complete the HHE-200 Form.\n")
        )
      )


    )
  )

; reset to variable for inserts with attributes being forced to the command line 
  (setvar "ATTDIA" ATTDIA_DEFAULT)
  (setvar "ORTHOMODE" ortho_DEF)

  (setvar "ANGDIR" ANGDIR_DEF)
  (SETVAR "ANGBASE" ANGBASE_DEF)

  (toggle-OSNAP-Vars-RESET)

  (setq *error* preSepticadErrorFunction)


  (if (and (/= ddiag 1) (= previewDesign 0))
    (c:septic)
  )



  (if (= $ALLCAPS "YES")
    (ALLCAPS (ssget "X" '((0 . "TEXT,MTEXT") (8 . "SEPTICAD"))))
  )

  ;;;--- Suppress the last echo for a clean exit
  (princ)




)
;;;;;;;;;;;;;;;;;;;;;;;;



(defun demoSystem-func (/ demoList demoSystem)
        ; this is used to set eljen inverted as the only system that can be used
  (setq demoSystem nil count 0 demoList (list))
  (foreach item sysList (setq demoList (append demoList (list (car item)))))
  (foreach item demoList
    (if (= item "ELJEN_INVERTED") (setq demoSystem (rtos count 2 0)))
    (setq count (1+ count))
  )
  demoSystem
)



(defun checkSeptic ()
	; Set an Alert string up
  (setq alertStr "")
  (setq valid "NO")

  (if (= $septicadFullVersion 0) (setq valid "YES"))
  (if (= $septicadFullVersion 1) (setq valid "YES"))

  (if (= num_rows 0)
    (progn
      (setq valid "NO")
      (setq alertStr (strcat alertStr " Number of Rows or Width of System is not valid.\n"))
    )       ; progn
  )         ; if

  (if (= num_units 0)
    (progn
      (setq valid "NO")
      (setq alertStr (strcat alertStr " Number of Units or Length of System is not valid.\n"))
    )       ; progn
  )         ; if

  (if (/= alertStr "") (alert alertStr))

  (if (= valid "YES")
    (progn
      (mapScale)
    )       ; progn
  )         ; if

  (if (= valid "NO")
    (progn
      (c:septic)
    )       ; progn
  )         ; if

)           ; end checkSeptic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function called by Toolbar ONLY ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun continueSeptic (/ layoutPrint CMDECHO_DEF ANGDIR_DEF ANGBASE_DEF EXPERT_DEF)




  ;;;;;;;;;; Start Subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
   
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (defun page2notes ()

            (if (= page2NotesVars nil) (getUserVars))
            (if (AND (/= page2NotesVars nil) (/= c_u1 nil))
              (progn
                (getUserVars)  ; get latest userVars
                ; Zoom to Model Space and center on Page 2
                (COMMAND "LAYOUT" "S" "MODEL")  ; Changes to model tab
                (command "ZOOM" "c" cPage2 (* sPage2 (/ (last (getvar "screensize")) 8.0)))
              )  ; progn
            )    ; if


            (pageNotes "PAGE2" (nth 0 page2NotesVars) (nth 1 page2NotesVars) (nth 2 page2NotesVars))

          )      ; end function

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (defun page3notes ()

            (if (= page3NotesVars nil) (getUserVars))

            (if (AND (/= page3NotesVars nil) (/= c_u1 nil))
              (progn

                (getUserVars)  ; get latest userVars
                ; Zoom to Model Space and Center on Page 3
                (COMMAND "LAYOUT" "S" "MODEL")  ; Changes to model tab
                (command "ZOOM" "c" cPage3 (* sPage3 (/ (last (getvar "screensize")) 8.0)))
              )      ; progn
            )        ; if


            (pageNotes "PAGE3" (nth 0 page3NotesVars) (nth 1 page3NotesVars) (nth 2 page3NotesVars))

          )          ; end function
 
  
    ;;;;;;;;;; End Subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





; if no data loaded then check to see if there is design data attached to PAGE3_GUIDE block
  (if (= top_fill nil)
    (progn
      (if (xDataSystemStringLoad)
        (progn
          (getUserVars)
          (setScales)
        )
      )
    )
  )


  (toggle-OSNAP-Vars-OFF)
  (setBlockUnitsVars)
  (SETQ CMDECHO_DEF (GETVAR "CMDECHO")) (setvar "CMDECHO" 0)
  (setq EXPERT_DEF (getvar "EXPERT")) (setvar "EXPERT" 2)
  (setq ANGDIR_DEF (GETVAR "ANGDIR")) (setvar "ANGDIR" 0)
  (SETQ ANGBASE_DEF (GETVAR "ANGBASE")) (setvar "ANGBASE" 0)


  (command "UNDO" "BE")

  (if (/= sys_type nil)
    (progn
      ; lock designguide layer
      (command "layer" "LO" "SEPTICAD_GUIDES" "")

      (COMMAND "LAYOUT" "S" "MODEL")
      (SETVAR "CLAYER" "SEPTICAD")

      (RUN-DIM_SYSTEM)

      ; Zoom to Model Space and Center on Page 3
      (COMMAND "LAYOUT" "S" "MODEL")
      (command "ZOOM" "c" cPage3 (* sPage3 (/ (last (getvar "screensize")) 10.0)))

      (if (/= sys_type "STONE") (doDistributionPipes))

      (setq lastErrorFunction *error*)
      (setq *error* ERPHRP_ERROR)
      (if (= $SWING "YES") (swingtiemain))

      (if (= $TANKELV "YES") (doTankInvertElevation))

      (setq *error* lastErrorFunction)

      (setq lastErrorFunction *error*)
      (setq *error* PAGENOTES_ERROR)
      (if (= page2NoteON "YES") (page2Notes))
      (if (= page3NoteON "YES") (page3Notes))
      (setq *error* lastErrorFunction)

      (setq lastErrorFunction *error*)
      (setq *error* HHE200_ERROR)

      
      (HHE-200_PAGE1A_DCL)
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (if (member "HHE_200_PG1" (layoutList))
        (progn
          (princ "Printing to Form.....")

          ;; get information for printing to HHE-200 form on all four pages
          ;; example record in master_list: 
          ;;    (("B1") ("HHE_200_PG1") ((0.50 5.8 0.0)) (0.11) (5) (0.0))
          ;;      tag       layout          point      height  attach width
          ;; checkbox_fields-> block insert   //  text_fields -> mtext
          
          (setq checkbox_fields (get_HHE-200_form_hidden_check_boxes))
          (setq text_fields (get_HHE-200_form_hidden_text_fields))
          (setq master_list (get_HHE-200_form_hidden))

          (setvar "CLAYER" "FORM_INPUT")
       
          (septicad_print_page1 master_list checkbox_fields text_fields)
          (septicad_print_page2 master_list checkbox_fields text_fields)
          (septicad_print_page3 master_list checkbox_fields text_fields)
          (septicad_print_page4 master_list checkbox_fields text_fields)


          (if (= $ALLCAPS "YES")
            (ALLCAPS (ssget "X" '((0 . "MTEXT") (8 . "FORM_INPUT"))))
          )

          (setMapFunctionVars)

        )
      )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



      (setq *error* lastErrorFunction)

    )
  )

  (if (or (= num_rows nil) (= num_rows 0))
    (progn
      (Alert " Septic Design must be completed and in memory first!\n\nType septic or click on icon to complete septic design first!\n\nIntended to be used after Design Only button is pressed ")
      (c:septic)
    )
  )

  (SETVAR "CLAYER" "SEPTICAD")

  (resetBlockUnitsVars)
  (toggle-OSNAP-Vars-RESET)
  (SETVAR "CMDECHO" CMDECHO_DEF)
  (setvar "EXPERT" EXPERT_DEF)
  (setvar "ANGDIR" ANGDIR_DEF)
  (SETVAR "ANGBASE" ANGBASE_DEF)


  ; sets undo spot end 
  (command "UNDO" "E")

  (princ)
  )
      ; end function

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun setMapFunctionVars (/ latD latM latS longD longM LongS)

;  (if (= (strcase (getvar "PRODUCT")) "BRICSCAD")
;   (progn


   ; get strings from pg1Data
  (setq latD (cdr (assoc "S1" pg1Data))
        latM (cdr (assoc "S2" pg1Data))
        latS (cdr (assoc "S3" pg1Data))
        longD (cdr (assoc "S4" pg1Data))
        longM (cdr (assoc "S5" pg1Data))
        longS (cdr (assoc "S6" pg1Data))
  )

	; remove N or S on latitude and convert to + or -
  (if (= (STRCASE (substr latD 1 1)) "N") (setq latD (substr latD 2 (- (strlen latD) 1))))
  (if (= (STRCASE (substr latD 1 1)) "S") (setq latD (strcat "-" (substr latD 2 (- (strlen latD) 1)))))

  	; remove E or W on longitude and convert to + or -
  (if (= (STRCASE (substr longD 1 1)) "E") (setq longD (substr longD 2 (- (strlen longD) 1))))
  (if (= (STRCASE (substr longD 1 1)) "W") (setq longD (strcat "-" (substr longD 2 (- (strlen longD) 1)))))
  (if (> (atof longD) 0.0) (setq longD (strcat "-" longD)))

        ;  set strings to numbers
  (setq latD (atof latD)
        latM (atof latM)
        latS (atof latS)
        longD (atof longD)
        longM (atof longM)
        longS (atof longS)
  )

        ; syntax (DMS->DD longD longM longS latD latM latS)
        ; returns $septicad-latDD $septicad-longDD
  (DMS->DD longD longM LongS latD latM latS)

  (setq mapQuestURL (strcat "http://www.mapquest.com/maps?latitude=" (rtos $septicad-latDD 2 8) "&longitude=" (rtos $septicad-longDD 2 8) "&geocode=LATLNG"))
  (setq MEgisURL (strcat "http://maine.maps.arcgis.com/apps/webappviewer/index.html?id=28e35c8fcf514d2685357b78bdd0b246&center=" (rtos $septicad-longDD 2 8) "," (rtos $septicad-latDD 2 8) "&scale=1000"))
  (setq googleURL (strcat "http://www.google.com/maps/@" (rtos $septicad-latDD 2 8) "," (rtos $septicad-longDD 2 8) ",17z"))
; lindan ln
;N43 56' 2.95"
;W70 20' 44.36"

  	; tag page2 guide iwth xdata
  (xDataSiteLocationString)

;     )
;  )

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sVolume (printNote / testVal fillFudge loamCompaction sandCompaction loamFudge xup xdown xleft xright temp CenterVol tVol bVol Lvol Rvol TLvol TRvol BLvol BRvol A_TL A_BL B_TR B_BR C_TL C_TR D_BL D_BR CENTER_L CENTER_R)

  (setvar "cmdecho" 0)

	; Fudge factor to account for the curves at the corners of the fill - should be 1.13 to 1.05 based on actual tests
  (setq loamFudge 1.1)

	; Variable volVar set in suser (list of text values --> fillFudge sandComp loamComp chambersubstract) chambersubtrate is "1" if on
  (setq fillFudge (atof (nth 0 volVar)))
  (setq loamCompaction (atof (nth 1 volVar)))
  (setq sandCompaction (atof (nth 2 volVar)))

; cross-sectional areas areas [syntax: CROSSECTION_XYarea]
;;;;;;;;;;  TL  A         B  TR
;;;;;;;;;;   C  -----------  C'
;;;;;;;;;;   L  | CENTER  |  R
;;;;;;;;;;   D  -----------  D'
;;;;;;;;;;  BL  A'        B' BR

  (setq A_TL 0.0
        A_BL 0.0
        B_TR 0.0
        B_BR 0.0
        C_TL 0.0
        C_TR 0.0
        D_BL 0.0
        D_BR 0.0
        CENTER_L 0.0
        CENTER_R 0.0
  )

; Fill lengths
;;;;;;;;;;     |         |
;;;;;;;;;;    -U1-------U2-
;;;;;;;;;;     |         |  
;;;;;;;;;;    -D1-------D2-  
;;;;;;;;;;     |         |


  (setq u1_left 0.0
        u1_up 0.0
        u2_right 0.0
        u2_up 0.0
        d1_left 0.0
        d1_down 0.0
        d2_right 0.0
        d2_down 0.0
  )
;;;;;;;;;;;;;; Cross Section A-A';;;;;;;;;;;;;
  (fillCalculate u1u2 u1u1 (list 0.0 U1) (list sys_w D1) d1d1 d1d2 fill_top_sh (POLAR FILL_TOP_SH UP_PRO_SLOPE 100) fill_bot_sh (POLAR FILL_BOT_SH DOWN_SLOPE 100))

  (if (= topInt nil) (setq topInt fill_top))
  (if (= botInt nil) (setq botInt fill_bot))

  (command "AREA" (list 0.0 U1) u1u1 u1u2 u1u1 topInt fill_top_sh fill_top "")
  (setq A_TL (getvar "area"))
  (setq u1_up (distance topInt fill_top_sh))
;	(command "pline" (list 0.0 U1) u1u1 u1u2 u1u1 topInt fill_top_sh fill_top (LIST 0.0 U1) "")

  (command "AREA" (list sys_w D1) d1d1 d1d2 d1d1 botInt fill_bot_sh fill_bot "")
  (setq A_BL (getvar "area"))
  (setq d1_down (distance botInt fill_bot_sh))
;	(command "pline" (list sys_w D1) d1d1 d1d2 d1d1 botInt fill_bot_sh fill_bot (LIST SYS_w D1) "")

	; final grade points from left to right... adds crown point if applicable
  (if (/= slopeType "CROWN")
    (progn  ; if
;	    (command "PLINE" topInt fill_top_sh fill_top fill_bot fill_bot_sh botInt "")
      (setq $Cross-A-FGrade (list topInt fill_top_sh fill_top fill_bot fill_bot_sh botInt))
    )       ; end if
    (progn  ;else
;	    (command "PLINE" topInt fill_top_sh fill_top crown_pt fill_bot fill_bot_sh botInt "")
      (setq $Cross-A-FGrade (list topInt fill_top_sh fill_top crown_pt fill_bot fill_bot_sh botInt))
    )       ; end else
  )
	; draw grade points from left to right
;	   (command "PLINE" topInt u1u1 u1u2 u1u1 (list 0.0 U1) (list sys_w D1) d1d1 d1d2 d1d1 botInt "")
  (setq $Cross-A-EGrade (list topInt u1u1 u1u2 u1u1 (list 0.0 U1) (list sys_w D1) d1d1 d1d2 d1d1 botInt))


	; set to 0 if corner buried
	  ; if TL
  (if (<= (last fill_top) u1)
    (setq A_TL 0 u1_up 0)
  )
  (if (<= (last fill_bot) d1)
    (setq A_BL 0 d1_down 0)
  )



;####;;;;; LEFT - variable used later
  (if (/= slopeType "CROWN")
    (progn
      (setq Left (+ (distance fill_top_sh fill_top) (distance fill_top fill_bot) (distance fill_bot fill_bot_sh)))
    )
  )

  (if (= slopeType "CROWN")
    (progn
      (setq Left (+ (distance fill_top_sh crown_pt) (distance crown_pt fill_bot_sh)))
    )
  )


;;;;;;;;;;;;;; Cross Section B-B';;;;;;;;;;;;;
  (fillCalculate u2u2 u2u1 (list 0.0 U2) (list sys_w D2) d2d1 d2d2 fill_top_sh (POLAR FILL_TOP_SH UP_PRO_SLOPE 100) fill_bot_sh (POLAR FILL_BOT_SH DOWN_SLOPE 100))

  (if (= topInt nil) (setq topInt fill_top))
  (if (= botInt nil) (setq botInt fill_bot))

;	(command "PLINE" (list 0.0 U2) u2u1 u2u2 u2u1 topInt fill_top_sh fill_top (LIST 0.0 U2) "")
  (command "AREA" (list 0.0 U2) u2u1 u2u2 u2u1 topInt fill_top_sh fill_top "")
  (setq B_TR (getvar "area"))

;	(command "PLINE" (list sys_w D2) d2d1 d2d2 d2d1 botInt fill_bot_sh fill_bot (LIST SYS_W D2) "")
  (command "AREA" (list sys_w D2) d2d1 d2d2 d2d1 botInt fill_bot_sh fill_bot "")
  (setq B_BR (getvar "area"))


	; final grade points from left to right... adds crown point if applicable
  (if (/= slopeType "CROWN")
    (progn  ; if
;	    (command "PLINE" topInt fill_top_sh fill_top fill_bot fill_bot_sh botInt "")
      (setq $Cross-B-FGrade (list topInt fill_top_sh fill_top fill_bot fill_bot_sh botInt))
    )       ; end if
    (progn  ;else
;	    (command "PLINE" topInt fill_top_sh fill_top crown_pt fill_bot fill_bot_sh botInt "")
      (setq $Cross-B-FGrade (list topInt fill_top_sh fill_top crown_pt fill_bot fill_bot_sh botInt))
    )       ; end else
  )


	; draw grade points from left to right
;	   (command "PLINE" topInt u2u1 u2u2 u2u1 (list 0.0 U2) (list sys_w D2) d2d1 d2d2 d2d1 botInt "")
  (setq $Cross-B-EGrade (list topInt u2u1 u2u2 u2u1 (list 0.0 U2) (list sys_w D2) d2d1 d2d2 d2d1 botInt))


  (setq u2_up (distance topInt fill_top_sh))
  (setq d2_down (distance botInt fill_bot_sh))

	; set to 0 if corner buried
  (if (<= (last fill_top) u2)
    (setq B_TR 0 u2_up 0)
  )
  (if (<= (last fill_bot) d2)
    (setq B_BR 0 d2_down 0)
  )


;;;;;;;;;;;;;; Cross Section C-C';;;;;;;;;;;;;

  (setq leftFill (list (* left_shoulder -1) (last fill_top)))
  (setq rightFill (list (+ right_shoulder sys_L) (last fill_top)))

  (fillCalculate U1L2 U1L1 (list 0.0 U1) (list sys_L U2) U2R1 U2R2 leftFill (POLAR leftFill LEFT_SLOPE 100) rightFill (POLAR rightFill RIGHT_SLOPE 100))

  (if (= topInt nil) (setq topInt fill_top))
  (if (= botInt nil) (setq botInt fill_bot))

  (command "AREA" (list 0.0 U1) u1L1 u1L2 u1L1 topInt leftFill fill_top "")
;	(command "PLINE" (list 0.0 U1) u1L1 u1L2 u1L1 topInt leftFill fill_top (list 0.0 U1) "")
  (setq C_TL (getvar "area"))

  (command "AREA" (list sys_L U2) u2r1 u2r2 u2r1 botInt rightFill (list sys_l (last fill_top)) "")
;	(command "PLINE" (list sys_L U2) u2r1 u2r2 u2r1 botInt rightFill (list sys_l (last fill_top)) (list sys_L U2) "")
  (setq C_TR (getvar "area"))


	; final grade points from left to right... does not need to consider crown point
;	  (command "PLINE" topInt leftFill fill_top (list sys_l (last fill_top)) rightFill botInt "")
  (setq $Cross-C-FGrade (list topInt leftFill fill_top (list sys_l (last fill_top)) rightFill botInt))

	; draw grade points from left to right
;	   (command "PLINE" topInt u1L1 u1L2 u1L1 (list 0.0 U1) (list sys_L U2) u2r1 u2r2 u2r1 botInt "")
  (setq $Cross-C-EGrade (list topInt u1L1 u1L2 u1L1 (list 0.0 U1) (list sys_L U2) u2r1 u2r2 u2r1 botInt))

  (setq u1_Left (distance topInt leftFill))
  (setq u2_Right (distance botInt rightFill))

	; set to 0 if corner buried
  (if (<= (last fill_top) u1)
    (setq C_TL 0 u1_Left 0)
  )
  (if (<= (last fill_top) u2)
    (setq C_TR 0 u2_right 0)
  )

;####;;;;; TOP - variable used later
  (setq Top (distance leftFill rightFill))

;;;;;;;;;;;;;; Cross Section D-D';;;;;;;;;;;;;


  (setq leftFill (list (* left_shoulder -1) (last fill_bot)))
  (setq rightFill (list (+ right_shoulder sys_L) (last fill_bot)))

  (fillCalculate D1L2 D1L1 (list 0.0 D1) (list sys_L D2) D2R1 D2R2 leftFill (POLAR leftFill LEFT_SLOPE 100) rightFill (POLAR rightFill RIGHT_SLOPE 100))

  (if (= topInt nil) (setq topInt fill_top))
  (if (= botInt nil) (setq botInt fill_bot))

  (command "AREA" (list 0.0 D1) D1L1 D1L2 D1L1 topInt leftFill (list 0.0 (last fill_bot)) "")
;	(command "PLINE" (list 0.0 D1) D1L1 D1L2 D1L1 topInt leftFill (list 0.0 (last fill_bot)) (list 0.0 D1) "")
  (setq D_BL (getvar "area"))

  (command "AREA" (list sys_L d2) d2r1 d2r2 d2r1 botInt rightFill (list sys_l (last fill_bot)) "")
;	(command "PLINE" (list sys_L d2) d2r1 d2r2 d2r1 botInt rightFill (list sys_l (last fill_bot)) (list sys_L d2) "")
  (setq D_BR (getvar "area"))

	; final grade points from left to right... does not need to consider crown point
;	  (command "PLINE" topInt leftFill (list 0.0 (last fill_bot)) (list sys_l (last fill_bot)) rightFill botInt "")
  (setq $Cross-D-FGrade (list topInt leftFill (list 0.0 (last fill_bot)) (list sys_l (last fill_bot)) rightFill botInt))

	; draw grade points from left to right
;	   (command "PLINE" topInt d1L1 d1L2 d1L1 (list 0.0 d1) (list sys_L d2) d2r1 d2r2 d2r1 botInt "")
  (setq $Cross-D-EGrade (list topInt d1L1 d1L2 d1L1 (list 0.0 d1) (list sys_L d2) d2r1 d2r2 d2r1 botInt))


  (setq d1_Left (distance topInt leftFill))
  (setq d2_Right (distance botInt rightFill))

	; set to 0 if corner buried
  (if (<= (last fill_bot) d1)
    (setq D_BL 0 d1_Left 0)
  )
  (if (<= (last fill_bot) d2)
    (setq D_BR 0 d2_Right 0)
  )


	;;;;; Find CENTER side areas, including shoulders
  (if (/= slopeType "CROWN")
    (progn
      ;;--LEFT TO RIGHT
      (command "AREA" (list 0.0 U1) (list sys_W D1) fill_bot fill_top "")
;		(command "PLINE" (list 0.0 U1) (list sys_W D1) fill_bot fill_top (list 0.0 U1) "")
      (setq CENTER_L (getvar "area"))

      (command "AREA" (list 0.0 U2) (list sys_W D2) fill_bot fill_top "")
;		(command "PLINE" (list 0.0 U2) (list sys_W D2) fill_bot fill_top (list 0.0 U2) "")
      (setq CENTER_R (getvar "area"))
    )
  )

  (if (= slopeType "CROWN")
    (progn
      ;;--LEFT TO RIGHT
      (command "AREA" (list 0.0 U1) (list sys_W D1) fill_bot crown_pt fill_top "")
;		(command "PLINE" (list 0.0 U1) (list sys_W D1) fill_bot crown_pt fill_top (list 0.0 U1) "")
      (setq CENTER_L (getvar "area"))

      (command "AREA" (list 0.0 U2) (list sys_W D2) fill_bot crown_pt fill_top "")
;		(command "PLINE" (list 0.0 U2) (list sys_W D2) fill_bot crown_pt fill_top (list 0.0 U2) "")
      (setq CENTER_R (getvar "area"))
    )
  )

	; if all corners are buried then ignore the body volume

  (if
    (and
      (<= (last fill_top) u1)
      (<= (last fill_top) u2)
      (<= (last fill_bot) d1)
      (<= (last fill_bot) d2)
    )
    (setq CENTER_R 0 CENTER_L 0)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Start calculating volumes ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq CenterVol (* (/ (+ CENTER_L CENTER_R) 2.0) sys_L))

  (setq Lvol (* (/ (+ C_TL D_BL) 2.0) sys_W))
  (setq Rvol (* (/ (+ C_TR D_BR) 2.0) sys_W))

  (setq Tvol (* (/ (+ A_TL B_TR) 2.0) sys_L))
  (setq Bvol (* (/ (+ A_BL B_BR) 2.0) sys_L))

  (setq TEMP (* (/ A_TL 2.0) U1_L))
  (setq TLvol (* (/ C_TL 2.0) U1_U))
  (IF (> TEMP TLvol) (setq TLvol temp))

  (setq TEMP (* (/ B_TR 2.0) U2_R))
  (setq TRvol (* (/ C_TR 2.0) U2_U))
  (IF (> TEMP TRvol) (setq TRvol temp))

  (setq TEMP (* (/ A_BL 2.0) D1_L))
  (setq BLvol (* (/ D_BL 2.0) D1_D))
  (IF (> TEMP BLvol) (setq BLvol temp))

  (setq TEMP (* (/ B_BR 2.0) D2_R))
  (setq BRvol (* (/ D_BR 2.0) D2_D))
  (IF (> TEMP BRvol) (setq BRvol temp))

  (setq totalVol (+ CenterVol tVol bVol Lvol Rvol TLvol TRvol BLvol BRvol))
  (setq totalVolRaw totalVol)
  (setq totalVol (* fillFudge (/ (/ totalVol 1728.0) 27.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  END calculating volumes ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	;;;;;;;;;  Determine Area of Loam Required ;;;;;;;;;;
	;;;;  Lengths take into account angle of slope ;;;;;;

  (setq xdown (/ (* 3.0 pi) 2.0))
  (setq xup (/ pi 2.0))
  (setq xright 0.0)
  (setq xleft pi)

  (setq base c_u1)
  (setq base (polar base xleft (/ left_shoulder 12.0)))
  (setq base (polar base xUp (/ up_shoulder 12.0)))
  (setq base2 (polar base xdown left))

  (setq AA (polar base xleft u1_left))
  (setq BB (polar base xup u1_up))
  (setq CC (polar (polar base xright TOP) xup u2_up))
  (setq DD (polar (polar base xright TOP) xright u2_right))
  (setq EE (polar (polar base2 xright TOP) xright d2_right))
  (setq FF (polar (polar base2 xright TOP) xdown d2_down))
  (setq GG (polar base2 xdown d1_down))
  (setq HH (polar base2 xleft d1_left))

	;(command "PLINE" aa bb cc dd ee ff gg hh "")
  (command "AREA" aa bb cc dd ee ff gg hh "")
  (setq loamArea (getvar "area"))

	; default depth of loam in inches
  (setq depthLoam 4.0)
;	commented out below by forcing 10" fill above for geoflow
;	(if (AND (or (= sys_type "ENVIRO")(= sys_type "GEOFLOW"))(= fill_above 8.0))(setq depthLoam 2.0))
;	(if (AND (or (= sys_type "ENVIRO")(= sys_type "GEOFLOW"))(= fill_above 9.0))(setq depthLoam 3.0))
;	(if (AND (or (= sys_type "ENVIRO")(= sys_type "GEOFLOW"))(= fill_above 10.0))(setq depthLoam 4.0))

  (setq loamVol (* depthLoam loamArea loamFudge))
  (setq loamVolRaw loamVol)
	; convert to cubic yards
  (setq loamVol (/ (/ loamVol 1728.0) 27.0))
  (setq sandVol (- totalVol loamVol))

	; apply compaction factors
  (setq sandVol (* sandVol sandCompaction))
  (setq loamVol (* loamVol loamCompaction))
  (setq totalVol (+ sandVol loamVol))

	; determine # of buried corners
  (setq testVal 0)
  (foreach item (list u1_f u2_f d1_f d2_f)
    (if (<= item 0.0) (setq testVal (+ testVal 1)))
  )

  (if (= testVal 3) (princ "\nApproximate Volume of Above Grade Fill NOT PRINTED --> Calculation method not accurate when 3 system corners are buried."))
  (if (= testVal 4) (princ "\nApproximate Volume of Above Grade Fill NOT PRINTED --> All system corners are below existing grade."))
  (if (= printNote "NO") (princ "\nApproximate Volume of Above Grade Fill NOT PRINTED --> User Variable set to not print."))

	; write mText Note on Cross section and insert mText entity
  (if (AND (/= testVal 3) (/= testVal 4) (= printNote "YES"))
    (progn
      (setq volTxt (strcat "{\\LAPPROXIMATE ABOVE}\\P{\\LGRADE FILL REQUIRED:}\\P" (rtos loamVol 2 1) " cubic yards of LOAM\\P" (rtos sandVol 2 1) " cubic yards of SAND"))

      (setq volTxt (strcat volTxt "\\PCompaction: +" (rtos (* (- loamCompaction 1) 100) 2 0) "% Loam & +" (rtos (* (- sandCompaction 1) 100) 2 0) "% Sand"))


      (if (OR (= sys_type "STONE")
              (= sys_type "TRENCH2")
              (= sys_type "TRENCH3")
          )

        (setq volTxt (strcat volTxt "\\PVolume of Stone Not Considered"))
      )
      (if (= (substr sys_type 5 8) "CONCRETE")
        (setq volTxt (strcat volTxt "\\PVolume of Chambers and Stone Not Considered"))
      )
      (if
        (AND
          (/= sys_type "STONE")
          (/= (substr sys_type 5 8) "CONCRETE")
          (/= sys_type "TRENCH2")
          (/= sys_type "TRENCH3")
        )
        (setq volTxt (strcat volTxt "\\PVolume of System Components Not Considered"))
      )

      (setq scaleFactor (/ profileScale 5.0))
      (SETQ width (* 120.0 scaleFactor))

      (setq cProfile (cProfileGet))

      (SETQ insPt (polar (polar cProfile 0.0 (* 148 scaleFactor)) (/ pi 2.0) (* 106.5 scaleFactor)))

      ; set text size
      (setq ts (* profileTextSize scaleProfileNote))
      (command "-mtext" insPt "J" "TR" "H" ts "W" width volTxt "")
    )
  )





  (princ (strcat "\n>---------------------- SeptiCAD Unmodified Fill Volumes ----------------------<\n"))
  (princ (strcat ">-----  The total amount of fill required is:     " (rtos (/ (/ totalVolRAW 1728.0) 27.0) 2 1) " cubic yards\n"))
  (princ (strcat ">-----  Total Volume of 4\" Loam Layer:            " (rtos (/ (/ loamVolRaw 1728.0) 27.0) 2 1) " cubic yards\n"))
  (princ (strcat ">-----  Total Volume of Remaining Sand Layer:     " (rtos (/ (/ (- totalVolRAW loamVolRAW) 1728.0) 27.0) 2 1) " cubic yards\n"))
  (princ (strcat ">-----  Fill Fudge Factor:                        " (rtos fillFudge 2 2) " or +" (rtos (* (- fillFudge 1) 100) 2 2) "%\n"))
  (princ (strcat ">-----  Loam Compaction Factor:                   " (rtos loamCompaction 2 2) " or +" (rtos (* (- loamCompaction 1) 100) 2 2) "%\n"))
  (princ (strcat ">-----  Sand Compaction Factor:                   " (rtos sandCompaction 2 2) " or +" (rtos (* (- sandCompaction 1) 100) 2 2) "%\n"))
  (princ (strcat ">-----  Volumes Adjusted for Compaction are shown in Cross Section note\n"))
  (princ (strcat ">-----  Volume of Stone or Chambers has not been considered\n"))
  (princ (strcat ">---------------------- SeptiCAD Unmodified Fill Volumes ----------------------<\n\n"))


)
; end sVolume
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun auto-crown (/ ddiag)

  (setq dcl_id (load_dialog (strcat $septicadPathDCL "CROWN.dcl")))

      ;;;--- See if the dialog box is already loaded
  (if (not (new_dialog "CROWN" dcl_id))
    (progn
      (alert "The CROWN.DCL file was not found!")
      (exit)
    )
  )

      ;;;--- If an action event occurs, do this function
  (action_tile "cancel" "(setq ddiag 1)(done_dialog)")
  (action_tile "left" "(setq ddiag 2)(done_dialog)")
  (action_tile "right" "(setq ddiag 3)(done_dialog)")


  (set_tile "TEXT" "How should the top of the fill be graded?")


      ;;;--- Display the dialog box
  (start_dialog)

      ;;;--- Unload the dialog box
  (unload_dialog dcl_id)

      ;;;--- If the cancel button was pressed
  (if (= ddiag 1)
    (progn
      (princ "\nExisting grade and final grade are the same.")
      (setq ddiag 3)
    )
  )


      ;;;--- If the "left" button was pressed than create a 3% Crown
  (if (= ddiag 2)
    (progn
      (setq crown_pt (polar slope_top 3upRight (/ (/ sys_w 2.0) (cos 3upRight))))
      (setq crown_pt (polar crown_pt up (* up_shoulder 0.03)))
      (LINE_INTER crown_pt (polar crown_pt 3downLeft 0.001) slope_top slope_bot)
      (setq up_int xy_int)
      (LINE_INTER crown_pt (polar crown_pt 3downRight 0.001) slope_top slope_bot)
      (setq down_int xy_int)
      (COMMAND "PLINE" up_int crown_pt down_int "")
      (command "text" "J" "BC" crown_pt profileTextSize "0" "3% Crown")
      (setq crownFlat "DO NOTHING")
    )
  )

      ;;;--- If the "right" button was pressed
  (if (= ddiag 3)
    (progn
      ; flat
      (setq crownFlat 1)
    )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun septiLog (/ f)

  (setq f (open (strcat $septicadPathRoot "septiLog.txt") "w"))
  (write-line "------------------  ERROR LOG  ------------------" f)
  (write-line (strcat (getvar "PRODUCT") " " (getvar "_VERNUM")) f)
  (write-line $septicadVersion f)
  (write-line (strcat "DATE			" (strcat (substr (rtos (getvar "CDATE") 2 0) 5 2) "/" (substr (rtos (getvar "CDATE") 2 0) 7 2) "/" (substr (rtos (getvar "CDATE") 2 0) 3 2))) f)
  (write-line (strcat "TIME			" (strcat (substr (rtos (getvar "CDATE") 2 8) 10 2) ":" (substr (rtos (getvar "CDATE") 2 8) 12 2) ":" (substr (rtos (getvar "CDATE") 2 8) 14 2))) f)
  (write-line (strcat "SE NAME			" (cdr (assoc "I3" pg1Data))) f)
  (write-line (strcat "SE EMAIL			" (cdr (assoc "I5" pg1Data))) f)
  (WRITE-LINE (STRCAT "NUM_ROWS      		" (RTOS NUM_ROWS 2 0)) F)
  (WRITE-LINE (STRCAT "NUM_UNITS     		" (RTOS NUM_UNITS 2 0)) F)
  (WRITE-LINE (STRCAT "ROW_SEP       		" (RTOS ROW_SEP 2 0)) F)
  (WRITE-LINE (STRCAT "SYS_TYPE      		" SYS_TYPE) F)
  (WRITE-LINE (STRCAT "U1				" (RTOS U1 2 0)) F)
  (WRITE-LINE (STRCAT "U2				" (RTOS U2 2 0)) F)
  (WRITE-LINE (STRCAT "D1				" (RTOS D1 2 0)) F)
  (WRITE-LINE (STRCAT "D2				" (RTOS D2 2 0)) F)
  (WRITE-LINE (STRCAT "FILL_ABOVE			" (RTOS FILL_ABOVE 2 0)) F)
  (WRITE-LINE (STRCAT "STEP_TYPE			" STEP_TYPE) F)
  (WRITE-LINE (STRCAT "DEPTH_TOP			" (RTOS DEPTH_TOP 2 0)) F)
  (WRITE-LINE (STRCAT "DEPTH_BOT			" (RTOS DEPTH_BOT 2 0)) F)
  (WRITE-LINE (STRCAT "STEP_SIZE			" (RTOS STEP_SIZE 2 0)) F)
  (WRITE-LINE (STRCAT "DEPTH_STEP			" (RTOS DEPTH_STEP 2 0)) F)
  (WRITE-LINE (STRCAT "STONEBESIDE		" (RTOS STONEBESIDE 2 0)) F)
  (WRITE-LINE (STRCAT "STONEBELOW			" (RTOS STONEBELOW 2 0)) F)
;	(WRITE-LINE (STRCAT "SHELF_SEP			" (RTOS SHELF_SEP 2 0)) F)
;	(WRITE-LINE (STRCAT "shelfToggle		" (RTOS shelfToggle 2 0)) F)
  (WRITE-LINE (STRCAT "ROW_SEP			" (RTOS ROW_SEP 2 0)) F)
  (WRITE-LINE (STRCAT "LIM_SEP			" (RTOS LIM_SEP 2 0)) F)
  (WRITE-LINE (STRCAT "DOWN_SLOPE			" (RTOS DOWN_SLOPE 2 0)) F)
  (WRITE-LINE (STRCAT "UP_SLOPE			" (RTOS UP_SLOPE 2 0)) F)
  (WRITE-LINE (STRCAT "LEFT_SLOPE			" (RTOS LEFT_SLOPE 2 0)) F)
  (WRITE-LINE (STRCAT "RIGHT_SLOPE		" (RTOS RIGHT_SLOPE 2 0)) F)
  (WRITE-LINE (STRCAT "DOWN_SHOULDER		" (RTOS DOWN_SHOULDER 2 0)) F)
  (WRITE-LINE (STRCAT "UP_SHOULDER		" (RTOS UP_SHOULDER 2 0)) F)
  (WRITE-LINE (STRCAT "LEFT_SHOULDER  		" (RTOS LEFT_SHOULDER 2 0)) F)
  (WRITE-LINE (STRCAT "RIGHT_SHOULDER		" (RTOS RIGHT_SHOULDER 2 0)) F)
  (WRITE-LINE (STRCAT "BEARING			" (RTOS BEARING 2 0)) F)
  (WRITE-LINE (STRCAT "CROSSSECTION		" CROSSSECTION) F)
  (WRITE-LINE (STRCAT "SLOPETYPE			" SLOPETYPE) F)
  (WRITE-LINE (STRCAT "DOCOMPOUND			" DOCOMPOUND) F)


  (close f)
  (princ (strcat "\nSeptiCAD error log can be found in --> " (strcat $septicadPathRoot "septiLog.txt")))
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun profileNote (/ text scaleFactor point width topLeft text ex ts noteType)

        ;  get profile note associated wtih the system, if not 
  (if (setq text (cdr (assoc "%X-3%" $customSysList)))
    (progn
      ;;;; Check to see if 3 Scale are stored in USERI1, USERI2, USERI3
      ;;;; If not then call mapScale
      (setScales)

      ; set current layer
      (SETVAR "CLAYER" "SEPTICAD")

      ; detemine Top left corner of mText based on center of profile window
      (setq scaleFactor profileScale)
      (setq point (polar (cProfileGet) pi (* 47.65 scaleFactor)))
      (SETQ topLeft (polar point (/ pi 2.0) (* 23.59 scaleFactor)))
      (setq width 30); setq reasonable text with 
      (setq width (* width scaleFactor)) 
      
      ; % of normal text size
      (setq ts (* profileTextSize scaleProfileNote))
 
      (setq noteType "TEXT")
      
      ; if the first character of the note is a * then its a block insert path
      (if (= (substr text 1 1) "*") (setq noteType "BLOCK"))

      (if (= noteType "BLOCK")
        (progn
          
          ;sets block insert unit vars
          (setBlockUnitsVars)
          (setq ex (getvar "Expert"))
          (setvar "Expert" 2)          

          ; remove * from beginning
          (setq text (substr text 2 (strlen text)))

          (COMMAND "-insert" text topLeft profileScale profileScale "0")
          
          (setvar "Expert" ex); resets the Expert system variable
          (resetBlockUnitsVars);reset block insert unit vars
        )
      )

      (if (= noteType "TEXT")
        (progn


          
          ; insert mText with error control if no note is entered
          (if (/= text "") (command "-mtext" topLeft "H" ts "W" width text ""))
          (if (= text "") (princ "The cross section note is blank. Modify cross sections notes in SeptiCAD preference [PREF icon]"))
        )
      )

      


    )        ; end progn

    (princ "\nNo top left corner cross section note associated with this System")
  )          ; end if
  (princ)
)
; end function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




















(defun pageNotes (page corner percentWidth tsScale / inputList fil a ddiag ts cPage3 cPage2 scaleFactor insertPoint noteNum text noteList en en1 ts pt1 pt2)


;;;; Check to see if 3 Scale are stored in USERI1, USERI2, USERI3
;;;; If not then call mapScale
  (setScales)
	; create empty selection set do change to ALLCAPS if needed
  (setq sset1 (ssadd))

	; Set font size
  (if (= page "PAGE2") (setq ts (* tsScale page2TextSize)))
  (if (= page "PAGE3") (setq ts (* tsScale page3TextSize)))


	  ;;;--- Set up an empty list
  (setq inputList (list))

	  ;;;--- Open the text file
  (if (findfile (strcat $septicadPathText "pageNotes.txt"))
    (progn

      (if (setq fil (open (findfile (strcat $septicadPathText "pageNotes.txt")) "r"))
        (progn

          ;;;--- Skip the first three lines        
          (read-line fil)
          (read-line fil)
          (read-line fil)

          ;;;--- Read the lines of data in the text file
          (while (setq a (read-line fil))
            ;;;--- Add the data to the list
            (setq inputList
                  (append
                    inputList
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
  (setq noteList (list))
  (foreach a inputList
    (setq noteList (append noteList (list a)))
  )





       ;;;--- Load the DCL file
  (setq dcl_id (load_dialog (strcat $septicadPathDCL "pageNotes.dcl")))

      ;;;--- See if the dialog box is already loaded
  (if (not (new_dialog "pageNotes" dcl_id))
    (progn
      (alert "The pageNotes.DCL file was not found!")
      (exit)
    )
  )

  (if (= page "PAGE2") (set_tile "DESCRIPTION" "Insert Notes on Page 2 Map"))
  (if (= page "PAGE3") (set_tile "DESCRIPTION" "Insert Notes on Page 3 Map"))

  (if (AND (= page "PAGE3") (/= tankNote nil)) (set_tile "altText1" tankNote))


      ;;;--- Add the label names to the dialog box
  (start_list "noteList" 3)
  (mapcar 'add_list noteList)
  (end_list)

      ;;;--- If an action event occurs, do this function
  (action_tile "cancel" "(setq ddiag 1)(done_dialog)")
  (action_tile "okay" "(setq ddiag 2)(saveNoteListVars page)(done_dialog)")
  (action_tile "customize" "(setq ddiag -99)(done_dialog)")

      ;;;--- Display the dialog box
  (start_dialog)

      ;;;--- Unload the dialog box
  (unload_dialog dcl_id)

      ;;;--- If the cancel button was pressed
  (if (= ddiag 1)
    (progn
      ;;;--- Set an exit message
      (princ)
      (Print "...PageNotes Cancelled.")
    )
  )


      ;;;--- If the "Okay" button was pressed
  (if (and (= ddiag 2) (OR (/= retList nil) (/= altText1 "") (/= altText2 "")))
    (progn
      ; begin if okay

      ;; START makes a string of all the chosen text
      (setq text "{\\LNOTES:}\\P")
      ; count set to number notes 
      (setq noteNum 1)
      (foreach a retList
        (setq text (strcat text (rtos noteNum 2 0) ".  " a "\\P"))
        (setq noteNum (+ noteNum 1))
      )
      (if (/= altText1 "")
        (progn
          (setq text (strcat text (rtos noteNum 2 0) ".  " altText1 "\\P"))
          (setq noteNum (+ noteNum 1))
        )  ; end progn
      )    ; end if
      (if (/= altText2 "")
        (progn
          (setq text (strcat text (rtos noteNum 2 0) ".  " altText2 "\\P"))
          (setq noteNum (+ noteNum 1))
        )  ; end progn
      )    ; end if
      ;; END makes a string of all the chosen text


      (if (AND (= page "PAGE3") (/= c_u1 nil))
        (progn
          ; set width of mText based on page3Scale
          ; scale based on page3Scale - original code written for 1":10'
          (setq scaleFactor (/ page3Scale 10.0))
          (setq width (* (/ percentWidth 100.0) 75.0 scaleFactor))


          ; detemine Top left corner of mText based on center of page 3 map window

          ; Center of page 3 map window
          (setq cPage3 (polar page3BaseMap (angle c_U1 c_D2) (/ (distance c_u1 c_D2) 2.0)))  ; center insert point

          ; create the right point to place the Mtext corner
          (if (= corner "BL")
            (progn
              ; Corner of mtext based on cPage3
              (setq point (polar cPage3 pi (* 39.65 scaleFactor)))
              (SETQ insertPoint (polar point (/ (* 3.0 pi) 2.0) (* 20.875 scaleFactor)))
              (command "-mtext" insertPoint "J" "BL" "H" ts "W" width "tempjjjj" "")         ; dummy text
            )  ; progn
          )    ; if

          (if (= corner "BR")
            (progn
              ; Corner of mtext based on cPage3
              (setq point (polar cPage3 0.0 (- (* 39.65 scaleFactor) width)))
              (SETQ insertPoint (polar point (/ (* 3.0 pi) 2.0) (* 20.875 scaleFactor)))
              (command "-mtext" insertPoint "J" "BR" "H" ts "W" width "tempjjjj" "")  ; dummy text
            )  ; progn
          )    ; if

          (if (= corner "TL")
            (progn
              ; Corner of mtext based on cPage3
              (setq point (polar cPage3 pi (* 39.65 scaleFactor)))
              (SETQ insertPoint (polar point (/ pi 2.0) (* 13.075 scaleFactor)))
              (command "-mtext" insertPoint "J" "TL" "H" ts "W" width "tempjjjj" "")  ; dummy text
            )  ; progn
          )    ; if

          (if (= corner "TR")
            (progn
              ; Corner of mtext based on cPage3
              (setq point (polar cPage3 0.0 (- (* 39.65 scaleFactor) width)))
              (SETQ insertPoint (polar point (/ pi 2.0) (* 20.875 scaleFactor)))
              (command "-mtext" insertPoint "J" "TL" "H" ts "W" width "tempjjjj" "")  ; dummy text
            )  ; progn
          )    ; if
          (ssadd (entlast) sset1)
        )      ; end progn
      )        ; end if


      (if (AND (= page "PAGE2") (/= c_u1 nil))
        (progn
          ; set width of mText based on page3Scale
          ; scale based on page2Scale - original code written for 1":10'
          (setq scaleFactor (/ page2Scale 10.0))
          (setq width (* (/ percentWidth 100.0) 75.0 scaleFactor))

          ; detemine Top left corner of mText based on center of page 3 map window

          ; Center of page 2 map window
          (setq cPage2 (polar page2BaseMap (angle t_U1 t_D2) (/ (distance t_u1 t_D2) 2.0)))  ; center insert point

          ; create the right point to place the Mtext corner
          (if (= corner "BL")
            (progn
              ; Corner of mtext based on cPage3
              (setq point (polar cPage2 pi (* 39.35 scaleFactor)))
              (SETQ insertPoint (polar point (/ (* 3.0 pi) 2.0) (* 25.19 scaleFactor)))
              (command "-mtext" insertPoint "J" "BL" "H" ts "W" width "tempjjjj" "")         ; dummy text
            )  ; progn
          )    ; if

          (if (= corner "BR")
            (progn
              ; Corner of mtext based on cPage3
              (setq point (polar cPage2 0.0 (- (* 39.35 scaleFactor) width)))
              (SETQ insertPoint (polar point (/ (* 3.0 pi) 2.0) (* 25.19 scaleFactor)))
              (command "-mtext" insertPoint "J" "BR" "H" ts "W" width "tempjjjj" "")  ; dummy text
            )  ; progn
          )    ; if

          (if (= corner "TL")
            (progn
              ; Corner of mtext based on cPage3
              (setq point (polar cPage2 pi (* 39.35 scaleFactor)))
              (SETQ insertPoint (polar point (/ pi 2.0) (* 25.19 scaleFactor)))
              (command "-mtext" insertPoint "J" "TL" "H" ts "W" width "tempjjjj" "")  ; dummy text
            )  ; progn
          )    ; if

          (if (= corner "TR")
            (progn
              ; Corner of mtext based on cPage3
              (setq point (polar cPage2 0.0 (- (* 39.35 scaleFactor) width)))
              (SETQ insertPoint (polar point (/ pi 2.0) (* 5.5 scaleFactor)))
              (command "-mtext" insertPoint "J" "TL" "H" ts "W" width "tempjjjj" "")  ; dummy text
            )  ; progn
          )    ; if
          (ssadd (entlast) sset1)
        )      ; end progn
      )        ; end if




      ; if SD vars are not in memory
      (if (= c_u1 nil)
        (progn
          (alert " Choose topleft and bottom right corner of text box")
          (princ "\n\nPlacing Multi-Line Text Note --> ")
          (princ text)
          (setq pt1 (getpoint "\nClick Top Left of Text--> "))
          (setq pt2 (getcorner pt1 "\nClick Bottom Right of Text--> "))
          (princ "\n")
          (command "-mtext" pt1 "H" ts pt2 "temporary" "")
          (ssadd (entlast) sset1)
        )      ; end progn
      )        ; end if


      ; adds mtext to object
      (setq en (entlast))
      (setq en1 (entget en))
      (setq en1 (subst (cons 1 text) (assoc 1 en1) en1))
      (entmod en1)


      ; end if okay
    )
  )


;;;; If Custom - Do custom then bring back to pageNotes
  (if (= ddiag -99) (pageNotesCustom))


  (if (= ddiag -99)
    (progn
      (if (= page "PAGE3") (pageNotes "PAGE3" (nth 0 page3NotesVars) (nth 1 page3NotesVars) (nth 2 page3NotesVars)))
      (if (= page "PAGE2") (pageNotes "PAGE2" (nth 0 page2NotesVars) (nth 1 page2NotesVars) (nth 2 page2NotesVars)))
    )          ; progn
  )            ; if

  (if (= $ALLCAPS "YES")
    (ALLCAPS sset1)
  )

; set some variables to nil
  (setq altText1 nil altText2 nil retList nil)

  (princ)

)  ;;; end function





(defun saveNoteListVars (page / readlist count item)

 ;;;--- Get the two custom notes
  (setq altText1 (get_tile "altText1"))
  (setq altText2 (get_tile "altText2"))

  ;;;--- Setup a list to hold the selected items
  (setq retList (list))

  ;;;--- Save the list setting
  (setq readlist (get_tile "noteList"))

  ;;;--- Setup a variable to run through the list
  (setq count 1)

  ;;;--- cycle through the list getting all of the selected items
  (while (setq item (read readlist))
    (setq retList (append retList (list (nth item noteList))))
    (while
      (and
        (/= " " (substr readlist count 1))
        (/= "" (substr readlist count 1))
      )
      (setq count (1+ count))
    )
    (setq readlist (substr readlist count))
  )

; Now retlist has all of the selections in list format


)
; end saveNoteListVars




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun proBearing (/ DOWN UP RIGHT LEFT insertPt textPt textString DIMASZ_DEF DIMTXT_DEF EX blockName)
  (setvar "cmdecho" 0)
  ;sets block insert unit vars
  (setBlockUnitsVars)
  (setq down (/ (* 3.0 pi) 2.0))
  (setq up (/ pi 2.0))
  (setq right 0.0)
  (setq left pi)

  (setq ex (getvar "Expert"))
     ; suppresses "Block already defined. Redefined it? ALERT"
  (setvar "Expert" 2)

  (setq blockName (strcat $septicadPathBlockRequired "COMPASS.DWG"))

      ; insert on Page 2 -USE NORMAL DIRECTIONS OR ROTATE
  (setq insertPt (polar cPage2 right (* sPage2 32.7)))
  (setq insertPt (polar insertPt down (* sPage2 18.54)))
  (if (= %RotateSystem 1)
    (command "-insert" blockName insertPt sPage2 sPage2 "90")
    (command "-insert" blockName insertPt sPage2 sPage2 BEARING)
  )

       ; insert on Page 3 -USE NORMAL DIRECTIONS OR ROTATE
  (setq insertPt (polar cPage3 right (* sPage3 33)))
  (setq insertPt (polar insertPt down (* sPage3 14.23)))
  (if (= %RotateSystem 1)
    (command "-insert" blockName insertPt sPage3 sPage3 "90")
    (command "-insert" blockName insertPt sPage3 sPage3 BEARING)
  )



; insert system bearing on Page 3 map

  (setq textPt (polar c_u1 %RIGHT (* 0.5 (distance c_u1 c_u2))))
  (setq insertPt (polar textPt %UP (/ up_shoulder 24.0)))
  (setq blockName (strcat $septicadPathBlockRequired "PLAN-BEARING.DWG"))

  ; 1 = yes
  (if (= %RotateSystem 1)
    (progn
      (if (<= bearing 180)
        (progn
          (setq textString (strcat (rtos BEARING 2 0) " %%d"))
          (setq blockRotation (- 90 bearing))
        )
      )
      (if (> bearing 180)
        (progn
          (setq textString (strcat (rtos (- BEARING 180) 2 0) " %%d"))
          (setq blockRotation (+ 180 (- 90 bearing)))
        )
      )
      (command "-insert" blockName insertPt sPage3 sPage3 blockRotation textString)
    )
    (progn  ; start else normal way
      (setq textString (strcat (rtos BEARING 2 0) " %%d"))
      (setq blockRotation (- 90 bearing))
      (command "-insert" blockName insertPt sPage3 sPage3 "0" textString)
    )
  )

;;; PROBABLY EASIER TO FIX USING A BLOCK WITH 1 ATTRIBUTE FOR THE #


  (SETVAR "Expert" ex)

;reset block insert unit vars
  (resetBlockUnitsVars)


)
; end proBearing





















(defun printGrade (pt1 pt2 tsize side textEnding / pt1x pt2x pt1y pt2y percentGrade ang insertPoint)

  (setq pt1x (car pt1))
  (setq pt2x (car pt2))

  (setq pt1y (cadr pt1))
  (setq pt2y (cadr pt2))

  (if (/= (- pt2x pt1x) 0)
    (setq percent (* (/ (- pt2y pt1y) (- pt2x pt1x)) 100))
    (setq percent 0.0)
  )
  (if (< percent 0.0) (setq percent (* -1 percent)))

  (setq percentGrade (strcat (rtos percent 2 1) textEnding))

	; If < 1.0 % Make percentGrade a string with a leading 0 and add textEnding string
  (if (< percent 1.0)
    (progn
      (setq percentGrade (strcat "0" (rtos percent 2 1) textEnding))
    )          ; end progn
  )            ; end if

  (if (< percent 0.1)
    (progn
      (setq percentGrade (strcat "0." (rtos percent 2 1) textEnding))
    )          ; end progn
  )            ; end if

  (if (= percent 0.0)
    (progn
      (if (= pt1x pt2x)
        (setq percentGrade "Vertical")
      )
      (if (= pt1y pt2y)
        (setq percentGrade "Horizontal")
      )
    )
  )

  (setq ang (angle pt1 pt2))

  (setq insertPoint (polar pt1 ang (/ (distance pt1 pt2) 2.0)))

  (if (= side "B")
    (progn
      (setq insertPoint (polar insertPoint (- ang (/ pi 2.0)) 1.5))
      (command "text" "J" "TC" insertPoint tsize (* ang (/ 180.0 pi)) percentGrade)
    )
  )


  (if (= side "T")
    (progn
      (setq insertPoint (polar insertPoint (+ ang (/ pi 2.0)) 1))
      (command "text" "J" "BC" insertPoint tsize (* ang (/ 180.0 pi)) percentGrade)
    )
  )

)
;;;;;;; end printGrade
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






(defun depthOfFill (/ DIMASZ_DEF DIMTXT_DEF down left up right pt ptText offset str rad)

   ; if this is true than this is a re-opened document
  (if (and (= u1 nil) (tblsearch "block" "page3_guide"))
    (progn
      (GetUserVars)
      (setScales)
      (xDataSystemStringLoad)
    )
  )


  (if (/= u1 nil)
    (progn

      (SETQ DIMASZ_DEF (GETVAR "DIMASZ"))
      (SETVAR "DIMASZ" (* page3TextSize scaleArrowSize))
      (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
      (SETVAR "DIMTXT" page3TextSize)

      ;;;--- Set default directions
      (setq down %DOWN)
      (setq up %UP)
      (setq right %RIGHT)
      (setq left %LEFT)

      (setq offsetX 15.0)
      (setq offsetY (/ offsetX 3.0))

      (setq pt (polar c_u1 left offsetX))
      (setq pt (polar pt up offsetY))
      (setq str (- (car (cdr fill_top)) u1))
      (if (< str 0) (setq str 0))
      (setq str (strcat (rtos str 2 0) "\""))
      (COMMAND "LEADER" c_u1 PT "" str "FILL" "")

      (setq pt (polar c_u2 right offsetX))
      (setq pt (polar pt up offsetY))
      (setq str (- (car (cdr fill_top)) u2))
      (if (< str 0) (setq str 0))
      (setq str (strcat (rtos str 2 0) "\""))
      (COMMAND "LEADER" c_u2 PT "" str "FILL" "")

      ; bottom left
      (setq pt (polar c_d1 left offsetX))
      (setq pt (polar pt down offsetY))
      (setq str (- (car (cdr fill_bot)) d1))
      (if (< str 0) (setq str 0))
      (setq str (strcat (rtos str 2 0) "\""))
      (COMMAND "LEADER" c_d1 PT "" str "FILL" "")

      ; bottom right
      (setq pt (polar c_d2 right offsetX))
      (setq pt (polar pt down offsetY))
      (setq str (- (car (cdr fill_bot)) d2))
      (if (< str 0) (setq str 0))
      (setq str (strcat (rtos str 2 0) "\""))
      (COMMAND "LEADER" c_d2 PT "" str "FILL" "")

      (setq pt (polar pt right (* page3TextSize 3.0)))
      (setq ptText (polar pt right (* page3TextSize 3.0)))
      (COMMAND "LEADER" pt ptText "" "Depth of Fill" "Required" "")

      (SETVAR "DIMASZ" DIMASZ_DEF)
      (SETVAR "DIMTXT" DIMTXT_DEF)
    )
    (Alert "Design Information Not Found")
  )
)
;; END depthOfFill




(defun elevationCorners (/ blockLeftorRight DIMASZ_DEF DIMTXT_DEF pt ptText offsetX offsetY temp str rad)
;PURPOSE: Prints elevations at four corners of the system

       (if (= c_u1 nil) (xDataSystemStringLoad))
       (if (/= c_u1 nil) (GetUserVars))
       (if (/= c_u1 nil) (setScales))


  (if (/= c_u1 nil)
    (progn

      ;sets block insert unit vars
      (setBlockUnitsVars)
      (SETQ DIMASZ_DEF (GETVAR "DIMASZ"))
      (SETVAR "DIMASZ" (* page3TextSize scaleArrowSize))
      (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
      (SETVAR "DIMTXT" page3TextSize)

      ;;;--- Set default directions
      (setq down %down)
      (setq up %up)
      (setq right %right)
      (setq left %left)

      (setq blockLeft (strcat $septicadPathBlockRequired "elevation_left.dwg"))
      (setq blockRight (strcat $septicadPathBlockRequired "elevation_right.dwg"))


      ; Distance moved from corner
      (setq offsetX 15.0)
      (setq offsetY (/ offsetX 3.0))

      ;;;;; ---- TOP LEFT
      (setq pt (polar c_u1 left offsetX))
      (setq pt (polar pt up offsetY))
      (setq str (strcat (rtos u1 2 0) "\""))
      ; if 3 DIGITS
      (if (>= (ABS (/ u1 10.0)) 10.0) (setq rad (* 1.6 page3TextSize)))
      ; if 2 DIGITS
      (if (< (ABS (/ u1 10.0)) 10.0) (setq rad (* 1.3 page3TextSize)))
      (setq temp (polar pt right rad))
      (if (> (car c_u1) (car temp))
        (setq blockLeftorRight blockLeft)
        (setq blockLeftorRight blockRight)
      )
      (command "leader" C_U1 temp "" "" "BLOCK" blockLeftorRight temp (/ page3Scale 10) (/ page3Scale 10) 0 str)


      ;;;;; ----  TOP RIGHT
      (setq pt (polar c_u2 right offsetX))
      (setq pt (polar pt up offsetY))
      (setq str (strcat (rtos u2 2 0) "\""))
      ; if 3 DIGITS
      (if (>= (ABS (/ u2 10.0)) 10.0) (setq rad (* 1.6 page3TextSize)))
      ; if 2 DIGITS
      (if (< (ABS (/ u2 10.0)) 10.0) (setq rad (* 1.3 page3TextSize)))
      (setq temp (polar pt left rad))
      (if (> (car c_u1) (car temp))
        (setq blockLeftorRight blockLeft)
        (setq blockLeftorRight blockRight)
      )
      (command "leader" C_U2 temp "" "" "BLOCK" blockLeftorRight temp (/ page3Scale 10) (/ page3Scale 10) 0 str)

      ;;;;; ----  BOTTOM LEFT
      (setq pt (polar c_d1 left offsetX))
      (setq pt (polar pt down offsetY))
      (setq str (strcat (rtos d1 2 0) "\""))
      ; if 3 DIGITS
      (if (>= (ABS (/ d1 10.0)) 10.0) (setq rad (* 1.6 page3TextSize)))
      ; if 2 DIGITS
      (if (< (ABS (/ d1 10.0)) 10.0) (setq rad (* 1.3 page3TextSize)))
      (setq temp (polar pt right rad))
      (if (> (car c_u1) (car temp))
        (setq blockLeftorRight blockLeft)
        (setq blockLeftorRight blockRight)
      )
      (command "leader" C_D1 temp "" "" "BLOCK" blockLeftorRight temp (/ page3Scale 10) (/ page3Scale 10) 0 str)

      ;;;;; ----  BOTTOM RIGHT
      (setq pt (polar c_d2 right offsetX))
      (setq pt (polar pt down offsetY))
      (setq str (strcat (rtos d2 2 0) "\""))
      ; if 3 DIGITS
      (if (>= (ABS (/ d2 10.0)) 10.0) (setq rad (* 1.6 page3TextSize)))
      ; if 2 DIGITS
      (if (< (ABS (/ d2 10.0)) 10.0) (setq rad (* 1.3 page3TextSize)))
      (setq temp (polar pt left rad))
      (if (> (car c_u1) (car temp))
        (setq blockLeftorRight blockLeft)
        (setq blockLeftorRight blockRight)
      )
      (command "leader" C_D2 temp "" "" "BLOCK" blockLeftorRight temp (/ page3Scale 10) (/ page3Scale 10) 0 str)

      ;;;;; ----  Print label for elevations on bottom right corner
      (setq pt (polar pt right rad))
      (setq ptText (polar pt right (* page3TextSize 6)))
      (COMMAND "LEADER" pt ptText "" "Existing Grade" "Elevations" "")


      (SETVAR "DIMASZ" DIMASZ_DEF)
      (SETVAR "DIMTXT" DIMTXT_DEF)
;reset block insert unit vars
      (resetBlockUnitsVars)
    )
    (Alert "Design Information Not Found")
  )



)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun elevationTable (/ cpage3 point insertPoint)

;sets block insert unit vars
  (setBlockUnitsVars)

  (if (= c_u1 nil) (xDataSystemStringLoad))
  (if (/= c_u1 nil) (GetUserVars))
  (if (/= c_u1 nil) (setScales))


  (if (/= c_u1 nil)
    (progn

      ; determined center of page 3 map
      (setq cPage3 (polar page3BaseMap (angle c_U1 c_D2) (/ (distance c_u1 c_D2) 2.0)))

      ; create point by moving horiz then vertical
      (setq insertPoint (polar cPage3 pi (* (/ page3Scale 10) 33.5)))
      (SETQ insertPoint (polar insertPoint (/ pi 2.0) (* (/ page3Scale 10) 7.0)))

      ; 1 = yes
      (if (= %RotateSystem 1)
        (progn
          (setq blockRotation (- 90 BEARING))
          (if (< bearing 180)
            (setq blockName (strcat $septicadPathBlockRequired "EXISTING-GRADE0-179.dwg"))
            (setq blockName (strcat $septicadPathBlockRequired "EXISTING-GRADE180-360.dwg"))
          )
        )
      )

      ; 0 = no
      (if (= %RotateSystem 0)
        (progn
          (setq blockRotation 0)
          (setq blockName (strcat $septicadPathBlockRequired "EXISTING-GRADE0-179.dwg"))
        )
      )

      (command "_-INSERT" blockName insertPoint (/ page3Scale 10) (/ page3Scale 10) blockRotation (strcat (rtos u1 2 0) "\"") (strcat (rtos u2 2 0) "\"") (strcat (rtos d1 2 0) "\"") (strcat (rtos d2 2 0) "\""))

    )
    (Alert "Design Information Not Found")
  )

;reset block insert unit vars
  (resetBlockUnitsVars)


)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

















































;;; strzero - redone for version 5 to work with autocad correctly 
(defun strZero (num ending)
  (setq strZ (strcat (dropTrailingZero num) ending))
)
; end strZero





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun DEFAULT_ERROR (msg)

  (if (= msg "rejected function")
    (prompt (strcat "\nSeptiCAD Error - Invalid Input - A point or mouse click was required - " msg))
  )

  (if (= msg "bad argument type")
    (prompt (strcat "\nSeptiCAD Error - Invalid Input - A point or mouse click was required - " msg))
  )

   ; reset error function	
  (setq *error* lastErrorFunction)
  (princ)
)
; end DEFAULT_ERROR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun HHE200_ERROR (msg)
  (alert (strcat "Error Printing HHE-200 Pages " msg))

   ; reset error function	
  (setq *error* lastErrorFunction)
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun PAGENOTES_ERROR (msg)
  (alert (strcat "Error Printing Notes " msg))

   ; reset error function	
  (setq *error* lastErrorFunction)
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ERPHRP_ERROR (msg)
  (alert (strcat "Error inserting Swing Tie information " msg))

   ; reset error function	
  (setq *error* lastErrorFunction)
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;














;;; this function needs to attached xdata squentiall with the right dataType # to PAGE3_GUIDE
(defun xDataSystemString (ent tag)

  (setq e (entget ent))

    ;create an list of xdata list
  (setq e1 (list "SEPTICAD"
                 (cons 1000 tag)
                 (cons 1040 U1)
                 (cons 1040 U2)
                 (cons 1040 D1)
                 (cons 1040 D2)
                 (cons 1040 U1_F)
                 (cons 1040 U2_F)
                 (cons 1040 D1_F)
                 (cons 1040 D2_F)
                 (cons 1010 fill_top)
                 (cons 1010 fill_bot)
                 (cons 1040 angleBot)
                 (cons 1040 angleTop)
                 (cons 1040 sys_w)
                 (cons 1040 sys_L)
                 (cons 1040 chamber_W)
                 (cons 1040 chamber_H)
                 (cons 1010 row_start)
                 (cons 1010 row_end)
                 (cons 1040 row_sep)
                 (cons 1000 sys_type)
                 (cons 1040 fill_above)
                 (cons 1010 C_U1)
                 (cons 1010 C_U2)
                 (cons 1010 C_D1)
                 (cons 1010 C_D2)
                 (cons 1010 T_U1)
                 (cons 1010 T_U2)
                 (cons 1010 T_D1)
                 (cons 1010 T_D2)

                 (cons 1040 sPage2)
                 (cons 1040 sPage3)
                 (cons 1040 sProfile)
                 (cons 1010 cPage2)
                 (cons 1010 cPage3)
                 (cons 1010 cProfile)
                 (cons 1040 page2scale)
                 (cons 1040 page3scale)
                 (cons 1040 profilescale)
                 (cons 1040 num_rows)
                 (cons 1040 num_units)
                 (cons 1010 page2BaseMap)
                 (cons 1010 page3BaseMap)
                 (cons 1040 %LEFT)
                 (cons 1040 %RIGHT)
                 (cons 1040 %UP)
                 (cons 1040 %DOWN)
                 (cons 1040 %RotateSystem)
                 (cons 1040 bearing)
           )
  )

     ; Main design window DCL tile reload list
  (foreach item $septicadMainDesign
    (progn
      (setq e1 (append e1 (list (cons 1000 item))))
    )
  )

     ; Compound Grade window DCL tile reload lists
  (foreach item gradeLength
    (progn
      (setq e1 (append e1 (list (cons 1000 item))))
    )
  )
  (foreach item gradeElevation
    (progn
      (setq e1 (append e1 (list (cons 1000 item))))
    )
  )

    ; add xdata DXF code
  (setq e1 (list (list -3 e1)))

    ;append to to the main list
  (setq e (append e e1))

    ;modify the entity
  (entmod e)

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xDataSystemStringLoad (/ xdataList dataList xDataItem)


  ;get all the SEPTICAD xData in the drawing
  (if (setq a (ssget "x" '((-3 ( "SEPTICAD")))))
    (progn

;;; NOW loop through and change text for scales
      (setq lg (sslength a))
      (setq cnt 0)

      ;repeat for the number of entities SEPTICAD xData
      (repeat lg

        ;get the entity name
        (setq ent (ssname a cnt))

        ;get the entity list
        (setq dxfList (entget ent '( "SEPTICAD")))

        (setq cnt (1+ cnt))

        ;get the xdata list
        (setq d (assoc -3 dxfList))

        ;get just the xData
        (setq xDataItem (cdr (car (cdr d))))


        ;;;--- IF TEXT ON HHE-200 LAYOUT
        (if (= (cdr (car xDataItem)) "PAGE3_GUIDE")
          (progn
            (setq xDataList (assoc -3 (entget ent '( "SEPTICAD"))))
            (setq dataList (car (cdr xDataList)))


            (setq U1 (cdr (nth 2 dataList)))
            (setq U2 (cdr (nth 3 dataList)))
            (setq D1 (cdr (nth 4 dataList)))
            (setq D2 (cdr (nth 5 dataList)))
            (setq U1_F (cdr (nth 6 dataList)))
            (setq U2_F (cdr (nth 7 dataList)))
            (setq D1_F (cdr (nth 8 dataList)))
            (setq D2_F (cdr (nth 9 dataList)))

            (setq fill_top (list (cadr (nth 10 dataList)) (caddr (nth 10 dataList))))
            (setq fill_bot (list (cadr (nth 11 dataList)) (caddr (nth 11 dataList))))
            (setq angleBot (cdr (nth 12 dataList)))
            (setq angleTop (cdr (nth 13 dataList)))
            (setq sys_w (cdr (nth 14 dataList)))
            (setq sys_L (cdr (nth 15 dataList)))
            (setq chamber_W (cdr (nth 16 dataList)))
            (setq chamber_H (cdr (nth 17 dataList)))
            (setq row_start (list (cadr (nth 18 dataList)) (caddr (nth 18 dataList))))
            (setq row_end (list (cadr (nth 19 dataList)) (caddr (nth 19 dataList))))
            (setq row_sep (cdr (nth 20 dataList)))
            (setq sys_type (cdr (nth 21 dataList)))
            (setq fill_above (cdr (nth 22 dataList)))
            (setq C_U1 (list (cadr (nth 23 dataList)) (caddr (nth 23 dataList))))
            (setq C_U2 (list (cadr (nth 24 dataList)) (caddr (nth 24 dataList))))
            (setq C_D1 (list (cadr (nth 25 dataList)) (caddr (nth 25 dataList))))
            (setq C_D2 (list (cadr (nth 26 dataList)) (caddr (nth 26 dataList))))
            (setq T_U1 (list (cadr (nth 27 dataList)) (caddr (nth 27 dataList))))
            (setq T_U2 (list (cadr (nth 28 dataList)) (caddr (nth 28 dataList))))
            (setq T_D1 (list (cadr (nth 29 dataList)) (caddr (nth 29 dataList))))
            (setq T_D2 (list (cadr (nth 30 dataList)) (caddr (nth 30 dataList))))

            (setq sPage2 (cdr (nth 31 dataList)))
            (setq sPage3 (cdr (nth 32 dataList)))
            (setq sProfile (cdr (nth 33 dataList)))

            (setq cPage2 (list (cadr (nth 34 dataList)) (caddr (nth 34 dataList))))
            (setq cPage3 (list (cadr (nth 35 dataList)) (caddr (nth 35 dataList))))
            (setq cProfile (list (cadr (nth 36 dataList)) (caddr (nth 36 dataList))))

            (setq page2scale (cdr (nth 37 dataList)))
            (setq page3scale (cdr (nth 38 dataList)))
            (setq profilescale (cdr (nth 39 dataList)))
            (setq num_rows (cdr (nth 40 dataList)))
            (setq num_units (cdr (nth 41 dataList)))

            (setq page2BaseMap (list (cadr (nth 42 dataList)) (caddr (nth 42 dataList))))
            (setq page3BaseMap (list (cadr (nth 43 dataList)) (caddr (nth 43 dataList))))

            (setq %LEFT (cdr (nth 44 dataList)))
            (setq %RIGHT (cdr (nth 45 dataList)))
            (setq %UP (cdr (nth 46 dataList)))
            (setq %DOWN (cdr (nth 47 dataList)))
            (setq %RotateSystem (cdr (nth 48 dataList)))
            (setq bearing (cdr (nth 49 dataList)))


            (setq count 50)
            (setq $septicadMainDesign (list))
            ; there are 38 items in $septicadMainDesign
            (setq listEnd (+ count 38))
            (while (< count listEnd)
;  (print (cdr (nth count dataList)))
              (setq $septicadMainDesign (append $septicadMainDesign (list (cdr (nth count dataList)))))
              (setq count (+ count 1))
            )

            (setq gradeLength (list))
            ; there are 16 items in gradeLength
            (setq listEnd (+ count 16))
            (while (< count listEnd)
;  (print (cdr (nth count dataList)))
              (setq gradeLength (append gradeLength (list (cdr (nth count dataList)))))
              (setq count (+ count 1))
            )

            (setq gradeElevation (list))
            ; there are 16 items in gradeElevation
            (setq listEnd (+ count 16))
            (while (< count listEnd)
;  (print (cdr (nth count dataList)))
              (setq gradeElevation (append gradeElevation (list (cdr (nth count dataList)))))
              (setq count (+ count 1))
            )

          )
        )
      )

    )



  )      ;end repeat

  xDataList
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;  Tags "PAGE2_GUIDE" with xData needed for Mapquest and GoogleEarth 
(defun xDataSiteLocationStringLoad (/ a lg dxfList s cnt xDataItem d ent)

   ;get all the SEPTICAD xData in the drawing
  (if (setq a (ssget "x" '((-3 ( "SEPTICAD")))))
    (progn

      ;unlock layer
      (command "layer" "U" "SEPTICAD_GUIDES" "")

      (setq lg (sslength a))
      (setq cnt 0)

      ;repeat for the number of entities SEPTICAD xData
      (repeat lg

        ;get the entity name
        (setq ent (ssname a cnt))

        ;get the entity list
        (setq dxfList (entget ent '( "SEPTICAD")))

        (setq cnt (1+ cnt))

        ;get the xdata list
        (setq d (assoc -3 dxfList))

        ;get just the xData
        (setq xDataItem (cdr (car (cdr d))))


        (if (= (cdr (car xDataItem)) "PAGE2_GUIDE")
          (progn

            (setq xDataList (assoc -3 (entget ent '( "SEPTICAD"))))
            (setq dataList (car (cdr xDataList)))

            (setq mapQuestURL (cdr (nth 2 dataList)))
            (setq $septicad-latDD (cdr (nth 3 dataList)))
            (setq $septicad-LongDD (cdr (nth 4 dataList)))

          )
        )


      )  ;end repeat

      (command "layer" "LOCK" "SEPTICAD_GUIDES" "")
      (princ)

    )
  )

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;  Tags "PAGE2_GUIDE" with xData needed for Mapquest and GoogleEarth 
(defun xDataSiteLocationString (/ a lg dxfList s cnt d1 d ent)

   ;get all the SEPTICAD xData in the drawing
  (if (setq a (ssget "x" '((-3 ( "SEPTICAD")))))
    (progn

      ;unlock layer
      (command "layer" "U" "SEPTICAD_GUIDES" "")

      (setq lg (sslength a))
      (setq cnt 0)

      ;repeat for the number of entities SEPTICAD xData
      (repeat lg

        ;get the entity name
        (setq ent (ssname a cnt))

        ;get the entity list
        (setq dxfList (entget ent '( "SEPTICAD")))

        (setq cnt (1+ cnt))

        ;get the xdata list
        (setq d (assoc -3 dxfList))

        ;get just the xData
        (setq d1 (cdr (car (cdr d))))


        (if (= (cdr (car d1)) "PAGE2_GUIDE")
          (progn

            (setq e (entget ent))
            ;if not register, register it
            (if (not (tblsearch "APPID" "SEPTICAD"))
              (regapp "SEPTICAD")
            )

            ;create an list of xdata list
            (setq e1 (list (list -3 (list "SEPTICAD" (cons 1000 "PAGE2_GUIDE") (cons 1000 mapQuestURL) (cons 1040 $septicad-latDD) (cons 1040 $septicad-longDD)))))

            ;append to to the main list
            (setq e (append e e1))

            ;modify the entity
            (entmod e)

          )
        )


      )  ;end repeat

      (command "layer" "LOCK" "SEPTICAD_GUIDES" "")
      (princ)

    )
  )

)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



















(defun STONE_DRAW (base L W pipeSpacing shStone / blockRotation ex dboxLoc down up right left otherPoint plwidth text1 text2 dBoxW dBoxl pU1 pU2 pD1 pD2 p1 p2 text1 text2)

  (setq $sset (ssadd))

;sets block insert unit vars
  (setBlockUnitsVars)

    ;  lots of references to "RIGHT" etc so i did it this way
  (setq down %DOWN)
  (setq up %UP)
  (setq right %RIGHT)
  (setq left %LEFT)

    ; Variables from inches to feet
  (setq pipeSpacing (/ pipeSpacing 12.0))
  (setq shStone (/ shStone 12.0))
  (setq W (/ W 12.0))
  (setq L (/ L 12.0))


    ; set font and arrow size for page3
  (SETQ DIMASZ_DEF (GETVAR "DIMASZ"))
  (SETVAR "DIMASZ" (* page3TextSize scaleArrowSize))
  (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
  (SETVAR "DIMTXT" page3TextSize)


    ;;;; set D-box Dimensions 2.0' X 2.0' for display purposes  
  (setq dBoxW 2.0 dBoxL 2.0)

    ; creats points defining system size
  (setq c_u1 base)
  (setq c_u2 (polar base right L))
  (setq c_d1 (polar c_u1 down W))
  (setq c_d2 (polar c_u2 down W))


  (setq pU1 (polar (polar base right shStone) down shStone))
  (setq pU2 (polar pU1 right (- L (* 2 shStone))))
  (setq pD1 (polar pU1 down (- W (* 2 shStone))))
  (setq pD2 (polar pU2 down (- W (* 2 shStone))))

    ; edge of stone
  (setq plwidth 0.6)
  (COMMAND "PLINE" c_U1 "W" plwidth plwidth c_U2 c_D2 c_D1 c_U1 c_U2 "")
  (setvar "PLINEWID" 0)

    ; draw outer 4" dia. perforated pipe
  (command "PLINE" pU1 pU2 "")
  (ssadd (entlast) $sset)
  (command "PLINE" pD1 pD2 "")
  (ssadd (entlast) $sset)

    ; draw outer solid pipes
  (command "PLINE" pU1 pD1 "")
  (command "PLINE" pU2 pD2 "")

    ; if bed is >= 20' wide then draw interior 4" dia. pipes
  (setq nPipes (- (/ (- W (* 2 shStone)) pipeSpacing) 1))

  (if (/= nPipes 0)
    (progn
      (setq p1 (polar pU1 down pipeSpacing))
      (setq p2 (polar pU2 down pipeSpacing))
      (command "PLINE" p1 p2 "")
      (ssadd (entlast) $sset)

      (if (> W 10)
        (progn
          (setq count 1)
          (while (/= count nPipes)
            (setq p1 (polar p1 down pipeSpacing))
            (setq p2 (polar p2 down pipeSpacing))
            (command "PLINE" p1 p2 "")
            (ssadd (entlast) $sset)
            (setq count (+ count 1))
          )
        )
      )

    )
  )

    ; makes pipes perforated
  (modifySS $sset 6 "PERFORATED-PIPE")

    ; Draws Box for Page2 map
  (MAP_PG2 W L)

    ; Inserts Text Description of System based on Type of System
  (MAP_TEXT)

    ; Draws Fill Extension
  (MAP_FILL)

   ;;; Zoom to Page 3 Plan
  (setq cPage3 (polar page3BaseMap (angle c_U1 c_D2) (/ (distance c_u1 c_D2) 2.0)))
  (setq sPage3 (/ page3Scale 10))
  (command "ZOOM" "c" cPage3 (* sPage3 (/ (last (getvar "screensize")) 13.0)))

    ;;; START DCL DIALOG
  (setq dcl_id (load_dialog (strcat $septicadPathDCL "DBOX.dcl")))

      ;;;--- See if the dialog box is already loaded
  (if (not (new_dialog "DBOX" dcl_id))
    (progn
      (alert "The DBOX.DCL file was not found!")
      (exit)
    )
  )

      ;;;--- If an action event occurs, do this function
  (action_tile "cancel" "(setq ddiag 1)(done_dialog)")
  (action_tile "left" "(setq ddiag 2)(done_dialog)")
  (action_tile "right" "(setq ddiag 3)(done_dialog)")

  (if (= W 10) (set_tile "TEXT" "Insert a Distribution pipe on what side?"))
  (if (/= W 10) (set_tile "TEXT" "Insert a D-box and Distribution pipe on what side?"))

      ;;;--- Display the dialog box
  (start_dialog)

      ;;;--- Unload the dialog box
  (unload_dialog dcl_id)

      ;;;--- If the cancel button was pressed
  (if (= ddiag 1)
    (progn
      (Print "D-box inserted on left side of system as default.")
      (setq dboxLoc "LEFT")
    )
  )


      ;;;--- If the "left" button was pressed
  (if (= ddiag 2) (setq dboxLoc "LEFT"))

      ;;;--- If the "right" button was pressed
  (if (= ddiag 3) (setq dboxLoc "RIGHT"))
    ;;; END DCL DIALOG
    ;;; dboxLoc is now set to LEFT or RIGHT


		 ; suppresses "Block already defined. Redefined it? ALERT"
  (setq ex (getvar "Expert"))
  (setvar "Expert" 2)


      		; 1 = yes
  (if (= %RotateSystem 1)
    (progn
      (setq blockRotation (- 90 BEARING))
    )
    (setq blockRotation 0)
  )


	; insert d-box
  (if (> W 10)
    (progn

      (if (AND (= (rem nPipes 2) 0) (/= nPipes 0))
        (progn
          (setq shift (* (/ nPipes 2) pipeSpacing))
          (if (= dboxLoc "LEFT") (setq dBoxCenter (polar pU1 down shift)))
          (if (= dboxLoc "RIGHT") (setq dBoxCenter (polar pU2 down shift)))
          (COMMAND "-insert" (strcat $septicadPathBlockRequired "STONE_DBOX.DWG") dBoxCenter "1" "1" blockRotation)
        )
      )


      (if (AND (/= (rem nPipes 2) 0) (/= nPipes 0))
        (progn
          (setq shift (/ (distance pU1 pD1) 2.0))
          (if (= dboxLoc "LEFT") (setq dBoxCenter (polar pU1 down shift)))
          (if (= dboxLoc "RIGHT") (setq dBoxCenter (polar pU2 down shift)))
          (COMMAND "-insert" (strcat $septicadPathBlockRequired "STONE_DBOX.DWG") dBoxCenter "1" "1" blockRotation)
        )
      )

      (if (= nPipes 0)
        (progn
          (setq shift (/ (distance pU1 pD1) 2.0))
          (if (= dboxLoc "LEFT") (setq dBoxCenter (polar pU1 down shift)))
          (if (= dboxLoc "RIGHT") (setq dBoxCenter (polar pU2 down shift)))
          (COMMAND "-insert" (strcat $septicadPathBlockRequired "STONE_DBOX.DWG") dBoxCenter "1" "1" blockRotation)
        )
      )

      ; Label Distribution Box
      (if (= dboxLoc "LEFT") (setq p1 (polar (polar dBoxCenter left 11.0) down 8.0)))
      (if (= dboxLoc "RIGHT") (setq p1 (polar (polar dBoxCenter right 11.0) down 8.0)))
      (command "LEADER" dBoxCenter p1 "" (cdr (assoc "%P3-8%" $customSysList)) "")



    )
  )




	; Create line the will go to septic tank or pump station
  (if (/= W 10) (setq p1 dBoxCenter))

  (if (= W 10)
    (progn

      (IF (= dboxLoc "LEFT") (setq p1 (polar p1 (/ pi 2.0) pipeSpacing)))
      (IF (= dboxLoc "RIGHT") (setq p1 (polar p1 (/ pi 2.0) pipeSpacing)))
      (IF (= dboxLoc "RIGHT") (setq p1 (polar p1 0.0 (- L pipeSpacing pipeSpacing))))

      (setq shift (/ (distance pU1 pD1) 2.0))
      (if (= dboxLoc "LEFT") (setq dBoxCenter (polar pU1 down shift)))
      (if (= dboxLoc "RIGHT") (setq dBoxCenter (polar pU2 down shift)))
      (COMMAND "-insert" (strcat $septicadPathBlockRequired "STONE_DBOX.DWG") dBoxCenter "1" "1" blockRotation)
      ; Label Distribution Box
      (if (= dboxLoc "LEFT") (setq txtPt (polar (polar dBoxCenter left 11.0) down 8.0)))
      (if (= dboxLoc "RIGHT") (setq txtPt (polar (polar dBoxCenter right 11.0) down 8.0)))
      (command "LEADER" dBoxCenter txtPt "" (cdr (assoc "%P3-8%" $customSysList)) "")

    )
  )

		; reset expert var
  (setvar "Expert" ex)

  (if (= dboxLoc "LEFT")
    (progn
      (setq p2 (polar p1 left (* 2 shStone)))
      (setq p3 (polar p2 up shStone))
    )
  )

  (if (= dboxLoc "RIGHT")
    (progn
      (setq p2 (polar p1 right (* 2 shStone)))
      (setq p3 (polar p2 up shStone))
    )
  )


  (command "PLINE" p1 p2 p3 "")



  (if (= %RotateSystem 1)
    (progn

      (if (OR (>= bearing 315) (< bearing 45))
        (progn
          (setq p1 (polar pU1 right (/ (distance pU1 pU2) 4.0)))
          (setq p2 (polar (polar p1 up 20.0) left 5))
        )
      )

      (if (AND (>= bearing 45) (< bearing 135))
        (progn
          (setq p1 (polar pU2 left (/ (distance pU1 pU2) 4.0)))
          (setq p2 (polar (polar p1 up 20.0) left 5))
        )
      )
      (if (AND (>= bearing 135) (< bearing 225))
        (progn
          (setq p1 (polar pD2 left (/ (distance pD1 pD2) 4.0)))
          (setq p2 (polar p1 down 30.0))
        )
      )
      (if (AND (>= bearing 225) (< bearing 315))
        (progn
          (setq p1 (polar pU1 right (/ (distance pU1 pU2) 4.0)))
          (setq p2 (polar p1 up 20.0))
        )
      )

    )
  )




  (if (= %RotateSystem 0)
    (progn
      (setq p1 (polar pU1 right (/ (distance pU1 pU2) 4.0)))
      (setq p2 (polar p1 (/ (* 2 pi) 3.0) 20.0))
    )
  )




  (if (= W 10)
    (progn
      (setq str (cdr (assoc "%P3-6%" $customSysList)))
      (setq str (replaceString str "|NUM-PIPES|" (rtos (+ nPipes 2.0) 2 0)))
      (setq str (replaceString str "|LEN-PIPES|" (rtos (distance pD1 pD2) 2 0)))
      (setq str (replaceString str "|SPACING-PIPES|" (rtos pipeSpacing 2 0)))
    )
  )

  (if (/= W 10)
    (progn
      (setq str (cdr (assoc "%P3-7%" $customSysList)))
      (setq str (replaceString str "|NUM-PIPES|" (rtos (+ nPipes 2.0) 2 0)))
      (setq str (replaceString str "|LEN-PIPES|" (rtos (distance pD1 pD2) 2 0)))
      (setq str (replaceString str "|SPACING-PIPES|" (rtos pipeSpacing 2 0)))
    )
  )


  (command "LEADER" p1 p2 "" str "")

;reset block insert unit vars
  (resetBlockUnitsVars)

    ; reset dimension variables
  (SETVAR "DIMASZ" DIMASZ_DEF)
  (SETVAR "DIMTXT" DIMTXT_DEF)
)
;;; END STONE_DRAW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun CHAMBER_DRAW (base chamber_W chamber_L sys_L sys_W row_sep / pt1 pt2 pt3 pt4 plinewid_temp)
    ; CONVERT THESE VARIABLES TO FEET FROM INCHES
  (setq chamber_L (/ chamber_L 12.0))
  (setq chamber_W (/ chamber_W 12.0))
  (setq sys_L (/ sys_L 12.0))
  (setq sys_W (/ sys_W 12.0))
  (setq row_sep (/ row_sep 12.0))

  (SETVAR "CLAYER" "SEPTICAD")

    ;Insertion point
  (setq pt1 base)
    ;create three new points based on chamber dimensions and origin point (pt1)
  (setq pt2 (polar pt1 %RIGHT chamber_L))
  (setq pt3 (polar pt2 %UP chamber_W))
  (setq pt4 (polar pt3 %LEFT chamber_L))

    ;determine corner of the system and length and width of system
  (setq c_u1 pt4)
  (setq c_u2 (polar c_u1 %RIGHT sys_L))
  (setq c_d2 (polar c_u2 %DOWN sys_W))
  (setq c_d1 (polar c_u1 %DOWN sys_W))

    ; Draws Box for Page2 map
  (MAP_PG2 sys_w sys_L)  ; else

    ;Draw chambers and manifold piping
  (mapDrawChambers pt1 pt2 pt3 pt4)

  ; if concrete chambers then modify corners of system on page 3 so that they are at corners of stone
  (if (= (substr sys_type 5 16) "CONCRETE_CLUSTER")
    (progn
      ; adjust corners for stone beside
      (setq c_u1 (polar c_u1 %LEFT (/ stonebeside 12.0)))
      (setq c_u2 (polar c_u2 %LEFT (/ stonebeside 12.0)))
      (setq c_d1 (polar c_d1 %LEFT (/ stonebeside 12.0)))
      (setq c_d2 (polar c_d2 %LEFT (/ stonebeside 12.0)))
      (setq c_u1 (polar c_u1 %UP (/ stonebeside 12.0)))
      (setq c_u2 (polar c_u2 %UP (/ stonebeside 12.0)))
      (setq c_d1 (polar c_d1 %UP (/ stonebeside 12.0)))
      (setq c_d2 (polar c_d2 %UP (/ stonebeside 12.0)))
    )
  )
  (if (= (substr sys_type 5 15) "CONCRETE_TRENCH")
    (progn
      ; width is alread accounted for, so just change left and right;
      (setq c_u1 (polar c_u1 %LEFT 1))
      (setq c_u2 (polar c_u2 %LEFT 1))
      (setq c_d1 (polar c_d1 %LEFT 1))
      (setq c_d2 (polar c_d2 %LEFT 1))
    )
  )

  (if (= (substr sys_type 5 16) "CONCRETE_CLUSTER")
    (if (> stoneBeside 0) (clusterConcreteDrawStoneAround))
  )


    ; Inserts Text Description of System based on Type of System
  (MAP_TEXT)

    ; Draws Fill Extension
  (MAP_FILL)

)
; end of FUNCTION MAP_DRAW

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun clusterConcreteDrawStoneAround (/ offset p1 p2 p3 p4 interiorPointList exteriorPointList hatchScale hatchLineweight hatchName)
  (setq offset (/ stoneBeside 12.0))

  (setq exteriorPointList (list (car c_u1) (cadr c_u1) (car c_u2) (cadr c_u2) (car c_d2) (cadr c_d2) (car c_d1) (cadr c_d1)))

  ; draw pl, offset by stonebeside, add to sset
  (setq p1 (polar (polar c_u1 %DOWN offset) %RIGHT offset))
  (setq p2 (polar (polar c_u2 %DOWN offset) %LEFT offset))
  (setq p3 (polar (polar c_d2 %UP offset) %LEFT offset))
  (setq p4 (polar (polar c_d1 %UP offset) %RIGHT offset))

  (setq interiorPointList (list (car p1) (cadr p1) (car p2) (cadr p2) (car p3) (cadr p3) (car p4) (cadr p4)))
  (if (= (strcase (getvar "PRODUCT")) "BRICSCAD")
    (setq hatchScale 3)
    (setq hatchScale 1.5)
  )

  (setq hatchLineweight 5)
  (setq hatchName (gravelHatchName))

  (hatchInnerOuterPlines interiorPointList exteriorPointList hatchScale hatchlineweight hatchName)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hatchInnerOuterPlines (interiorPointList exteriorPointList hatchScale hatchLineweight hatchName / acdoc acspc hobj obj1 obj2)

  (vl-load-com)

  (setq acdoc (vla-get-activedocument (vlax-get-acad-object))
        acspc (vlax-get-property acdoc (if (= 1 (getvar 'CVPORT)) 'paperspace 'modelspace))
  )

;; Create the exterior polyline

  (setq obj1
        (vla-addlightweightpolyline acspc
                                    (vlax-make-variant
                                      (vlax-safearray-fill (vlax-make-safearray vlax-vbdouble '(0 . 7))
                                                           interiorPointList
                                      )
                                    )
        )
  )
  (vla-put-closed obj1 :vlax-true)

;; Create the interior polyline
  (setq obj2
        (vla-addlightweightpolyline acspc
                                    (vlax-make-variant
                                      (vlax-safearray-fill (vlax-make-safearray vlax-vbdouble '(0 . 7))
                                                           exteriorPointList
                                      )
                                    )
        )
  )
  (vla-put-closed obj2 :vlax-true)


;; Add the Hatch Object:
  (setq hobj (vla-addhatch acspc achatchpatterntypepredefined hatchName :vlax-true achatchobject))

;; The Hatch Object is currently volatile, the next step is important:
  (vla-appendouterloop hobj
                       (vlax-make-variant
                         (vlax-safearray-fill
                           (vlax-make-safearray vlax-vbobject '(0 . 0))
                           (list obj1)
                         )
                       )
  )

;; Add the void:

  (vla-appendinnerloop hobj
                       (vlax-make-variant
                         (vlax-safearray-fill
                           (vlax-make-safearray vlax-vbobject '(0 . 0))
                           (list obj2)
                         )
                       )
  )

  (vla-put-patternscale hobj hatchScale)
  (vla-put-lineweight hobj hatchLineweight)
;; Finished manipulation of the Hatch boundary, time to evaluate:
  (vla-evaluate hobj)

  (vla-put-lineweight obj2 hatchLineweight)
  (vla-put-lineweight obj1 hatchLineweight)

)
; end defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun mapDrawRow (pnt1 pnt2 pnt3 pnt4 / count topLeft $sset u1 u2 d1 d2)




;;;;; start subroutines;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun trenchConcreteDrawStoneAround (a b c d / offset p1 p2 p3 p4 lw hatchScale hatchlineweight hatchName)
  (setq offset 1)

 ; draw pl, add to sset
  (setq interiorPointList (list (car a) (cadr a) (car b) (cadr b) (car d) (cadr d) (car c) (cadr c)))

  ; draw pl, offset by stonebeside, add to sset
  (setq p1 (polar (polar a %UP offset) %LEFT offset))
  (setq p2 (polar (polar b %UP offset) %RIGHT offset))
  (setq p3 (polar (polar d %DOWN offset) %RIGHT offset))
  (setq p4 (polar (polar c %DOWN offset) %LEFT offset))

  (setq exteriorPointList (list (car p1) (cadr p1) (car p2) (cadr p2) (car p3) (cadr p3) (car p4) (cadr p4)))

  (if (= (strcase (getvar "PRODUCT")) "BRICSCAD")
    (setq hatchScale 3)
    (setq hatchScale 1.5)
  )

  (setq hatchLineweight 5)
  (setq hatchName (gravelHatchName))

  (hatchInnerOuterPlines interiorPointList exteriorPointList hatchScale hatchlineweight hatchName)

)


;;; end subroutines;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






  (setq count 0)

; first chamber on left side of row        
; pnt4 pnt3
; pnt1 pnt2 

       ;  adjust points to work with trench system
  (if (= (substr sys_type 5 15) "CONCRETE_TRENCH")
    (progn
      (setq pnt1 (polar pnt1 %up 1))
      (setq pnt2 (polar pnt2 %up 1))
      (setq pnt3 (polar pnt3 %down 1))
      (setq pnt4 (polar pnt4 %down 1))
    )
  )

  (setq topLeft pnt4)

  (while (< count num_units)
    (progn
      (command "PLINE" pnt1 pnt2 pnt3 pnt4 pnt1 "")
      (setq pnt1 (polar pnt1 %RIGHT chamber_L))
      (setq pnt2 (polar pnt2 %RIGHT chamber_L))
      (setq pnt3 (polar pnt3 %RIGHT chamber_L))
      (setq pnt4 (polar pnt4 %RIGHT chamber_L))

      (if (AND
            (= count (- num_units 1))
            (= (substr sys_type 5 15) "CONCRETE_TRENCH")
          )
        (progn
          (setq u1 topLeft)
          (setq d1 (polar topLeft %down (- chamber_w 2.0)))
          (setq u2 (polar u1 %right (- sys_L 2.0)))
          (setq d2 (polar d1 %right (- sys_L 2.0)))
          (trenchConcreteDrawStoneAround u1 u2 d1 d2)
        )
      )

      (setq count (+ count 1))
    )
  )


;;;;;;; draw endcaps test code


	;$endCapChambers is a list defined in createSysList
  (if (member sys_type $endCapChambers)
    (progn

;(alert " choose end cap type if necessary, leave variable for endcap choice so system length is calculated correctly")

      ;; end cap points on left side of row (p1 p2 series) or right side of row (p3 and p4 series)
      ;;; p1u p2u ------------p3u    p4u
      ;;; p1uu                       p4uu
      ;; p1c                            p4c
      ;;; p1dd                       p4dd  
      ;;; p1d p2d-------------p3d    p4d

      ; these length adjust how the arc is drawn	
      (setq tweakLen (* 0.4 endCapLen))
      (setq tweakWid (* 0.1 chamber_W))

      (setq p2u topLeft)
      (setq p2d (polar topLeft %down chamber_w))
      (setq p1u (polar p2u %left tweakLen))
      (setq p1uu (polar p1u %down tweakWid))
      (setq p1d (polar p2d %left tweakLen))
      (setq p1dd (polar p1d %up tweakWid))
      (setq p1c (polar (polar p2d %left endCapLen) %UP (* 0.5 chamber_w)))
      (command "PLINE" p2u p2d p1d p1dd "A" "S" p1c p1uu "L" p1u "CL")

      (setq p3u (polar p2u %right sys_L))
      (setq p3d (polar p2d %right sys_L))
      (setq p4u (polar p3u %right tweakLen))
      (setq p4uu (polar p4u %down tweakWid))
      (setq p4d (polar p3d %right tweakLen))
      (setq p4dd (polar p4d %up tweakWid))
      (setq p4c (polar (polar p3d %right endCapLen) %UP (* 0.5 chamber_w)))
      (command "PLINE" p3u p3d p4d p4dd "A" "S" p4c p4uu "L" p4u "CL")
    )
  )


    ; draw perforated pipe if eljens
  (if (or
        (= SYS_TYPE "ELJEN")
        (= SYS_TYPE "ELJEN_INVERTED")
        (= SYS_TYPE "TRENCH2")
        (= SYS_TYPE "TRENCH3")
      )
    (progn
      (setq $sset (ssadd))
      (setq pipe1 (polar topLeft %DOWN (* chamber_W 0.5)))
      (setq pipe2 (polar pipe1 %RIGHT sys_L))
      (command "PLINE" pipe1 pipe2 "")
      (ssadd (entlast) $sset)
      (modifySS $sset 6 "PERFORATED-PIPE")
    )
  )

  (if (= SYS_TYPE "ENVIRO")
    (progn
      (setq ptA (polar pt1 %RIGHT 10.0))
      (setq ptB (polar pt4 %RIGHT 10.0))
      (setq cnt 0)
      (while (< cnt (fix (/ chamber_L 10.0)))
        (command "PLINE" ptA ptB "")
        (setq ptA (polar ptA %RIGHT 10.0))
        (setq ptB (polar ptB %RIGHT 10.0))
        (setq cnt (1+ cnt))
      )
    )
  )













)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MAP_PG2 (sys_W sys_L)
       (setq t_u1 page2BaseMap)

       (setq t_u2 (polar t_u1 %RIGHT sys_L))
       (setq t_d2 (polar t_u2 %DOWN sys_W))
       (setq t_d1 (polar t_u1 %DOWN sys_W))

       (command "PLINE" t_u1 t_u2 t_d2 t_d1 t_u1 "")
)
; end MAP_PG2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MAP_TEXT (/ TEXT_ANG total_l total_string title_string row_string dim_string DIMASZ_DEF DIMTXT_DEF arrowPage2 arrowPage3 textPage2 textPage3)



;;; finally print the leaders

       (SETQ DIMASZ_DEF (GETVAR "DIMASZ"))
       (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))


  (if (= %RotateSystem 1)
    (progn
      (SETQ textDist (+ (* page2TextSize 5) (/ sys_L 24.0)))
      (SETQ textAng (/ pi 3))
      (setq arrowPage2 (polar t_u1 %RIGHT (/ (distance t_u1 t_u2) 2)))
      (setq arrowPage2 (polar arrowPage2 %DOWN (/ (distance t_u1 t_d1) 2)))
      (setq textPage2 (polar arrowPage2 0 textDist))


      ;arrowPage3 textPage3
      (if (OR (>= bearing 315) (< bearing 45))
        (progn
          (setq arrowPage3 (polar c_D2 %LEFT 5))
          (setq arrowPage3 (polar arrowPage3 %UP 0.2))
          (setq textPage3 (polar arrowPage3 %DOWN (* page3TextSize 15)))
        )
      )

      (if (AND (>= bearing 45) (< bearing 135))
        (progn
          (setq arrowPage3 (polar c_U2 %LEFT 5))
          (setq arrowPage3 (polar arrowPage3 %DOWN 0.2))
          (setq textPage3 (polar arrowPage3 %UP (* page3TextSize 10)))
        )
      )
      (if (AND (>= bearing 135) (< bearing 225))
        (progn
          (setq arrowPage3 (polar c_U1 %RIGHT 5))
          (setq arrowPage3 (polar arrowPage3 %DOWN 0.2))
          (setq textPage3 (polar arrowPage3 %UP (* page3TextSize 10)))
        )
      )
      (if (AND (>= bearing 225) (< bearing 315))
        (progn
          (setq arrowPage3 (polar c_U1 %RIGHT 5))
          (setq arrowPage3 (polar arrowPage3 %DOWN 0.2))
          (setq textPage3 (polar arrowPage3 %UP (* page3TextSize 10)))
        )
      )

    )
  )


  (if (= %RotateSystem 0)
    (progn

      (SETQ textDist 20)
      ;(SETQ textAng 1.55)

      (setq arrowPage2 (polar t_u1 %RIGHT (/ (distance t_u1 t_u2) 2)))
      (setq arrowPage2 (polar arrowPage2 %DOWN (/ (distance t_u1 t_d1) 2)))
      (setq textPage2 (polar t_u2 0 textDist))
      (setq textPage3 (polar c_u2 1.55 textDist))

      (setq arrowPage3 (polar c_u2 %LEFT (/ (/ (distance t_u1 t_d1) 2) 5)))

      (if (/= sys_type "STONE")
        (setq arrowPage3 (polar arrowPage3 %DOWN (* chamber_W 0.5)))
      )


    )
  )




;If Linear Feet Type System
       (if (or (= SYS_TYPE "ENVIRO")
               (= SYS_TYPE "MOUNDBUSTER")
               (= SYS_TYPE "TRENCH2")
               (= SYS_TYPE "TRENCH3")
           )

         (progn
           (setq total_l (* sys_L num_rows))

           ; total length - may be decimal
           (if (= (/ (fix total_l) total_l) 1)
             (setq total_string (rtos total_l 2 0))
           )

           (if (/= (/ (fix total_l) total_l) 1)
             (setq total_string (rtos total_l 2 1))
           )

           (setq titlePage2 (cdr (assoc "%P2-1%" $customSysList)))
           (setq titlePage2 (replaceString titlePage2 "|ROWS|" (rtos num_rows 2 0)))
           (setq titlePage2 (replaceString titlePage2 "|TOTAL|" total_string))
           ;(setq titlePage2 (replaceString titlePage2 "|UNITS|" (rtos NUM_UNITS 2 0)))
           (setq titlePage2 (replaceString titlePage2 "|WIDTH|" (rtos (* sys_W 12.0) 3 2)))
           (setq titlePage2 (replaceString titlePage2 "|LENGTH|" (rtos (* sys_L 12.0) 3 2)))
           (setq titlePage2 (replaceString titlePage2 "|SYSTEM|" (cdr (assoc sys_type sysList))))

           (setq titlePage3 (cdr (assoc "%P3-1%" $customSysList)))
           (setq titlePage3 (replaceString titlePage3 "|ROWS|" (rtos num_rows 2 0)))
           (setq titlePage3 (replaceString titlePage3 "|TOTAL|" total_string))
           ;(setq titlePage3 (replaceString titlePage3 "|UNITS|" (rtos NUM_UNITS 2 0)))
           (setq titlePage3 (replaceString titlePage3 "|WIDTH|" (rtos (* sys_W 12.0) 3 2)))
           (setq titlePage3 (replaceString titlePage3 "|LENGTH|" (rtos (* sys_L 12.0) 3 2)))
           (setq titlePage3 (replaceString titlePage3 "|SYSTEM|" (cdr (assoc sys_type sysList))))

         )
       )

; End if


;;;;; if plastic chambers
       (if (or
             (= SYS_TYPE "ADS_HI_CAP")
             (= SYS_TYPE "QUICK4_STD")
             (= SYS_TYPE "ARC18")
             (= SYS_TYPE "ARC36HC")
             (= SYS_TYPE "ARC36")
             (= SYS_TYPE "ADS_STD")
             (= SYS_TYPE "INF_STD")
             (= SYS_TYPE "INF_HI_CAP")
             (= SYS_TYPE "INF_EQ24")
             (= SYS_TYPE "ELJEN")
             (= SYS_TYPE "ELJEN_INVERTED")
             (= SYS_TYPE "ADS_BIO2")
             (= SYS_TYPE "QUICK4_EQ24")
             (= SYS_TYPE "QUICK4_HI_CAP")
             (= SYS_TYPE "ARC24")
             (= SYS_TYPE "Q4_PLUS_EQ_36_LP")
             (= SYS_TYPE "Q4_PLUS_HC")
             (= SYS_TYPE "Q4_PLUS_STD")
             (= SYS_TYPE "Q4_PLUS_STD_LP")
             (= SYS_TYPE "QUICK4_EQ24LP")
             (= SYS_TYPE "QUICK4_EQ36")
           )
         (progn

           (setq titlePage2 (cdr (assoc "%P2-1%" $customSysList)))
           (setq titlePage2 (replaceString titlePage2 "|ROWS|" (rtos num_rows 2 0)))
           (setq titlePage2 (replaceString titlePage2 "|TOTAL|" (rtos (* num_units num_rows) 2 0)))
           (setq titlePage2 (replaceString titlePage2 "|UNITS|" (rtos NUM_UNITS 2 0)))
           (setq titlePage2 (replaceString titlePage2 "|WIDTH|" (rtos (* sys_W 12.0) 3 2)))
           (setq titlePage2 (replaceString titlePage2 "|LENGTH|" (rtos (* sys_L 12.0) 3 2)))
           (setq titlePage2 (replaceString titlePage2 "|SYSTEM|" (cdr (assoc sys_type sysList))))

           (setq titlePage3 (cdr (assoc "%P3-1%" $customSysList)))
           (setq titlePage3 (replaceString titlePage3 "|ROWS|" (rtos num_rows 2 0)))
           (setq titlePage3 (replaceString titlePage3 "|TOTAL|" (rtos (* num_units num_rows) 2 0)))
           (setq titlePage3 (replaceString titlePage3 "|UNITS|" (rtos NUM_UNITS 2 0)))
           (setq titlePage3 (replaceString titlePage3 "|WIDTH|" (rtos (* sys_W 12.0) 3 2)))
           (setq titlePage3 (replaceString titlePage3 "|LENGTH|" (rtos (* sys_L 12.0) 3 2)))
           (setq titlePage3 (replaceString titlePage3 "|SYSTEM|" (cdr (assoc sys_type sysList))))


           (if (member sys_type $endCapChambers)
             (progn
               (print "system lenght") (print sys_L)
               (setq titlePage2 (replaceString titlePage2 "|LENGTH-ALT|" (rtos (+ (* 12.0 2.0 endCapLen) (* sys_L 12.0)) 3 2)))
               (setq titlePage3 (replaceString titlePage3 "|LENGTH-ALT|" (rtos (+ (* 12.0 2.0 endCapLen) (* sys_L 12.0)) 3 2)))
             )
           )


;(alert "add actual system length for quick 4 and quick 4 plus as replacement string here")
         )
       )


; if Stone Bed
  (if (= sys_type "STONE")
    (progn

      (setq titlePage2 (cdr (assoc "%P2-1%" $customSysList)))
      (setq titlePage2 (replaceString titlePage2 "|WIDTH|" (rtos num_rows 2 0)))
      (setq titlePage2 (replaceString titlePage2 "|LENGTH|" (rtos num_units 2 0)))

      (setq titlePage3 (cdr (assoc "%P3-1%" $customSysList)))
      (setq titlePage3 (replaceString titlePage3 "|WIDTH|" (rtos num_rows 2 0)))
      (setq titlePage3 (replaceString titlePage3 "|LENGTH|" (rtos num_units 2 0)))

    )
  )
; End if


; if 4 x 8 concrete chambers
       (if (or (= SYS_TYPE "4x8_CONCRETE_CLUSTER")
               (= SYS_TYPE "8x4_CONCRETE_CLUSTER")
           )
         (progn

           (setq total_l (rtos (* NUM_UNITS num_rows) 2 0))

           (if (= $H20-RATED "NO")
             (progn
               (setq titlePage2 (cdr (assoc "%P2-1%" $customSysList)))
               (setq titlePage3 (cdr (assoc "%P3-1%" $customSysList)))
             )
             (progn
               (setq titlePage2 (cdr (assoc "%P2-1-H20%" $customSysList)))
               (setq titlePage3 (cdr (assoc "%P3-1-H20%" $customSysList)))
             )
           )

           (if (/= stonebeside 0)
             (progn
               (setq titlePage2 (strcat titlePage2 (cdr (assoc "%P2-2%" $customSysList))))
               (setq titlePage3 (strcat titlePage3 (cdr (assoc "%P3-2%" $customSysList))))
             )
           )

           (setq titlePage2 (strcat titlePage2 (cdr (assoc "%P2-3%" $customSysList))))
           (setq titlePage3 (strcat titlePage3 (cdr (assoc "%P3-3%" $customSysList))))

           (setq titlePage2 (replaceString titlePage2 "|ROWS|" (rtos num_rows 2 0)))
           (setq titlePage2 (replaceString titlePage2 "|TOTAL|" (rtos (* num_units num_rows) 2 0)))
           (setq titlePage2 (replaceString titlePage2 "|UNITS|" (rtos NUM_UNITS 2 0)))
           (setq titlePage2 (replaceString titlePage2 "|WIDTH|" (rtos (* sys_W 12.0) 3 2)))
           (setq titlePage2 (replaceString titlePage2 "|LENGTH|" (rtos (* sys_L 12.0) 3 2)))
           (setq titlePage2 (replaceString titlePage2 "|SYSTEM|" (cdr (assoc sys_type sysList))))
           (setq titlePage2 (replaceString titlePage2 "|STONE-BESIDE|" (rtos stonebeside 3 0)))

           (setq titlePage3 (replaceString titlePage3 "|ROWS|" (rtos num_rows 2 0)))
           (setq titlePage3 (replaceString titlePage3 "|TOTAL|" (rtos (* num_units num_rows) 2 0)))
           (setq titlePage3 (replaceString titlePage3 "|UNITS|" (rtos NUM_UNITS 2 0)))
           (setq titlePage3 (replaceString titlePage3 "|WIDTH|" (rtos (* sys_W 12.0) 3 2)))
           (setq titlePage3 (replaceString titlePage3 "|LENGTH|" (rtos (* sys_L 12.0) 3 2)))
           (setq titlePage3 (replaceString titlePage3 "|SYSTEM|" (cdr (assoc sys_type sysList))))
           (setq titlePage3 (replaceString titlePage3 "|STONE-BESIDE|" (rtos stonebeside 3 0)))


           (SETVAR "DIMASZ" (* page3TextSize scaleArrowSize))
           (SETVAR "DIMTXT" page3TextSize)

           (setq txtPt (polar (polar c_d1 (angle c_d1 c_d2) (/ (distance c_d1 c_d2) 2.0)) %DOWN 1.5))
           (COMMAND "DIM" "ro" c_d1 c_d2 c_d1 c_d2 txtPt "" "EXIT")

           (setq txtPt (polar (polar c_u2 (angle c_u2 c_d2) (/ (distance c_u2 c_d2) 2.0)) %RIGHT 1.5))
           (COMMAND "DIM" "ro" c_u2 c_d2 c_u2 c_d2 txtPt "" "EXIT")

         )
       )
; End if


; if trench concrete
       (if (or (= SYS_TYPE "4x8_CONCRETE_TRENCH")
               (= SYS_TYPE "8x4_CONCRETE_TRENCH")
           )
         (progn
           (setq total_l (rtos (* NUM_UNITS num_rows) 2 0))

           (if (= $H20-RATED "NO")
             (progn
               (setq titlePage2 (cdr (assoc "%P2-1%" $customSysList)))
               (setq titlePage3 (cdr (assoc "%P3-1%" $customSysList)))
             )
             (progn
               (setq titlePage2 (cdr (assoc "%P2-1-H20%" $customSysList)))
               (setq titlePage3 (cdr (assoc "%P3-1-H20%" $customSysList)))
             )
           )

           (setq titlePage2 (strcat titlePage2 (cdr (assoc "%P2-2%" $customSysList))))
           (setq titlePage3 (strcat titlePage3 (cdr (assoc "%P3-2%" $customSysList))))

           (setq titlePage2 (strcat titlePage2 (cdr (assoc "%P2-3%" $customSysList))))
           (setq titlePage3 (strcat titlePage3 (cdr (assoc "%P3-3%" $customSysList))))

           (setq titlePage2 (replaceString titlePage2 "|ROWS|" (rtos num_rows 2 0)))
           (setq titlePage2 (replaceString titlePage2 "|TOTAL|" (rtos (* num_units num_rows) 2 0)))
           (setq titlePage2 (replaceString titlePage2 "|UNITS|" (rtos NUM_UNITS 2 0)))
           (setq titlePage2 (replaceString titlePage2 "|WIDTH|" (rtos (* sys_W 12.0) 3 2)))
           (setq titlePage2 (replaceString titlePage2 "|LENGTH|" (rtos (* sys_L 12.0) 3 2)))
           (setq titlePage2 (replaceString titlePage2 "|SYSTEM|" (cdr (assoc sys_type sysList))))

           (setq titlePage3 (replaceString titlePage3 "|ROWS|" (rtos num_rows 2 0)))
           (setq titlePage3 (replaceString titlePage3 "|TOTAL|" (rtos (* num_units num_rows) 2 0)))
           (setq titlePage3 (replaceString titlePage3 "|UNITS|" (rtos NUM_UNITS 2 0)))
           (setq titlePage3 (replaceString titlePage3 "|WIDTH|" (rtos (* sys_W 12.0) 3 2)))
           (setq titlePage3 (replaceString titlePage3 "|LENGTH|" (rtos (* sys_L 12.0) 3 2)))
           (setq titlePage3 (replaceString titlePage3 "|SYSTEM|" (cdr (assoc sys_type sysList))))

           (SETVAR "DIMASZ" (* page3TextSize scaleArrowSize))
           (SETVAR "DIMTXT" page3TextSize)

           (setq txtPt (polar (polar c_d1 (angle c_d1 c_d2) (/ (distance c_d1 c_d2) 2.0)) %DOWN 1.5))
           (COMMAND "DIM" "ro" c_d1 c_d2 c_d1 c_d2 txtPt "" "EXIT")

           (setq txtPt (polar (polar c_u2 (angle c_u2 c_d2) (/ (distance c_u2 c_d2) 2.0)) %RIGHT 1.5))
           (COMMAND "DIM" "ro" c_u2 c_d2 c_u2 c_d2 txtPt "" "EXIT")

         )
       )
; End if




   ;Creates insertion point and inserts text pg2 
       (SETVAR "DIMASZ" (* page2TextSize scaleArrowSize))
       (SETVAR "DIMTXT" page2TextSize)
       (COMMAND "LEADER" arrowPage2 textPage2 "" titlePage2 "")

   ;Creates insertion point and inserts text pg3 
       (SETVAR "DIMASZ" (* page3TextSize scaleArrowSize))
       (SETVAR "DIMTXT" page3TextSize)
       (COMMAND "LEADER" arrowPage3 textPage3 "" titlePage3 "")


       (SETVAR "DIMASZ" DIMASZ_DEF)
       (SETVAR "DIMTXT" DIMTXT_DEF)

)
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; End MAP_TEXT
;;;;;;;;;;;;;;;;;;;;;;;;;

























(defun PRO_STONE (/ temp ch_pt1 ch_pt2 CH_TXT ch_txt down up right left nPipes pipeCenter pArrow pText p1 p2 p3 dimasz_def dimtxt_def stoneUpTop stoneDownTop)
       (SETVAR "CLAYER" "SEPTICAD")

       (SETQ DIMASZ_DEF (GETVAR "DIMASZ"))
       (SETVAR "DIMASZ" (* profileTextSize scaleArrowSize))
       (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
       (SETVAR "DIMTXT" profileTextSize)

	;sets block insert unit vars
       (setBlockUnitsVars)

       (setq down (/ (* 3 pi) 2))
       (setq up (/ pi 2))
       (setq right 0.0)
       (setq left pi)

       (setq row_end (polar row_start right sys_W))

       (setq stoneUpTop (polar row_start up chamber_H))
       (setq stoneDownTop (polar row_end up chamber_H))

	;(COMMAND "RECTANGLE" stoneUpTop row_end)
       (command "PLINE" stoneUpTop stoneDownTop row_end row_start stoneUpTop "")

       (setq nPipes (+ (/ (- sys_W (* 2 shStone)) pipeSpacing) 1))

	; determine center for first pipe
       (setq pipeCenter (polar row_start up 9.0))
       (setq pipeCenter (polar pipeCenter right shStone))
	; center of pipe 1

	; insert circle with radius = 2"
       (command "circle" pipeCenter "2")

       (setq count 1)

  (while (/= count nPipes)
    (setq pipeCenter (polar pipeCenter right pipeSpacing))
    (command "circle" pipeCenter "2")
    (setq count (+ count 1))
  )

	;;;; insert the left and right stone bed blocks
       (COMMAND "_.insert" (strcat $septicadPathChambers "STONE_LEFT.dwg") ROW_START "1" "1" "0")
       (COMMAND "_.insert" (strcat $septicadPathChambers "STONE_RIGHT.dwg") ROW_END "1" "1" "0")


       (setq pArrow (polar row_start right (/ shStone 2.0)))
       (setq pArrow (polar pArrow up (/ chamber_H 2.0)))
       (setq pText (polar pArrow 1.3 (* (/ profileScale 5.0) 65)))

	; labeling the stone itself
       (setq str (cdr (assoc "%X-1%" $customSysList)))
       (command "LEADER" pArrow pText "" str "")

       (command "pline" row_start "W" "0.5" "0.5" stoneUpTop stoneDownTop row_end "")
       (setvar "plinewid" 0)

	; label stone cover geotextile or hay
       (setq pArrow (polar stoneUpTop right 12))
       (setq pText (polar pArrow 2 (* (/ profileScale 5.0) 50)))
       (setq str (cdr (assoc "%X-4%" $customSysList)))
       (command "LEADER" pArrow pText "" str "")

  (if (> (/ sys_w 12.0) 10)
    (progn
      (setq p1 pipeCenter)
      (setq p2 (polar p1 left pipeSpacing))
      (setq p3 (polar p2 down 4.0))
      (setq pText (rtos (/ pipeSpacing 12.0) 2 0))
      (setq pText (strcat pText " ft"))
      (COMMAND "DIM" "ro" p1 p2 p1 p2 p3 pText "EXIT")
    )
  )

	;; insert Pipe label
       (setq pArrow (polar stoneUpTop right shStone))
       (setq pArrow (polar pArrow down 3.0))

	;; move down to center of pipe
       (setq pText (polar pArrow 4.3 (* (/ profileScale 5.0) 45)))
       (setq str (cdr (assoc "%X-5%" $customSysList)))
       b (command "LEADER" pArrow pText "" str "")


;;;;; Insert elevation on top and bottom of bed
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	; BOTTOM ELEVATION

       (setq UPREC 0)
        ; default precision
       (SETQ CH_PT1 ROW_START)
       (SETQ CH_PT2 (POLAR ROW_START (/ (* 13.0 PI) 12.0) (* (/ profileScale 5.0) 20)))
       (strZero (last row_start) "\"")

       (COMMAND "LEADER" CH_PT1 CH_PT2 "" strZ "")
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	; TOP ELEVATION

       (setq UPREC 1)
; default precision
       (SETQ CH_PT1 (POLAR ROW_START up chamber_H))
       (SETQ CH_PT2 (POLAR CH_PT1 (/ (* 13.0 PI) 12.0) (* (/ profileScale 5.0) 20)))
       (strZero (LAST CH_PT1) "\"")

       (COMMAND "LEADER" CH_PT1 CH_PT2 "" strZ "")

	; TOP of Left-most Pipe
       (setq pArrow (polar stoneUpTop right shStone))
       (setq pArrow (polar pArrow down 1.0))
; move down to top of pipe
       (setq pText (polar pArrow (/ pi 2.0) (* (/ profileScale 5.0) 25)))
       (strZero (last pArrow) "\"")

       (command "LEADER" pArrow pText "" strZ "")

;reset block insert unit vars
       (resetBlockUnitsVars)
; reset dimension variables
       (SETVAR "DIMASZ" DIMASZ_DEF)
       (SETVAR "DIMTXT" DIMTXT_DEF)

)
; END PRO_STONE


(defun PRO_4x8_CONCRETE_CLUSTER (/ str1 str2 chamberInletTop fillString left right up down ROW_INSERT COUNTER ELV_TXT_B TEXT_LEAD FILL_PT1 FILL_PT2 CH_PT1 CH_PT2)

       (setq down (/ (* 3.0 pi) 2.0))
       (setq up (/ pi 2.0))
       (setq right 0.0)
       (setq left pi)

	;sets block insert unit vars
       (setBlockUnitsVars)

       (SETVAR "CLAYER" "SEPTICAD")
       (SETQ DIMASZ_DEF (GETVAR "DIMASZ"))
       (SETVAR "DIMASZ" (* profileTextSize scaleArrowSize))
       (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
       (SETVAR "DIMTXT" profileTextSize)

       (setq chamberptList (list))
       (setq $chamberBlock (strcat $septicadPathChambers "4x8_CONCRETE_CLUSTER.dwg"))

	; CORRECT ROW_START BASED ON STONE BELOW
       (setq row_start (polar row_start up stoneBelow))
       (SETQ ROW_INSERT ROW_START)
       (setq chamberptList (append chamberPtList (list row_insert)))

	; CREATE LOCAL VARIABLE TO MODIFY IN WHILE LOOP
       (SETQ COUNTER 1)

	; insert first chamber
       (COMMAND "._insert" $chamberBlock ROW_START "1" "1" "0")


	;CREATES ROWS
  (while (< COUNTER NUM_ROWS)  ;

    ;RECALCULATE NEXT ROW INSERT
    (SETQ ROW_INSERT (POLAR ROW_INSERT 0.0 (+ ROW_SEP CHAMBER_W)))
    (SETQ ROW_INSERT (POLAR ROW_INSERT (/ (* 3 PI) 2) STEP))

    ;PLACE NEXT ROW
    (command "._insert" $chamberBlock ROW_INSERT "1" "1" "0")
    (SETQ COUNTER (+ COUNTER 1))
    (setq chamberptList (append chamberPtList (list row_insert)))
  )
	;end while
       (setq chamberptList (list chamberptlist))

       (SETQ ROW_END (POLAR ROW_INSERT 0.0 CHAMBER_W))

	;;;;; Insert elevation on base of chamber or stone
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	; BOTTOM ELEVATION

       (SETQ CH_PT1 (polar row_start down stoneBelow))
       (SETQ CH_PT1 (polar CH_PT1 left stoneBeside))
       (SETQ CH_PT2 (POLAR CH_PT1 (/ (* 5.0 PI) 4.0) (* (/ profileScale 5.0) 20)))
       (strZero (LAST CH_PT1) "\"")

       (COMMAND "LEADER" CH_PT1 CH_PT2 "" strZ "")
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	; TOP OF CHAMBER
       (SETQ CH_PT1 (POLAR ROW_START RIGHT (/ CHAMBER_W 4.0)))
       (SETQ CH_PT1 (POLAR CH_PT1 (/ PI 2.0) CHAMBER_H))
       (SETQ CH_PT2 (POLAR CH_PT1 (/ (* 7.0 PI) 12.0) (* (/ profileScale 5.0) 24)))
       (strZero (LAST CH_PT1) "\"")

       (COMMAND "LEADER" CH_PT1 CH_PT2 "" strZ "")


  (if (OR (/= stoneBeside 0) (/= stoneBelow 0))
    (clusterConcreteStoneHatch row_start chamber_W)
  )


	; #### Label Fill Above Chambers
       (SETQ FILL_PT1 (POLAR (POLAR ROW_END (/ PI 2) (+ CHAMBER_H FILL_ABOVE -2.0)) (ANGLE ROW_END ROW_START) (* 0.25 SYS_W)))
       (SETQ FILL_PT2 (POLAR FILL_PT1 (/ PI 3.0) (* (/ profileScale 5.0) 30)))

       (setq fillString (cdr (assoc "%X-2%" $customSysList)))
       (setq fillString (replaceString fillString "|MIN-FILL-ABOVE|" (rtos fill_above 3 0)))

       (COMMAND "LEADER" FILL_PT1 FILL_PT2 "" fillString "")

       (labelClusterConcreteStone)

       (setq chamberInletTop (rtos (+ 4.0 (last row_start) invertHeight) 2 1))
       (setq chamberInletBot (rtos (+ (last row_start) invertHeight) 2 1))

  (if (= $H20-RATED "YES")
    (progn
      (setq str (cdr (assoc "%X-1-H20%" $customSysList)))
      (setq str (replaceString str "|INLET-TOP|" chamberInletTop))
      (setq str (replaceString str "|INLET-INVERT|" chamberInletBot))
    )
  )
       (if (= $H20-RATED "NO")
         (progn
           (setq str (cdr (assoc "%X-1%" $customSysList)))
           (setq str (replaceString str "|INLET-TOP|" chamberInletTop))
           (setq str (replaceString str "|INLET-INVERT|" chamberInletBot))
         )
       )
	;#### Label Chamber (reusing points for Stone label)
       (SETQ CH_PT1 (POLAR ROW_START 0.0 (/ CHAMBER_W 2.0)))
       (setq ch_pt1 (polar ch_pt1 (/ pi 2.0) chamber_H))
       (SETQ CH_PT2 (POLAR CH_PT1 (/ pi 3.0) (* (/ profileScale 5.0) 40)))
       (COMMAND "LEADER" CH_PT1 CH_PT2 "" str "")


	;reset block insert unit vars
       (resetBlockUnitsVars)


       (SETVAR "DIMASZ" DIMASZ_DEF)
       (SETVAR "DIMTXT" DIMTXT_DEF)



)
; END PRO_4x8_CONCRETE_CLUSTER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun clusterConcreteStoneHatch (row_start X / hatchName up down left right lwTemp temp stone1 stone2 stone3 stone4 stone5 stone6 stone7 stone8)

  (setq down (/ (* 3.0 pi) 2.0))
  (setq up (/ pi 2.0))
  (setq right 0.0)
  (setq left pi)

  (setq lwTemp (getvar "CELWEIGHT"))
  (setq temp 5)
  (command "CELWEIGHT" temp)


	   ; create list of points
  (setq stone1 row_start)
  (setq stone2 (polar stone1 up chamber_H))
  (setq stone3 (polar stone2 left stoneBeside))
  (setq stone4 (polar stone3 down (+ stoneBelow chamber_H)))
  (setq stone5 (polar stone4 right (+ (* stoneBeside 2.0) (* num_rows X))))
;		(setq stone5 (polar stone4 right (+ (* stoneBeside 2.0) (* num_rows chamber_W))))
  (setq stone6 (polar stone5 up (+ stoneBelow chamber_H)))
  (setq stone7 (polar stone6 left stoneBeside))
  (setq stone8 (polar stone7 down chamber_H))

  (setq hatchName (gravelHatchName))

	        ; create hatch based on application - this should really be done in VLISP
  (if (= (strcase (getvar "PRODUCT")) "BRICSCAD")
    (progn
      (command "PLINE" stone1 stone2 stone3 stone4 stone5 stone6 stone7 stone8 "C")
      (setq ent1 (entlast))
      (command "_.hatch" "P" hatchName "10" "0" "S" ent1 "" "")
    )
  )

  (if (= (strcase (getvar "PRODUCT")) "AUTOCAD")
    (command "_.hatch" hatchName "10" "0" "" "Y" stone1 stone2 stone3 stone4 stone5 stone6 stone7 stone8 "C" "")
  )

  (command "CELWEIGHT" lwTemp)

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun labelClusterConcreteStone (/ down up left right stoneText arrowPt textPt)

   ; if true then stone a stone label is needed
  (if (> (+ stoneBeside stoneBelow) 0)
    (progn
      (setq down (/ (* 3.0 pi) 2.0))
      (setq up (/ pi 2.0))
      (setq right 0.0)
      (setq left pi)

      ; arrow and label points when there is stone below the system
      (if (> stoneBelow 0)
        (progn
          (SETQ arrowPt (POLAR ROW_END left (/ sys_w 2.0)))
          (SETQ arrowPt (POLAR arrowPt down (/ stoneBelow 2.0)))
          (SETQ textPt (POLAR arrowPt (/ (* 4.0 PI) 3.0) (* (/ profileScale 5.0) 40)))
        )
        ; else lable the side of stone
        (progn
          (SETQ arrowPt (POLAR ROW_START left (/ stoneBeside 2.0)))
          (SETQ arrowPt (POLAR arrowPt up 1))
          (SETQ textPt (POLAR arrowPt 5 (* (/ profileScale 5.0) 40)))
        )
      )
      ; #### Label Stone below
      (if (AND (/= stoneBelow 0) (= stoneBeside 0))
        (setq stoneText (cdr (assoc "%X-4%" $customSysList)))
      )

      (if (AND (/= stoneBeside 0) (= stoneBelow 0))
        (setq stoneText (cdr (assoc "%X-5%" $customSysList)))
      )

      (if (AND (/= stoneBeside 0) (/= stoneBelow 0))
        (setq stoneText (cdr (assoc "%X-6%" $customSysList)))
      )

      (setq stoneText (replaceString stoneText "|STONE-BELOW|" (rtos stoneBelow 3 0)))
      (setq stoneText (replaceString stoneText "|STONE-BESIDE|" (rtos stoneBeside 3 0)))

      (SETQ DIMASZ_DEF (GETVAR "DIMASZ"))
      (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
      (SETVAR "DIMTXT" profileTextSize)
      (SETVAR "DIMASZ" (* profileTextSize scaleArrowSize))

      (COMMAND "LEADER" arrowPt textPt "" stoneText "")

      (setq ss (ssadd (entlast)))
      ; changed mText Width
      (modifySS ss 41 (* profilescale 35.4))


      (SETVAR "DIMASZ" DIMASZ_DEF)
      (SETVAR "DIMTXT" DIMTXT_DEF)

    )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun PRO_8x4_CONCRETE_CLUSTER (/ str1 str2 chamberInletTop fillString left right up down ROW_INSERT COUNTER ELV_TXT_B TEXT_LEAD CHAMBER_NUM FILL_PT1 FILL_PT2 CH_PT1 CH_PT2)

	;sets block insert unit vars
       (setBlockUnitsVars)

       (setq down (/ (* 3.0 pi) 2.0))
       (setq up (/ pi 2.0))
       (setq right 0.0)
       (setq left pi)

       (SETVAR "CLAYER" "SEPTICAD")
       (SETQ DIMASZ_DEF (GETVAR "DIMASZ"))
       (SETVAR "DIMASZ" (* profileTextSize scaleArrowSize))
       (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
       (SETVAR "DIMTXT" profileTextSize)


       (setq chamberptList (list))
       (setq $chamberBlock (strcat $septicadPathChambers "8x4_CONCRETE_CLUSTER.dwg"))

	; CORRECT ROW_START BASED ON STONE BELOW
       (setq row_start (polar row_start up stoneBelow))
       (SETQ ROW_INSERT ROW_START)
       (setq chamberptList (append chamberPtList (list row_insert)))

	; CREATE LOCAL VARIABLE TO MODIFY IN WHILE LOOP
       (SETQ COUNTER 1)

	; insert first chamber
       (COMMAND "._insert" $chamberBlock ROW_START "1" "1" "0")

	;CREATES ROWS
  (while (< COUNTER NUM_ROWS)  ;

    ;RECALCULATE NEXT ROW INSERT
    (SETQ ROW_INSERT (POLAR ROW_INSERT 0.0 (+ ROW_SEP CHAMBER_W)))
    (SETQ ROW_INSERT (POLAR ROW_INSERT (/ (* 3 PI) 2) STEP))

    ;PLACE NEXT ROW
    (command "._insert" $chamberBlock ROW_INSERT "1" "1" "0")
    (SETQ COUNTER (+ COUNTER 1))
    (setq chamberptList (append chamberPtList (list row_insert)))
  )
       (setq chamberptList (list chamberptlist))

       (SETQ ROW_END (POLAR ROW_INSERT 0.0 CHAMBER_W))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	; BOTTOM ELEVATION

       (SETQ CH_PT1 (polar row_start down stoneBelow))
       (SETQ CH_PT1 (polar CH_PT1 left stoneBeside))
       (SETQ CH_PT2 (POLAR CH_PT1 (/ (* 5.0 PI) 4.0) (* (/ profileScale 5.0) 20)))
       (strZero (LAST CH_PT1) "\"")

       (COMMAND "LEADER" CH_PT1 CH_PT2 "" strZ "")
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	; TOP OF CHAMBER
       (SETQ CH_PT1 (POLAR ROW_START RIGHT (/ CHAMBER_W 4.0)))
       (SETQ CH_PT1 (POLAR CH_PT1 (/ PI 2.0) CHAMBER_H))
       (SETQ CH_PT2 (POLAR CH_PT1 (/ (* 7.0 PI) 12.0) (* (/ profileScale 5.0) 24)))
       (strZero (LAST CH_PT1) "\"")

       (COMMAND "LEADER" CH_PT1 CH_PT2 "" strZ "")


  (if (OR (/= stoneBeside 0) (/= stoneBelow 0))
    (clusterConcreteStoneHatch row_start chamber_W)
  )

	; #### Label Fill Above Chambers
       (SETQ FILL_PT1 (POLAR (POLAR ROW_END (/ PI 2) (+ CHAMBER_H FILL_ABOVE -2.0)) (ANGLE ROW_END ROW_START) (* 0.25 SYS_W)))
       (SETQ FILL_PT2 (POLAR FILL_PT1 (/ PI 3.0) (* (/ profileScale 5.0) 30)))

       (setq fillString (cdr (assoc "%X-2%" $customSysList)))
       (setq fillString (replaceString fillString "|MIN-FILL-ABOVE|" (rtos fill_above 3 0)))

       (COMMAND "LEADER" FILL_PT1 FILL_PT2 "" fillString "")

       (labelClusterConcreteStone)

       (setq chamberInletTop (rtos (+ 4.0 (last row_start) invertHeight) 2 1))
       (setq chamberInletBot (rtos (+ (last row_start) invertHeight) 2 1))

  (if (= $H20-RATED "YES")
    (progn
      (setq str (cdr (assoc "%X-1-H20%" $customSysList)))
      (setq str (replaceString str "|INLET-TOP|" chamberInletTop))
      (setq str (replaceString str "|INLET-INVERT|" chamberInletBot))
    )
  )
       (if (= $H20-RATED "NO")
         (progn
           (setq str (cdr (assoc "%X-1%" $customSysList)))
           (setq str (replaceString str "|INLET-TOP|" chamberInletTop))
           (setq str (replaceString str "|INLET-INVERT|" chamberInletBot))
         )
       )
	;#### Label Chamber (reusing points for Stone label)
       (SETQ CH_PT1 (POLAR ROW_START 0.0 (/ CHAMBER_W 2.0)))
       (setq ch_pt1 (polar ch_pt1 (/ pi 2.0) chamber_H))
       (SETQ CH_PT2 (POLAR CH_PT1 (/ pi 3.0) (* (/ profileScale 5.0) 40)))
       (COMMAND "LEADER" CH_PT1 CH_PT2 "" str "")

       (SETVAR "DIMASZ" DIMASZ_DEF)
       (SETVAR "DIMTXT" DIMTXT_DEF)


	;reset block insert unit vars
       (resetBlockUnitsVars)

)
; END PRO_8x4_CONCRETE_CLUSTER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;











;;;
;;;;START
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;------  CROSS SECTION LABELING  -------;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun PRO_LABEL (/ DIMASZ_DEF DIMTXT_DEF)
  ;;;--- SET DEFAULT ARROWHEAD/TEXT SIZES
  (SETQ DIMASZ_DEF (GETVAR "DIMASZ"))
  (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
  (SETVAR "DIMTXT" profileTextSize)

  ; call these two function first because arrowsize is set to zero for these functions
  (SETVAR "DIMASZ" (* profileTextSize scaleArrowSize))

   ; CALL LABELING FUNCTION BASED ON SYS_TYPE
  (PRO_LABEL_FILL_ABOVE)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SEPCIAL CALLS
  ; High Capacity or Standard Chambers - print stone note
  (if (or (= SYS_TYPE "ADS_HI_CAP")
          (= SYS_TYPE "QUICK4_HI_CAP")
          (= SYS_TYPE "QUICK4_STD")
          (= SYS_TYPE "Q4_PLUS_HC")
          (= SYS_TYPE "Q4_PLUS_STD")
          (= SYS_TYPE "ARC36HC")
          (= SYS_TYPE "ARC36")
          (= SYS_TYPE "ADS_STD")
          (= SYS_TYPE "INF_STD")
          (= SYS_TYPE "INF_HI_CAP")
      )
    (progn
      (if (= $stoneChamber "YES")
        (progn
;	  (SETQ arrowPoint (POLAR (POLAR ROW_END PI (+ CHAMBER_W 2.0)) (/ PI 3.0) (/ CHAMBER_H 4.0))) 
          (SETQ arrowPoint (POLAR (POLAR ROW_START 0.0 (* CHAMBER_W 0.95)) (/ PI 2.0) (/ CHAMBER_H 4.0)))
          (SETQ textPoint (POLAR arrowPoint 5.0 (* (/ profileScale 5.0) 50)))
          (COMMAND "LEADER" arrowPoint textPoint "" (cdr (assoc "%X-4%" $customSysList)) "")
        )
      )
    )
  )


; if concrete stone trench label the stone beside
  (if (or
        (= SYS_TYPE "4x8_CONCRETE_TRENCH")
        (= SYS_TYPE "8x4_CONCRETE_TRENCH")
      )
    (progn
      (SETQ arrowPoint (POLAR (POLAR ROW_START 0 6) (/ PI 3.0) (/ CHAMBER_H 4.0)))
      (SETQ textPoint (POLAR arrowPoint 5 (* (/ profileScale 5.0) 40)))
      (COMMAND "LEADER" arrowPoint textPoint "" (cdr (assoc "%X-4%" $customSysList)) "")
    )
  )

    ; cluster concrete is done in the simplified profile function



     ; if stardard system - the exclusions are listed, special labeling for excluded system follow
  (if (and
        (/= SYS_TYPE "ELJEN")
        (/= SYS_TYPE "ELJEN_INVERTED")
        (/= SYS_TYPE "ENVIRO")
        (/= SYS_TYPE "TRENCH2")
        (/= SYS_TYPE "TRENCH3")
        (/= sys_type "STONE")
        (/= SYS_TYPE "8x4_CONCRETE_TRENCH")
        (/= SYS_TYPE "4x8_CONCRETE_TRENCH")
      )
    (progn

      ;;;----- ELEVATION BOTTOM OF CHAMBER
      (SETQ arrowPoint (polar row_start (/ (* 3.0 PI) 2.0) stoneBelow))
      (SETQ arrowPoint (polar arrowPoint PI stoneBeside))
      (SETQ textPoint (POLAR arrowPoint (/ (* 5.0 PI) 4.0) (* (/ profileScale 5.0) 20)))
      (strZero (LAST arrowPoint) "\"")
      (COMMAND "LEADER" arrowPoint textPoint "" strZ "")

      ;;;-----ELEVATION TOP OF CHAMBER	
      (SETQ arrowPoint (POLAR ROW_START 0.0 (/ CHAMBER_W 2)))
      (SETQ arrowPoint (POLAR arrowPoint (/ PI 2.0) CHAMBER_H))
      (SETQ textPoint (POLAR arrowPoint (/ (* 2 pi) 3.0) (* (/ profileScale 5.0) 20)))
      (strZero (LAST arrowPoint) "\"")
      (COMMAND "LEADER" arrowPoint textPoint "" strZ "")

      ;#### Label Chamber
      (SETQ arrowPoint (POLAR (POLAR ROW_START 0.0 (/ CHAMBER_W 2.0)) (/ PI 2.0) CHAMBER_H))
      (setq arrowPoint (polar arrowPoint (/ (* 3.0 pi) 2.0) (/ CHAMBER_H 4.0)))
      (SETQ textPoint (POLAR arrowPoint (/ PI 3.0) (* (/ profileScale 5.0) 35)))

      (setq txt (cdr (assoc "%X-1%" $customSysList)))
      (setq txt (replaceString txt "|SYSTEM|" (cdr (assoc sys_type sysList))))
      (setq txt (replaceString txt "|INLET-TOP|" (rtos (+ 4.0 (last row_start) invertHeight) 2 1)))
      (setq txt (replaceString txt "|INLET-INVERT|" (rtos (+ (last row_start) invertHeight) 2 1)))

      (COMMAND "LEADER" arrowPoint textPoint "" txt "")
    )
  )



  (if (or
        (= SYS_TYPE "8x4_CONCRETE_TRENCH")
        (= SYS_TYPE "4x8_CONCRETE_TRENCH")
      )
    (progn

      ;;;----- ELEVATION BOTTOM OF CHAMBER
      (SETQ arrowPoint (polar row_start (/ (* 3.0 PI) 2.0) stoneBelow))
      (SETQ arrowPoint (polar arrowPoint PI stoneBeside))
      (SETQ textPoint (POLAR arrowPoint (/ (* 5.0 PI) 4.0) (* (/ profileScale 5.0) 20)))
      (strZero (LAST arrowPoint) "\"")
      (COMMAND "LEADER" arrowPoint textPoint "" strZ "")

      ;;;-----ELEVATION TOP OF CHAMBER	
      (SETQ arrowPoint (POLAR ROW_START 0.0 (/ CHAMBER_W 2)))
      (SETQ arrowPoint (POLAR arrowPoint (/ PI 2.0) CHAMBER_H))
      (SETQ textPoint (POLAR arrowPoint (/ (* 2 pi) 3.0) (* (/ profileScale 5.0) 20)))
      (strZero (LAST arrowPoint) "\"")
      (COMMAND "LEADER" arrowPoint textPoint "" strZ "")

      ;#### Label Chamber
      (SETQ arrowPoint (POLAR (POLAR ROW_START 0.0 (/ CHAMBER_W 2.0)) (/ PI 2.0) CHAMBER_H))
      (setq arrowPoint (polar arrowPoint (/ (* 3.0 pi) 2.0) (/ CHAMBER_H 4.0)))
      (SETQ textPoint (POLAR arrowPoint (/ PI 3.0) (* (/ profileScale 5.0) 40)))

      (if (= $H20-RATED "YES")
        (setq txt (cdr (assoc "%X-1-H20%" $customSysList)))
      )
      (if (= $H20-RATED "NO")
        (setq txt (cdr (assoc "%X-1%" $customSysList)))
      )

      (setq txt (replaceString txt "|SYSTEM|" (cdr (assoc sys_type sysList))))
      (setq txt (replaceString txt "|INLET-TOP|" (rtos (+ 4.0 (last row_start) invertHeight) 2 1)))
      (setq txt (replaceString txt "|INLET-INVERT|" (rtos (+ (last row_start) invertHeight) 2 1)))
      (COMMAND "LEADER" arrowPoint textPoint "" txt "")

    )
  )
      ;;;; end if concrete trench



  (if (or
        (= SYS_TYPE "ELJEN")
        (= SYS_TYPE "ELJEN_INVERTED")
      )
    (progn

      ; sand labeled in (drawChamberSandEljen)

      ;#### Label Chamber (reusing points for sand)
      (SETQ arrowPoint (POLAR (POLAR ROW_START 0.0 (+ (/ CHAMBER_W 2.0) 4)) (/ PI 2.0) (- CHAMBER_H 4)))
      (SETQ textPoint (POLAR arrowPoint (/ PI 3.0) (* (/ profileScale 5.0) 40)))


      (setq txt (cdr (assoc "%X-1%" $customSysList)))
      (setq txt (replaceString txt "|SYSTEM|" (cdr (assoc sys_type sysList))))
      (setq txt (replaceString txt "|INLET-TOP|" (rtos (+ 4.0 (last row_start) invertHeight) 2 1)))
      (setq txt (replaceString txt "|INLET-INVERT|" (rtos (+ (last row_start) invertHeight) 2 1)))
      (COMMAND "LEADER" arrowPoint textPoint "" txt "")



      ;;;----- ELEVATION BOTTOM OF CHAMBER
      (SETQ arrowPoint row_start)
      (SETQ textPoint (POLAR arrowPoint pi (* (/ profileScale 5.0) 20)))
      (strZero (LAST arrowPoint) "\"")
      (COMMAND "LEADER" arrowPoint textPoint "" strZ "")

      ;;;-----ELEVATION TOP OF CHAMBER	
      (SETQ arrowPoint (POLAR ROW_START 0.0 (/ CHAMBER_W 2)))
      (SETQ arrowPoint (POLAR arrowPoint (/ PI 2.0) CHAMBER_H))
      (SETQ textPoint (POLAR arrowPoint (/ (* 7.0 PI) 12.0) (* (/ profileScale 5.0) 30)))
      (strZero (LAST arrowPoint) "\"")
      (COMMAND "LEADER" arrowPoint textPoint "" strZ "")

      ; Label top-slope bottom of sand
      (setq arrowPoint (polar (POLAR row_start pi 9.0) (/ (* 3.0 pi) 2.0) 6))
      (setq textPoint (POLAR arrowPoint (/ (* 5.0 PI) 4.0) (* (/ profileScale 5.0) 24)))
      ; create string
      (strZero (cadr arrowPoint) "\"")
      (COMMAND "LEADER" arrowPoint textPoint "" strZ "")

      ; Label bottom-slope bottom of sand if system is sloping
      ; create point for label
      (setq arrowPoint (polar (POLAR row_end 0 9.0) (/ (* 3.0 pi) 2.0) 6))
      (setq textPoint (POLAR arrowPoint 6.0 25.0))
      ; create string
      (strZero (cadr arrowPoint) "\"")
      (COMMAND "LEADER" arrowPoint textPoint "" strZ "")
    )
  )
      ;;;; end if eljen

   ;;; start enviro
  (if (= SYS_TYPE "ENVIRO")
    (progn

      ; sand labeled in (drawChamberSandEnviro)

      ; #### Label Chambers
      (setq arrowPoint (polar row_start 0.0 (* chamber_H 0.7)))
      (setq arrowPoint (polar arrowPoint (/ pi 2.0) (* chamber_H 0.7)))
      (SETQ textPoint (POLAR arrowPoint (/ PI 3.0) (* (/ profileScale 5.0) 40)))

      (setq txt (cdr (assoc "%X-1%" $customSysList)))
      (setq txt (replaceString txt "|SYSTEM|" (cdr (assoc sys_type sysList))))
      (setq txt (replaceString txt "|INLET-TOP|" (rtos (+ 4.0 (last row_start) invertHeight) 2 1)))
      (setq txt (replaceString txt "|INLET-INVERT|" (rtos (+ (last row_start) invertHeight) 2 1)))
      (COMMAND "LEADER" arrowPoint textPoint "" txt "")

      ;;;----- ELEVATION BOTTOM OF CHAMBER
      (SETQ arrowPoint (POLAR ROW_START 0.0 (/ CHAMBER_W 2.0)))
      (SETQ textPoint (POLAR arrowPoint (/ (* 5.0 PI) 4.0) (* (/ profileScale 5.0) 20)))
      (strZero (LAST arrowPoint) "\"")
      (COMMAND "LEADER" arrowPoint textPoint "" strZ "")

      ;;;-----ELEVATION TOP OF CHAMBER	
      (SETQ arrowPoint (POLAR ROW_START 0.0 (/ CHAMBER_W 2.0)))
      (SETQ arrowPoint (POLAR arrowPoint (/ PI 2.0) CHAMBER_H))
      (SETQ textPoint (POLAR arrowPoint (/ (* 2 pi) 3.0) (* (/ profileScale 5.0) 20)))
      (strZero (LAST arrowPoint) "\"")
      (COMMAND "LEADER" arrowPoint textPoint "" strZ "")
    )
  )
   ; end enviro

     ;;;;; start stone trench labeling
  (if (or
        (= SYS_TYPE "TRENCH3")
        (= SYS_TYPE "TRENCH2")
      )
    (progn
      ;;;----- ELEVATION BOTTOM OF STONE
      (SETQ arrowPoint (polar row_start (/ (* 3.0 PI) 2.0) stoneBelow))
      (SETQ arrowPoint (polar arrowPoint PI stoneBeside))
      (SETQ textPoint (POLAR arrowPoint (/ (* 5.0 PI) 4.0) (* (/ profileScale 5.0) 20)))
      (strZero (LAST arrowPoint) "\"")
      (COMMAND "LEADER" arrowPoint textPoint "" strZ "")

      ;;;-----ELEVATION TOP OF STONE	
      (SETQ arrowPoint (POLAR ROW_START 0.0 (* CHAMBER_W 0.8)))
      (SETQ arrowPoint (POLAR arrowPoint (/ PI 2.0) CHAMBER_H))
      (SETQ textPoint (POLAR arrowPoint 1.0 (* (/ profileScale 5.0) 20)))
      (strZero (LAST arrowPoint) "\"")
      (COMMAND "LEADER" arrowPoint textPoint "" strZ "")

      ;;;----- ELEVATION top of perforated pipe
      (SETQ arrowPoint (POLAR ROW_START 0.0 (/ CHAMBER_W 2)))
      (SETQ arrowPoint (polar arrowPoint (/ pi 2) (- chamber_H 1)))
      (SETQ textPoint (POLAR arrowPoint 2.1 (* (/ profileScale 5.0) 40)))
      (strZero (LAST arrowPoint) "\"")
      (COMMAND "LEADER" arrowPoint textPoint "" (replaceString (cdr (assoc "%X-5%" $customSysList)) "|PERF-TOP|" strZ) "")

      ;#### Label Stone
      (SETQ arrowPoint (POLAR ROW_START 0.0 (* CHAMBER_W 0.2)))
      (setq arrowPoint (polar arrowPoint (/ pi 2) (* CHAMBER_H 0.2)))
      (SETQ textPoint (POLAR arrowPoint 5.1 (* (/ profileScale 5.0) 55)))

      (setq txt (cdr (assoc "%X-1%" $customSysList)))
      (setq txt (replaceString txt "|SYSTEM|" (cdr (assoc sys_type sysList))))
      (setq txt (replaceString txt "|INLET-TOP|" (rtos (+ 4.0 (last row_start) invertHeight) 2 1)))
      (setq txt (replaceString txt "|INLET-INVERT|" (rtos (+ (last row_start) invertHeight) 2 1)))
      (COMMAND "LEADER" arrowPoint textPoint "" txt "")


    )
  )
   ;;;;;; end trench


   ; no stone bed or concrete chambers
  (if (/= sys_type "STONE")
    (progn
      (if (/= isShelf "YES") (PRO_LABEL_ROW_SEPARATION)) (if (= isShelf "YES") (PRO_LABEL_ROW_SEPARATION_ON_SHELF))
      (if (and (= isShelf "YES") (> (length shelfList) 1)) (PRO_LABEL_SHELF_SEPARATION))
    )
  )

  (SETVAR "DIMASZ" DIMASZ_DEF)
  (SETVAR "DIMTXT" DIMTXT_DEF)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;;;;;;;;;;;;;;;;;;;;;;;; Label Fill Above
(defun PRO_LABEL_FILL_ABOVE (/ fillStr sandStr arrowPoint textPoint)

    ; this is for all systems except for two varaints of cluster concrete label in the PRO_* functions

	;;;--- LABEL FILL ABOVE CHAMBERS
  (SETQ arrowPoint (POLAR (POLAR ROW_END (/ PI 2) (+ CHAMBER_H FILL_ABOVE -2.0)) (ANGLE ROW_END ROW_START) (* 0.25 SYS_W)))
  (SETQ textPoint (POLAR arrowPoint (/ PI 3.0) (* (/ profileScale 5.0) 30)))

  (setq str (cdr (assoc "%X-2%" $customSysList)))
  (setq str (replaceString str "|MIN-FILL-ABOVE|" (strcat (dropTrailingZero fill_above) "\"")))
  (COMMAND "LEADER" arrowPoint textPoint "" str "")
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







;Label Center to Center
(defun PRO_LABEL_ROW_SEPARATION_ON_SHELF (/ moveUp pt1 pt2 count stopLoop txtPt)


    ; center-top of row 1 chamber 
       (setq count -1 stopLoop 0)

  (while (= stopLoop 0)
    (progn
      (setq count (+ count 1))
      (if (> (nth count shelfList) 1) (setq stopLoop 1))
      (if (> count (length shelfList)) (setq stopLoop 2))
    )
  )

  (if (= stopLoop 1)
    (progn
      (SETQ pt1 (POLAR (car (nth count chamberPtList)) (/ PI 2) CHAMBER_H))
      (setq pt1 (polar pt1 0.0 (/ chamber_W 2.0)))

      (SETQ pt2 (POLAR (cadr (nth count chamberPtList)) (/ PI 2) CHAMBER_H))
      (setq pt2 (polar pt2 0.0 (/ chamber_W 2.0)))

      (setq moveUp 2.0)
      (setq txtPt (POLAR pt1 (/ PI 2.0) moveup))
      (setq txtPt (polar txtPt 0.0 (/ (distance pt1 pt2) 2.0)))

      ;print dimension to screen center to center
      (if (or (= $defaultSpacingDim "0") (= $defaultSpacingDim "2"))
        (COMMAND "DIM" "HORIZONTAL" pt1 pt2 txtPt (strcat (rtos (+ chamber_W row_SEP) 2 0) "\"") "EXIT")
      )
      ;print dimension to screen center to center
      (if (or (= $defaultSpacingDim "1") (= $defaultSpacingDim "2"))
        (progn
          (setq pt1 (polar pt1 0.0 (/ chamber_W 2.0)))
          (setq pt1 (polar pt1 (/ (* 3 Pi) 2.0) chamber_H))
          (setq pt2 (polar pt2 pi (/ chamber_W 2.0)))
          (setq pt2 (polar pt2 (/ (* 3 Pi) 2.0) chamber_H))

          (setq txtPt (POLAR pt2 (/ (* 3 Pi) 2.0) moveUp))
          (setq txtPt (polar txtPt 0.0 (/ (distance pt1 pt2) 2.0)))

          (if (or (= sys_type "ELJEN") (= sys_type "ELJEN_INVERTED"))
            (setq txtPt (polar txtPt (/ pi 2.0) -10))
          )

          (COMMAND "DIM" "HORIZONTAL" pt1 pt2 txtPt (strcat (rtos row_SEP 2 0) "\"") "EXIT")
        )
      )

    )
  )


)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun PRO_LABEL_ROW_SEPARATION (/ moveUp pt1 pt2 count txtPt)

  (if (> num_rows 1)
    (progn
      (if (= num_rows 2)
        (progn
          (SETQ pt1 (POLAR row_start (/ PI 2) CHAMBER_H))
          (setq pt1 (polar pt1 0.0 (/ chamber_W 2.0)))
          (SETQ pt2 (POLAR (list (car pt1) (+ (nth 1 shelfElvList) chamber_H)) 0.0 (+ CHAMBER_W ROW_SEP)))
          (setq txtPt (POLAR pt1 (/ PI 2.0) (/ fill_above 2.0)))
          (setq txtPt (polar txtPt 0.0 (/ (distance pt1 pt2) 2.0)))
        )
      )

      (if (> num_rows 2)
        (progn

          (if (/= (car (caddr chamberptList)) nil)
            (progn
              (SETQ pt1 (POLAR (car (cadr chamberptList)) (/ PI 2) CHAMBER_H))
              (setq pt1 (polar pt1 0.0 (/ chamber_W 2.0)))
              (SETQ pt2 (POLAR (car (caddr chamberptList)) (/ PI 2) CHAMBER_H))
              (setq pt2 (polar pt2 0.0 (/ chamber_W 2.0)))
              (setq txtPt (POLAR pt1 (/ PI 2.0) (/ fill_above 2.0)))
              (setq txtPt (polar txtPt 0.0 (/ (distance pt1 pt2) 2.0)))
            )
          )
          (if (= (car (caddr chamberptList)) nil)
            (progn
              (SETQ pt1 (POLAR (car (car chamberptList)) (/ PI 2) CHAMBER_H))
              (setq pt1 (polar pt1 0.0 (/ chamber_W 2.0)))
              (SETQ pt2 (POLAR (car (cadr chamberptList)) (/ PI 2) CHAMBER_H))
              (setq pt2 (polar pt2 0.0 (/ chamber_W 2.0)))
              (setq txtPt (POLAR pt1 (/ PI 2.0) (/ fill_above 2.0)))
              (setq txtPt (polar txtPt 0.0 (/ (distance pt1 pt2) 2.0)))
            )
          )
        )
      )

      (setq moveUp 2.0)
      (setq txtPt (POLAR pt1 (/ PI 2.0) moveup))
      (setq txtPt (polar txtPt 0.0 (/ (distance pt1 pt2) 2.0)))

      ;print dimension to screen center to center
      (if (or (= $defaultSpacingDim "0") (= $defaultSpacingDim "2"))
        (progn
          (if (/= (substr sys_type 5 15) "CONCRETE_TRENCH")
            (COMMAND "DIM" "HORIZONTAL" pt1 pt2 txtPt (strcat (rtos (+ chamber_W row_SEP) 2 0) "\"") "EXIT")
          )
          (if (= (substr sys_type 5 15) "CONCRETE_TRENCH")
            (COMMAND "DIM" "HORIZONTAL" pt1 pt2 txtPt (strcat (rtos (+ chamber_W row_SEP) 3 2) "") "EXIT")
          )
        )
      )

      (if (or (= $defaultSpacingDim "1") (= $defaultSpacingDim "2"))
        (progn
          (setq pt1 (polar pt1 0.0 (/ chamber_W 2.0)))
          (setq pt1 (polar pt1 (/ (* 3 Pi) 2.0) chamber_H))
          (setq pt2 (polar pt2 pi (/ chamber_W 2.0)))
          (setq pt2 (polar pt2 (/ (* 3 Pi) 2.0) chamber_H))

          (setq txtPt (POLAR pt2 (/ (* 3 Pi) 2.0) moveUp))
          (setq txtPt (polar txtPt 0.0 (/ (distance pt1 pt2) 2.0)))

          (if (or (= sys_type "ELJEN") (= sys_type "ELJEN_INVERTED"))
            (setq txtPt (polar txtPt (/ pi 2) -8))
          )

          (if (/= (substr sys_type 5 15) "CONCRETE_TRENCH")
            (COMMAND "DIM" "HORIZONTAL" pt1 pt2 txtPt (strcat (rtos row_SEP 2 0) "\"") "EXIT")
          )
          (if (= (substr sys_type 5 15) "CONCRETE_TRENCH")
            (COMMAND "DIM" "HORIZONTAL" pt1 pt2 txtPt (strcat (rtos row_SEP 3 2) "") "EXIT")
          )

        )
      )

    )
  )


)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun PRO_LABEL_SHELF_SEPARATION (/ moveUp pt1 pt2 count txtPt)

    ; first row in last shelf
       (setq pt2 (car (last chamberptList)))
       (setq pt2 (polar pt2 0.0 (/ chamber_W 2.0)))
       (setq pt2 (polar pt2 (/ pi 2) chamber_H))

    ; last row in next to last shelf
       (setq pt1 (last (nth (- (length chamberptList) 2) chamberptList)))
       (setq pt1 (polar pt1 0.0 (/ chamber_W 2.0)))
       (setq pt1 (polar pt1 (/ pi 2) chamber_H))

       (setq moveUp 2)
       (setq txtPt (POLAR pt1 (/ PI 2.0) moveUp))
       (setq txtPt (polar txtPt 0.0 (/ (distance pt2 pt1) 2.0)))

    ;print dimension to screen center to center
  (if (or (= $defaultSpacingDim "0") (= $defaultSpacingDim "2"))
    (COMMAND "DIM" "HORIZONTAL" pt2 pt1 txtPt (strcat (rtos (+ chamber_W shelf_SEP) 2 0) "\"") "EXIT")
  )

  (if (or (= $defaultSpacingDim "1") (= $defaultSpacingDim "2"))
    (progn
      (setq pt1 (polar pt1 0.0 (/ chamber_W 2.0)))
      (setq pt1 (polar pt1 (/ (* 3 Pi) 2.0) chamber_H))
      (setq pt2 (polar pt2 pi (/ chamber_W 2.0)))
      (setq pt2 (polar pt2 (/ (* 3 Pi) 2.0) chamber_H))

      (setq txtPt (POLAR pt2 (/ (* 3 Pi) 2.0) moveUp))
      (setq txtPt (polar txtPt 0.0 (/ (distance pt1 pt2) 2.0)))
      (if (or (= sys_type "ELJEN") (= sys_type "ELJEN_INVERTED"))
        (setq txtPt (polar txtPt (/ pi 2) -8))
      )

      (COMMAND "DIM" "HORIZONTAL" pt2 pt1 txtPt (strcat (rtos shelf_SEP 2 0) "\"") "EXIT")
    )
  )

)
; end function




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun calculateChamberElevations (/ stepCount HYPOT nextLF firstLF horzDist)

     ; var ROW_START & LIM_TOP is set in defun STEP_CAL
  (SETQ ROW_INSERT ROW_START)
  (setq firstLF LIM_TOP)

     ;;; -- shelfList define in (maindesignsavevars)
     ;;;-- Create list of elevations for each chamber - used in defun PRO_TABLE_SHELF to print elevation table
  (setq shelfElvList (list))


     ; start if AUTO-STEP
     ;;;-- Determine Chamber Elevations - and then ask user to edit
  (if (= elevationMethod "AUTO")
    (progn
      (foreach item shelfList
        (progn
          (setq firstRow row_insert)

          ; Add elevation to list
          (setq shelfElvList (append shelfElvList (list (last firstRow))))

          ; Cycle along shelf
          (setq count 1)
          (while (and (> item 1) (< count item))
            (SETQ ROW_INSERT (POLAR ROW_INSERT 0.0 (+ ROW_SEP CHAMBER_W)))
            (setq count (+ count 1))
          )

          ; Variable is overwritten when each sequential shelf is inserted
          (SETQ ROW_END (POLAR ROW_INSERT 0.0 CHAMBER_W))

          ; move next shelf insert point horizontally
          (SETQ ROW_INSERT (POLAR ROW_INSERT 0.0 (+ ROW_SEP CHAMBER_W)))

          ; calculate the allowable step based on the height of the LF below the 
          (setq horzDist (- (car row_insert) (car firstRow)))

          ; calculate the length of the diagonal (hypotenuse) 
          ; to define the limiting factor for the next shelf

          (SETQ HYPOT (ABS (/ horzDist (COS (- PI (ANGLE LIM_TOP LIM_BOT))))))
          (SETQ nextLF (POLAR firstLF (ANGLE LIM_TOP LIM_BOT) HYPOT))

          ; setq the elevation of the next shelf
          (setq rowElevation (+ (last nextLF) lim_sep))
          (setq rowElevation (fix rowElevation))

          ;;-- Create insert point for next shelf row
          (SETQ ROW_INSERT (list (car row_insert) rowElevation))

          ;;-- Determine top of LF below ROW_INSERT
          (setq firstLF nextLF)
        )
      )
      ; end foreach
    )
  )
     ; end if AUTO-STEP

     ; start if FORCE-STEP
  (if (= elevationMethod "FORCE")
    (progn

      ; create list starting with DEPTH_STEP and going down by STEP_SIZE
      (setq stepCount 0)
      (foreach item shelflist
        (progn
          (setq shelfElvList (append shelfElvList (list (+ depth_step (* stepCount -1)))))
          (setq stepCount (+ stepCount STEP_SIZE))
        )
      )
      ; end foreach

    )
  )
     ; end if FORCE-STEP

     ;shelfElvList variable is now defined

)


;;;;;;;  Insert Chambers In Cross Section ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun PRO_SHELF (chamberBlock / shelfNum shelfCount ROW_INSERT count chamberNum chamberShelfPtList)

;sets block insert unit vars
       (setBlockUnitsVars)

       (SETVAR "CLAYER" "SEPTICAD")

       (setq shelfCount 0)
       (setq shelfNum 1)
    ; list of all shelf first and last chamber insert points - used in sand drawing functions for eljen and enviro 
       (setq chamberPtList (list))
       (setq chamberShelfPtList (list))

    ; set first chamber row
       (SETQ ROW_INSERT (list 0.0 (nth shelfCount shelfElvList)))
       (setq ROW_START ROW_INSERT chamberNum 0)

  (foreach item shelfList
    (progn
      ; insert first row of shelf
      (setq shelfNum (1+ shelfNum))
      (setq chamberNum (+ chamberNum 1))
      (setq chamberShelfPtList (append chamberShelfPtList (list ROW_INSERT)))
      (if (or (= sys_type "MOUNDBUSTER")
              (= sys_type "TRENCH2")
              (= sys_type "TRENCH3")
          )
        (COMMAND "INSERT" chamberBlock ROW_INSERT 1 1 0)  ;if
        (COMMAND "INSERT" chamberBlock ROW_INSERT 1 1 0 (rtos chamberNum 2 0))  ;else
      )

      ; Insert each row on a shelf
      (setq count 1)
      (while (and (> item 1) (< count item))
        (setq chamberNum (+ chamberNum 1))
        (SETQ ROW_INSERT (POLAR ROW_INSERT 0.0 (+ ROW_SEP CHAMBER_W)))
        (if (or (= sys_type "MOUNDBUSTER")
                (= sys_type "TRENCH2")
                (= sys_type "TRENCH3")
            )
          (COMMAND "INSERT" chamberBlock ROW_INSERT 1 1 0)  ;if
          (COMMAND "INSERT" chamberBlock ROW_INSERT 1 1 0 (rtos chamberNum 2 0))  ;else
        )
        (setq count (+ count 1))
        (setq chamberShelfPtList (append chamberShelfPtList (list ROW_INSERT)))
      )

      ; list of shelves with chamber insert point list - a list of lists

      (setq chamberPtList (append chamberPtList (list chamberShelfPtList)))
      (setq chamberShelfPtList (list))


      ; IF Moundbuster then Label Each Shelf - cooresponding label on plan view too
      (if (AND (= SYS_TYPE "MOUNDBUSTER") (= isShelf "YES"))
        (command "-mtext" (polar (polar row_insert (/ pi 4) chamber_H) 0.0 chamber_W) "J" "BL" "H" profileTextSize (polar (polar row_insert (/ pi 4) chamber_H) 0.0 chamber_W) (rtos (- shelfNum 1) 2 0) "")
      )



      (SETQ ROW_END (POLAR ROW_INSERT 0.0 CHAMBER_W))

      (setq shelfCount (+ shelfCount 1))

      (if (/= isShelf "YES")
        (setq row_insert (polar ROW_END 0.0 ROW_SEP))
      )

      (if (= isShelf "YES")
        (setq row_insert (polar ROW_END 0.0 SHELF_SEP))
      )

      (SETQ ROW_INSERT (list (car row_insert) (nth shelfCount shelfElvList)))

    )
  )


;;;;--- Variable used to define location of FILL_TOP - (i.e. above right side of last row on shelf)
       (setq topShelfWidth
             (+ (* (nth 0 shelfList) chamber_w) (* row_sep (- (nth 0 shelfList) 1)))
       )

  ;reset block insert unit vars
       (resetBlockUnitsVars)

)
; END PRO_SHELF
;######################################################################################









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;sTART
;;;;;;drawChamberSandEljen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun drawChamberSandEljen (chamberInsertPointList / DIMTXT_DEF DIMASZ_DEF sset1 shelf chamber sandBeside sandBelow ptA ptB ptC ptD sandPtTop sandPtBot sandPtBotReverse down up right left)

  (setq down (/ (* 3.0 pi) 2.0))
  (setq up (/ pi 2.0))
  (setq right 0.0)
  (setq left pi)
  (setq sset1 (ssadd))
  (setq sandPt (list))
  (setq sandBeside 9.0 sandBelow 6.0)

  ; chamberInsertPointList set in (PRO_SHELF)

  ; START if sand is not continous between rows
  (if (> shelf_sep (* sandBeside 2.0))
    (progn

      (foreach shelf chamberInsertPointList
        (progn

          (foreach chamber shelf
            (progn

              (if (= chamber (nth 0 shelf))
                (progn
                  ; first point in sand line is lower left of Row#1, then up, right to top of left corner of chamber, then down to chamber insert point, etc.
                  (setq ptA chamber)
                  (setq ptA (polar ptA left sandBeside))
                  (setq ptA (polar ptA down sandBelow))

                  (setq ptB (polar ptA up (+ sandBelow (- chamber_H 4))))
                  (setq ptC (polar ptB right sandBeside))
                  (setq ptD chamber)
                  (setq ptE (polar ptD right chamber_W))
                  (setq ptF (polar ptC right chamber_W))
                  (setq sandPt (append sandPt (list ptA)))
                  (setq sandPt (append sandPt (list ptB)))
                  (setq sandPt (append sandPt (list ptC)))
                  (setq sandPt (append sandPt (list ptD)))
                  (setq sandPt (append sandPt (list ptE)))
                  (setq sandPt (append sandPt (list ptF)))
                )
              )
              ; if not first or last
              (if (and (/= chamber (nth 0 shelf)) (/= chamber (last shelf)))
                (progn
                  (setq ptH chamber)
                  (setq ptG (polar chamber up 7))
                  (setq ptJ (polar ptG right chamber_W))
                  (setq ptI (polar ptH right chamber_W))

                  (setq sandPt (append sandPt (list ptG)))
                  (setq sandPt (append sandPt (list ptH)))
                  (setq sandPt (append sandPt (list ptI)))
                  (setq sandPt (append sandPt (list ptJ)))
                )
              )

              (if (= chamber (last shelf))
                (progn
                  (setq ptH chamber)
                  (setq ptG (polar chamber up 7))
                  (setq ptJ (polar ptG right chamber_W))
                  (setq ptI (polar ptH right chamber_W))
                  (setq ptK (polar ptJ right sandBeside))
                  (setq ptL (polar (polar chamber right (+ sandBeside chamber_W)) down sandBelow))

                  (setq sandPt (append sandPt (list ptG)))
                  (setq sandPt (append sandPt (list ptH)))
                  (setq sandPt (append sandPt (list ptI)))
                  (setq sandPt (append sandPt (list ptJ)))
                  (setq sandPt (append sandPt (list ptK)))
                  (setq sandPt (append sandPt (list ptL)))
                  (setq sandPt (append sandPt (list ptA)))
                )
              )

            )
          )
          ; print out one for the shelf
          (command "PLINE")
          (foreach item sandPt (command item))
          (command "")
          (ssadd (entlast) sset1)

          (setq
            sandPt (list)
            ptA nil
            ptB nil
            ptC nil
            ptD nil
            ptE nil
            ptF nil
            ptG nil
            ptH nil
            ptI nil
            ptJ nil
            ptK nil
          )
        )
      )

    )
  )
  ; END if sand is not continous between rows

  ; START if sand is continous between rows
  (setq sandPtTop (list) sandPtBot (list))
  (if (<= shelf_sep (* sandBeside 2.0))
    (progn
      (setq count 0)
      (foreach shelf chamberInsertPointList
        (progn
          ;;;;;;;;;;;;;;;;;;
          ;ptA----------ptB;
          ; |            | ;
          ;ptC----------ptD;
          ;;;;;;;;;;;;;;;;;;
          (foreach chamber shelf
            (progn

              (setq count (+ count 1))

              (if (= count 1)
                (progn

                  (setq ptA (polar (polar chamber left sandBeside) down sandBelow))
                  (setq ptB (polar ptA up (+ sandBelow 7)))
                  (setq ptC (polar chamber up 7))
                  (setq ptD chamber)
                  (setq ptE (polar chamber right chamber_w))
                  (setq ptF (polar (polar chamber right chamber_w) up 7))

                  ; (command "PLINE" ptA ptB ptC ptD ptE ptF (list 0 0 0) "")

                  (setq sandPtTop (append sandPtTop (list ptA)))
                  (setq sandPtTop (append sandPtTop (list ptB)))
                  (setq sandPtTop (append sandPtTop (list ptC)))
                  (setq sandPtTop (append sandPtTop (list ptD)))
                  (setq sandPtTop (append sandPtTop (list ptE)))
                  (setq sandPtTop (append sandPtTop (list ptF)))

                  (setq sandPtBot (append sandPtBot (list (polar chamber down sandBelow))))
                  (setq sandPtBot (append sandPtBot (list (polar (polar chamber right chamber_W) down sandBelow))))

                )
              )

              (if (and (/= count 1) (/= count num_rows))
                (progn

                  (setq ptH chamber)
                  (setq ptG (polar ptH up 7))
                  (setq ptJ (polar ptG right chamber_W))
                  (setq ptI (polar ptH right chamber_W))

                  (setq sandPtBot (append sandPtBot (list (polar chamber down sandBelow))))
                  (setq sandPtBot (append sandPtBot (list (polar (polar chamber right chamber_W) down sandBelow))))

                  (setq sandPtTop (append sandPtTop (list ptG)))
                  (setq sandPtTop (append sandPtTop (list ptH)))
                  (setq sandPtTop (append sandPtTop (list ptI)))
                  (setq sandPtTop (append sandPtTop (list ptJ)))

                )
              )

              (if (= count num_rows)
                (progn

                  (setq ptH chamber)
                  (setq ptG (polar ptH up 7))
                  (setq ptJ (polar ptG right chamber_W))
                  (setq ptI (polar ptH right chamber_W))
                  (setq ptK (polar ptJ right sandBeside))
                  (setq ptL (polar (polar chamber right (+ sandBeside chamber_W)) down sandBelow))

                  (setq sandPtTop (append sandPtTop (list ptG)))
                  (setq sandPtTop (append sandPtTop (list ptH)))
                  (setq sandPtTop (append sandPtTop (list ptI)))
                  (setq sandPtTop (append sandPtTop (list ptJ)))
                  (setq sandPtTop (append sandPtTop (list ptK)))
                  (setq sandPtTop (append sandPtTop (list ptL)))

                  (setq sandPtBot (append sandPtBot (list (polar chamber down sandBelow))))
                  (setq sandPtBot (append sandPtBot (list (polar (polar chamber right chamber_W) down sandBelow))))

                )
              )
            )
          )

        )
      )


      (setq sandPtBotReverse (list))
      (foreach item sandPtBot
        (setq sandPtBotReverse (append (list item) sandPtBotReverse))
      )


      (if (= isShelf "YES")
        (progn
          (command "PLINE")
          (foreach item sandPtTop (command item))
          (foreach item sandPtBotReverse (command item))
          (command (nth 0 sandPtTop) "")
          (ssadd (entlast) sset1)
        )
      )
      (if (/= isShelf "YES")
        (progn
          (command "PLINE")
          (foreach item sandPtTop (command item))
          (command (nth 0 sandPtBotReverse))
          (command (nth 1 sandPtBotReverse))
;    (command (nth 2 sandPtBotReverse))
;    (command (nth (- (length sandPtBotReverse) 2) sandPtBotReverse))
          (command (nth (- (length sandPtBotReverse) 1) sandPtBotReverse))
          (command (nth 0 sandPtTop) "")
          (ssadd (entlast) sset1)
        )
      )

    )
  )


  (if (= (strcase (getvar "PRODUCT")) "BRICSCAD")
    (progn
      (command "-hatch" "S" sset1 "" "P" "DOTS2" "10" "0" "D" "B" "")
    )
  )

  (if (= (strcase (getvar "PRODUCT")) "AUTOCAD")
    (progn
      (command "-hatch" "S" sset1 "" "P" "AR-SAND" "3" "0" "D" "B" "")
    )
  )


  (SETQ DIMASZ_DEF (getvar "DIMASZ"))
  (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
  (SETVAR "DIMASZ" (* profileTextSize scaleArrowSize))
  (SETVAR "DIMTXT" profileTextSize)
;(print chamberInsertPointList)
; label sand - choose last shelf and pick first chamber OR choose middle chamber
  (if (= isShelf "YES") (setq arrowPoint (car (last chamberInsertPointList))))
  (if (/= isShelf "YES") (setq arrowPoint (car (nth (fix (/ (length chamberInsertPointList) 2.0)) chamberInsertPointList))))

    ; #### Label sand
  (SETQ arrowPoint (POLAR arrowPoint down 4.0))
  (setq arrowPoint (polar arrowPoint right (/ chamber_w 2.0)))
  (SETQ textPoint (POLAR arrowPoint 4.1 (* (/ profileScale 5.0) 40)))
  (COMMAND "LEADER" arrowPoint textPoint "" (cdr (assoc "%X-4%" $customSysList)) "")
    ; change the leader mtext based on profile scale  
  (modifySS (ssadd (entlast)) 41 (* profilescale 45))


  (SETVAR "DIMTXT" DIMTXT_DEF)
  (SETVAR "DIMASZ" DIMASZ_DEF)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;END
;;;;;;drawChamberSandEljen



;;;;;start
;;;;;;drawChamberSandEnviro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun drawChamberSandEnviro (chamberInsertPointList / DIMTXT_DEF DIMASZ_DEF sysAngle sysSlope sandDownhillExtra sandBeside sandBelow ptA ptB ptC ptD ptE ptF ptG sandPtTop sandPtBot sandPtBotReverse down up right left)

  (setq down (/ (* 3.0 pi) 2.0))
  (setq up (/ pi 2.0))
  (setq right 0.0)
  (setq left pi)

  (setq sandBeside 12.0 sandBelow 6.0 sandAbove 6.0)
  (setq sandDownhillExtra 36.0)
  (setq sysSlope (/ (- (last row_start) (last row_end)) (- sys_w chamber_W)))
  (setq sysAngle (angle row_start (polar row_end left chamber_W)))

  (if (= sys_type "ENVIRO")
    (progn
      ; if sysSlope is >10 then set downhill shoulder to 60" or 5' and set sandDownhill to 48"
      (if (>= sysSlope 0.1)
        (progn
          (setq DOWN_SHOULDER 60.0)
          (Alert "
**** SLOPE OF PIPES EXCEEDS 10% ********
  The Design Manuals require the following:
      1. An extra 36\" System Sand Extension
      2. The down slope shoulder must be 5 feet wide
      **** THESE MODIFICATIONS HAVE BEEN AUTOMATICALLY APPLIED ****"
          )


        )
      )
    )
  )



  ; chamberInsertPointList set in (PRO_SHELF), first and last chamber in each shelf


  (if (> shelf_sep (* sandBeside 2.0))
    (progn
      (foreach item chamberInsertPointList
        (progn
          ;;;;;;;;;;;;;;;;;;
          ;ptA----------ptB;
          ; |            | ;
          ;ptC----------ptD;
          ;;;;;;;;;;;;;;;;;;

          (setq ptC (nth 0 item))
          (setq ptC (polar ptC left sandBeside))
          (setq ptC (polar ptC down sandBelow))
          (setq ptA (polar ptC up (+ sandBelow chamber_H sandAbove)))


          (setq ptD (last item))
          (setq ptD (polar ptD right (+ chamber_W sandBeside)))
          (setq ptD (polar ptD down sandBelow))
          (setq ptB (polar ptD up (+ sandBelow chamber_H sandAbove)))

          (command "PLINE" ptC ptA ptB ptD ptC "")

        )
      )
    )
  )


  (setq sandPtTop (list) sandPtBot (list))
  (if (<= shelf_sep (* sandBeside 2.0))
    (progn
      (foreach item chamberInsertPointList
        (progn
          ;;;;;;;;;;;;;;;;;;
          ;ptA----------ptB;
          ; |            | ;
          ;ptC----------ptD;
          ;;;;;;;;;;;;;;;;;;

          (if (= item (nth 0 chamberInsertPointList))
            (progn
              (setq ptC (nth 0 item))
              (setq ptC (polar ptC left sandBeside))
              (setq ptC (polar ptC down sandBelow))
              (setq ptA (polar ptC up (+ sandBelow chamber_H sandAbove)))
            )
            ; else
            (progn
              (setq ptC (nth 0 item))
              (setq ptC (polar ptC right (/ chamber_W 2.0)))
              (setq ptC (polar ptC down sandBelow))
              (setq ptA (polar ptC up (+ sandBelow chamber_H sandAbove)))
            )
          )


          (if (= item (last chamberInsertPointList))
            (progn
              (setq ptD (last item))
              (setq ptD (polar ptD right (/ chamber_W 2.0)))
              (setq ptD (polar ptD sysAngle (/ (+ (/ chamber_W 2.0) sandBeside) (cos sysAngle))))
              (setq ptD (polar ptD down sandBelow))
              (setq ptB (polar ptD up (+ sandBelow chamber_H sandAbove)))
              ; create points for extra down slope sand if >10%
              (setq ptE (polar ptD up sandBelow))
              (setq ptF (polar ptE sysAngle sandDownhillExtra))
              (setq ptG (polar ptD sysAngle sandDownhillExtra))

            )
            ; else
            (progn
              (setq ptD (last item))
              (setq ptD (polar ptD right (/ chamber_W 2.0)))
              (setq ptD (polar ptD down sandBelow))
              (setq ptB (polar ptD up (+ sandBelow chamber_H sandAbove)))
            )
          )


          (setq sandPtTop (append sandPtTop (list ptA)))
          (setq sandPtTop (append sandPtTop (list ptB)))
          (setq sandPtBot (append sandPtBot (list ptC)))
          (setq sandPtBot (append sandPtBot (list ptD)))

        )
      )



      (if (AND (>= sysSlope 0.1) (= sys_type "ENVIRO"))
        (progn
          (setq sandPtTop (append sandPtTop (list ptE)))
          (setq sandPtTop (append sandPtTop (list ptF)))
          (setq sandPtTop (append sandPtTop (list ptG)))
        )
      )


      (setq sandPtBotReverse (list))
      (foreach item sandPtBot
        (setq sandPtBotReverse (append (list item) sandPtBotReverse))
      )

      (command "PLINE")
      (foreach item sandPtTop (command item))

      (foreach item sandPtBotReverse (command item))

      (command (nth 0 sandPtTop) "")

    )
  )

; label sand - choose last shelf and pick first chamber OR choose middle chamber
  (if (= isShelf "YES") (setq arrowPoint (car (last chamberInsertPointList))))
  (if (/= isShelf "YES") (setq arrowPoint (car (nth (fix (/ (length chamberInsertPointList) 2.0)) chamberInsertPointList))))


    ; #### Label sand
  (SETQ arrowPoint (POLAR arrowPoint down 3.0))
  (setq arrowPoint (polar arrowPoint right (/ chamber_w 2.0)))
  (SETQ textPoint (POLAR arrowPoint (/ (* 5.0 PI) 4.0) (* (/ profileScale 5.0) 40)))

  (SETQ DIMASZ_DEF (getvar "DIMASZ"))
  (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
  (SETVAR "DIMASZ" (* profileTextSize scaleArrowSize))
  (SETVAR "DIMTXT" profileTextSize)

  (COMMAND "LEADER" arrowPoint textPoint "" (cdr (assoc "%X-4%" $customSysList)) "")
    ; change the leader mtext based on profile scale  
  (modifySS (ssadd (entlast)) 41 (* profilescale 30))



	; if sysSlope is >10 then set downhill shoulder to 60" or 5' and set sandDownhill to 48"
  (if (>= sysSlope 0.1)
    (progn
      (setq arrowPoint (polar ptE (angle ptE ptG) (/ (distance ptE ptG) 2.0)))
      (SETQ textPoint (POLAR arrowPoint (/ (* 5.0 PI) 3.0) (* (/ profileScale 5.0) 40)))
      (COMMAND "LEADER" arrowPoint textPoint "" (cdr (assoc "%X-5%" $customSysList)) "")
      ; change the leader mtext based on profile scale  
      (modifySS (ssadd (entlast)) 41 (* profilescale 27.2))
    )
  )

  (SETVAR "DIMTXT" DIMTXT_DEF)
  (SETVAR "DIMASZ" DIMASZ_DEF)

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;END
;;;;;;drawChamberSandEnviro





















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun createShelfList (/ shelfCount rowCount rowFirst rowLast txt txtList)
  (setq shelfCount 1 rowCount 1 rowFirst nil rowLast nil txt "" txtList (list))

; check to see if there are shelfs or not
;(setq isShelf nil)
;(foreach item shelfList
; (if (> item 1)(setq isShelf "YES")) 
;)

  (foreach item shelfList
    (progn
      (setq rowFirst rowCount)
      (setq rowLast (+ (- item 1) rowCount))

      (if (= isShelf "YES")
        (progn
          (if (/= item 1)
            (setq txt (strcat "Shelf #" (rtos shelfCount 2 0) " (Rows " (rtos rowFirst 2 0) "-" (rtos rowLast 2 0) ") at elevation " (rtos (nth (- shelfCount 1) shelfElvList) 2 0) "\""))
          )
          (if (= item 1)
            (setq txt (strcat "Shelf #" (rtos shelfCount 2 0) " (Row #" (rtos rowFirst 2 0) ") at elevation " (rtos (nth (- shelfCount 1) shelfElvList) 2 0) "\""))
          )
        )
      )

      (if (= isShelf nil)
        (progn
          (setq txt (strcat "Row #" (rtos rowFirst 2 0) " at elevation " (rtos (nth (- shelfCount 1) shelfElvList) 2 0) "\""))
        )
      )


      (setq shelfCount (+ shelfCount 1))
      (setq rowCount (+ rowLast 1))
      (setq txtList (append txtList (list txt)))
    )
  )
  txtList
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun shelfEditElevation (/ newElevation listItem listCount)
  (setq listItem (get_tile "SHELFLIST"))
  (setq newElevation (get_tile "NEWELEVATION"))

  ; update shelfElvList
  (setq newList (list))
  (setq listCount 0)
  (foreach item shelfElvList
    (progn
      (if (= listCount (atoi listItem))
        (setq newList (append newList (list (atoi newElevation))))
        (setq newList (append newList (list item)))
      )
      (setq listCount (+ listCount 1))
    )
  )

  (setq shelfElvList newList)

 ; recreate txtList and update list
  (setq txtList (createShelfList))

  (start_list "SHELFLIST" 3)
  (mapcar 'add_list txtList)
  (end_list)

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; funtion that updates text field with currently chosen item in list_box DCL
(defun updateShelftoEdit ()
  (set_tile "SHELFTOEDIT" (nth (atoi (get_tile "SHELFLIST")) txtList))
  (set_tile "NEWELEVATION" (rtos (nth (atoi (get_tile "SHELFLIST")) shelfElvList) 2 1))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






(defun PRO_TABLE_SHELF (/ elvInlet invNote realChamber_H realShelfElvList tempList rowList selSet1 scaleFactor cProfile TAB_ROW_WID TAB_L TAB_W P1 P2 P3 P4 P5 P6 P7 COL_W COUNT ELV_B ELV_T TXT_BOT TXT_MID TXT_TOP TXT_ROW)

       (setq selSet1 (ssadd))

       (SETVAR "CLAYER" "SEPTICAD")
       (setvar "cmdecho" 0)

    ; set width of table based on sProfile - original code written for 1":5'
       (setq scaleFactor (/ (* scaleProfileText profileScale) 5.0))

       (setq temp-ShelfElvListBOTTOM (convertListNumbersToText shelfElvList))

       (setq tempList (list))
  (foreach item shelfElvList
    (setq tempList (append tempList (list (+ item chamber_H))))
  )

       (setq temp-ShelfElvListTOP (convertListNumbersToText tempList))


    ; set table cell width if decimal or whole number

	; IF shelfList is all 1's then there are really no shelves
        ; ELSE shelfList is not all 1's then there are shelves
  (if (= (length shelfList) num_rows)
    (setq widthFactor 18.0)
    (setq widthFactor 24.0)
  )

    ; WIDTH - height is TAB_ROW_WID/3
       (SETQ TAB_ROW_WID (* widthFactor scaleFactor))

    ; Table length based on # of shelfs
       (SETQ TAB_L (* TAB_ROW_WID (length shelfList)))

    ; table width/height
       (SETQ TAB_W TAB_ROW_WID)

    ; DETEMINE BOTTOM LEFT CORNER OF TABLE BASED ON CENTER OF PROFILE WINDOW
       ; Center of Profile window
       (setq cProfile (cProfileGet))

       ;base point for table bottom left corner of table

       (SETQ TAB_BL (polar (polar cProfile pi (* 19.25 profileScale))
                           (/ pi 2.0)
                           (* 13.86 profileScale)
                    )
       )



     ;BOX 
       (SETQ P1 (POLAR TAB_BL 0.0 TAB_L))
       (SETQ P2 (POLAR (POLAR TAB_BL (/ PI 2) TAB_W) 0.0 TAB_L))
       (SETQ P3 (POLAR TAB_BL (/ PI 2.0) TAB_W))

     ;ROWS
       (SETQ P4 (POLAR TAB_BL (/ PI 2.0) (/ TAB_W 3)))
       (SETQ P5 (POLAR (POLAR TAB_BL (/ PI 2.0) (/ TAB_W 3.0)) 0.0 TAB_L))
       (SETQ P6 (POLAR P4 (/ PI 2.0) (/ TAB_W 3.0)))
       (SETQ P7 (POLAR (POLAR P4 (/ PI 2.0) (/ TAB_W 3.0)) 0.0 TAB_L))

       (COMMAND "PLINE" P4 P5 "")
       (ssadd (entlast) selSet1)

       (COMMAND "PLINE" P6 P7 "")
       (ssadd (entlast) selSet1)

       (command "PLINE" TAB_BL P1 P2 P3 TAB_BL "")
       (ssadd (entlast) selSet1)

       (SETQ COL_W (/ TAB_L (length shelfList)))

       (SETQ TXT_BOT (POLAR TAB_BL (/ PI 2.0) 1.0))
       (SETQ TXT_BOT (POLAR TXT_BOT PI 1.0))

       (SETQ TXT_MID (POLAR TXT_BOT (/ PI 2.0) (/ TAB_ROW_WID 3.0)))
       (SETQ TXT_TOP (POLAR TXT_MID (/ PI 2.0) (/ TAB_ROW_WID 3.0)))

  (if (= (length shelfList) num_rows)
    (command "text" "J" "R" TXT_TOP profileTextSize 0 "ROW #")
    (command "text" "J" "R" TXT_TOP profileTextSize 0 "ROW #'s")
  )
       (ssadd (entlast) selSet1)

       (command "text" "J" "R" TXT_MID profileTextSize 0 "TOP")
       (ssadd (entlast) selSet1)

       (command "text" "J" "R" TXT_BOT profileTextSize 0 "BOTTOM")
       (ssadd (entlast) selSet1)

     ;  Create list of chamber number for top line of text.. group rows if shelfing
       (setq count 1)
       (setq rowList (list))
  (foreach item shelfList
    (progn
      (if (> item 1) (setq rowList (append rowList (list (strcat (rtos count 2 0) " - " (rtos (- (+ count item) 1) 2 0))))))
      (if (= item 1) (setq rowList (append rowList (list (rtos count 2 0)))))
      (setq count (+ count item))
    )
  )

       (SETQ COUNT 0)

  (while (< COUNT (length shelfList))
    (SETQ PT1 (POLAR TAB_BL 0.0 (* COL_W COUNT)))
    (SETQ PT2 (POLAR P3 0.0 (* COL_W COUNT)))
    (SETQ PT_ELV_B (POLAR PT1 0.0 (/ COL_W 2.0)))
    (SETQ PT_ELV_B (POLAR PT_ELV_B (/ PI 2.0) (* (/ scaleProfileText 2.0) (/ TAB_ROW_WID (- TAB_ROW_WID 4.0)))))
    (SETQ PT_ELV_T (POLAR PT_ELV_B (/ PI 2.0) (/ TAB_ROW_WID 3.0)))

    (SETQ PT_ROW (POLAR PT_ELV_T (/ PI 2.0) (/ TAB_ROW_WID 3.0)))
    (SETQ TXT_TOP (STRCAT (nth count temp-shelfElvListTOP) "\""))
    (SETQ TXT_BOT (STRCAT (nth count temp-shelfElvListBOTTOM) "\""))

    (SETQ TXT_ROW (nth count rowList))
    (command "text" "C" PT_ROW profileTextSize 0 TXT_ROW)
    (ssadd (entlast) selSet1)
    (command "text" "C" PT_ELV_T profileTextSize 0 TXT_TOP)
    (ssadd (entlast) selSet1)
    (command "text" "C" PT_ELV_B profileTextSize 0 TXT_BOT)
    (ssadd (entlast) selSet1)

    (COMMAND "PLINE" PT1 PT2 "")
    (ssadd (entlast) selSet1)

    (SETQ COUNT (+ COUNT 1))
  )
     ; end while loop


       (setq elvInlet (dropTrailingZero (+ 4.0 (cadr row_start) invertHeight)))

  ; add invert note to table
       (setq invNote (strcat "TOP OF ROW #1 INLET AT " elvInlet "\""))
       (setq invNotePt (polar TAB_BL pi (* profileScale 5.95)))
       (setq invNotePt (polar invNotePt 0.0 (/ (distance invNotePt (POLAR TAB_BL 0.0 TAB_L)) 2.0)))
       (setq invNotePt (polar invNotePt (/ (* pi 3.0) 2.0) (* profileScale 0.15)))
       (command "text" "J" "TC" invNotePt profileTextSize 0 invNote)
       (ssadd (entlast) selSet1)

       (makeBlock (strcat "ElvTable" (rtos (getvar "cdate") 2 7)) selSet1 TAB_BL)


       (princ)
)
;END PRO_TABLE_SHELF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun isLevelSystem (/ test flag)
       (setq flag "NO")
       (setq test (nth 0 shelfelvlist))
       (FOREACH item shelfelvlist
         (if (= item test)
           (setq flag "YES")
           (setq flag "NO")
         )
       )
       flag
)















;;;;;;;;;;;;;;;;;;;;;
(defun getShelfVar (/ altstr count item readList)

;;-- Get data from DCL
  (setq readList (get_tile "SHELF_SEQUENCE"))

  ;;-- If begins with white space than remove it
  (setq flag nil)
  (while (= flag nil)
    (if (= (substr readlist 1 1) " ")
      (setq readList (substr readlist 2 (- (strlen readList) 1)))
      ; else
      (setq flag 1)
    )
  )

  ;;-- Set variable to refresh DCL if reopened
  (setq shelfStrDCL readList)

;;-- Turn string into list of #'s
  (setq count 1)
  (setq retList (list))
  (while (setq item (read readlist))
    (setq retList (append retList (list item)))
    (while
      (and
        (/= " " (substr readlist count 1))
        (/= "" (substr readlist count 1))
      )
      (setq count (1+ count))
    )
    (setq readlist (substr readlist count))
  )
  (setq shelfList retList)

;;-- determine # of chambers entered
  (setq count 0)
  (foreach item shelfList
    (setq count (+ count item))
  )

;;-- COMPARE input to NUM_ROWS and alert user if input incorrect!
	; if BAD
  (if (/= count num_rows)
    (progn
      (setq altstr (strcat "The total # of Rows in the Shelf Sequence [# Rows = " (rtos count 2 0) "] is not consistent with the system design [# Rows = " (rtos num_rows 2 0) "]"))
      (setq altstr (strcat altstr "\nYou just entered a Shelf Sequence of = '" (get_tile "SHELF_SEQUENCE") "' = " (rtos count 2 0) ".\nThe total # of Rows in the Shelf Sequence should be = " (rtos num_rows 2 0)))
      (alert altstr)
    )
  )


	; if OKAY
  (if (= count num_rows) (done_dialog))

)
;;;;;;;;;;;;;;;;;;;;;





(defun septicPreview ()




   ;;;;;;;;; Start Subroutine  ;;;;;;;;
   
      ;;; THIS FUNCTION IS CALLED ONLY BY A DCL TILE ACTION
      (defun checkMapScale (/ page3Val page2Val profileVal)
          (setq profileVal (atoi (get_tile "profile_scale")))
          (setq page3Val (atoi (get_tile "page3_scale")))
          (setq page2Val (atoi (get_tile "page2_scale")))

          (if (AND (/= (REM profileVal 1) 0) (/= profileVal 0))
            (progn
              (set_tile "profile_scale" "")
              (alert "Profile Scale is NOT valid - Must be multiple of 1")
            )
          )

          (if (AND (/= (REM page3Val 10) 0) (/= page3Val 0))
            (progn
              (set_tile "page3_scale" "")
              (alert "Page 3 Scale is NOT valid - Must be multiple of 10")
            )
          )

          (if (AND (/= (REM page2Val 10) 0) (/= page2Val 0))
            (progn
              (set_tile "page2_scale" "")
              (alert "Page 2 Scale is NOT valid - Must be multiple of 10")
            )
          )

        )
   
   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (defun septicPreviewStrings (/ pStrL-1 pStrL-2 pStrL-3 pStrL-4 pStrL-5 pStrL-6 pStrL-7 pStrL-8 pStrL-9 pStrL-10 pStrL-11
                                     pStrR-1 pStrR-2 pStrR-3 pStrR-4 pStrR-5
                                     pStrShoulder-1 pStrShoulder-2 pStrShoulder-3 pStrShoulder-4)

        (setq pStrL-1 ""
              pStrL-2 ""
              pStrL-3 ""
              pStrL-4 ""
              pStrL-5 ""
              pStrL-6 ""
              pStrL-7 ""
              pStrL-8 ""
              pStrL-9 ""
              pStrL-10 ""
              pStrL-11 ""
              pStrR-1 ""
              pStrR-2 ""
              pStrR-3 ""
              pStrR-4 ""
              pStrR-5 ""
              pStrShoulder-1 ""
              pStrShoulder-2 ""
              pStrShoulder-3 ""
              pStrShoulder-4 ""
        )

      ; create exceptions list, those not on exceptions lists get the generic treatment (i.e. plastic chambers)
        (setq exList (list
                       "4x8_CONCRETE_CLUSTER"
                       "8x4_CONCRETE_CLUSTER"
                       "4x8_CONCRETE_TRENCH"
                       "8x4_CONCRETE_TRENCH"
                       "STONE"
                       "ENVIRO"
                       "TRENCH2"
                       "TRENCH3"
                     )
        )

         ; go thru exceptions, then do generic treatment, pStrL-1 thru pStrL-11 are the chamber specific ones	
         ;;;;;;;;; Start if CONCRETE CHAMBERS
        (if (= (substr sys_type 5 16) "CONCRETE_CLUSTER")
          (progn
            (setq pStrL-1 (strcat (rtos (* NUM_UNITS NUM_ROWS) 2 0) " " (cdr (assoc sys_type sysList))))
            (setq pStrL-2 (strcat "Each unit is " (rtos CHAMBER_W 2 0) "\" wide x " (rtos CHAMBER_L 3 2) " long x " (rtos CHAMBER_H 2 0) "\" high"))
            (setq pStrL-4 (strcat (rtos NUM_ROWS 2 0) " rows X " (rtos NUM_UNITS 2 0) " units long ["))
            (setq pStrL-4 (strcat pStrL-4 (rtos sys_W 3 2) " wide x " (rtos sys_L 3 2) " long]"))
            (setq pStrL-6 (strcat (rtos stoneBeside 2 0) "\" stone beside chambers"))
            (setq pStrL-7 (strcat (rtos stoneBelow 2 0) "\" stone below chambers"))
            (setq pStrL-9 (strcat (rtos fill_above 2 0) "\" fill above chambers"))
            (setq pStrL-10 (strcat "Compass bearing [left to right] -> " (rtos bearing 2 0) " degrees"))
          )
        )
        (if (= (substr sys_type 5 15) "CONCRETE_TRENCH")
          (progn
            (setq pStrL-1 (strcat (rtos (* NUM_UNITS NUM_ROWS) 2 0) " - " (cdr (assoc sys_type sysList))))
            (setq pStrL-2 (strcat "Each chamber is " (rtos (/ (- CHAMBER_W 24.0) 12.0) 2 0) "' wide x " (rtos (/ CHAMBER_L 12.0) 2 0) "' long x " (rtos CHAMBER_H 2 0) "\" high"))
            (setq pStrL-3 (strcat "With 12\" of Stone along " (rtos (/ CHAMBER_L 12.0) 2 0) "' sidewalls"))
            (setq pStrL-5 (strcat (rtos NUM_ROWS 2 0) " rows X " (rtos NUM_UNITS 2 0) " units long ["))
            (setq pStrL-5 (strcat pStrL-5 (rtos (/ sys_W 12.0) 2 0) "' wide x " (rtos (/ sys_L 12.0) 2 0) "' long]"))
            (setq pStrL-6 (strcat "Rows " (rtos (/ row_sep 12.0) 2 0) "' edge of stone to edge of stone"))
            (setq pStrL-7 (strcat (rtos fill_above 2 0) "\" fill above chambers"))
            (setq pStrL-9 (strcat "Compass bearing [left to right] -> " (rtos bearing 2 0) " degrees"))
          )
        )
         ;;;;;;;;; end if CONCRETE


        (if (= sys_type "STONE")
          (progn
            (setq pStrL-1 (strcat (rtos NUM_ROWS 2 0) "' x " (rtos NUM_UNITS 2 0) "' Stone Bed, "))
            (setq pStrL-1 (strcat pStrL-1 (rtos (* NUM_ROWS NUM_UNITS) 2 0) " square feet"))
            (setq pStrL-3 (strcat (rtos fill_above 2 0) "\" fill above Stone Bed"))
            (setq pStrL-4 (strcat "Compass bearing [left to right] -> " (rtos bearing 2 0) " degrees"))
          )
        )

        ;;;; If EnviroSeptic - linear feet is used
        (if (= SYS_TYPE "ENVIRO")
          (progn
            (setq pStrL-1 (strcat (rtos (* NUM_ROWS (/ sys_L 12.0)) 2 0) " l.f. of " (cdr (assoc sys_type sysList)) ""))
            (setq pStrL-2 (strcat "Each unit is " (rtos CHAMBER_W 3 2) " wide x 10' long x " (rtos CHAMBER_H 2 0) "\" high."))
            (setq pStrL-3 (strcat (rtos NUM_ROWS 2 0) " rows X " (rtos (/ sys_L 12.0) 2 0) "' long [" (rtos sys_W 3 2) " wide x " (rtos sys_L 3 2) " long]"))
            (setq pStrL-4 (strcat "Rows " (rtos (+ row_sep chamber_w) 2 0) "\" center to center or " (rtos row_sep 2 0) "\" edge to edge"))
            (setq pStrL-5 (strcat (rtos fill_above 2 0) "\" fill above chambers"))
            (setq pStrL-6 (strcat "Compass bearing [left to right] -> " (rtos bearing 2 0) " degrees"))


          )
        )
        ;;;; If stone trenches
        (if (or (= SYS_TYPE "TRENCH2") (= SYS_TYPE "TRENCH3"))
          (progn
            (setq pStrL-1 (strcat (rtos (* NUM_ROWS (/ sys_L 12.0)) 2 0) " l.f. of " (cdr (assoc sys_type sysList)) ""))
            (setq pStrL-2 (strcat (rtos NUM_ROWS 2 0) " Trenches X " (rtos (/ sys_L 12.0) 2 0) "' long [" (rtos sys_W 3 2) " wide x " (rtos sys_L 3 2) " long]"))
            (setq pStrL-4 (strcat "Trenches " (rtos (+ row_sep chamber_w) 2 0) "\" center to center or " (rtos row_sep 2 0) "\" edge to edge"))
            (setq pStrL-5 (strcat (rtos fill_above 2 0) "\" minimum fill above stone"))
            (setq pStrL-6 (strcat "Compass bearing [left to right] -> " (rtos bearing 2 0) " degrees"))

          )
        )

      ;;;;; if not in exceptics list then do GENERIC PLASTIC CHAMBERS
        (if (= (searchListStr exList sys_type) nil)
          (progn

            (setq pStrL-4 (strcat (rtos NUM_ROWS 2 0) " rows X " (rtos NUM_UNITS 2 0) " units long ["))
            (setq pStrL-4 (strcat pStrL-4 (rtos sys_W 3 2) " wide x " (rtos sys_L 3 2) " long]"))

            (setq pStrL-5 (strcat "Rows " (rtos (+ row_sep chamber_w) 2 0) "\" center to center or " (rtos row_sep 2 0) "\" edge to edge"))
            (setq pStrL-7 (strcat (rtos fill_above 2 0) "\" fill above chambers"))
            (setq pStrL-8 (strcat "Compass bearing [left to right] -> " (rtos bearing 2 0) " degrees"))
          )
        )
         ;end if 

        (if (= slopeType "AUTODESIGN") (setq pStrR-1 "Final Grade Type -> AUTO"))
        (if (= slopeType "CROWN") (setq pStrR-1 "Final Grade Type -> 3% Crown"))
        (if (= slopeType "SLOPING") (setq pStrR-1 "Final Grade Type -> Sloping"))
        (if (= slopeType "SLOPING3") (setq pStrR-1 "Final Grade Type -> Sloping, with 3% Up-slope Fill Extension"))

        (if (= step_type "AUTO")
          (progn
            (setq pStrR-2 "Auto-Step Row Elevation Calculation Method")
            (setq pStrR-3 (strcat "Vertical separation from Limiting Factor -> " (rtos LIM_SEP 2 0) "\""))
            (setq pStrR-4 (strcat "Elevation of Limiting Factor Up-slope    -> " (rtos DEPTH_TOP 2 0) "\""))
            (setq pStrR-5 (strcat "Elevation of Limiting Factor Down-slope  -> " (rtos DEPTH_BOT 2 0) "\""))
          )
        )

        (if (= step_type "FORCE")
          (progn
            (if (/= sys_type "STONE") (setq pStrR-3 "Force-Step Row Elevation Calculation Method"))
            (if (/= sys_type "STONE") (setq pStrR-4 (strcat "Base of Chamber Row #1 Elevation -> " (rtos DEPTH_STEP 2 0) "\"")))
            (if (/= sys_type "STONE") (setq pStrR-5 (strcat "Vertical Drop Between Rows -> " (rtos STEP_SIZE 2 0) "\"")))

            (if (= sys_type "STONE") (setq pStrR-3 (strcat "Base of Stone Bed Elevation -> " (rtos DEPTH_STEP 2 0) "\"")))
          )
        )

        (COND          ((= UP_SLOPE "0") (SETQ slopeText "3:1 Fill Extension"))        )
        (COND          ((= UP_SLOPE "1") (SETQ slopeText "4:1 Fill Extension"))        )
        (COND          ((= UP_SLOPE "2") (SETQ slopeText "5:1 Fill Extension"))        )
        (COND          ((= UP_SLOPE "3") (SETQ slopeText "6:1 Fill Extension"))        )
        (COND          ((= UP_SLOPE "4") (SETQ slopeText "3% Grade - Tapered into Slope"))        )
        (setq pStrShoulder-1 (strcat pStrShoulder-1 "Up-slope         -> " (rtos UP_SHOULDER 2 0) "\" Shoulder, " slopeText "\n"))
        (if (= UP_SLOPE "4") (setq pStrShoulder-1 (strcat pStrShoulder-1 "Up-slope         ->" slopeText "\n")))
        
        (COND          ((= DOWN_SLOPE "0") (SETQ slopeText "3:1 Fill Extension"))        )
        (COND          ((= DOWN_SLOPE "1") (SETQ slopeText "4:1 Fill Extension"))        )
        (COND          ((= DOWN_SLOPE "2") (SETQ slopeText "5:1 Fill Extension"))        )
        (COND          ((= DOWN_SLOPE "3") (SETQ slopeText "6:1 Fill Extension"))        )
        (setq pStrShoulder-2 (strcat pStrShoulder-2 "Down-slope    -> " (rtos DOWN_SHOULDER 2 0) "\" Shoulder, " slopeText "\n"))
        
        (COND          ((= LEFT_SLOPE "0") (SETQ slopeText "3:1 Fill Extension"))        )
        (COND          ((= LEFT_SLOPE "1") (SETQ slopeText "4:1 Fill Extension"))        )
        (COND          ((= LEFT_SLOPE "2") (SETQ slopeText "5:1 Fill Extension"))        )
        (COND          ((= LEFT_SLOPE "3") (SETQ slopeText "6:1 Fill Extension"))        )
        (setq pStrShoulder-3 (strcat pStrShoulder-3 "Left-slope       -> " (rtos LEFT_SHOULDER 2 0) "\" Shoulder, " slopeText "\n"))
        
        (COND          ((= RIGHT_SLOPE "0") (SETQ slopeText "3:1 Fill Extension"))        )
        (COND          ((= RIGHT_SLOPE "1") (SETQ slopeText "4:1 Fill Extension"))        )
        (COND          ((= RIGHT_SLOPE "2") (SETQ slopeText "5:1 Fill Extension"))        )
        (COND        ((= RIGHT_SLOPE "3") (SETQ slopeText "6:1 Fill Extension"))        )
        (setq pStrShoulder-4 (strcat pStrShoulder-4 "Right-slope     -> " (rtos RIGHT_SHOULDER 2 0) "\" Shoulder, " slopeText "\n"))
        
        (set_tile "systemInfoMain-1" pStrL-1)
        (set_tile "systemInfoMain-2" pStrL-2)
        (set_tile "systemInfoMain-3" pStrL-3)
        (set_tile "systemInfoMain-4" pStrL-4)
        (set_tile "systemInfoMain-5" pStrL-5)
        (set_tile "systemInfoMain-6" pStrL-6)
        (set_tile "systemInfoMain-7" pStrL-7)
        (set_tile "systemInfoMain-8" pStrL-8)
        (set_tile "systemInfoMain-9" pStrL-9)
        (set_tile "systemInfoMain-10" pStrL-10)
        (set_tile "systemInfoMain-11" pStrL-11)

        (set_tile "systemInfoGrades-1" pStrR-1)
        (set_tile "systemInfoGrades-2" pStrR-2)
        (set_tile "systemInfoGrades-3" pStrR-3)
        (set_tile "systemInfoGrades-4" pStrR-4)
        (set_tile "systemInfoGrades-5" pStrR-5)

        (set_tile "systemInfoShoulders-1" pStrShoulder-1)
        (set_tile "systemInfoShoulders-2" pStrShoulder-2)
        (set_tile "systemInfoShoulders-3" pStrShoulder-3)
        (set_tile "systemInfoShoulders-4" pStrShoulder-4)


      )
      ;;;;;end subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (setq previewDesign 0)

       ;;;--- Load the DCL file

  (setq dcl_id2 (load_dialog (strcat $septicadPathDCL "DESIGN_PREVIEW.dcl")))


      ;;;--- See if the dialog box is already loaded
  (if (not (new_dialog "DESIGN_PREVIEW" dcl_id2))
    (progn
      (alert "The DESIGN_PREVIEW.DCL file was not found!")
      (exit)
    )
  )

      ;;;--- If an action event occurs, do this function
  (action_tile "cancel-button" "(setq previewDesign 0)(done_dialog)")
  (action_tile "okay-button" "(setq previewDesign 1)(saveMapScaleVars)(done_dialog)")
  (action_tile "edit" "(shelfEditElevation)")

      ;;;; --- INSERT SYSTEM INFORMATION HERE ---- ;;;;
  (septicPreviewStrings)


  (if (and (/= (substr sys_type 5 16) "CONCRETE_CLUSTER") (/= sys_type "STONE"))
    (progn
      ;;;; Define shelfElvList variable
      (calculateChamberElevations)

      ; create txtList variable with Row/Shelf #s and Elevations
      (setq txtList (createShelfList))

      (start_list "SHELFLIST" 3)
      (mapcar 'add_list txtList)
      (end_list)

      (set_tile "SHELFTOEDIT" (nth 0 txtList))
      (set_tile "NEWELEVATION" (rtos (nth 0 shelfElvList) 2 1))

      ; call funtion that updates text field with currently chosen item in list_box DCL
      (updateShelftoEdit)
    )
  )

	; Automatically Set Profile Scale based on sys_w if scale is not already set
  (if (= (getvar "USERI3") 0)
    (progn
      (if (<= sys_w 240.0) (set_tile "profile_scale" "5"))
      (if (> sys_w 240.0) (set_tile "profile_scale" "6"))
      (if (>= sys_w 360.0) (set_tile "profile_scale" "8"))
      (if (>= sys_w 540.0) (set_tile "profile_scale" "10"))
    )
  )

	; Set tiles to existing information
  (if (/= (getvar "USERI1") 0)
    (progn
      (set_tile "page2_scale" (rtos (* 10 (getvar "USERI1")) 2 0))
      (set_tile "page3_scale" (rtos (* 10 (getvar "USERI2")) 2 0))
      (set_tile "profile_scale" (rtos (getvar "USERI3") 2 0))
    )
  )


  (if (or (= (substr sys_type 5 16) "CONCRETE_CLUSTER") (= sys_type "STONE"))
    (progn

      (set_tile "SHELFTOEDIT" "")
      (set_tile "NEWELEVATION" "")

      (mode_tile "SHELFLIST" 1)
      (mode_tile "NEWELEVATION" 1)
      (mode_tile "edit" 1)
    )
  )



      ;;;--- Display the dialog box
  (start_dialog)

      ;;;--- Unload the dialog box
  (unload_dialog dcl_id2)

      ;;;--- If the cancel button was pressed
  (if (= previewDesign 0)
    (Princ "\n... Back to Modify the Design Input")
  )

      ;;;--- If okay button is set
  (if (= previewDesign 1)
    (princ (strcat "\nCreating Drawings\nSeptiCAD Drawing Scales\nPage 2 Map: 1\":" (RTOS page2Scale 2 0) "'\nPage 3 Map: 1\":" (RTOS page3Scale 2 0) "'\nPage 3 Cross Section: 1\":" (RTOS profileScale 2 0) "'\nType MAPSCALE to change scales."))
  )


)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




































;; used by more than one function
(defun saveMapScaleVars ()

  (setq page3Scale (atoi (get_tile "page3_scale")))
  (setq page2Scale (atoi (get_tile "page2_scale")))
  (setq profileScale (atoi (get_tile "profile_scale")))

   ; scaleProfileText is legacy and set as scaleMapText in getuservars
  (setq profileTextSize (* profileScale scaleProfileText))
  (setq page2TextSize (* (/ page2Scale 10) scaleMapText))
  (setq page3TextSize (* (/ page3Scale 10) scaleMapText))

;   (setvar "USERI1" page2TextSize)
;   (setvar "USERI2" page3TextSize)
;   (setvar "USERI3" profileTextSize)

  (setvar "USERI1" (/ page2Scale 10))
  (setvar "USERI2" (/ page3Scale 10))
  (setvar "USERI3" profileScale)

)
; end saveMapScaleVars


(defun mapScale (/ str)

  (SETVAR "CMDECHO" 0)
  (if (/= (getvar "dimscale") 1) (princ (strcat "\nSeptiCAD: DIMSCALE variable changed from '" (rtos (getvar "DIMSCALE") 2 0) "' to '1'.")))
  (setvar "dimscale" 1)

  (getUserVars)

       ;;;--- Load the DCL file
  (setq dcl_id (load_dialog (strcat $septicadPathDCL "mapScale.dcl")))

      ;;;--- See if the dialog box is already loaded
  (if (not (new_dialog "mapScale" dcl_id))
    (progn
      (alert "The mapScale.DCL file was not found!")
      (exit)
    )
  )


      ;;;--- If an action event occurs, do this function
  (action_tile "cancel" "(setq MapScaleDiag 1)(done_dialog)")
  (action_tile "okay" "(setq MapScaleDiag 2)(saveMapScaleVars)(done_dialog)")


	; Set tiles to existing information
  (if (/= (getvar "USERI1") 0)
    (progn
      (set_tile "page2_scale" (rtos (* 10 (getvar "USERI1")) 2 0))
      (set_tile "page3_scale" (rtos (* 10 (getvar "USERI2")) 2 0))
      (set_tile "profile_scale" (rtos (getvar "USERI3") 2 0))
    )
  )



      ;;;--- Display the dialog box
  (start_dialog)

      ;;;--- Unload the dialog box
  (unload_dialog dcl_id)


      ;;;--- If the cancel button was pressed
  (if (= MapScaleDiag 1)
    (Princ "\n...mapScale function cancelled - Scales Not Set")
  )

      ;;;--- If okay button is set
  (if (= MapScaleDiag 2)
    (princ (strcat "\nSeptiCAD Drawing Scales\nPage 2 Map: 1\":" (RTOS page2Scale 2 0) "'\nPage 3 Map: 1\":" (RTOS page3Scale 2 0) "'\nPage 3 Cross Section: 1\":" (RTOS profileScale 2 0) "'\nType MAPSCALE to change scales."))
  )

 ; IF LAYOUT IS FOUND
  (if (tblsearch "layer" "FORM_HHE-200")
    (progn
      (changeGuideBlocks)
      (resetVports-based-on-guideBlocks)
    )
  )


    ;;;--- suppress the last echo for a clean exit
  (princ)

)


(defun setScales ()

  (if (= scaleMapText nil) (getuservars))

	;;; check to see if Text sizes are set  --- if not call (mapScale)
	;;; If Text Sizes are stored in DWG FILE
  (if (/= (getvar "USERI1") 0)
    (progn
      (if (/= (getvar "dimscale") 1) (print (strcat "SeptiCAD: DIMSCALE variable changed from '" (rtos (getvar "DIMSCALE") 2 0) "' to '1'")))
      (setvar "dimscale" 1)

      (setq page2Scale (* (getvar "USERI1") 10))
      (setq page3Scale (* (getvar "USERI2") 10))
      (setq profileScale (getvar "USERI3"))

      (setq profileTextSize (* profileScale scaleProfileText))
      (setq page2TextSize (* (/ page2Scale 10) scaleMapText))
      (setq page3TextSize (* (/ page3Scale 10) scaleMapText))

    )
  )

  (if (= profileTextSize nil) (mapScale))

	; if user excaped
  (if (= profileTextSize nil)
    (progn
      (princ "\nMust Select Drawing Scales to Proceed - Press ESC key or Cancel Button to Exit.\n")
      (mapScale)
    )
  )

  (if (= profileTextSize nil) (exit))
  (princ)
)
; end function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cProfileGet (/ shiftRight cProfile)

	; cProfile is used in SCALE_BOT for assigning max arrow lenght to left or right
        ; this is the default location
  (setq cProfile (polar row_start (angle row_start row_end) (/ (distance row_start row_end) 2.0)))
  (setq cProfile (polar cProfile (/ pi 2.0) 20))
        ; if both intersections are on or off the page then center it view
  (if (or
        (and
          (> (car up_int) (car (polar cProfile pi (* 45 profileScale))))
          (< (car down_int) (car (polar cProfile 0.0 (* 45 profileScale))))
        )
          (and
            (< (car up_int) (car (polar cProfile pi (* 45 profileScale))))
            (> (car down_int) (car (polar cProfile 0.0 (* 45 profileScale))))
          )
      )
      ; IF center it and move it down a bit
    (progn
      (setq cProfile cProfile)
    )
    ; else shift standard point to the right to adjust uphill fill extension intersection until next to left margin
    (progn
      (setq cProfile (polar row_start (angle row_start row_end) (/ (distance row_start row_end) 2.0)))
      (setq cProfile (polar cProfile (/ pi 2.0) 20))
      (setq cProfileLeftCenter (polar cProfile pi (* 45 profileScale)))
      (setq shiftRight (* (- (car up_int) (car cProfileLeftCenter)) 0.90))
      (setq cProfile (list
                       (car (polar cProfile 0 shiftRight))
                       (last cProfile)
                     )
      )
    )
  )
        ; return the point
  cProfile
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







(defun boxModel (/ cProfile sProfile current_layer scaleString tPage2 tPage3a tPage3b tPage3c page2LocHorzOffset page2LocVertOffset cPage2Loc)
	; suppresses "Block already defined. Redefined it? ALERT"
  (setq ex (getvar "Expert"))
  (setvar "Expert" 2)

	;sets block insert unit vars
  (setBlockUnitsVars)

	;;; Saves current layer
  (setq current_layer (GETVAR "CLAYER"))

	; cPage2, cPage3, sPage2 & sPage3 are GLOBAL VARIABLES -kinda
  (setq page2LocHorzOffset (* 86.5043 (* 0.1 page2scale))
        page2LocVertOffset (* 15.9621 (* 0.1 page2scale))
  )
  (setq cPage2 (polar page2BaseMap (angle t_U1 t_D2) (/ (distance t_u1 t_D2) 2.0)))
  (setq cPage3 (polar page3BaseMap (angle c_U1 c_D2) (/ (distance c_u1 c_D2) 2.0)))
  (setq cProfile (cProfileGet))
  (setq cPage2Loc (polar (polar cPage2 0.0 page2LocHorzOffset) (/ pi 2) page2LocVertOffset))

	;;; determine insert scale
  (setq sPage2 (/ page2Scale 10))
  (setq sPage3 (/ page3Scale 10))
  (setq sProfile profileScale)

  (deleteGuideBlocks)

	;;; insert model space guides and associate with xData
      (COMMAND "LAYOUT" "S" "MODEL")
      (setvar "clayer" "SEPTICAD_GUIDES")

      (COMMAND "-insert" (strcat $septicadPathBlockRequired "PAGE2_GUIDE.DWG") cPage2 sPage2 sPage2 "0")
      (xDataTag (entlast) "PAGE2_GUIDE" 1000 "SEPTICAD")

      (COMMAND "-insert" (strcat $septicadPathBlockRequired "PAGE3_GUIDE.DWG") cPage3 sPage3 sPage3 "0")
      (xDataSystemString (entlast) "PAGE3_GUIDE")

      (COMMAND "-insert" (strcat $septicadPathBlockRequired "PROFILE_GUIDE.DWG") cProfile sProfile sProfile "0")
      (xDataTag (entlast) "PROFILE_GUIDE" 1000 "SEPTICAD")



    ;;; PAGE 3 - HHE-200
      (setq layout_name "HHE_200_PG3")
      (if (/= (getvar "CTAB") layout_name) (COMMAND "LAYOUT" "S" layout_name))
      (if (/= (getvar "CVPORT") 1) (command "PSPACE"))
 
      (vPortUnlock "") 	;unlock all viewports function

      (setvar "CVPORT" 2)
      (setq scaleString (strcat "1/" (rtos page2Scale 2 0) "xp"))
      ;(command "ZOOM" "c" cPage2 scaleString)
      (command "ZOOM" "c" cPage2 1)
      (command "ZOOM" scaleString)
 
 
      ; Make location map Viewport active and set ZOOM
      (setvar "cvport" 3)
      (setq scaleString (strcat "1/" (rtos page2Scale 2 0) "xp"))
      ;(command "ZOOM" "c" cPage2Loc scaleString)
      (command "ZOOM" "c" cPage2Loc 1)
      (command "ZOOM" scaleString)
 
      (setvar "CVPORT" 1)  ;; exit to paperspace to finish
      (vPortLock "") 	;Locks all viewports function

    ;;; PAGE 4 - HHE-200
      (setq layout_name "HHE_200_PG4")
      (if (/= (getvar "CTAB") layout_name) (COMMAND "LAYOUT" "S" layout_name))
      (if (/= (getvar "CVPORT") 1) (command "PSPACE"))
 
      (vPortUnlock "") 	;unlock all viewports function

      ; Make Page 3 Viewports active and set ZOOM
      ; map viewport
      (setvar "cvport" 2)  ; make first viewport active
      (setq scaleString (strcat "1/" (rtos page3Scale 2 0) "xp"))
      ;(command "ZOOM" "c" cPage3 scaleString)
      (command "ZOOM" "c" cPage3 1)
      (command "ZOOM" scaleString)


      ; cross-section viewport
      (setvar "cvport" 3)  ; make second viewport active
      (setq scaleString (strcat "0" (rtos (/ 1.0 12.0 profileScale) 2 6) "xp"))
      ;(command "ZOOM" "c" cProfile scaleString)
      (command "ZOOM" "c" cProfile 1)
      (command "ZOOM" scaleString)


      (setvar "CVPORT" 1)  ;; exit to paperspace to finish
      (vPortLock "") 	;Locks all viewports function



	; reset current layer
  (SETVAR "CLAYER" current_layer)

	;reset block insert unit vars
  (resetBlockUnitsVars)

	; resets the Expert system variable
  (setvar "Expert" ex)

)
; end boxModel




























;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Modifieds Guides and Scale Text if scales changed;;;
;;;;; Req.->("SEPTICAD" (1000 . "PAGE3_GUIDE")) ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun changeGuideBlocks (/ a lg dxfList s cnt d1 d ent)


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;; Changes scale of Block ;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun changeBlockScale (ent newScale / ent1)
      (setq ent1 (entget ent))
      (setq ent1 (subst (cons 41 newScale) (assoc 41 ent1) ent1))
      (setq ent1 (subst (cons 42 newScale) (assoc 42 ent1) ent1))
      (setq ent1 (subst (cons 43 newScale) (assoc 43 ent1) ent1))
      (entmod ent1)
      (princ)
    )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



   ;get all the SEPTICAD xData in the drawing
  (if (setq a (ssget "x" '((-3 ( "SEPTICAD")))))
    (progn

      ;unlock layer
      (command "layer" "U" "SEPTICAD_GUIDES" "")

      (setq lg (sslength a))
      (setq cnt 0)

      ;repeat for the number of entities SEPTICAD xData
      (repeat lg

        ;get the entity name
        (setq ent (ssname a cnt))

        ;get the entity list
        (setq dxfList (entget ent '( "SEPTICAD")))

        (setq cnt (1+ cnt))

        ;get the xdata list
        (setq d (assoc -3 dxfList))

        ;get just the xData
        (setq d1 (cdr (car (cdr d))))

        ;;;--- IF GUIDES
        (if (= (cdr (car d1)) "PAGE3_GUIDE")
          (changeBlockScale ent (/ page3scale 10))
        )

        (if (= (cdr (car d1)) "PAGE2_GUIDE")
          (changeBlockScale ent (/ page2scale 10))
        )

        (if (= (cdr (car d1)) "PROFILE_GUIDE")
          (changeBlockScale ent profileScale)
        )


      )      ;end repeat

      ; relock layer
      (command "layer" "LO" "SEPTICAD_GUIDES" "")

      (princ)

    )
  )


)




(defun deleteGuideBlocks (/ a lg dxfList s cnt d1 d ent)

   ;get all the SEPTICAD xData in the drawing
  (if (setq a (ssget "x" '((-3 ( "SEPTICAD")))))
    (progn

      ;unlock layer
      (command "layer" "U" "SEPTICAD_GUIDES" "")

      (setq lg (sslength a))
      (setq cnt 0)

      ;repeat for the number of entities SEPTICAD xData
      (repeat lg

        ;get the entity name
        (setq ent (ssname a cnt))

        ;get the entity list
        (setq dxfList (entget ent '( "SEPTICAD")))

        (setq cnt (1+ cnt))

        ;get the xdata list
        (setq d (assoc -3 dxfList))

        ;get just the xData
        (setq d1 (cdr (car (cdr d))))

        ;;;--- IF GUIDES
        (if (= (cdr (car d1)) "PAGE3_GUIDE")
          (entdel ent)
        )

        (if (= (cdr (car d1)) "PAGE2_GUIDE")
          (entdel ent)
        )

        (if (= (cdr (car d1)) "PROFILE_GUIDE")
          (entdel ent)
        )


      )      ;end repeat


      (princ)

    )
  )

)





(defun resetVports-based-on-guideBlocks (/ a lg dxfList s cnt d1 d ent tttt
                                           page2LocHorzOffset page2LocVertOffset cPage2Loc
                                           mapScaleX-2map mapScaleX-3map mapScaleX-3profile
                                           mapScaleX mapScaleY mapScaleZ mapCenter
                                           scaleString)


    ;;;start subroutines;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;; Replaces text in mText/Text ;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun modTextEntity (ent newText / ent1)
      (setq ent1 (entget ent))
      (setq ent1 (subst (cons 1 newText) (assoc 1 ent1) ent1))
      (entmod ent1)
      (princ)
    )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;; end subroutines ;;;;;;



  ;get all the SEPTICAD xData in the drawing
  (if (setq a (ssget "x" '((-3 ( "SEPTICAD")))))
    (progn

      ;unlock layer
      (command "layer" "U" "SEPTICAD_GUIDES" "")

      (setq lg (sslength a))
      (setq cnt 0)

      ;repeat for the number of entities SEPTICAD xData
      (repeat lg
        (setq ent (ssname a cnt))
        (setq dxfList (entget ent '( "SEPTICAD")))
        (setq cnt (1+ cnt))
        (setq d (assoc -3 dxfList))
        (setq d1 (cdr (car (cdr d))))


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;--- IF PAGE2_GUIDE then update map and location map vports
        (if (= (cdr (car d1)) "PAGE2_GUIDE")
          (progn
            (setq tttt (assoc 10 (entget ent)))
            (setq mapCenter (list (cadr tttt) (caddr tttt) (cadddr tttt)))
            (setq mapScaleX (cdr (assoc 41 (entget ent))))
            (setq mapScaleY (cdr (assoc 42 (entget ent))))
            (setq mapScaleZ (cdr (assoc 43 (entget ent))))
            (setq mapScaleX-2map mapScaleX)
            (setvar "USERI1" mapScaleX-2map)

            (setq layout_name "HHE_200_PG3")
            (if (/= (getvar "CTAB") layout_name) (COMMAND "LAYOUT" "S" layout_name))
            (if (/= (getvar "CVPORT") 1) (command "PSPACE"))
       
            (vPortUnlock "") 	;unlock all viewports function

            ; Make Page 2 Viewport active and set ZOOM
            (setvar "CVPORT" 2)
            (setq scaleString (strcat "1/" (rtos (* mapScaleX 10) 2 0) "xp"))

            (if (AND (= (strcase (getvar "PRODUCT")) "BRICSCAD") (= (atoi (substr (getvar "_VERNUM") 1 2)) 8))
              (progn
                (command "ZOOM" "C" mapCenter "")
                (command "ZOOM" scaleString)
              )
              (command "ZOOM" "C" mapCenter scaleString)
            )

            ; Make location map Viewport active and set ZOOM
            ; cPage2, cPage3, sPage2 & sPage3 are GLOBAL VARIABLES -kinda
            (setq page2LocHorzOffset (* 86.5043 mapScaleX)
                  page2LocVertOffset (* 15.9621 mapScaleX)
            )

            (setq cPage2Loc (polar (polar mapCenter 0.0 page2LocHorzOffset) (/ pi 2) page2LocVertOffset))

            (if (setvar "cvport" 3)
              (progn
                (setq scaleString (strcat "1/" (rtos (* mapScaleX 10) 2 0) "xp"))
                ;(command "ZOOM" "c" cPage2Loc scaleString)
                (command "ZOOM" "c" cPage2Loc 1)
                (command "ZOOM" scaleString)
              )
            )

            (setvar "CVPORT" 1)  ;; exit to paperspace to finish
            (vPortLock "") 	;Locks all viewports function

          )
        )

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (if (= (cdr (car d1)) "PAGE3_GUIDE")
          (progn
            (setq tttt (assoc 10 (entget ent)))
            (setq mapCenter (list (cadr tttt) (caddr tttt) (cadddr tttt)))
            (setq mapScaleX (cdr (assoc 41 (entget ent))))
            (setq mapScaleY (cdr (assoc 42 (entget ent))))
            (setq mapScaleZ (cdr (assoc 43 (entget ent))))
            (setq mapScaleX-3map mapScaleX)
            (setvar "USERI2" mapScaleX-3map)
            
            (setq layout_name "HHE_200_PG4")
            (if (/= (getvar "CTAB") layout_name) (COMMAND "LAYOUT" "S" layout_name))
            (if (/= (getvar "CVPORT") 1) (command "PSPACE"))
       
            (vPortUnlock "") 	;unlock all viewports function

            ; Make Page 3 map Viewports active and set ZOOM

            (setvar "cvport" 2)
            
            (setq scaleString (strcat "1/" (rtos (* mapScaleX 10) 2 0) "xp"))

            (if (AND (= (strcase (getvar "PRODUCT")) "BRICSCAD") (= (atoi (substr (getvar "_VERNUM") 1 2)) 8))
              (progn
                (command "ZOOM" "C" mapCenter "")
                (command "ZOOM" scaleString)
              )
              (command "ZOOM" "C" mapCenter scaleString)
            )
            
            (setvar "CVPORT" 1)  ;; exit to paperspace to finish
            (vPortLock "") 	;Locks all viewports function
          )
        )

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (if (= (cdr (car d1)) "PROFILE_GUIDE")
          (progn
            (setq tttt (assoc 10 (entget ent)))
            (setq mapCenter (list (cadr tttt) (caddr tttt) (cadddr tttt)))
            (setq mapScaleX (cdr (assoc 41 (entget ent))))
            (setq mapScaleY (cdr (assoc 42 (entget ent))))
            (setq mapScaleZ (cdr (assoc 43 (entget ent))))
            (setq mapScaleX-3profile mapScaleX)
            (setvar "USERI3" mapScaleX-3profile)

            (setq layout_name "HHE_200_PG4")
            (if (/= (getvar "CTAB") layout_name) (COMMAND "LAYOUT" "S" layout_name))
            (if (/= (getvar "CVPORT") 1) (command "PSPACE"))
       
            (vPortUnlock "") 	;unlock all viewports function

            ; Make Page 3 Cross Section Viewport active and set ZOOM
            (setvar "CVPORT" 3)
            (setq scaleString (strcat "1/" (rtos (* mapScaleX 12.0) 2 0) "xp"))

            (if (AND (= (strcase (getvar "PRODUCT")) "BRICSCAD") (= (atoi (substr (getvar "_VERNUM") 1 2)) 8))
              (progn
                (command "ZOOM" "C" mapCenter "")
                (command "ZOOM" scaleString)
              )
              (command "ZOOM" "C" mapCenter scaleString)
            )
          )
        )

        (if (= (cdr (car d1)) "PAGE2_SCALE")
          (progn
            (setq layout_name "HHE_200_PG3")
            (if (/= (getvar "CTAB") layout_name) (COMMAND "LAYOUT" "S" layout_name))            
            (if (/= mapScaleX-2map nil) (modTextEntity ent (rtos (* mapScaleX-2map 10) 2 0)))
          )
        )
        
        (if (= (cdr (car d1)) "PAGE3_SCALE")
          (progn
            (setq layout_name "HHE_200_PG4")
            (if (/= (getvar "CTAB") layout_name) (COMMAND "LAYOUT" "S" layout_name))
            (if (/= mapScaleX-3map nil) (modTextEntity ent (rtos (* mapScaleX-3map 10) 2 0)))
          )
        )

        (if (= (cdr (car d1)) "PROFILE_SCALE")
          (progn
            (setq layout_name "HHE_200_PG4")
            (if (/= (getvar "CTAB") layout_name) (COMMAND "LAYOUT" "S" layout_name))
            (if (/= mapScaleX-3profile nil) (modTextEntity ent (rtos mapScaleX-3profile 2 0)))
          )
        )

      )      ;end repeat

      ;;; END modify viewports in HHE-200 Layout
      (COMMAND "LAYOUT" "S" "MODEL")

      ; relock layer
      (command "layer" "LO" "SEPTICAD_GUIDES" "")

    )
  ) 




)
;;;;;end resetVports-based-on-guideBlocks