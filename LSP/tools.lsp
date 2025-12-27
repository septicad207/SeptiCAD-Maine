;;; this file includes labeling and drawing tools that are not directly
;;; related to filling out the HHE_200 form or drafting the septic system


(defun c:septicad ()
  (c:septic)
)


(defun c:vportunlock ()
  (if (= (getvar "CTAB") "MODEL")
    (alert "Function Not Available in Model Space")
    (vportunlock "VERBOSE")
  )
)

(defun c:vportlock ()
  (if (= (getvar "CTAB") "MODEL")
    (alert "Function Not Available in Model Space")
    (vportlock "VERBOSE")
  )
)


(defun c:depthfill ()
  (depthoffill)
)
(defun c:elevationTable ()
  (elevationTable)
)
(defun c:elevationCorners ()
  (elevationCorners)
)


(defun c:mapscale ()
  (mapscale)
)

(defun c:ard2 ()
  (septiDIM 2)
)

(defun c:ard3 ()
  (septiDIM 3)
)

(defun c:sbb ()
  (c:sblock)
)

(defun c:sll ()
  (c:slabel)
)

(defun c:dd2 ()
  (darrow 2)
)

(defun c:dd3 ()
  (darrow 3)
)


(defun c:sMaps ()
  (c:goMapquest)
  (princ)
)

(defun c:yyy ()
  (setvar "OSMODE" 5091)
  (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:ars3 (/ t1 t2)

	;;;; Check to see if 3 Scale are stored in USERI1, USERI2, USERI3
	;;;; If not then call mapScale
  (setScales)

  (if (= scaleArrowSize nil) (getuservars))

	;Creates insertion point and inserts text pg2 
  (SETQ t1 (GETVAR "DIMASZ"))
  (SETVAR "DIMASZ" (* page3TextSize scaleArrowSize))
  (SETQ t2 (GETVAR "DIMTXT"))
  (SETVAR "DIMTXT" page3TextSize)

  (arSpline "BLANK")

  (SETVAR "DIMASZ" t1)
  (SETVAR "DIMTXT" t2)
  (princ)

)

(defun c:ars2 (/ t1 t2)

	;;;; Check to see if 3 Scale are stored in USERI1, USERI2, USERI3
	;;;; If not then call mapScale
  (setScales)

  (if (= scaleArrowSize nil) (getuservars))

	;Creates insertion point and inserts text pg2 
  (SETQ t1 (GETVAR "DIMASZ"))
  (SETVAR "DIMASZ" (* page2TextSize scaleArrowSize))
  (SETQ t2 (GETVAR "DIMTXT"))
  (SETVAR "DIMTXT" page2TextSize)

  (arSpline "BLANK")

  (SETVAR "DIMASZ" t1)
  (SETVAR "DIMTXT" t2)
  (princ)

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ARSPLINE_ERROR (msg)

    ; erase lines guiding everything in sset1
  (command "erase" sset1 "")

    ; end undo group
  (command "UNDO" "E")

   ; reset error function	
  (setq *error* lastErrorFunction)
  (princ)
)
; end ARSPLINE_ERROR

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun arSpline (text / pointList p1 count txt)

  (setq lastErrorFunction *error*)
  (setq *error* ARSPLINE_ERROR)

  (setvar "cmdecho" 0)
  (command "UNDO" "BE")

  (setq sset1 (ssadd))

  (setq pointList (list))

  (setq p1 (getpoint "\nChoose First Point"))
  (princ "\nChoose at least 2 more points")
  (setq pointList (append pointList (list (getpoint p1))))

  (if (= (length pointList) nil)
    (progn
      (princ "\nAt least 3 point are required.")
      (command "UNDO" "E")
      (exit)
    )
  )

  (princ "\nRight Click or <enter> to input text ")
  (command "PLINE" p1 (nth 0 pointList) "")
  (ssadd (entlast) sset1)


  (setq count 0)
  (while (setq userPnt (getpoint (nth count pointList)))
    ; add point to list	
    (setq pointList (append pointList (list userPnt)))

    ; create guidline
    (command "PLINE" (nth count pointList) userPnt "")
    (ssadd (entlast) sset1)

    ; increment counter
    (setq count (+ count 1))
  )


  (if (= (length pointList) 1)
    (progn
      (prompt "\n\nAt least 3 point are required.")
      (command "UNDO" "E")
      (command "erase" sset1 "")
      (exit)
    )
  )


  (if (= TEXT "BLANK")
    (progn
      (setq txt (getstring T "Text for Label: "))
      (while (/= (setq TEXT (getstring T "Next Line or <enter>: ")) "")
        (setq txt (strcat txt "\\P" TEXT))
      )
      (setq text txt)
    )
  )


  (command "LEADER" p1 (nth 0 pointList) "F" "SP" "E")


      ; if only one item then use that number again
  (if (= (length pointList) 1)
    (progn
      ;(setq p1 (polar (nth 0 pointList)  p1) 0.01))
      (command (nth 0 pointList) "" TEXT "")
    )
  )

      ; if more than 1 point
  (if (> (length pointList) 1)
    (progn
      ;; go through the list of points
      (foreach item pointList
        (progn
          (setq temp item)
          (if (/= item (nth 0 pointList)) (command item))
        )
      )
      (command "" TEXT "")
    )
  )


      ; erase the selection set of polylines
  (command "erase" sset1 "")


  (command "UNDO" "E")
  (setq *error* lastErrorFunction)


)
; end arSpline




(defun C:ART2 (/ PT1 PT2 DIMASZ_DEF DIMTXT_DEF)

       (SETQ DIMASZ_DEF (getvar "DIMASZ"))
       (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
       (SETVAR "CMDECHO" 0)
       (COMMAND "LAYOUT" "S" "MODEL")  ; Change to Model Layout

        ; PUT IN SO CAN USE WHEN NOT IN SeptiCAD
  (if (tblsearch "layer" "SEPTICAD")
    (progn
      (SETVAR "CLAYER" "SEPTICAD")     ; CHANGES LAYER
    )  ; end progn
  )    ; end if


	;;;; Check to see if 3 Scale are stored in USERI1, USERI2, USERI3
	;;;; If not then call mapScale
       (setScales)

       (if (= scaleArrowSize nil) (getuservars))

       (SETVAR "DIMASZ" (* page2TextSize scaleArrowSize))
       (SETVAR "DIMTXT" page2TextSize)


;;;; Get points	
       (SETQ pt1 (GETPOINT "\nCLICK Location of Arrowhead->"))
       (SETQ pt2 (GETPOINT pt1 "\nCLICK Location of Text Leader->"))
       (grdraw pt1 pt2 7)
       (Princ "\nType multi-line text.  Enter for new line. Enter on empty line to exit.")
       (COMMAND "LEADER" pt1 pt2 "")

;;;; Reset Variables
       (SETVAR "DIMTXT" DIMTXT_DEF)  ; resets dimension text size
       (SETVAR "DIMASZ" DIMASZ_DEF)  ; resets dimension arrow size
;;;; Clean Exit
       (princ)

)  ; END C:ART2

(defun C:ART3 (/ PT1 PT2 DIMASZ_DEF DIMTXT_DEF)

       (SETQ DIMASZ_DEF (getvar "DIMASZ"))
       (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
       (SETVAR "CMDECHO" 0)
       (COMMAND "LAYOUT" "S" "MODEL")  ; Change to Model Layout

        ; PUT IN SO CAN USE WHEN NOT IN SeptiCAD
  (if (tblsearch "layer" "SEPTICAD")
    (progn
      (SETVAR "CLAYER" "SEPTICAD")     ; CHANGES LAYER
    )  ; end progn
  )    ; end if

;;;; check to see if mapscales are set

	;;;; Check to see if 3 Scale are stored in USERI1, USERI2, USERI3
	;;;; If not then call mapScale
       (setScales)

       (if (= scaleArrowSize nil) (getuservars))

       (SETVAR "DIMASZ" (* page3TextSize scaleArrowSize))
       (SETVAR "DIMTXT" page3TextSize)


;;;; Get points	
       (SETQ pt1 (GETPOINT "\nCLICK Location of Arrowhead->"))
       (SETQ pt2 (GETPOINT pt1 "\nCLICK Location of Text Leader->"))
       (grdraw pt1 pt2 7)
       (Princ "\nType multi-line text.  Enter for new line. Enter on empty line to exit.")
       (COMMAND "LEADER" pt1 pt2 "")

;;;; Reset Variables
       (SETVAR "DIMTXT" DIMTXT_DEF)  ; resets dimension text size
       (SETVAR "DIMASZ" DIMASZ_DEF)  ; resets dimension arrow size
;;;; Clean Exit
       (princ)

)  ; END C:ART3



(defun C:ARTP (/ PT1 PT2 DIMASZ_DEF DIMTXT_DEF)

       (setq lastErrorFunction *error*)
       (setq *error* DEFAULT_ERROR)

       (SETQ DIMASZ_DEF (getvar "DIMASZ"))
       (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
       (SETVAR "CMDECHO" 0)
       (COMMAND "LAYOUT" "S" "MODEL")  ; Change to Model Layout

        ; PUT IN SO CAN USE WHEN NOT IN SeptiCAD
  (if (tblsearch "layer" "SEPTICAD")
    (progn
      (SETVAR "CLAYER" "SEPTICAD")     ; CHANGES LAYER
    )  ; end progn
  )    ; end if

     ;;;; check to see if mapscales are set
  (if (/= profileTextSize nil)
    (progn
      (SETVAR "DIMASZ" (* profileTextSize scaleArrowSize))
      (SETVAR "DIMTXT" profileTextSize)
    )
  )
       (if (= profileTextSize nil)
         (progn
           ;;;; Check to see if 3 Scale are stored in USERI1, USERI2, USERI3
           ;;;; If not then call mapScale
           (setScales)
           (SETVAR "DIMASZ" (* profileTextSize scaleArrowSize))
           (SETVAR "DIMTXT" profileTextSize)
         )
       )


;;;; Get points	
       (SETQ pt1 (GETPOINT "\nCLICK Location of Elevation ->"))
       (SETQ pt2 (GETPOINT pt1 "\nCLICK Location of Text with Elevation ->"))
       (strZero (cadr pt1) "\"")

       (COMMAND "LEADER" pt1 pt2 "" strZ "")

;;;; Reset Variables
       (SETVAR "DIMTXT" DIMTXT_DEF)  ; resets dimension text size
       (SETVAR "DIMASZ" DIMASZ_DEF)  ; resets dimension arrow size
       (setq *error* lastErrorFunction)

;;;; Clean Exit
       (princ)

)      ; END C:ARTP

(defun septiDIM (page / DIMASZ_DEF DIMTXT_DEF pnt1 pnt2 textDimPt midDimPt textDim pnt1a pnt2a arrowWidth arrowLength ang)

	; default error control
       (setq lastErrorFunction *error*)
       (setq *error* DEFAULT_ERROR)

  (if (>= $$# 3)
    (progn
      (Alert " UNREGISTERED VERSION - PLEASE REGISTER\n Function only supported in Full Version.")
      (setUniqueSerialNumber "ALERT")
    )  ; progn
  )    ; if

  (if (< $$# 3)
    (progn

      ;;;; Set some variables
      (SETVAR "CMDECHO" 0)  ; turn echo off
      (SETQ DIMASZ_DEF (getvar "DIMASZ"))
      (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
      (if (= $septicadFullVersion nil) (septicadCheckReg))
      (if (and (= $$# nil) (= $septicadFullVersion 0))
        (progn
          (setq $$# 1)
          (septicadCheckReg)
        )  ; progn
      )    ;if


      ; sets undo spot end 
      (command "UNDO" "BE")



      (getUserVars)

      ; PUT IN SO CAN USE WHEN NOT IN SeptiCAD
      (if (tblsearch "layer" "SEPTICAD")
        (SETVAR "CLAYER" "SEPTICAD")
      )

      (COMMAND "LAYOUT" "S" "MODEL")  ; Change to Model Layout


      (if (= page 2)
        (progn
          (if (/= page2TextSize nil)
            (progn
              (SETVAR "DIMASZ" (* page2TextSize scaleArrowSize))
              (SETVAR "DIMTXT" page2TextSize)
              (setq textSize page2TextSize)
            )
          )
          (if (= page2TextSize nil)
            (progn
              ;;;; Check to see if 3 Scale are stored in USERI1, USERI2, USERI3
              ;;;; If not then call mapScale
              (setScales)
              (SETVAR "DIMASZ" (* page2TextSize scaleArrowSize))
              (SETVAR "DIMTXT" page2TextSize)
              (setq textSize page2TextSize)
            )
          )
        )  ; progn
      )    ; if


      (if (= page 3)
        (progn
          (if (/= page3TextSize nil)
            (progn
              (SETVAR "DIMASZ" (* page3TextSize scaleArrowSize))
              (SETVAR "DIMTXT" page3TextSize)
              (setq textSize page3TextSize)
            )
          )
          (if (= page3TextSize nil)
            (progn
              ;;;; Check to see if 3 Scale are stored in USERI1, USERI2, USERI3
              ;;;; If not then call mapScale
              (setScales)
              (SETVAR "DIMASZ" (* page3TextSize scaleArrowSize))
              (SETVAR "DIMTXT" page3TextSize)
              (setq textSize page3TextSize)
            )
          )
        )  ; progn
      )    ; if


      (setq ANGDIR_DEF (GETVAR "ANGDIR"))
      (setvar "ANGDIR" 0)
      (SETQ ANGBASE_DEF (GETVAR "ANGBASE"))
      (setvar "ANGBASE" 0)

      ;;;; Ask user for two points

      (setq pnt1 (getpoint "\nCLICK first point of Dimension Line->"))
      (PRINC)
      (setq pnt2 (getpoint pnt1 "\nCLICK second point of Dimension Line->"))
      (setq midDimPt (polar pnt1 (angle pnt1 pnt2) (/ (distance pnt1 pnt2) 2.0)))


      ;;;; Create Rotated Dimension Line
      ; print normal
      (if (>= (distance pnt1 pnt2) (* (/ (* textSize scaleArrowSize) 2.0) 7.0))
        (COMMAND "DIM" "ro" pnt1 pnt2 pnt1 pnt2 midDimPt "" "EXIT")
      )


      ; print double arrow with label if not too small
      (if (AND (< (distance pnt1 pnt2) (* (/ (* textSize scaleArrowSize) 2.0) 7.0)) (> (distance pnt1 pnt2) (* (* textSize scaleArrowSize) 2.1)))
        (progn

          (setq ang (angle pnt1 pnt2))

          (setq arrowLength (* textSize scaleArrowSize))
          (setq arrowWidth (/ arrowLength 3.0))

          (setq pnt1a (polar pnt1 ang arrowLength))
          (setq pnt2a (polar pnt2 (- ang pi) arrowLength))

          (command "PLINE" pnt1 "w" "0" arrowWidth pnt1a "w" "0" "0" pnt2a "w" arrowWidth "0" pnt2 "")

          (setq midDimPt (polar pnt1 (angle pnt1 pnt2) (/ (distance pnt1 pnt2) 2.0)))
          (setq textDimPt (polar midDimPt (+ (/ pi 2.0) (angle pnt1 pnt2)) textSize))
          (setq textDim (strcat (rtos (distance pnt1 pnt2) 2 1) "'"))

          (COMMAND "LEADER" midDimPt textDimPt "" textDim "")

        )
        ; progn
      )
      ; if 



      ; print smaller arrowhead if really small
      (if (<= (distance pnt1 pnt2) (* (* textSize scaleArrowSize) 2.1))
        (progn

          (setq ang (angle pnt1 pnt2))

          (setq arrowLength (* (/ 4.0 11.0) (distance pnt1 pnt2)))
          (setq arrowWidth (/ arrowLength 3.0))

          (setq pnt1a (polar pnt1 ang arrowLength))
          (setq pnt2a (polar pnt2 (- ang pi) arrowLength))

          (command "PLINE" pnt1 "w" "0" arrowWidth pnt1a "w" "0" "0" pnt2a "w" arrowWidth "0" pnt2 "")

          (setq midDimPt (polar pnt1 (angle pnt1 pnt2) (/ (distance pnt1 pnt2) 2.0)))
          (setq textDimPt (polar midDimPt (+ (/ pi 2.0) (angle pnt1 pnt2)) textSize))
          (setq textDim (strcat (rtos (distance pnt1 pnt2) 2 1) "'"))

          (COMMAND "LEADER" midDimPt textDimPt "" textDim "")

        )  ; progn
      )    ; if



      ;;;; Reset Variables
      (SETVAR "DIMTXT" DIMTXT_DEF)  ; resets dimension text size
      (SETVAR "DIMASZ" DIMASZ_DEF)  ; resets dimension arrow size
      (setvar "ANGDIR" ANGDIR_DEF)
      (SETVAR "ANGBASE" ANGBASE_DEF)

      ; sets undo spot end 
      (command "UNDO" "E")


      (if (= $septicadFullVersion 0) (setq $$# (+ $$# 1)))


    )  ; progn
  )    ; if
; end if DEMO mode

       (setq *error* lastErrorFunction)

;;;; Clean Exit
       (princ)

)      ; END septiDIM



(defun treeline (pnt1 pnt2 arcLength / stickOut count)


; also set below in loop - make this an editable value defined in TTT
  (setq stickOut (/ arcLength 3.5))

   ;;;;;;; If line is long enough for 2 arcs
  (if (> (distance pnt1 pnt2) (* arcLength 1.9))
    (progn

      (setq arcAngle (angle pnt1 pnt2))

      (setq pntStartArc pnt1)
      (setq pntMidArc (polar pntStartArc (+ (/ pi 2.0) arcAngle) stickOut))
      (setq pntMidArc (polar pntMidArc arcAngle (/ arcLength 2.0)))
      (setq pntEndArc (polar pntStartArc arcAngle arcLength))

      ; print first arc
      (command "arc" pntStartArc pntMidArc pntEndArc)
      (if (= (sslength ssetALL) 0) (setq primaryEnt (entlast)))
      (if (/= (sslength ssetALL) 0) (ssadd (entlast) ssetALL))

      (setq count 1)

      ; print the majority of the line
      (while (> (distance pntEndArc pnt2) (* arcLength 1.9))
        (setq stickOut (/ arcLength 3.5))

        (setq random_num (+ (+ (cadr pntStartArc) (car pntStartArc)) count))
        (setq random_num (rtos random_num 2 2))

        (setq random_num (atoi (substr random_num (- (strlen random_num) 3) 1)))

        (if (= (rem random_num 1) 0) (setq stickout (* 0.7 stickout)))
        (if (= (rem random_num 2) 0) (setq stickout (* 0.8 stickout)))
        (if (= (rem random_num 3) 0) (setq stickout (* 0.9 stickout)))
        (if (= (rem random_num 4) 0) (setq stickout (* 1.1 stickout)))
        (if (= (rem random_num 5) 0) (setq stickout (* 1.0 stickout)))
        (if (= (rem random_num 6) 0) (setq stickout (* 1.2 stickout)))
        (if (= (rem random_num 7) 0) (setq stickout (* 1.3 stickout)))
        (if (= (rem random_num 8) 0) (setq stickout (* 0.9 stickout)))
        (if (= (rem random_num 9) 0) (setq stickout (* 1.1 stickout)))

        (if (= (rem random_num 1) 0) (setq arcLengthT (* 0.8 arcLength)))
        (if (= (rem random_num 2) 0) (setq arcLengthT (* 0.8 arcLength)))
        (if (= (rem random_num 3) 0) (setq arcLengthT (* 0.9 arcLength)))
        (if (= (rem random_num 4) 0) (setq arcLengthT (* 1.0 arcLength)))
        (if (= (rem random_num 5) 0) (setq arcLengthT (* 1.1 arcLength)))
        (if (= (rem random_num 6) 0) (setq arcLengthT (* 1.2 arcLength)))
        (if (= (rem random_num 7) 0) (setq arcLengthT (* 1.2 arcLength)))
        (if (= (rem random_num 8) 0) (setq arcLengthT (* 1.0 arcLength)))
        (if (= (rem random_num 9) 0) (setq arcLengthT (* 1.3 arcLength)))

        (setq pntStartArc pntEndArc)
        (setq pntMidArc (polar pntStartArc (+ (/ pi 2.0) arcAngle) stickOut))
        (setq pntMidArc (polar pntMidArc arcAngle (/ arcLengthT 2.0)))
        (setq pntEndArc (polar pntStartArc arcAngle arcLengthT))
        (command "arc" pntStartArc pntMidArc pntEndArc)
        (ssadd (entlast) ssetALL)  ; add last entity to selection set
        (setq count (+ count 1))
      )  ; end while


      ; second to last arc
      (setq pntStartArc pntEndArc)
      (setq pntEndArc (polar pntStartArc arcAngle (/ (distance pntStartArc pnt2) 2.0)))
      (setq pntMidArc (polar pntStartArc arcAngle (/ (distance pntStartArc pnt2) 4.0)))
      (setq pntMidArc (polar pntMidArc (+ (/ pi 2.0) arcAngle) (* (/ (distance pntStartArc pntEndArc) arcLength) stickOut)))
      (command "arc" pntStartArc pntMidArc pntEndArc)
      (ssadd (entlast) ssetALL)  ; add last entity to selection set

      ; last arc
      (setq pntStartArc pntEndArc)
      (setq pntEndArc (polar pntStartArc arcAngle (distance pntStartArc pnt2)))
      (setq pntMidArc (polar pntStartArc arcAngle (/ (distance pntStartArc pnt2) 2.0)))
      (setq pntMidArc (polar pntMidArc (+ (/ pi 2.0) arcAngle) (* (/ (distance pntStartArc pntEndArc) arcLength) stickOut)))
      (command "arc" pntStartArc pntMidArc pntEndArc)
      (ssadd (entlast) ssetALL)  ; add last entity to selection set

    )  ; progn
  )    ; if


   ;;; If line is long enough for 2 arcs
  (if (<= (distance pnt1 pnt2) (* arcLength 1.9))
    (progn

      (setq arcAngle (angle pnt1 pnt2))
      (setq arcLength (/ (distance pnt1 pnt2) 2.0))
      (setq stickOut (/ arcLength 2))

      (setq pntStartArc pnt1)
      (setq pntMidArc (polar pntStartArc (+ (/ pi 2.0) arcAngle) stickOut))
      (setq pntMidArc (polar pntMidArc arcAngle (/ arcLength 2.0)))
      (setq pntEndArc (polar pntStartArc arcAngle arcLength))

      ; print first arc
      (command "arc" pntStartArc pntMidArc pntEndArc)
      (if (= (sslength ssetALL) 0) (setq primaryEnt (entlast)))
      (if (/= (sslength ssetALL) 0) (ssadd (entlast) ssetALL))

      (setq pntStartArc pntEndArc)
      (setq pntMidArc (polar pntStartArc (+ (/ pi 2.0) arcAngle) stickOut))
      (setq pntMidArc (polar pntMidArc arcAngle (/ arcLength 2.0)))
      (setq pntEndArc (polar pntStartArc arcAngle arcLength))

      ; print second arc
      (command "arc" pntStartArc pntMidArc pntEndArc)
      (ssadd (entlast) ssetALL)  ; add last entity to selection set

    )  ; progn
  )    ; if

)      ; end treeline


(defun c:tree (/ sset1 temp pointList userPnt count arcLength)

  (setq lastErrorFunction *error*)
  (setq *error* DEFAULT_ERROR)

  (setvar "cmdecho" 0)

	; create empty selection sets
  (setq sset1 (ssadd))
  (setq ssetALL (ssadd))

	; set the arcLength
  (setq arcLength 10.0)

  (setq temp (getint "\nAverage length of chord <ENTER = 10> :"))
  (if (AND (/= temp 0) (/= temp nil)) (setq arcLength temp))


  (setq pointList (list))
  (setq count 0)

	; getpoints and add to list
  (setq pointList (append (list (getpoint "Choose First Point")) pointList))

  (command "UNDO" "BE")

  (while (setq userPnt (getpoint (nth count pointList) "\nClick Next Point:"))
    ; add point to list	
    (setq pointList (append pointList (list userPnt)))

    ; create guidline
    (command "PLINE" (nth count pointList) userPnt "")
    (ssadd (entlast) sset1)  ; add last entity to selection set

    ; increment counter
    (setq count (+ count 1))
  )

	; erase the selection set of polylines
  (command "erase" sset1 "")

  (setq count 0)
  (foreach item pointList
    (progn
      (if (/= count 0)
        (treeline (nth (- count 1) pointList) item arcLength)
      )      ; if
      (setq count (+ count 1))
    )        ; progn
  )          ; foreach
  (command "editpline" primaryEnt "" "J" ssetALL "" "")
  (command "regen")
  (command "UNDO" "E")


  (setq yesNo (strcase (getstring "\n\nDo you want to reverse the tree line? [Y or N]")))

  (if (or (= yesNo "Y") (= yesNo "YES"))
    (progn
      ; undo last
      (command "erase" (entlast) "")

      (setq ssetALL (ssadd))

      ; create backwards list
      (setq tempList (list))
      (foreach item pointList
        (progn
          (setq tempList (append (list item) tempList))
        )    ; progn
      )      ; foreach

      ; create new tree line
      (command "UNDO" "BE")
      (setq count 0)
      (foreach item tempList
        (progn
          (if (/= count 0)
            (treeline (nth (- count 1) tempList) item arcLength)
          )  ; if
          (setq count (+ count 1))
        )    ; progn
      )      ; foreach

      (command "editpline" primaryEnt "" "J" ssetALL "" "")

      (command "UNDO" "E")

    )        ; progn
  )          ; if
  (setq *error* lastErrorFunction)


  (princ)

)            ; end c:tree



(defun c:geors ()
  (c:geor)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:geor (/ ddiag)

	;;;;START
	;;;;; CHOOSE TO PUT STONE AROUND CHAMBERS
	;;;--- Load the DCL file
  (setq dcl_id (load_dialog (strcat $septicadPathDCL "georef.dcl")))

	;;;--- See if the dialog box is already loaded
  (if (not (new_dialog "GEOREF" dcl_id))
    (progn
      (alert "The geoRef.DCL file was not found!")
      (exit)
    )
  )

	;;;--- If an action event occurs, do this function
  (action_tile "cancel" "(setq ddiag 0)(done_dialog)")
  (action_tile "mode1" "(setq ddiag 1)(done_dialog)")
  (action_tile "mode2" "(setq ddiag 2)(done_dialog)")
  (action_tile "mode3" "(setq ddiag 3)(done_dialog)")

	;;;--- Display the dialog box
  (start_dialog)

	;;;--- Unload the dialog box
  (unload_dialog dcl_id)

  (if (= ddiag 0) (Princ "\nCommand Cancelled."))
  (if (= ddiag 1) (geoRef-Mode-1))
  (if (= ddiag 2) (geoRef-Mode-2))
  (if (= ddiag 3) (geoRef-Mode-3))

)
;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ROTATION AND SCALING;;;;;;;;;;;;;;;;;;;
(defun geoRef-Mode-1 (/ temp lastErrorFunction sset1 dist known pt1 sc)


  (setvar "cmdecho" 0)
  (setq temp (getvar "orthomode"))
  (setvar "orthomode" 1)

  (setq lastErrorFunction *error*)
  (setq *error* DEFAULT_ERROR)

	; intro message
  (princ "\n\nSeptiCAD - Scale - Tool")

	; get the item to rotate if already selected

  (if (= (last (ssgetfirst)) nil) (princ "\nSelect Entities to Scale"))
  (setq sset1 (ssget))

	; get points

  (setq dist (getdist (setq pt1 (getpoint "Select 1st match point: ")) "Select 2nd match point: "))
  (setq known (atof (getstring "\nKnown Distance: ")))

	; obtain distance and set scale factor
  (setq sc (/ known dist))

  (toggle-OSNAP-Vars-OFF)
  (command "UNDO" "BE")
  (command "scale" "P" "" pt1 sc)
  (command "UNDO" "E")

  (setq *error* lastErrorFunction)
  (toggle-OSNAP-Vars-RESET)
  (setvar "orthomode" temp)

  (princ)
)




(defun geoRef-Mode-2 (/ ANGDIR_DEF ANGBASE_DEF ang sset1 base match1 match2)

  (setq ANGDIR_DEF (GETVAR "ANGDIR"))
  (setvar "ANGDIR" 0)
  (SETQ ANGBASE_DEF (GETVAR "ANGBASE"))
  (setvar "ANGBASE" 0)
  (setvar "cmdecho" 0)

  (setq lastErrorFunction *error*)
  (setq *error* DEFAULT_ERROR)

	; intro message
  (princ "\n\nSeptiCAD - Move and Rotate - Align Tool")

	; get the item to rotate if already selected

  (if (= (last (ssgetfirst)) nil) (princ "\nSelect Entities to Align"))
  (setq sset1 (ssget))

	; get points
  (setq base (getpoint "\nSelect 1st match point: "))
  (setq match2 (getpoint base "\nSelect 2nd match point: "))
  (setq base2 (getpoint "\nSelect 1st reference point: "))
  (setq match1 (getpoint base2 "\nSelect 2nd reference point: "))


	; Measure angles and convert to degrees
  (setq match1 (* (angle base2 match1) (/ 180.0 pi)))
  (setq match2 (* (angle base match2) (/ 180.0 pi)))

	; Choose what angle to rotate based on the size of both angle relative to on another.	
  (if (> match2 match1)
    (setq ang (+ (- 360 match2) match1))
  )

  (if (> match1 match2)
    (setq ang (- match1 match2))
  )

  (toggle-OSNAP-Vars-OFF)
  (command "UNDO" "BE")
  (command "rotate" "P" "" base ang)
  (command "move" "P" "" base base2)
  (command "draworder" "P" "" "B")
  (command "UNDO" "E")

  (setq *error* lastErrorFunction)

  (toggle-OSNAP-Vars-RESET)

  (setvar "ANGDIR" ANGDIR_DEF)
  (SETVAR "ANGBASE" ANGBASE_DEF)

  (princ)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ROTATION AND SCALING;;;;;;;;;;;;;;;;;;;
(defun geoRef-Mode-3 (/ ANGDIR_DEF ANGBASE_DEF sset1 scaleFactor base base2 match1 match2)

  (setvar "cmdecho" 0)

  (setq ANGDIR_DEF (GETVAR "ANGDIR"))
  (setvar "ANGDIR" 0)
  (SETQ ANGBASE_DEF (GETVAR "ANGBASE"))
  (setvar "ANGBASE" 0)

  (setq lastErrorFunction *error*)
  (setq *error* DEFAULT_ERROR)

	; intro message
  (princ "\n\nSeptiCAD - Move, Rotate & Scale - Align Tool")

	; get the item to rotate if already selected

  (if (= (last (ssgetfirst)) nil) (princ "\nSelect Entities to Align and Scale"))
  (setq sset1 (ssget))

	; get points
  (setq base (getpoint "\nSelect 1st match point: "))
  (setq match2 (getpoint base "\nSelect 2nd match point: "))
  (setq base2 (getpoint "\nSelect 1st reference point: "))
  (setq match1 (getpoint base2 "\nSelect 2nd reference point: "))

	; obtain distance and set scale factor
  (setq scaleFactor (/ (distance base2 match1) (distance base match2)))

	; Measure angles and convert to degrees
  (setq match1 (* (angle base2 match1) (/ 180.0 pi)))
  (setq match2 (* (angle base match2) (/ 180.0 pi)))

	; Choose what angle to rotate based on the size of both angle relative to on another.	
  (if (> match2 match1)
    (setq ang (+ (- 360 match2) match1))
  )

  (if (> match1 match2)
    (setq ang (- match1 match2))
  )

  (toggle-OSNAP-Vars-OFF)
  (command "UNDO" "BE")
  (command "rotate" "P" "" base ang)
  (command "scale" "P" "" base scaleFactor)
  (command "move" "P" "" base base2)
  (command "draworder" "P" "" "B")
  (command "UNDO" "E")

  (setq *error* lastErrorFunction)
  (toggle-OSNAP-Vars-RESET)

  (setvar "ANGDIR" ANGDIR_DEF)
  (SETVAR "ANGBASE" ANGBASE_DEF)

  (princ)
)






















(defun doLabel_ERROR (msg)

  (SETVAR "DIMASZ" DIMASZ_DEF)
  (SETVAR "DIMTXT" DIMTXT_DEF)
  (SETVAR "DIMGAP" DIMGAP_DEF)

   ; reset error function	
  (setq *error* lastErrorFunction)
  (princ)
)
; end doLabel_ERROR


(defun doLabel (textType / A)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;; Start subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (defun placeLeader (text / pt1 pt2)

    (princ "\n\nPlacing Arrow to Text Note --> ")
    (princ text)
    (setq pt1 (getpoint "\nClick Arrowhead Location--> "))
    (setq pt2 (getpoint pt1 "\nClick Text Leader Location--> "))
    (princ "\n")
    (COMMAND "LEADER" PT1 PT2 "" text "")

  )


  (defun placeMtext (text / pt1 pt2)

    (princ "\n\nPlacing Multi-Line Text Note --> ")
    (princ text)
    (setq pt1 (getpoint "\nClick Top Left of Text--> "))
    (setq pt2 (getcorner pt1 "\nClick Bottom Right of Text--> "))
    (princ "\n")
    (command "-mtext" pt1 "H" fontSize "J" "MC" pt2 text "")
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
        (vl-load-com)
        (setq obj (vlax-ename->vla-object (entlast)))
        (vla-put-BackgroundFill obj :VLAX-TRUE)
        (vla-update obj)

        ; set background mask width
        (setq sset1 (ssadd))
        (ssadd (entlast) sset1)
        (modifySS sset1 45 1.5)  ; text mask offset
        (modifySS sset1 46 0)    ; text mask in v12 based on TL/BR - reset back to 0 to just put around text
      )
    )


  )


  (defun placeText (text / pt1 pt2 insertPoint temp)

    (princ "\n\nPlacing Rotated Text Note --> ")
    (princ text)
    (setq pt1 (getpoint "\nClick Bottom Left Corner of Text--> "))
    (setq pt2 (getpoint pt1 "\nClick Bottom Right Corner of Text--> "))
    (setq ang (* (angle pt1 pt2) (/ 180.0 pi)))
    (princ "\n")
    ; move pt1 and pt2 off line by 1/3 of text size
    (setq pt1 (polar pt1 (+ (angle pt1 pt2) (/ pi 2.0)) (* 0.333 fontSize)))
    (setq pt2 (polar pt2 (+ (angle pt1 pt2) (/ pi 2.0)) (* 0.333 fontSize)))

    (setq insertPoint (polar pt1 (angle pt1 pt2) (/ (distance pt1 pt2) 2.0)))

    (command "-mtext" insertPoint "H" fontSize "J" "BC" "R" ang insertPoint text "")


  )
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;; End subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ; error control - will reset DIM* if cancelled during label creation
  (setq lastErrorFunction *error*)
  (setq *error* doLabel_ERROR)


	; Sets arrow and text height
  (SETQ DIMASZ_DEF (GETVAR "DIMASZ"))
  (SETVAR "DIMASZ" arrowSize)
  (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
  (SETVAR "DIMTXT" fontSize)

  (SETQ DIMGAP_DEF (GETVAR "DIMGAP"))


        ; IF WORKING IN PAPER SPACE - NOT IN VPORT SET DIMGAP SMALLER
  (IF
    (AND (/= (GETVAR "CTAB") "MODEL")
         (= (GETVAR "CVPORT") 1)
    )
      (SETVAR "DIMGAP" 0.02)
  )

  (if (= textType "arrow")
    (progn
      (foreach A retList
        (placeLeader A)
      )      ; end foreach
    )        ; end progn
  )          ; end if

  (if (= textType "mText")
    (progn
      (foreach A retList
        (placeMtext A)
      )      ; end foreach
    )        ; end progn
  )          ; end if

  (if (= textType "rText")
    (progn
      (foreach A retList
        (placeText A)
      )      ; end foreach
    )        ; end progn
  )          ; end if

  (if (= textType "spline")
    (progn
      (foreach A retList
        (arSpline A)
      )      ; end foreach
    )        ; end progn
  )          ; end if

  (if (= textType "arrow")
    (progn
      (if (/= altText1 "")
        (progn
          ; Place Leader from altText if not nil
          (placeLeader altText1)
        )
      )
      (if (/= altText2 "")
        (progn
          ; Place Leader from altText if not nil
          (placeLeader altText2)
        )
      )
    )        ; end progn
  )          ; end if

  (if (= textType "mText")
    (progn
      (if (/= altText1 "")
        (progn
          ; Place Leader from altText if not nil
          (placeMtext altText1)
        )
      )
      (if (/= altText2 "")
        (progn
          ; Place Leader from altText if not nil
          (placeMtext altText2)
        )
      )
    )        ; end progn
  )          ; end if

  (if (= textType "rText")
    (progn
      (if (/= altText1 "")
        (progn
          ; Place Leader from altText if not nil
          (placeText altText1)
        )
      )
      (if (/= altText2 "")
        (progn
          ; Place Leader from altText if not nil
          (placeText altText2)
        )
      )
    )        ; end progn
  )          ; end if

  (if (= textType "spline")
    (progn
      (if (/= altText1 "")
        (progn
          ; Place Leader from altText if not nil
          (arSpline altText1)
        )
      )
      (if (/= altText2 "")
        (progn
          ; Place Leader from altText if not nil
          (arSpline altText2)
        )
      )
    )        ; end progn
  )          ; end if

  (SETVAR "DIMASZ" DIMASZ_DEF)
  (SETVAR "DIMTXT" DIMTXT_DEF)
  (SETVAR "DIMGAP" DIMGAP_DEF)

  (setq *error* lastErrorFunction)

  





)







(defun saveSlabelVars (/ readlist count item)

  (setq altText1 (get_tile "altText1"))
  (setq altText2 (get_tile "altText2"))
  (setq lastFontSize (get_tile "scaleList"))


; IF WORKING IN MODEL SPACE - START
  (IF
    (OR
      (= (GETVAR "CTAB") "MODEL")
      (AND (/= (GETVAR "CTAB") "MODEL")
           (/= (GETVAR "CVPORT") 1)
      )
    )
    (PROGN
      (if (/= (get_tile "fontSize") "")
        (progn
          (setq fontSize (atof (get_tile "fontSize")))
          (setq arrowSize (* fontSize scaleArrowSize))
        )
      )

      (if (= (get_tile "fontSize") "")
        (progn
          ; get the pull down list number
          (setq fontSize (get_tile "scaleList"))
          (if (= fontSize "0") (setq fontSize page2TextSize))
          (if (= fontSize "1") (setq fontSize page3TextSize))
          (if (= fontSize "2") (setq fontSize profileTextSize))
          (setq arrowSize (* fontSize scaleArrowSize))
        )  ; end progn
      )    ; end if

    )
; IF WORKING IN MODEL SPACE - END
; ELSE - WORKING IN PAPER SPACE - START
    (PROGN
      (IF (/= sLabel-paperSpace-textSize nil)
          (setq fontSize sLabel-paperSpace-textSize)
          ;else
          (setq fontSize 0.08)
      )
           (IF (/= sLabel-paperSpace-arrowHeadSize nil)
               (setq arrowSize sLabel-paperSpace-arrowHeadSize)
               ;else
               (setq arrowSize 0.15)
           )
    )
; ELSE - WORKING IN PAPER SPACE - END
  )

  ;;;--- Setup a list to hold the selected items
  (setq retList (list)
        retValuelist (list)
  )

  ;;;--- Save the list setting
  (setq readlist (get_tile "textList"))
  ;;;--- Setup a variable to run through the list
  (setq count 0)


  (if (/= readlist "")
    (progn
      ;;; convert line of space deliminated values to list of values
      (setq retValuelist (ST_str2lst readlist " "))
      (foreach item retValueList
        (setq retList (append retList (list (nth (atoi item) formList))))
      )
    )
    (setq retlist (list))
  )


; Now retlist has all text notes in list format

)          ; end saveVars



(defun C:slabel (/ tOrtho page2_string page3_string profile_string scaleList textType ddiag fil)

  ;;;--- Turn off the command echo
  (setvar "cmdecho" 0)
  (setq ANGDIR_DEF (GETVAR "ANGDIR"))
  (setvar "ANGDIR" 0)
  (SETQ ANGBASE_DEF (GETVAR "ANGBASE"))
  (setvar "ANGBASE" 0)

  ;;; - turn "orthomode" off
  (setq tOrtho (getvar "orthomode"))
  (setvar "orthomode" 0)

  (getUserVars)

  ; sets undo spot start 
  (command "UNDO" "BE")

  (setq labelList (list))

	  ;;;--- Open the text file
  (if (findfile (strcat $septicadPathText "slabel.txt"))
    (progn

      (if (setq fil (open (findfile (strcat $septicadPathText "slabel.txt")) "r"))
        (progn

          ;;;--- Skip the first three lines        
          (read-line fil)
          (read-line fil)
          (read-line fil)

          ;;;--- Read the lines of data in the text file
          (while (setq a (read-line fil))
            ;;;--- Add the data to the list
            (setq labelList
                  (append
                    labelList
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
  (foreach a labelList
    (setq formList (append formList (list a)))
  )


  (setScales)

     ;;;; Create a List of Values for the pulldown ;;;;
  (setq page2_string (strcat "1\":" (RTOS page2Scale 2 0) "' scale - Page 2"))
  (setq page3_string (strcat "1\":" (RTOS page3Scale 2 0) "' scale - Page 3"))
  (setq profile_string (strcat "1\":" (RTOS profileScale 2 0) "' scale - Cross Section"))
  (setq scaleList (list page2_string page3_string profile_string))


       ;;;--- Load the DCL file
  (setq dcl_id (load_dialog (strcat $septicadPathDCL "slabel.dcl")))

      ;;;--- See if the dialog box is already loaded
  (if (not (new_dialog "slabel" dcl_id))
    (progn
      (alert "The slabel.DCL file was not found!")
      (exit)
    )
  )

     ;;;; if Labelling in Pspace then 
  (if
    (AND (/= (GETVAR "CTAB") "MODEL")
         (= (GETVAR "CVPORT") 1)
    )
    (progn
      (setq scaleList (list "Labeling In Paper Space"))
      (mode_tile "scaleList" 1)
    )
  )


      ;;;--- Add the Scales to the listbox
  (start_list "scaleList" 3)
  (mapcar 'add_list scaleList)
  (end_list)

      ;;;--- Add the label names to the dialog box
  (start_list "textList" 3)
  (mapcar 'add_list formList)
  (end_list)

      ;;;--- If an action event occurs, do this function
  (action_tile "cancel" "(setq ddiag 1)(done_dialog)")
  (action_tile "arrow" "(setq ddiag 2)(saveSlabelVars)(done_dialog)")
  (action_tile "mText" "(setq ddiag 3)(saveSlabelVars)(done_dialog)")
  (action_tile "rText" "(setq ddiag 4)(saveSlabelVars)(done_dialog)")
  (action_tile "spline" "(setq ddiag 5)(saveSlabelVars)(done_dialog)")
  (action_tile "customize" "(setq ddiag -99)(done_dialog)")

  (upDateImage (strcat $septicadPathDCL "arrowtext.sld") "arrow")
  (upDateImage (strcat $septicadPathDCL "multiline.sld") "mText")
  (upDateImage (strcat $septicadPathDCL "rotated.sld") "rText")
  (upDateImage (strcat $septicadPathDCL "splinetext.sld") "spline")

;;;--- Set font size pull-down to last used if not nil
  (if (/= lastFontSize nil) (set_tile "scaleList" lastFontSize))

; if working in pspace inside a vport then autofill the correct font size
  (if
    (AND 
          (or (= (GETVAR "CTAB") "HHE_200_PG1") (= (GETVAR "CTAB") "HHE_200_PG2") (= (GETVAR "CTAB") "HHE_200_PG3") (= (GETVAR "CTAB") "HHE_200_PG4"))
          (/= (GETVAR "CVPORT") 1)
    )
    (progn
      ; if page 2 plan
      (if (= (getvar "CVPORT") 2) (set_tile "scaleList" "0"))
      ; if page 3 plan
      (if (= (getvar "CVPORT") 3) (set_tile "scaleList" "1"))
      ; if page 3 cross-section
      (if (= (getvar "CVPORT") 4) (set_tile "scaleList" "2"))
    )
  )



      ;;;--- Display the dialog box
  (start_dialog)


      ;;;--- Unload the dialog box
  (unload_dialog dcl_id)

      ;;;--- If the cancel button was pressed
  (if (= ddiag 1)
    (progn
      ;;;--- Set an exit message
      (princ)
      (Print "...Septic Label Cancelled.")
    )
  )


  ; determine what type of insert
  (if (= ddiag 2) (setq textType "arrow"))
  (if (= ddiag 3) (setq textType "mText"))
  (if (= ddiag 4) (setq textType "rText"))
  (if (= ddiag 5) (setq textType "spline"))

  (if (= ddiag -99) (sLabelCustom))
  (if (= ddiag -99) (c:slabel))

      ;;;--- If the "Okay" button was pressed
  (if (> ddiag 1)
    (progn

      (if (and (and (= retList nil) (= altText2 "")) (= altText1 ""))
        (Princ "\n...Nothing Selected - Canceled Command...")
        (doLabel textType)
      )
    )
  )

  ;;;--- Set variable to nil
  (setq altText1 nil altText2 nil fontSize nil retList nil)

  (command "UNDO" "E")

  (setq formList nil labelList nil)

  ;;; - turn "orthomode" back to what it was
  (setvar "orthomode" tOrtho)

  (setvar "ANGDIR" ANGDIR_DEF)
  (SETVAR "ANGBASE" ANGBASE_DEF)

  ;;;--- suppress the last echo for a clean exit
  (princ)

)
; end slabel


(defun sLabelPageNotes (/ fil a)
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
            (setq labelList
                  (append
                    labelList
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
  (foreach a labelList
    (setq formList (append formList (list a)))
  )
	;;;;;;;;;;;;;;;;;;;;;; End of GetTextList ;;;;;;;;;;;;;;;;;;;;;;;;;;;;


      ;;;--- Add the label names to the dialog box
  (start_list "textList" 3)
  (mapcar 'add_list formList)
  (end_list)


)          ; end sLabelPageNotes

(defun GetBlockList (/ tempList)

  ;;;--- Set up an empty list
  (setq aList (list))  ; list of identifiers
  (setq bList (list))  ; list of default text
  (setq cList (list))  ; list of file locations
  (setq dList (list))  ; list of rotate Y or N
  (setq eList (list))  ; list of explode on insert Y or N
  (setq fList (list))  ; list of scale on insert Y or N
  ;;;--- Open the text file
  (if (findfile (strcat $septicadPathText "sblock.txt"))
    (progn

      (if (setq fil (open (findfile (strcat $septicadPathText "sblock.txt")) "r"))
        (progn

          ;;;--- Read the lines of data in the text file
          (while (setq a (read-line fil))
            ;;;--- Add the data to the list
            (setq aList
                  (append
                    aList
                    (list a)
                  )
            )

            (setq a (read-line fil))
            (setq bList
                  (append
                    bList
                    (list a)
                  )
            )

            (setq a (read-line fil))
            (setq cList
                  (append
                    cList
                    (list a)
                  )
            )
            (setq a (read-line fil))
            (setq dList
                  (append
                    dList
                    (list a)
                  )
            )
            (setq a (read-line fil))
            (setq eList
                  (append
                    eList
                    (list a)
                  )
            )
            (setq a (read-line fil))
            (setq fList
                  (append
                    fList
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
  (setq tempList (list))
  (foreach a aList
    (setq tempList (append tempList (list a)))
  )
  (setq aList tempList)

  (setq tempList (list))
  (foreach a bList
    (setq tempList (append tempList (list a)))
  )
  (setq bList tempList)

  (setq tempList (list))
  (foreach a cList
    (setq tempList (append tempList (list a)))
  )
  (setq cList tempList)

  (setq tempList (list))
  (foreach a dList
    (setq tempList (append tempList (list a)))
  )
  (setq dList tempList)

  (setq tempList (list))
  (foreach a eList
    (setq tempList (append tempList (list a)))
  )
  (setq eList tempList)

  (setq tempList (list))
  (foreach a fList
    (setq tempList (append tempList (list a)))
  )
  (setq fList tempList)
      ;;;--- REQUIRED --- Removes End of Line marker??

)  ; end function






(defun doBlock (/ string block item p2 ex dt da)

      ;sets block insert unit vars
  (setBlockUnitsVars)

  (setvar "CmdEcho" 0)
  (setq ex (getvar "Expert"))
  (setvar "Expert" 2)

  (setq dt (getvar "dimtxt"))
  (setvar "dimtxt" fontSize)

  (setq da (getvar "dimasz"))
  (setvar "dimasz" (* fontSize scaleArrowSize))

      ; file location of block
  (setq block (nth 0 cForm))

      ; Set string for getpoint function
  (setq string (strcat "\nClick Insert Point for " (nth 0 aForm)))

      ; scale block if YES
  (setq scale 1.0)

  (if (= (nth 0 fForm) "Y")
    (progn
      (if (= fontSize page2TextSize) (setq scale (/ page2Scale 10.0)))
      (if (= fontSize page3TextSize) (setq scale (/ page3Scale 10.0)))
      (if (= fontSize profileTextSize) (setq scale profileScale))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
  (setq tempPt (list 10000 0 0))

      ; insert block and have user rotate
  (if (= (nth 0 dForm) "Y")
    (progn
      (if (= (strcase (getvar "PRODUCT")) "AUTOCAD")
        (progn
          (command "-insert" block (setq p1 (getpoint string)) scale scale "0")
          (command "rotate" "L" "" p1 (getpoint p1 "\nRotation:"))
        )
      )


      (if (= (strcase (getvar "PRODUCT")) "BRICSCAD")
        (progn
          (command "-insert" block tempPt scale scale "0")
          (command "move" "L" "" tempPT (setq p1 (getpoint string)))
          (command "rotate" "L" "" p1 (getpoint))
        )
      )

      (if (= (strcase (getvar "PRODUCT")) "INTELLICAD")
        (progn
          (command "-insert" block tempPt scale scale "0")
          (command "move" "L" "" tempPT (setq p1 (getpoint string)))
          (command "rotate" "L" "" p1 (getpoint))
        )
      )
    )
  )


      ; insert block and NO ROTATE
  (if (= (nth 0 dForm) "N")
    (progn
      (if (= (strcase (getvar "PRODUCT")) "AUTOCAD")
        (command "-insert" block (setq p1 (getpoint string)) scale scale "0")
      )

      (if (= (strcase (getvar "PRODUCT")) "BRICSCAD")
        (progn
          (command "-insert" block tempPt scale scale "0")
          (command "move" "L" "" tempPT (setq p1 (getpoint string)))
        )
      )

      (if (= (strcase (getvar "PRODUCT")) "INTELLICAD")
        (progn
          (command "-insert" block tempPt scale scale "0")
          (command "move" "L" "" tempPT (setq p1 (getpoint string)))
        )
      )
    )
  )


      ; if user wants to explode
  (if (= (nth 0 eForm) "Y")
    (progn
      (command "EXPLODE" (entlast))
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	

      ; text leader point
  (setq p2 (polar p1 (/ (* 4 pi) 3.0) (* 5 fontSize)))

      ; insert text leader if not blank
  (if (/= text "") (command "LEADER" p1 p2 "" text ""))

      ; Reset System Variable
  (setvar "Expert" ex)
  (setvar "dimtxt" dt)
  (setvar "dimasz" da)

;reset block insert unit vars
  (resetBlockUnitsVars)




)
;;;;; END doBlock function

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun blockText (/ tile txt)
  (setq tile (ATOI (get_tile "list_tile")))
  (setq txt (nth tile bList))
  (set_tile "text_description" txt)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun saveSblockVars (/ readlist count item)
  ;;;--- Setup a list to hold the selected items
  (setq aForm (list))
  (setq bForm (list))
  (setq cForm (list))
  (setq dForm (list))
  (setq eForm (list))
  (setq fForm (list))

  (setq text (get_tile "text_description"))  ; text label for block

  (if (/= (get_tile "fontSize") "") (setq fontSize (atof (get_tile "fontSize"))))

  (if (= (get_tile "fontSize") "")
    (progn
      ; get the pull down list number
      (setq fontSize (get_tile "scaleList"))
      (setq defaultFontSize (get_tile "scaleList"))
      (if (= fontSize "0") (setq fontSize page2TextSize))
      (if (= fontSize "1") (setq fontSize page3TextSize))
      (if (= fontSize "2") (setq fontSize profileTextSize))
    )
  )




  ;;;--- Save the list setting
  (setq readlist (get_tile "list_tile"))

  ;;;--- Setup a variable to run through the list
  (setq count 1)

  ;;;--- cycle through the list getting all of the selected items
  (while (setq item (read readlist))
    (setq aForm (append aForm (list (nth item aList))))
    (setq bForm (append bForm (list (nth item bList))))
    (setq cForm (append cForm (list (nth item cList))))
    (setq dForm (append dForm (list (nth item dList))))
    (setq eForm (append eForm (list (nth item eList))))
    (setq fForm (append fForm (list (nth item fList))))
    (while
      (and
        (/= " " (substr readlist count 1))
        (/= "" (substr readlist count 1))
      )
      (setq count (1+ count))
    )
    (setq readlist (substr readlist count))
  )

; Now aForm, bForm, cForm, dForm and eForm have all of the selections in list format

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun C:sblock (/ scaleList page2_string page3_string profile_string)

  ; default error control
  (setq lastErrorFunction *error*)
  (setq *error* DEFAULT_ERROR)

  (setq ANGDIR_DEF (GETVAR "ANGDIR"))
  (setvar "ANGDIR" 0)
  (SETQ ANGBASE_DEF (GETVAR "ANGBASE"))
  (setvar "ANGBASE" 0)

  ;;;--- Turn off the command echo
  (setvar "cmdecho" 0)

  ;;; - turn "orthomode" off
  (setq tOrtho (getvar "orthomode"))
  (setvar "orthomode" 0)

  (getUserVars)

  ; sets undo spot start 
  (command "UNDO" "BE")


  ;;;--- Get the label list in the text file - variables nilled after done
  (GetBlockList)
  ; return aList=id, bList=default text, cList= file location, dList = rotate block [Y or N].

  (setScales)

  ;;;; Create a List of Values for the pulldown ;;;;
  (setq page2_string (strcat "1\":" (RTOS page2Scale 2 0) "' scale - Page 2"))
  (setq page3_string (strcat "1\":" (RTOS page3Scale 2 0) "' scale - Page 3"))
  (setq profile_string (strcat "1\":" (RTOS profileScale 2 0) "' scale - Cross Section"))
  (setq scaleList (list page2_string page3_string profile_string))


       ;;;--- Load the DCL file
  (setq dcl_id (load_dialog (strcat $septicadPathDCL "sblock.dcl")))

      ;;;--- See if the dialog box is already loaded
  (if (not (new_dialog "sblock" dcl_id))
    (progn
      (alert "The sblock.DCL file was not found!")
      (exit)
    )
  )

      ;;;--- Add the Scales to the listbox
  (start_list "scaleList" 3)
  (mapcar 'add_list scaleList)
  (end_list)

      ;;;--- Add the label names to the dialog box
  (start_list "list_tile" 3)
  (mapcar 'add_list aList)
  (end_list)

      ;;;--- If an action event occurs, do this function
  (action_tile "cancel" "(setq ddiag 1)(done_dialog)")
  (action_tile "okay" "(setq ddiag 2)(saveSblockVars)(done_dialog)")
  (action_tile "CUSTOM" "(setq ddiag 3)(done_dialog)")

      ;;;--- Set to last font size
  (if (/= defaultFontSize nil) (set_tile "scaleList" defaultFontSize))


       ; if working in pspace inside a vport then autofill the correct font size
  (if
    (AND (= (GETVAR "CTAB") "HHE-200")
         (/= (GETVAR "CVPORT") 1)
    )
    (progn
      ; if page 2 plan
      (if (= (getvar "CVPORT") 2) (set_tile "scaleList" "0"))
      ; if page 3 plan
      (if (= (getvar "CVPORT") 3) (set_tile "scaleList" "1"))
      ; if page 3 cross-section
      (if (= (getvar "CVPORT") 4) (set_tile "scaleList" "2"))
    )
  )

     ; if working in pspace inside a vport then autofill the correct font size
  (if
    (AND (/= (GETVAR "CTAB") "MODEL")
         (= (GETVAR "CVPORT") 1)
    )
    (COMMAND "LAYOUT" "S" "MODEL")
  )



      ;;;--- Display the dialog box
  (start_dialog)


      ;;;--- Unload the dialog box
  (unload_dialog dcl_id)


      ;;;--- If the cancel button was pressed
  (if (= ddiag 1)
    (progn
      ; reset variables to nil
      (setq aForm nil aList nil bForm nil bList nil cForm nil cList nil dForm nil dList nil eForm nil eList nil)
      ;;;--- Set an exit message
      (Print "\n...Septic Block Cancelled...")
    )
  )

      ;;;--- If the "Okay" button was pressed
  (if (= ddiag 2)
    (progn

      (if (/= cForm nil) (doBlock))
      (if (= cForm nil) (Princ "\n...Nothing Selected - Canceled Command..."))
      ; reset variables to nil
      (setq aForm nil aList nil bForm nil bList nil cForm nil cList nil dForm nil dList nil eForm nil eList nil)
    )
  )

	  ;;;--- If the user pressed the profileNotes Button
  (if (= ddiag 3)
    (progn
      (sblockCustom)
      (c:sBlock)
    )
  )
  ; sets undo spot end  
  (command "UNDO" "E")

  ;;; - turn "orthomode" back to what it was
  (setvar "orthomode" tOrtho)

  (setq *error* lastErrorFunction)

  (setvar "ANGDIR" ANGDIR_DEF)
  (SETVAR "ANGBASE" ANGBASE_DEF)

  ;;;--- suppress the last echo for a clean exit
  (princ)

)
; END sblock







(defun dArrow (page / pt1 pt2 pt1a pt2a ang arrowLength arrowWidth)

; default error control
  (setq lastErrorFunction *error*)
  (setq *error* DEFAULT_ERROR)

  (SETVAR "CMDECHO" 0)
  (COMMAND "LAYOUT" "S" "MODEL")    ; Change to Model Layout

  (if (tblsearch "layer" "SEPTICAD")
    (progn
      (SETVAR "CLAYER" "SEPTICAD")  ; CHANGES LAYER
    )  ; end progn
  )    ; end if

;;;; check to see if mapscales are set
  (if (= page 2)
    (progn
      (if (/= page2TextSize nil)
        (progn
          (setq scale page2TextSize)
        )
      )
      (if (= page2TextSize nil)
        (progn
          ;;;; Check to see if 3 Scale are stored in USERI1, USERI2, USERI3
          ;;;; If not then call mapScale
          (setScales)
          (setq scale page2TextSize)
        )
      )
    )  ; end progn
  )    ; end if

  (if (= page 3)
    (progn
      (if (/= page3TextSize nil)
        (progn
          (setq scale page3TextSize)
        )
      )
      (if (= page3TextSize nil)
        (progn
          ;;;; Check to see if 3 Scale are stored in USERI1, USERI2, USERI3
          ;;;; If not then call mapScale
          (setScales)
          (setq scale page3TextSize)
        )
      )
    )  ; end progn
  )    ; end if


  (setq pt1 (getpoint "\nCLICK Location of Arrowhead->"))
  (setq pt2 (getpoint pt1 "\nCLICK Location of Arrowhead->"))
  (setq ang (angle pt1 pt2))

  (setq arrowLength scale)
  (setq arrowWidth (/ arrowLength 3.0))

  (setq pt1a (polar pt1 ang arrowLength))
  (setq pt2a (polar pt2 (- ang pi) arrowLength))

  (command "PLINE" pt1 "w" "0" arrowWidth pt1a "w" "0" "0" pt2a "w" arrowWidth "0" pt2 "")


  (setq *error* lastErrorFunction)

;;;; Clean Exit
  (princ)

)      ; end dArrow







(defun upDateImage (sldName key / width height)

  ;;;--- Get the width of the slide
  (setq width (dimx_tile key))

  ;;;--- Get the height of the slide 
  (setq height (dimy_tile key))

  ;;;--- Start the slide definition
  (start_image key)

  ;;;--- Wipe out the background
  ; 0 0 is topleft corner of row or column
;  (fill_image 0 0 width height 254) ; old background color
  (fill_image 0 0 width height -15)  ; new background color

  ;;;--- Put the slide in the image area
  (slide_image 0 0 width height sldName)

  ;;;--- Finish the slide definition
  (end_image)

)
; end upDateImage





















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sLabelCustom (/ textType ddiag fil labelList)

  ;;;--- Turn off the command echo
  (setvar "cmdecho" 0)

  (COMMAND "LAYOUT" "S" "MODEL")

  (if (= $septicadFullVersion nil) (septicadCheckReg))

	  ;;;--- Set up an empty list
  (setq labelList (list))

	  ;;;--- Open the text file
  (if (findfile (strcat $septicadPathText "slabel.txt"))
    (progn

      (if (setq fil (open (findfile (strcat $septicadPathText "slabel.txt")) "r"))
        (progn

          ;;;--- Skip the first three lines        
          (read-line fil)
          (read-line fil)
          (read-line fil)

          ;;;--- Read the lines of data in the text file
          (while (setq a (read-line fil))
            ;;;--- Add the data to the list
            (setq labelList
                  (append
                    labelList
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
  (foreach a labelList
    (setq formList (append formList (list a)))
  )
	;;;;;;;;;;;;;;;;;;;;;; End of GetTextList ;;;;;;;;;;;;;;;;;;;;;;;;;;;;





       ;;;--- Load the DCL file
  (setq dcl_id (load_dialog (strcat $septicadPathDCL "customEdit.dcl")))

      ;;;--- See if the dialog box is already loaded
  (if (not (new_dialog "customEdit" dcl_id))
    (progn
      (alert "The customEdit.DCL file was not found!")
      (exit)
    )
  )

      ;;;--- Add the label names to the dialog box
  (start_list "textList" 3)
  (mapcar 'add_list formList)
  (end_list)

  (set_tile "DESCRIPTION" "Customize the Labels available with SLABEL or SLL functions.")


      ;;;--- If an action event occurs, do this function
  (action_tile "save" "(setq ddiag 0)(done_dialog)")
  (action_tile "cancel" "(setq ddiag 1)(done_dialog)")
  (action_tile "add" "(customEditVars)(customAdd)")
  (action_tile "edit" "(customEditVars)(customNote)")
  (action_tile "delete" "(customEditVars)(customDelete)")
  (action_tile "moveup" "(customMoveUp)")
  (action_tile "movedown" "(customMoveDown)")
  (action_tile "editMake" "(customEditVars)(customMake)")
  (action_tile "notepad" "(setq ddiag 2)(done_dialog)")

      ;;;--- Display the dialog box
  (start_dialog)


      ;;;--- Unload the dialog box
  (unload_dialog dcl_id)

      ;;;--- If the SAVE button was pressed
  (if (= ddiag 0)
    (progn
      ; if DEMO MODE
      (if (= $septicadFullVersion 1)
        (progn
          ;;;--- Open the text file

          (setq f (open (strcat $septicadPathText "slabel.txt") "w"))
          (write-line "Each line represents a note ---do not delete" f)
          (write-line "Special Characters \P = new line ---do not delete" f)
          (write-line "%%p = plus or minus, %%d = degree symbol  ---do not delete" f)

          ; loop to write list to file
          (foreach item formlist
            (progn
              (write-line item f)
            )  ; progn
          )    ; end foreach

          ;;;--- Close the file.
          (close f)

          ;;;--- Set an exit message
          (print "Septic Labels changed")
        )      ; progn
      )        ; if

      (if (= $septicadFullVersion 0)
        (progn
          (Alert " UNREGISTERED VERSION - PLEASE REGISTER\n Function only supported in Full Version.")
          (septicadCheckReg)
        )      ; progn
      )        ; if
    )
  )


      ;;;--- If the CANCEL button was pressed
  (if (= ddiag 1)
    (progn
      ;;;--- Set an exit message
      (print "Cancelled - No Changes Saved")
    )
  )


      ;;;--- If the Open with Notepad button was pressed
  (if (= ddiag 2)
    (startapp "NOTEPAD" (strcat $septicadPathText "slabel.txt"))
  )



  ;;;--- suppress the last echo for a clean exit
  (princ)

)  ; end sLabelCustom

(defun pageNotesCustom (/ textType ddiag fil labelList)

  (if (= $septicadFullVersion nil) (septicadCheckReg))

  ;;;--- Turn off the command echo
  (setvar "cmdecho" 0)

  (COMMAND "LAYOUT" "S" "MODEL")  ; Change to Model Layout


	  ;;;--- Set up an empty list
  (setq labelList (list))

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
            (setq labelList
                  (append
                    labelList
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
  (foreach a labelList
    (setq formList (append formList (list a)))
  )
	;;;;;;;;;;;;;;;;;;;;;; End of GetTextList ;;;;;;;;;;;;;;;;;;;;;;;;;;;;





       ;;;--- Load the DCL file
  (setq dcl_id (load_dialog (strcat $septicadPathDCL "customEdit.dcl")))

      ;;;--- See if the dialog box is already loaded
  (if (not (new_dialog "customEdit" dcl_id))
    (progn
      (alert "The customEdit.DCL file was not found!")
      (exit)
    )
  )

  (set_tile "DESCRIPTION" "Customize the Note Libary")

      ;;;--- Add the label names to the dialog box
  (start_list "textList" 3)
  (mapcar 'add_list formList)
  (end_list)

      ;;;--- If an action event occurs, do this function
  (action_tile "save" "(setq ddiag 0)(done_dialog)")
  (action_tile "cancel" "(setq ddiag 1)(done_dialog)")
  (action_tile "add" "(customEditVars)(customAdd)")
  (action_tile "edit" "(customEditVars)(customNote)")
  (action_tile "delete" "(customEditVars)(customDelete)")
  (action_tile "moveup" "(customMoveUp)")
  (action_tile "movedown" "(customMoveDown)")
  (action_tile "editMake" "(customEditVars)(customMake)")
  (action_tile "notepad" "(setq ddiag 2)(done_dialog)")

      ;;;--- Display the dialog box
  (start_dialog)


      ;;;--- Unload the dialog box
  (unload_dialog dcl_id)

      ;;;--- If the SAVE button was pressed
  (if (= ddiag 0)
    (progn

      (if (= $septicadFullVersion 1)
        (progn
          ;;;--- Open the text file
          (setq f (open (strcat $septicadPathText "pageNotes.txt") "w"))
          (write-line " Each line represents a note that can be inserted in on HHE-200 Page 2 and/or 3 Map View  ---do not delete" f)
          (write-line " Special Characters: see CAD documentation -- Examples: \\P = new line \\Lunderlined\\l      ---do not delete" f)
          (write-line " %%p = plus or minus %%d = degree symbol %%c = center symbol                              ---do not delete" f)

          ; loop to write list to file
          (foreach item formlist
            (progn
              (write-line item f)
            )  ; progn
          )    ; end foreach

          ;;;--- Close the file.
          (close f)

          ;;;--- Set an exit message
          (print "SeptiCAD - Page Notes Changed")
        )      ; progn
      )        ; if

      (if (= $septicadFullVersion 0)
        (progn
          (Alert " UNREGISTERED VERSION - PLEASE REGISTER\n Function only supported in Full Version.")
          (septicadCheckReg)
        )      ; progn
      )        ; if


    )
  )


      ;;;--- If the CANCEL button was pressed
  (if (= ddiag 1)
    (progn
      ;;;--- Set an exit message
      (print "Cancelled - No Changes Saved")
    )
  )


      ;;;--- If the Open with Notepad button was pressed
  (if (= ddiag 2)
    (startapp "NOTEPAD" (strcat $septicadPathText "pageNotes.txt"))
  )


  ;;;--- suppress the last echo for a clean exit
  (princ)

)      ; end pageNotesCustom


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;










(defun customEditVars (/ readlist count item)

  (setq textField (get_tile "textField"))

  ;;;--- Save the list setting
  (setq textItem (get_tile "textList"))

)
; end customEditVars



(defun customDelete (/ tempList count)

  (if (/= textItem "")
    (progn
      (setq count 0)
      (foreach item formList
        (progn
          (if (/= count (atoi textItem)) (setq tempList (append tempList (list item))))
          (setq count (+ count 1))
        )
; progn
      )
; foreach

      (setq formList tempList)

      (start_list "textList" 3)
      (mapcar 'add_list formList)
      (end_list)

    )
; progn
  )
; if 

)
; end customDelete

(defun customAdd (/ count tempList)

  (setq count 0)
  (if (/= textItem "")
    (progn
      (foreach item formList
        (progn
          (if (= count (atoi textItem))
            (setq tempList (append tempList (list textField)))
          )
          (setq count (+ count 1))
          (setq tempList (append tempList (list item)))
        )
; progn
      )
; foreach
    )
; progn
  )
; if



  (if (= textItem "")
    (setq tempList (append formList (list textField)))
  )
; if

  (setq formList tempList)

  (start_list "textList" 3)
  (mapcar 'add_list formList)
  (end_list)

)
; end customAdd

(defun customNote ()

  (if (/= (get_tile "textList") "")
    (progn
      (set_tile "newEditField" (nth (atoi (get_tile "textList")) formList))
      (set_tile "editField" (nth (atoi (get_tile "textList")) formList))
      (mode_tile "newEditField" 0)
    )
; progn
  )
; if 

  (if (= (get_tile "textList") "")
    (alert " Choose a note to edit then click the EDIT button.\nModify the text and click MAKE CHANGES button.")
  )
; if



)
; end customNote

(defun customMake (/ item tempList count)

  (if (/= (get_tile "editField") "")
    (progn
      (setq count 0)
      (foreach item formList
        (progn
          (if (= count (atoi textItem))
            (setq tempList (append tempList (list (get_tile "newEditField"))))
          )
          (if (/= count (atoi textItem))
            (setq tempList (append tempList (list item)))
          )
          (setq count (+ count 1))
        )
; progn
      )
; foreach


      (setq formList tempList)

      (start_list "textList" 3)
      (mapcar 'add_list formList)
      (end_list)

      (set_tile "editField" "")
      (set_tile "newEditField" "")

    )
; progn
  )
; if


)
; end customMake


(defun customMoveUp (/ item tempList count itemLocation)


  (if (and (/= (get_tile "textList") "") (/= (atoi (get_tile "textList")) 0))
    (progn
      (setq count 0)

      ; add the first part of the list
      (while (< count (- (atoi (get_tile "textList")) 1))
        (setq tempList (append tempList (list (nth count formList))))
        (setq count (+ count 1))

      )
; while
      (setq itemLocation count)

      (setq tempList (append tempList (list (nth (atoi (get_tile "textList")) formList))))
      (setq tempList (append tempList (list (nth (- (atoi (get_tile "textList")) 1) formList))))

      (setq count (+ count 2))

      (while (< count (length formList))
        (setq tempList (append tempList (list (nth count formList))))
        (setq count (+ count 1))
      )
; while

      (setq formList tempList)

      (start_list "textList" 3)
      (mapcar 'add_list formList)
      (end_list)

      (set_tile "textList" (rtos itemLocation 2 0))
    )
;progn
  )
; if


)
; end customMoveUp

(defun customMoveDown (/ item tempList count itemLocation)

  (setq count 0)

  (if (and (/= (get_tile "textList") "") (/= (- (length formList) 1) (atoi (get_tile "textList"))))
    (progn
      ; add the first part of the list
      (while (< count (atoi (get_tile "textList")))
        (setq tempList (append tempList (list (nth count formList))))
        (setq count (+ count 1))
      )
; while
      (setq itemLocation (+ count 1))
      (setq tempList (append tempList (list (nth (+ (atoi (get_tile "textList")) 1) formList))))
      (setq tempList (append tempList (list (nth (atoi (get_tile "textList")) formList))))

      (setq count (+ count 2))

      (while (< count (length formList))
        (setq tempList (append tempList (list (nth count formList))))
        (setq count (+ count 1))
      )
; while

      (setq formList tempList)

      (start_list "textList" 3)
      (mapcar 'add_list formList)
      (end_list)
      (set_tile "textList" (rtos itemLocation 2 0))
    )
;progn
  )
; if



)
; end customMoveDown


(defun sBlockCustomVars ()
  (setq sTile (get_tile "list_tile"))
)

(defun sblockCustom ()
  (setvar "cmdecho" 0)
	; GETS aList bList cList dList eList eList FROM FILE
  (if (= aList nil) (GetBlockList))

  (if (= $septicadFullVersion nil) (septicadCheckReg))

; bring up modified sblock window....
       ;;;--- Load the DCL file
  (setq dcl_id (load_dialog (strcat $septicadPathDCL "sBlockCustom.dcl")))

      ;;;--- See if the dialog box is already loaded
  (if (not (new_dialog "sBlockCustom" dcl_id))
    (progn
      (alert "The sBlockCustom.DCL file was not found!")
      (exit)
    )
  )

      ;;;--- Add the label names to the dialog box
  (start_list "list_tile" 3)
  (mapcar 'add_list aList)
  (end_list)

      ;;;--- If an action event occurs, do this function
  (action_tile "MOVEDOWN" "(sBlockMoveDown)")
  (action_tile "MOVEUP" "(sBlockMoveUp)")
  (action_tile "DELETE" "(sBlockDelete)")
  (action_tile "EDIT" "(sBlockCustomVars)(done_dialog)(setq bdiag 1)")
  (action_tile "ADD" "(sBlockCustomVars)(done_dialog)(setq bdiag 2)")
  (action_tile "SAVE" "(setq bdiag -99)(done_dialog)")
  (action_tile "CANCEL" "(setq bdiag 0)(done_dialog)")

  (if (/= sTile nil) (set_tile "list_tile" sTile))

      ;;;--- Display the dialog box
  (start_dialog)


      ;;;--- Unload the dialog box
  (unload_dialog dcl_id)

;;;--- If the Save button was pressed
  (if (= bdiag -99)
    (progn

      ; if DEMO MODE
      (if (= $septicadFullVersion 1)
        (progn
          (setq f (open (strcat $septicadPathText "sblock.txt") "w"))

          (setq cnt 0)
          (foreach item aList

            (write-line (strcat "" (nth cnt aList) "") f)
            (write-line (strcat "" (nth cnt bList) "") f)
            (write-line (strcat "" (nth cnt cList) "") f)
            (write-line (strcat "" (nth cnt dList) "") f)
            (write-line (strcat "" (nth cnt eList) "") f)
            (write-line (strcat "" (nth cnt fList) "") f)
            (setq cnt (+ cnt 1))
          )

          (close f)

        )
      )

      (if (= $septicadFullVersion 0)
        (progn
          (Alert " UNREGISTERED VERSION - PLEASE REGISTER\n Function only supported in Full Version.")
          (septicadCheckReg)
        )
      )

    )  ; progn
  )    ; if

  (if (= bdiag 1)
    (progn
      (princ)
      (if (= sTile "") (Alert "A Block must be selected"))
      (if (/= sTile "") (sBlockAddEdit "EDIT"))
      (sblockCustom)
    )
  )
  (if (= bdiag 2)
    (progn
      (princ)
      (sBlockAddEdit "ADD")

      (sblockCustom)
    )
  )

  (princ)
)
; end sblockCustom


(defun sBlockAddEditAdd-FileName (/ newFileName)
  (setq newFileName (getfiled "Select DWG File" " " "DWG" 2))
  (if (/= newFileName nil) (set_tile "FILE" newFileName))
)

(defun sBlockAddEdit (choice)

       ;;;--- Load the DCL file
  (setq dcl_id (load_dialog (strcat $septicadPathDCL "sBlockAddEdit.dcl")))

      ;;;--- See if the dialog box is already loaded
  (if (not (new_dialog "sBlockAddEdit" dcl_id))
    (progn
      (alert "The sBlockAddEdit.DCL file was not found!")
      (exit)
    )
  )


  (if (= choice "ADD") (set_tile "TITLE" "ADD SeptiCAD Block Library Item"))
  (if (= choice "EDIT") (set_tile "TITLE" "EDIT SeptiCAD Block Library Item"))

	; add data to form if item is chose and edit button is pressed
  (if (and (/= sTile "") (= choice "EDIT"))
    (progn
      (setq item (atoi sTile))
      (set_tile "ID" (nth item aList))
      (set_tile "TEXT" (nth item bList))
      (set_tile "FILE" (nth item cList))
      (if (= (nth item dList) "Y") (set_tile "ROTATE" "1"))
      (if (= (nth item eList) "Y") (set_tile "EXPLODE" "1"))
      (if (= (nth item fList) "Y") (set_tile "SCALE" "1"))
    )  ; progn
  )    ; if


      ;;;--- If an action event occurs, do this function
  (action_tile "SAVE" "(sBlockAddEditDCL)(setq bdiag -99)(done_dialog)")
  (action_tile "CANCEL" "(setq bdiag 0)(done_dialog)")
  (action_tile "FILEUPDATE" "(sBlockAddEditAdd-FileName)")


      ;;;--- Display the dialog box
  (start_dialog)


      ;;;--- Unload the dialog box
  (unload_dialog dcl_id)

;;;--- If the Save button was pressed
  (if (= bdiag -99)
    (progn
      (princ)
      (if (= choice "ADD") (sBlockAddDCL))
      (if (= choice "EDIT") (sBlockEditDCL))
    )
  )




  (princ)

)


(defun sBlockMoveUp (/ item tempList count)

  (if (AND (/= (get_tile "list_tile") "0") (/= (get_tile "list_tile") ""))
    (progn

      ;;;;;;;;;;;;;;;;;; change aList;;;;;;;;;;;;;;;;;;;;
      (setq count 0)
      (setq tempList (list))
      ; add the first part of the list
      (setq item (atoi (get_tile "list_tile")))
      (while (< count (- item 1))
        (setq tempList (append tempList (list (nth count aList))))
        (setq count (+ count 1))
      )
; while

      (setq tempList (append tempList (list (nth item aList))))
      (setq tempList (append tempList (list (nth (- item 1) aList))))

      (setq count (+ count 2))

      (while (< count (length aList))
        (setq tempList (append tempList (list (nth count aList))))
        (setq count (+ count 1))
      )
; while

      (setq aList tempList)

      ;;;;;;;;;;;;;;;;;; change bList;;;;;;;;;;;;;;;;;;;;
      (setq count 0)
      (setq tempList (list))
      ; add the first part of the list

      (while (< count (- item 1))
        (setq tempList (append tempList (list (nth count bList))))
        (setq count (+ count 1))
      )
; while

      (setq tempList (append tempList (list (nth item bList))))
      (setq tempList (append tempList (list (nth (- item 1) bList))))

      (setq count (+ count 2))

      (while (< count (length bList))
        (setq tempList (append tempList (list (nth count bList))))
        (setq count (+ count 1))
      )
; while

      (setq bList tempList)

      ;;;;;;;;;;;;;;;;;; change cList;;;;;;;;;;;;;;;;;;;;
      (setq count 0)
      (setq tempList (list))
      ; add the first part of the list

      (while (< count (- item 1))
        (setq tempList (append tempList (list (nth count cList))))
        (setq count (+ count 1))
      )
; while

      (setq tempList (append tempList (list (nth item cList))))
      (setq tempList (append tempList (list (nth (- item 1) cList))))

      (setq count (+ count 2))

      (while (< count (length cList))
        (setq tempList (append tempList (list (nth count cList))))
        (setq count (+ count 1))
      )
; while

      (setq cList tempList)

      ;;;;;;;;;;;;;;;;;; change dList;;;;;;;;;;;;;;;;;;;;
      (setq count 0)
      (setq tempList (list))
      ; add the first part of the list

      (while (< count (- item 1))
        (setq tempList (append tempList (list (nth count dList))))
        (setq count (+ count 1))
      )
; while

      (setq tempList (append tempList (list (nth item dList))))
      (setq tempList (append tempList (list (nth (- item 1) dList))))

      (setq count (+ count 2))

      (while (< count (length dList))
        (setq tempList (append tempList (list (nth count dList))))
        (setq count (+ count 1))
      )
; while

      (setq dList tempList)

      ;;;;;;;;;;;;;;;;;; change eList;;;;;;;;;;;;;;;;;;;;
      (setq count 0)
      (setq tempList (list))
      ; add the first part of the list

      (while (< count (- item 1))
        (setq tempList (append tempList (list (nth count eList))))
        (setq count (+ count 1))
      )
; while

      (setq tempList (append tempList (list (nth item eList))))
      (setq tempList (append tempList (list (nth (- item 1) eList))))

      (setq count (+ count 2))

      (while (< count (length eList))
        (setq tempList (append tempList (list (nth count eList))))
        (setq count (+ count 1))
      )
; while

      (setq eList tempList)


      ;;;;;;;;;;;;;;;;;; change fList;;;;;;;;;;;;;;;;;;;;
      (setq count 0)
      (setq tempList (list))
      ; add the first part of the list

      (while (< count (- item 1))
        (setq tempList (append tempList (list (nth count fList))))
        (setq count (+ count 1))
      )
; while

      (setq tempList (append tempList (list (nth item fList))))
      (setq tempList (append tempList (list (nth (- item 1) fList))))

      (setq count (+ count 2))

      (while (< count (length fList))
        (setq tempList (append tempList (list (nth count fList))))
        (setq count (+ count 1))
      )
; while

      (setq fList tempList)




      ; update DCL TILE
      (start_list "list_tile" 3)
      (mapcar 'add_list aList)
      (end_list)
      ; select last selection

      (set_tile "list_tile" (rtos (- item 1) 2 0))

    )
; progn
  )
; if

)
; end sBlockMoveUp







(defun sBlockMoveDown (/ item tempList count itemLocation)

  (if (AND (/= (atoi (get_tile "list_tile")) (- (length aList) 1)) (/= (get_tile "list_tile") ""))
    (progn
      (setq itemLocation 0)
      (setq item (atoi (get_tile "list_tile")))


      ;;;;;;;;;;;;;;;;;;;;;;;;; modify aList;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (setq count 0)
      (setq tempList (list))
      (while (< count item)
        (setq tempList (append tempList (list (nth count aList))))
        (setq count (+ count 1))
      )
; while
      (setq itemLocation (+ count 1))
      (setq tempList (append tempList (list (nth (+ item 1) aList))))
      (setq tempList (append tempList (list (nth item aList))))

      (setq count (+ count 2))

      (while (< count (length aList))
        (setq tempList (append tempList (list (nth count aList))))
        (setq count (+ count 1))
      )
; while

      (setq aList tempList)


      ;;;;;;;;;;;;;;;;;;;;;;;;; modify bList;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (setq count 0)
      (setq tempList (list))
      (while (< count item)
        (setq tempList (append tempList (list (nth count bList))))
        (setq count (+ count 1))
      )
; while
      (setq itemLocation (+ count 1))
      (setq tempList (append tempList (list (nth (+ item 1) bList))))
      (setq tempList (append tempList (list (nth item bList))))

      (setq count (+ count 2))

      (while (< count (length bList))
        (setq tempList (append tempList (list (nth count bList))))
        (setq count (+ count 1))
      )
; while

      (setq bList tempList)



      ;;;;;;;;;;;;;;;;;;;;;;;;; modify cList;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (setq count 0)
      (setq tempList (list))
      (while (< count item)
        (setq tempList (append tempList (list (nth count cList))))
        (setq count (+ count 1))
      )
; while
      (setq itemLocation (+ count 1))
      (setq tempList (append tempList (list (nth (+ item 1) cList))))
      (setq tempList (append tempList (list (nth item cList))))

      (setq count (+ count 2))

      (while (< count (length cList))
        (setq tempList (append tempList (list (nth count cList))))
        (setq count (+ count 1))
      )
; while

      (setq cList tempList)

      ;;;;;;;;;;;;;;;;;;;;;;;;; modify dList;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (setq count 0)
      (setq tempList (list))
      (while (< count item)
        (setq tempList (append tempList (list (nth count dList))))
        (setq count (+ count 1))
      )
; while
      (setq itemLocation (+ count 1))
      (setq tempList (append tempList (list (nth (+ item 1) dList))))
      (setq tempList (append tempList (list (nth item dList))))

      (setq count (+ count 2))

      (while (< count (length dList))
        (setq tempList (append tempList (list (nth count dList))))
        (setq count (+ count 1))
      )
; while

      (setq dList tempList)


      ;;;;;;;;;;;;;;;;;;;;;;;;; modify eList;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (setq count 0)
      (setq tempList (list))
      (while (< count item)
        (setq tempList (append tempList (list (nth count eList))))
        (setq count (+ count 1))
      )
; while
      (setq itemLocation (+ count 1))
      (setq tempList (append tempList (list (nth (+ item 1) eList))))
      (setq tempList (append tempList (list (nth item eList))))

      (setq count (+ count 2))

      (while (< count (length eList))
        (setq tempList (append tempList (list (nth count eList))))
        (setq count (+ count 1))
      )
; while

      (setq eList tempList)

      ;;;;;;;;;;;;;;;;;;;;;;;;; modify fList;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (setq count 0)
      (setq tempList (list))
      (while (< count item)
        (setq tempList (append tempList (list (nth count fList))))
        (setq count (+ count 1))
      )
; while
      (setq itemLocation (+ count 1))
      (setq tempList (append tempList (list (nth (+ item 1) fList))))
      (setq tempList (append tempList (list (nth item fList))))

      (setq count (+ count 2))

      (while (< count (length fList))
        (setq tempList (append tempList (list (nth count fList))))
        (setq count (+ count 1))
      )
; while

      (setq fList tempList)


;;;;;;;;;;;;;;;	
      ; update DCL TILE
      (start_list "list_tile" 3)
      (mapcar 'add_list aList)
      (end_list)

      ; select last selection
      (set_tile "list_tile" (rtos itemLocation 2 0))

    )
; progn
  )
; if


)
; end sBlockMoveDown

(defun sBlockDelete (/ item count tempList)
  (setq item (atoi (get_tile "list_tile")))
	; loop to change printerList
  (setq count 0)
  (setq tempList (list))
  (while (< count (length aList))
    (if (/= count item)
      (progn
        (setq tempList (append tempList (list (nth count aList))))
      )
; progn
    )
; if
    (setq count (+ count 1))
  )
; end while
  (setq aList tempList)

  (setq count 0)
  (setq tempList (list))
  (while (< count (length bList))
    (if (/= count item)
      (progn
        (setq tempList (append tempList (list (nth count bList))))
      )
; progn
    )
; if
    (setq count (+ count 1))
  )
; end while
  (setq bList tempList)

  (setq count 0)
  (setq tempList (list))
  (while (< count (length cList))
    (if (/= count item)
      (progn
        (setq tempList (append tempList (list (nth count cList))))
      )
; progn
    )
; if
    (setq count (+ count 1))
  )
; end while
  (setq cList tempList)

  (setq count 0)
  (setq tempList (list))
  (while (< count (length dList))
    (if (/= count item)
      (progn
        (setq tempList (append tempList (list (nth count dList))))
      )
; progn
    )
; if
    (setq count (+ count 1))
  )
; end while
  (setq dList tempList)

  (setq count 0)
  (setq tempList (list))
  (while (< count (length eList))
    (if (/= count item)
      (progn
        (setq tempList (append tempList (list (nth count eList))))
      )
; progn
    )
; if
    (setq count (+ count 1))
  )
; end while
  (setq eList tempList)

  (setq count 0)
  (setq tempList (list))
  (while (< count (length fList))
    (if (/= count item)
      (progn
        (setq tempList (append tempList (list (nth count fList))))
      )
; progn
    )
; if
    (setq count (+ count 1))
  )
; end while
  (setq fList tempList)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; update DCL TILE
  (start_list "list_tile" 3)
  (mapcar 'add_list aList)
  (end_list)
)
; defun sBlockDelete


(defun sBlockAddDCL (/ count tempList flag)
  (setq flag (atoi sTile))

	; add to aList
  (setq count 0)
  (setq tempList (list))
  (foreach item aList
    (progn
      (if (= count flag)
        (setq tempList (append tempList (list aDCL)))
      )
      (setq count (+ count 1))
      (setq tempList (append tempList (list item)))
    )
; progn
  )
; foreach
  (setq aList tempList)

	; add to bList
  (setq count 0)
  (setq tempList (list))
  (foreach item bList
    (progn
      (if (= count flag)
        (setq tempList (append tempList (list bDCL)))
      )
      (setq count (+ count 1))
      (setq tempList (append tempList (list item)))
    )
; progn
  )
; foreach
  (setq bList tempList)

	; add to cList
  (setq count 0)
  (setq tempList (list))
  (foreach item cList
    (progn
      (if (= count flag)
        (setq tempList (append tempList (list cDCL)))
      )
      (setq count (+ count 1))
      (setq tempList (append tempList (list item)))
    )
; progn
  )
; foreach
  (setq cList tempList)

	; add to dList
  (setq count 0)
  (setq tempList (list))
  (foreach item dList
    (progn
      (if (= count flag)
        (setq tempList (append tempList (list dDCL)))
      )
      (setq count (+ count 1))
      (setq tempList (append tempList (list item)))
    )
; progn
  )
; foreach
  (setq dList tempList)

	; add to eList
  (setq count 0)
  (setq tempList (list))
  (foreach item eList
    (progn
      (if (= count flag)
        (setq tempList (append tempList (list eDCL)))
      )
      (setq count (+ count 1))
      (setq tempList (append tempList (list item)))
    )
; progn
  )
; foreach
  (setq eList tempList)

	; add to fList
  (setq count 0)
  (setq tempList (list))
  (foreach item fList
    (progn
      (if (= count flag)
        (setq tempList (append tempList (list fDCL)))
      )
      (setq count (+ count 1))
      (setq tempList (append tempList (list item)))
    )
; progn
  )
; foreach
  (setq fList tempList)


	; UPDATE DCL TILE
  (start_list "list_tile" 3)
  (mapcar 'add_list aList)
  (end_list)

)
; defun sBlockAddDCL

(defun sBlockAddEditDCL ()
  (setq aDCL (get_tile "ID"))
  (setq bDCL (get_tile "TEXT"))
  (setq cDCL (get_tile "FILE"))

  (if (= (get_tile "ROTATE") "1") (setq dDCL "Y"))
  (if (= (get_tile "ROTATE") "0") (setq dDCL "N"))

  (if (= (get_tile "EXPLODE") "1") (setq eDCL "Y"))
  (if (= (get_tile "EXPLODE") "0") (setq eDCL "N"))

  (if (= (get_tile "SCALE") "1") (setq fDCL "Y"))
  (if (= (get_tile "SCALE") "0") (setq fDCL "N"))


)
; end sBlockAddEditDCL

(defun sBlockEditDCL (/ flag count tempList)
  (setq flag (atoi sTile))

	; add to aList
  (setq count 0)
  (setq tempList (list))
  (foreach item aList
    (progn
      (if (= count flag)
        (setq tempList (append tempList (list aDCL)))
      )
      (if (/= count flag) (setq tempList (append tempList (list item))))

      (setq count (+ count 1))
    )
; progn
  )
; foreach
  (setq aList tempList)

	; add to bList
  (setq count 0)
  (setq tempList (list))
  (foreach item bList
    (progn
      (if (= count flag)
        (setq tempList (append tempList (list bDCL)))
      )
      (if (/= count flag) (setq tempList (append tempList (list item))))

      (setq count (+ count 1))
    )
; progn
  )
; foreach
  (setq bList tempList)

	; add to cList
  (setq count 0)
  (setq tempList (list))
  (foreach item cList
    (progn
      (if (= count flag)
        (setq tempList (append tempList (list cDCL)))
      )
      (if (/= count flag) (setq tempList (append tempList (list item))))

      (setq count (+ count 1))
    )
; progn
  )
; foreach
  (setq cList tempList)

	; add to dList
  (setq count 0)
  (setq tempList (list))
  (foreach item dList
    (progn
      (if (= count flag)
        (setq tempList (append tempList (list dDCL)))
      )
      (if (/= count flag) (setq tempList (append tempList (list item))))

      (setq count (+ count 1))
    )
; progn
  )
; foreach
  (setq dList tempList)

	; add to eList
  (setq count 0)
  (setq tempList (list))
  (foreach item eList
    (progn
      (if (= count flag)
        (setq tempList (append tempList (list eDCL)))
      )
      (if (/= count flag) (setq tempList (append tempList (list item))))

      (setq count (+ count 1))
    )
; progn
  )
; foreach
  (setq eList tempList)


	; add to fList
  (setq count 0)
  (setq tempList (list))
  (foreach item fList
    (progn
      (if (= count flag)
        (setq tempList (append tempList (list fDCL)))
      )
      (if (/= count flag) (setq tempList (append tempList (list item))))

      (setq count (+ count 1))
    )
; progn
  )
; foreach
  (setq fList tempList)


	; UPDATE DCL TILE
  (start_list "list_tile" 3)
  (mapcar 'add_list aList)
  (end_list)


)
; end sBlockEditDCL













(defun c:sWell (/ w l p1 p2 p3 p4 pa pb pc pd pe pf pg ph ang_p1p2 ang_p2p1 ang_p2p3 ang_p3p2 setback)

  (if (tblsearch "layer" "SEPTICAD")
    (SETVAR "CLAYER" "SEPTICAD")
  )

    ; set the setback
  (setq setback 100.0)
  (setq temp (getreal "\nWell setback distance in feet [e.g 100]-->"))

  (if (AND (/= temp 0) (/= temp nil))
    (setq setback temp)
  )

  (setq p1 (getpoint "\nPick Top Left corner of bed"))
  (setq p2 (getpoint "\nPick Top Right corner of bed"))
  (setq p3 (getpoint "\nPick Bottom Right corner of bed"))

  (setvar "plinewid" 0)

  (setq ang_p1p2 (angle p1 p2))
  (setq ang_p2p1 (angle p2 p1))
  (setq ang_p2p3 (angle p2 p3))
  (setq ang_p3p2 (angle p3 p2))
  (setq w (distance p2 p3))
  (setq l (distance p1 p2))

  (setq p4 (polar p1 ang_p2p3 w))

  (setq pa (polar p1 ang_p2p1 setback))
  (setq pb (polar p1 ang_p3p2 setback))
  (setq pc (polar p2 ang_p3p2 setback))
  (setq pd (polar p2 ang_p1p2 setback))
  (setq pe (polar p3 ang_p1p2 setback))
  (setq pf (polar p3 ang_p2p3 setback))
  (setq pg (polar p4 ang_p2p3 setback))
  (setq ph (polar p4 ang_p2p1 setback))

    ; draws lines and arcs as a continous PLINE
  (COMMAND "PLINE" PE PD "A" PC "L" PB "A" PA "L" PH "A" PG "L" PF "A" PE "")

  (princ)
)
; end sWell






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun STAMP_SAVEVARS ()
  (setq $SE (get_tile "DESIGNER"))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:stamp (/ ex currentLayer
                  sDate sBlk sBlkName sBlk2 sBlkName2
                  ANGDIR_DEF ANGBASE_DEF
                  seInfo seName seNumber
                  master_list point
               )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun STAMP_ERROR (msg)
      (if (= msg "quit / exit abort")
        (prompt (strcat " SeptiCAD Error - " sBlk " NOT found."))
      )

       ; reset error function	
      (setq *error* lastErrorFunction)
      (princ)
    )
    ;;; end stampER
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



  (if (member "HHE_200_PG1" (layoutlist))
    (progn
      (setq ANGDIR_DEF (GETVAR "ANGDIR")) (setvar "ANGDIR" 0)
      (SETQ ANGBASE_DEF (GETVAR "ANGBASE")) (setvar "ANGBASE" 0)
      (toggle-OSNAP-Vars-OFF)
      (SETVAR "CMDECHO" 0)
      (setq currentLayer (GETVAR "CLAYER"))
      (setq lastErrorFunction *error*)
      (setq *error* STAMP_ERROR)

      ;;;--- Load the dcl file
      (setq dcl_id (load_dialog (strcat $septicadPathDCL "STAMP.DCL")))

      ;;;--- Load the dialog definition if it is not already loaded
      (if (not (new_dialog "STAMP" dcl_id))
        (progn
          (alert "The DCL file could not be loaded!")
          (exit)
        )
      )

      (start_list "DESIGNER" 3)
      (mapcar 'add_list (getSiteEvaluatorNames))
      (end_list)

      ;;;--- If an action event occurs, do this function
      (action_tile "sign" "(setq ddiag 2)(STAMP_SAVEVARS)(done_dialog)")
      (action_tile "cancel" "(setq ddiag 1)(done_dialog)")

      ;;;--- Display the dialog box
      (start_dialog)

      ;;;--- Unload the dialog box
      (unload_dialog dcl_id)

      ;;;--- If the user pressed the Cancel button
      (if (= ddiag 1)
        (progn
          (Princ "\nCancelled")
          (exit)
        )
      )

      ;;;--- If the user pressed the Okay button
      (if (= ddiag 2)
        (progn
          (setq seInfo (nth (atoi $SE) (getSiteEvaluatorList)))  ;  list with ("name" "se#" "phone" "email")
          (setq seName (nth 0 seInfo))
          (setq seNumber (nth 1 seInfo))

          ; convert julian date to real date and insert date
          (setq sDate (strcat (substr (rtos (getvar "CDATE") 2) 5 2) "/" (substr (rtos (getvar "CDATE") 2) 7 2) "/" (substr (rtos (getvar "CDATE") 2) 3 2)))

          ; create names for the blocks based on user
          (if (= $SE "0") (setq sBlkName "SIGNATURE-USER1"))
          (if (= $SE "1") (setq sBlkName "SIGNATURE-USER2"))
          (if (= $SE "2") (setq sBlkName "SIGNATURE-USER3"))

          (if (= $SE "0") (setq sBlkName2 "SIGNATURE-USER1-INITIALS"))
          (if (= $SE "1") (setq sBlkName2 "SIGNATURE-USER2-INITIALS"))
          (if (= $SE "2") (setq sBlkName2 "SIGNATURE-USER3-INITIALS"))

          (setq sBlk (strcat $septicadPathBlock sBlkName ".DWG"))
          (setq sBlk2 (strcat $septicadPathBlock sBlkName2 ".DWG"))

          (if (not (or (findfile sBlk) (findfile sBlk2)))
            (progn
              (Alert (strcat " SeptiCAD Error - " sBlk " NOT found.\n\n
                                                          See SeptiCAD Help for more information."
                     )
              )

              (exit)
            )
          )

          (if (and (findfile sBlk) (findfile sBlk2))
            (progn
              (setq ex (getvar "Expert"))
              (setvar "Expert" 2)
              (setBlockUnitsVars)
              (setq master_list (get_HHE-200_form_hidden))

              (command "UNDO" "BE")
              (SETVAR "CLAYER" "FORM_INPUT")

              ;;;;;;;;;    layout "HHE_200_PG1"   ;;;;;;;;;;;;;;;;;;
              (COMMAND "LAYOUT" "S" " HHE_200_PG1")
              (if (/= (getvar "CVPORT") 1) (command "PSPACE"))

              (create_mtext (ret_index "F3" master_list) seNumber)
              (create_mtext (ret_index "F4" master_list) sDate)

              (setq point (car (nth 2 (ret_index "F1" master_list))))
              (COMMAND "-insert" sBlk point "1" "1" "0")

              ;;;;;;;;;    layout "HHE_200_PG2"   ;;;;;;;;;;;;;;;;;;
              (COMMAND "LAYOUT" "S" " HHE_200_PG2")
              (if (/= (getvar "CVPORT") 1) (command "PSPACE"))

              (create_mtext (ret_index "T2" master_list) seNumber)
              (create_mtext (ret_index "T3" master_list) sDate)

              (setq point (car (nth 2 (ret_index "T1" master_list))))
              (COMMAND "-insert" sBlk2 point "1" "1" "0")

              ;;;;;;;;;    layout "HHE_200_PG3"   ;;;;;;;;;;;;;;;;;;
              (COMMAND "LAYOUT" "S" " HHE_200_PG3")
              (if (/= (getvar "CVPORT") 1) (command "PSPACE"))

              (create_mtext (ret_index "T5" master_list) seNumber)
              (create_mtext (ret_index "T6" master_list) sDate)

              (setq point (car (nth 2 (ret_index "T4" master_list))))
              (COMMAND "-insert" sBlkName2 point "1" "1" "0")

              ;;;;;;;;;    layout "HHE_200_PG4"   ;;;;;;;;;;;;;;;;;;
              (COMMAND "LAYOUT" "S" " HHE_200_PG4")
              (if (/= (getvar "CVPORT") 1) (command "PSPACE"))

              (create_mtext (ret_index "T8" master_list) seNumber)
              (create_mtext (ret_index "T9" master_list) sDate)

              (setq point (car (nth 2 (ret_index "T7" master_list))))
              (COMMAND "-insert" sBlkName2 point "1" "1" "0")

              ;reset block insert unit vars
              (resetBlockUnitsVars)

              ; end UNDO marker
              (command "UNDO" "E")
              (SETVAR "CLAYER" currentLayer)
              (setvar "EXPERT" ex)
            )
          )

          (setq *error* lastErrorFunction)
          (toggle-OSNAP-Vars-RESET)


        )
      )
      ;;;--- END diag = 2 (If the user pressed the Okay button)

    )       ; big progn
    ; start ELSE
    (Alert "Layout: HHE-200 Not Found")
  )         ; big if

  (setq $SE nil)
  (setvar "ANGDIR" ANGDIR_DEF)
  (SETVAR "ANGBASE" ANGBASE_DEF)
  (princ)





)
; end c:STAMP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



















(defun SPRINT_ERROR (msg)
  (if (= msg "rejected function") (Prompt "\nSeptiCAD Error -- Printer name or Paper name Not Valid. Check Printer name in [Menu --> File:Page Setup].  The most common paper name is \"Letter\". "))
  (if (/= msg "rejected function") (print msg))

   ; reset error function	
  (setq *error* lastErrorFunction)
  (princ)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun IsReadOnly (fileN / fileH)
  (cond
    ((setq fileH (open fileN "a"))
           (close fileH)
    )
  )
  (not fileH)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:sPrint (/ filedia-temp printHHE-200-YES printHHE-200-NOTES layoutPrint printerList paperList formList p1a p1b p2a p2b p3a p3b plotPrinter plotPaper plotCommand plotStyle fil temp)


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;; Start subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
        ;;------------------------------------------------------------;;
      ;;------------------------------------------------------------;;
      (defun ExplorerFolder (path / ShellObject)
        (vl-load-com)
        (setq ShellObject (vla-getInterfaceObject (vlax-get-acad-object) "Shell.Application"))
        (vlax-invoke-method ShellObject 'Explore path)
        (vlax-release-object ShellObject)
      )
      ;;------------------------------------------------------------;;
      ;;------------------------------------------------------------;;

  
      (defun FolderBox (message directory flag / folder sh)
      ;; Arguments:
      ;; message: the message displayed in th dialog box
      ;; directory: the directory to browse
      ;; flag values:
      ;; 0 = Default
      ;; 1 = Only file system folders can be selected. If this bit is set, the OK button is disabled if the user selects a folder that doesn't belong to the file system (such as the Control Panel folder).
      ;; 2 = The user is prohibited from browsing below the domain within a network (during a computer search).
      ;; 4 = Room for status text is provided under the text box.
      ;; 8 = Returns file system ancestors only.
      ;; 16 = Shows an edit box in the dialog box for the user to type the name of an item.
      ;; 32 = Validate the name typed in the edit box.
      ;; 512 = None "New folder" button
      ;; 4096 = Enables the user to browse the network branch of the shell's namespace for computer names.
      ;; 8192 = Enables the user to browse the network branch of the shell's namespace for printer names.
      ;; 16384 = Allows browsing for everything.

        (setq shell (vlax-create-object "Shell.Application"))
        (if (setq
              folder (vlax-invoke shell 'browseforfolder 0 message flag directory)
            )
          (setq folder (vlax-get-property (vlax-get-property folder 'self) 'path))
          (setq folder nil)
        )
        (vlax-release-object shell)
        folder
      )
  
        
        
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ; function syntax (setq chosenLayout (sPrint_Get_Layout "HHE-200")) 
      (defun sPrint_Get_Layout (page / ddiag selectedLayout)

             ;;;--- Load the DCL file
        (setq dcl_id (load_dialog (strcat $septicadPathDCL "LAYOUT-LIST.dcl")))


            ;;;--- See if the dialog box is already loaded
        (if (not (new_dialog "LAYOUT_LIST" dcl_id))
          (progn
            (alert "The LAYOUT-LIST.dcl file was not found!")
            (exit)
          )
        )


        (start_list "LAYOUTS" 3)
        (mapcar 'add_list (layoutList))
        (end_list)

            ;;;--- If an action event occurs, do this function
        (action_tile "cancel" "(done_dialog)")

        (if (= page "HHE-200")
          (progn
            (action_tile "okay" "(setq ddiag 0)(sPrint_Get_Layout_savevars)(done_dialog)")
            (if $lastHHE-200layout (set_tile "LAYOUTS" $lastHHE-200layout))
            (set_tile "LAYOUTS-TEXT" "\nDefault HHE-200 Layout NOT FOUND\n\nChoose Layout below with the HHE-200 Form")
          )
        )

            ;;;--- Display the dialog box
        (start_dialog)

            ;;;--- Unload the dialog box
        (unload_dialog dcl_id)

        (if (= ddiag 0)
          (progn
            (if (= page "HHE-200") (setq $lastHHE-200layout selectedLayout-DCL))
            (setq selectedLayout (nth (atoi selectedLayout-DCL) (layoutList)))
          )
        )
        (setq selectedLayout-DCL nil)
        selectedLayout
      )
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;;;;;;;;;;;;;;;;;;;
      (defun sPrint_Get_Layout_savevars ()
        (setq selectedLayout-DCL (get_tile "LAYOUTS"))
      )
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




    ;;--------------------=={ String Subst }==--------------------;;
    ;;                                                            ;;
    ;;  Substitutes a string for all occurrences of another       ;;
    ;;  string within a string.                                   ;;
    ;;------------------------------------------------------------;;
    ;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
    ;;------------------------------------------------------------;;
    ;;  Arguments:                                                ;;
    ;;  new - string to be substituted for 'old'                  ;;
    ;;  old - string to be replaced                               ;;
    ;;  str - the string to be searched                           ;;
    ;;------------------------------------------------------------;;
    ;;  Returns:  String with 'old' replaced with 'new'           ;;
    ;;------------------------------------------------------------;;

    (defun LM:StringSubst (new old str1 / inc len)
      (setq len (strlen new)
            inc 0
      )
      (while (setq inc (vl-string-search old str1 inc))
        (setq str1 (vl-string-subst new old str1 inc)
              inc (+ inc len)
        )
      )
      str1
    )
    ;;------------------------------------------------------------;;
    ;;------------------------------------------------------------;;

    ;;;;;;;;;;;;;;;;
    (defun GetPlotStyleTableNames (ad)
      (vla-RefreshPlotDeviceInfo
        (vla-get-activelayout ad)
      )
      (vlax-safearray->list
        (vlax-variant-value
          (vla-getplotstyletablenames
            (vla-item (vla-get-layouts ad) "Model")
          )
        )
      )
    )
    
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun saveVarsSPRINT ()
      (setq pList (LIST (get_tile "PAGE1") (get_tile "PAGE2") (get_tile "PAGE3")))
      (setq tempPrinter (get_tile "PRINTER"))
    )
    ;;;;;;;;;;;;;;;;;;;; end saveVarsSPRINT
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;; end subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


     ;; initial test to make sure this is not he new templatetemp
   ;; if this is the new version, then send to publish
  (if (not (member "HHE_200_PG1" (layoutList)))
    (progn  ; if


      ; Load the Visual LISP library
      (vl-load-com)

      ; set error control
      (setq lastErrorFunction *error*)
      (setq *error* SPRINT_ERROR)


      ;;;--- Set up an empty list
      (setq printerList (list))
      (setq paperList (list))

      ;;;--- Open the text file
      (if (findfile (strcat $septicadPathText "sPrint.txt"))
        (progn

          (if (setq fil (open (findfile (strcat $septicadPathText "sPrint.txt")) "r"))
            (progn

              ;;;--- Skip the first three lines
              (read-line fil)
              (read-line fil)
              (read-line fil)

              ;;;--- Read the lines of data in the text file
              (while (setq a (read-line fil))
                ;;;--- Add the data to the list
                (setq printerList
                      (append
                        printerList
                        (list a)
                      )
                )
                (setq a (read-line fil))
                ;;;--- Add the data to the list
                (setq paperList
                      (append
                        paperList
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
      ;;;--- REQUIRED --- Removes End of Line marker for Printer DCL PULL DOWN
      (setq formList (list))
      (foreach a printerList
        (setq formList (append formList (list a)))
      )
      (setq printerList formList)

      ;;;--- REQUIRED --- Removes End of Line marker for Printer DCL PULL DOWN
      (setq formList (list))
      (foreach a paperList
        (setq formList (append formList (list a)))
      )
      (setq paperList formList)



      ; set two point for each HHE-200 Page - 3 pages - a=Bottom left and b=Top Right
      (setq p1a (list -0.500000 -0.647059))
      (setq p1b (list 8.00000 10.3529))
      (setq p2a (list 8.50000 -0.500000))
      (setq p2b (list 17.0000 10.5000))
      (setq p3a (list 17.5000 -0.500000))
      (setq p3b (list 26.0000 10.5000))

      ;;;--- Load the DCL file
      (setq dcl_id (load_dialog (strcat $septicadPathDCL "sPrint.dcl")))


      ;;;--- See if the dialog box is already loaded
      (if (not (new_dialog "sPrint" dcl_id))
        (progn
          (alert "The sPrint.DCL file was not found!")
          (exit)
        )
      )


      (start_list "PRINTER" 3)
      (mapcar 'add_list printerList)
      (end_list)


      ;;;--- If an action event occurs, do this function
      (action_tile "CANCEL" "(setq sdiag 0)(done_dialog)")
      (action_tile "PRINT" "(setq sdiag 1)(saveVarsSPRINT)(done_dialog)")
      (action_tile "PRINTALL" "(setq sdiag 2)(saveVarsSPRINT)(done_dialog)")
      (action_tile "ADD" "(setq sdiag -99)(done_dialog)")

      (if (/= tempPrinter nil) (set_tile "PRINTER" tempPrinter))


      ;;;--- Display the dialog box
      (start_dialog)


      ;;;--- Unload the dialog box
      (unload_dialog dcl_id)


      ; If PRINT ALL set the DCL checkbox pList to all ON
      (if (= sdiag 2) (setq pList (LIST "1" "1" "1")))

      ;;; IF PRINTING START
      (if (> sdiag 0)
        (progn


          (setq plotPrinter (nth (ATOI tempPrinter) printerList))
          (setq plotPaper (nth (ATOI tempPrinter) paperList))

          (setq ad (vla-get-activedocument (vlax-get-acad-object)))
          (setq plotDeviceList (vlax-safearray->list (vlax-variant-value (vla-getplotdevicenames (vla-item (vla-get-layouts ad) "Model")))))
          (setq printerfound nil)
          (foreach item plotDeviceList
            (if (= item plotPrinter) (setq printerfound 1))
          )


          (if (= printerfound 1)
            (progn

              (SETVAR "CMDECHO" 0)

              ;        (setq frameState (getvar "FRAME"))
              ;        (if (= frameState 3)(setq frameState 2)); user can not set framestate to 3
              ;	(SETVAR "FRAME" 0)

              ;        (setq wipeFrameState (getvar "WIPEOUTFRAME"))
              ;        (SETVAR "WIPEOUTFRAME" 0)

              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              ;;;;       START IF PRINTING PAGES        ;;;;;;;;;;;;;;;;;;

              (if (= (nth 0 pList) "1") (SETQ printHHE-200-YES 1))
              (if (= (nth 1 pList) "1") (SETQ printHHE-200-YES 1))
              (if (= (nth 2 pList) "1") (SETQ printHHE-200-YES 1))
              ;     (if (= (nth 3 pList) "1")(SETQ printHHE-200-NOTES 1))

              ; $$$$ START if not add or cancel
              (if (and (or printHHE-200-YES printHHE-200-NOTES) (> sdiag 0))
                (progn

                  ; Set the command for PRINTING depending on Product
                  (if (= (strcase (getvar "PRODUCT")) "BRICSCAD") (setq plotCommand "_plot"))
                  (if (= (strcase (getvar "PRODUCT")) "AUTOCAD") (setq plotCommand "-plot"))

                  ;;;; style color table settings
                  (setq StyleColorTableList (GetPlotStyleTableNames ad))
                  (setq StyleColorTableCurrent (vlax-get-property (vla-get-ActiveLayout ad) "StyleSheet"))

                  (setq plotStyle nil)
                  (foreach item styleColorTableList
                    (if (= item StyleColorTableCurrent) (setq plotStyle item))
                  )
                  (if (= plotStyle nil) (setq plotStyle (nth 0 styleColorTableList)))
                  ;;;;  style color table settings



                  ; START IF printHHE-200-YES
                  (if printHHE-200-YES
                    (progn

                      (if (member "HHE-200" (layoutList))
                        (setq layoutPrint "HHE-200")
                        (setq layoutPrint (sPrint_Get_Layout "HHE-200"))
                      )


                      ; START IF layoutPrint
                      (if layoutPrint
                        (progn

                          (if (or (= plotPrinter "DWG To PDF.pc3") (= plotPrinter "Print As PDF.pc3") (wcmatch plotPrinter "*AutoCAD PDF*"))
                            (progn
                              (setq fileFolder (getvar 'DWGPREFIX))
                              (setq fileName (substr (getvar "dwgname") 1 (- (strlen (getvar "dwgname")) 4)))

                              (if
                                (or
                                  (= (strcase fileName) "DRAWING1")
                                  (wcmatch (strcase fileFolder) "*WINDOWS*")
                                  (wcmatch (strcase fileFolder) "*SEPTICAD*")
                                )
                                (progn
                                  ;(setq userDesktopFolder (vl-registry-read "HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders" "Desktop"))
                                  (setq fileFolder (vl-registry-read "HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders" "Desktop"))

                                )

                              )

                              ; determine the folder to save to														
                              (if (findfile (strcat (getvar "dwgprefix") (getvar "dwgname")))
                                (setq pdfPath (strcat (getvar "dwgprefix")))
                                (Setq pdfPath (folderBox "Select PDF File Location" fileFolder 0))
                              )



                              ; select the filename
                              (if
                                (or
                                  (= (strcase fileName) "DRAWING1")
                                  (= (strcase fileName) "SEPTICAD-TEMPLATE")
                                )
                                (progn

                                  (setq destPage1 (strcat pdfPath "\\HHE-200 Page 1.PDF"))
                                  (setq destPage2 (strcat pdfPath "\\HHE-200 Page 2.PDF"))
                                  (setq destPage3 (strcat pdfPath "\\HHE-200 Page 3.PDF"))

                                )
                                (progn

                                  (setq destPage1 (strcat pdfPath "\\" fileName " HHE-200 Page 1.PDF"))
                                  (setq destPage2 (strcat pdfPath "\\" fileName " HHE-200 Page 2.PDF"))
                                  (setq destPage3 (strcat pdfPath "\\" fileName " HHE-200 Page 3.PDF"))

                                )

                              )


                              ; if any are read only then add a julian time -onto it											
                              (setq cdate_val (rtos (getvar "CDATE") 2 6))

                              ; Break up the string into its separate parts
                              (setq YYYY (substr cdate_val 1 4)
                                    M (substr cdate_val 5 2)
                                    D (substr cdate_val 7 2)
                                    HH (substr cdate_val 10 2)
                                    MM (substr cdate_val 12 2)
                                    SS (substr cdate_val 14 2)
                              )


                              (if (or (IsReadOnly destpage1) (IsReadOnly destpage2) (IsReadOnly destpage3))
                                (progn
                                  (setq destPage1 (strcat pdfPath "\\" fileName " HHE-200 Page 1 - " M "-" D "-" YYYY " " HH " " MM " " SS ".PDF"))
                                  (setq destPage2 (strcat pdfPath "\\" fileName " HHE-200 Page 2 - " M "-" D "-" YYYY " " HH " " MM " " SS ".PDF"))
                                  (setq destPage3 (strcat pdfPath "\\" fileName " HHE-200 Page 3 - " M "-" D "-" YYYY " " HH " " MM " " SS ".PDF"))
                                  (alert "On of the destination PDF files is Read-Only or Open in another Application\nPDF file was saved with Year & Time appended to end of file name")
                                )
                              )


                            )
                          )

                          (toggle-OSNAP-Vars-OFF)

                          (COMMAND "LAYOUT" "S" layoutPrint)

                          (if (/= (getvar "CVPORT") 1) (command "PSPACE"))
                          (command "zoom" "e")
                          (princ "\n")


                          ;Command: -PLOT
                          ;Detailed plot configuration? [Yes/No] <No>: y
                          ;
                          ;Enter a layout name or [?] <HHE-200>:
                          ;Enter an output device name or [?] <AutoCAD PDF (General Documentation).pc3>:
                          ;Enter paper size or [?] <ANSI full bleed A (8.50 x 11.00 Inches)>:
                          ;Enter paper units [Inches/Millimeters] <Inches>:
                          ;Enter drawing orientation [Portrait/Landscape] <Portrait>:
                          ;Plot upside down? [Yes/No] <No>:
                          ;
                          ;Enter plot area [Display/Extents/Layout/View/Window] <Window>:
                          ;Enter lower left corner of window <9.000000,0.000000>:
                          ;Enter upper right corner of window <16.500000,10.000000>:
                          ;
                          ;Enter plot scale (Plotted Inches=Drawing Units) or [Fit] <1=1>:
                          ;Enter plot offset (x,y) or [Center] <Center>:
                          ;Plot with plot styles? [Yes/No] <Yes>:
                          ;Enter plot style table name or [?] (enter . for none) <>:
                          ;
                          ;Plot with lineweights? [Yes/No] <Yes>:
                          ;Scale lineweights with plot scale? [Yes/No] <Yes>:
                          ;Plot paper space first? [Yes/No] <No>:
                          ;Hide paperspace objects? [Yes/No] <No>:
                          ;-prompt for file
                          ;Save changes to page setup [Yes/No]? <N>
                          ;Proceed with plot [Yes/No] <Y>:


                          ;  filter for anything like this:  AutoCAD PDF (General Documentation).pc3
                          ;  (wcmatch str searchStr)
                          ;  (wcmatch plotPrinter "AutoCAD PDF")
                          ;(wcmatch "AutoCAD PDF fdfd" "*AutoCAD PDF*")


                          ; FILEDIA SET TO ZERO FOR SPRI
                          (setq filedia-temp (getvar "FILEDIA"))
                          (setvar "FILEDIA" 0)


                          ;;;--- If the PAGE1 Pressed -- HHE-200
                          (if (= (nth 0 pList) "1")
                            (progn
                              (princ (strcat "Printing " (getvar "SAVENAME") " - HHE-200 Page 1 ...."))

                              (if (or (= plotPrinter "DWG To PDF.pc3") (= plotPrinter "Print As PDF.pc3") (wcmatch plotPrinter "*AutoCAD PDF*"))

                                ; if autocad pdf printers
                                (progn

                                  ; strip the _'s if present
                                  (setq plotPaper (LM:StringSubst " " "_" plotPaper))
                                  ;(command plotCommand "Y" layoutPrint plotPrinter plotPaper "I" "P" "N" "W" p1a p1b "1=1" "CENTER" "Y" plotStyle "Y" "N" "N" "N" destpage1 "Y" "Y")
                                  (command plotCommand "Y" layoutPrint plotPrinter plotPaper "I" "P" "N" "W" p1a p1b "1=1" "CENTER" "Y" plotStyle "Y" "N" "N" "N" destpage1 "Y" "Y" "Y")
                                  (princ destpage1)
                                )

                                ; default
                                (progn
                                  (command plotCommand "Y" layoutPrint plotPrinter plotPaper "I" "P" "N" "W" p1a p1b "1:1" "" "Y" plotStyle "Y" "Y" "Y" "N" "N" "N" "Y")
                                )
                              )

                              (princ "DONE\n")
                            )
                          )


                          ;;;--- If the PAGE2 Pressed -- HHE-200
                          (if (= (nth 1 pList) "1")
                            (progn
                              (princ (strcat "Printing " (getvar "SAVENAME") " - HHE-200 Page 2 ...."))


                              (if (or (= plotPrinter "DWG To PDF.pc3") (= plotPrinter "Print As PDF.pc3") (wcmatch plotPrinter "*AutoCAD PDF*"))

                                ; if autocad pdf printers
                                (progn

                                  ; strip the _'s if present
                                  (setq plotPaper (LM:StringSubst " " "_" plotPaper))

                                  ;(command plotCommand "Y" layoutPrint plotPrinter plotPaper "I" "P" "N" "W" p2a p2b "1=1" "CENTER" "Y" plotStyle "Y" "N" "N" "N" destpage2 "Y" "Y")
                                  (command plotCommand "Y" layoutPrint plotPrinter plotPaper "I" "P" "N" "W" p2a p2b "1=1" "CENTER" "Y" plotStyle "Y" "N" "N" "N" destpage2 "Y" "Y" "Y")
                                  (princ destpage1)
                                )

                                ; default
                                (progn
                                  (command plotCommand "Y" layoutPrint plotPrinter plotPaper "I" "P" "N" "W" p2a p2b "1:1" "" "Y" plotStyle "Y" "Y" "Y" "N" "N" "N" "Y")
                                )
                              )


                              (princ "DONE\n")
                            )
                          )

                          ;;;--- If the PAGE3 Pressed -- HHE-200
                          (if (= (nth 2 pList) "1")
                            (progn
                              (princ (strcat "Printing " (getvar "SAVENAME") " - HHE-200 Page 3 ...."))

                              (if (or (= plotPrinter "DWG To PDF.pc3") (= plotPrinter "Print As PDF.pc3") (wcmatch plotPrinter "*AutoCAD PDF*"))

                                ; if autocad pdf printers
                                (progn

                                  ; strip the _'s if present
                                  (setq plotPaper (LM:StringSubst " " "_" plotPaper))

                                  ;(command plotCommand "Y" layoutPrint plotPrinter plotPaper "I" "P" "N" "W" p3a p3b "1=1" "CENTER" "Y" plotStyle "Y" "N" "N" "N" destpage3 "Y" "Y")
                                  (command plotCommand "Y" layoutPrint plotPrinter plotPaper "I" "P" "N" "W" p3a p3b "1=1" "CENTER" "Y" plotStyle "Y" "N" "N" "N" destpage3 "Y" "Y" "Y")
                                  (princ destpage1)
                                )

                                ; default
                                (progn
                                  (command plotCommand "Y" layoutPrint plotPrinter plotPaper "I" "P" "N" "W" p3a p3b "1:1" "" "Y" plotStyle "Y" "Y" "Y" "N" "N" "N" "Y")
                                )
                              )
                              (princ "DONE\n")
                            )
                          )
                          ; if autocad pdf printers open the finder folder
                          (if (or (= plotPrinter "DWG To PDF.pc3") (= plotPrinter "Print As PDF.pc3") (wcmatch plotPrinter "*AutoCAD PDF*"))
                            (ExplorerFolder pdfPath)
                          )
                          (setvar "FILEDIA" filedia-temp)
                          (toggle-OSNAP-Vars-RESET)
                        )
                      )  ; START IF layoutPrint
                    )
                  )      ;;; end IF PRINTING START
                )
              )

              ; TRIGGERS ALERT IF PRINTING AND VARIAINCE CHECKBOX IS PRESENT
              (checkVariance)

              ; Check to see if printing 0 pages, if so notify user
              (setq count 0)
              (foreach item pList
                (setq count (+ count (atof item)))
              )
              (if (< count 1)
                (progn
                  (Alert "No Pages Chosen")
                  (c:sprint)
                )
              )
              (setq pList nil)
            )
          )
          ;;; end if printer found
          (if (= printerfound nil)
            (Alert (strcat "Printer \ Plotter Device '" plotPrinter "' Not Found"))
          )
        )
      )     ;;; end PRINTING START

      (if (= sdiag -99) (sPrintAddPrinter))


      ;;;--- If the Cancel button was pressed
      (if (= sdiag 0)
        (print "SeptiCAD - Printing HHE-200 Cancelled...")
      )

      (setq *error* lastErrorFunction)

    )       ;; end entire function if


    (progn  ; else
            (alert "\nThis is for printing the (3) page HHE-200 Application\n
              For the new (4) page HHE-200 Application use the PUBLISH or PLOT commands\n              To print more than one page use PUBLISH\n              To print one page, go to layout page and use PLOT"
            )


      (checkVariance)  ; alert user if variance form need to be printed
    )
  )



	;;;--- suppress the last echo for a clean exit
  (princ)
  
  
 

)
;;;;;;;;;;;;;;;;;;;; end sPrint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;









































(defun sPrintAddPrinter ()



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;; Start subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;; called in sPrintAdd.dcl
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun sPrintAddResetPaperDCL (/ currentPlotDevice printerName)
      (setq currentPlotDevice (vla-get-ConfigName (vla-get-ActiveLayout ad)))

      ; get current printer field name and change active printer in document to get their paper sizes
      (setq sPrintPrinterField (atoi (get_tile "PRINTER_FIELD")))
      (setq printerName (nth sPrintPrinterField plotDeviceList))
      (vla-put-ConfigName (vla-get-ActiveLayout ad) PrinterName)

      ; get printer list names
      (setq paperTypeList (GetCanonicalMediaNames ad))

      (start_list "PAPER_FIELD" 3)
      (mapcar 'add_list paperTypeList)
      (end_list)

      (vla-put-ConfigName (vla-get-ActiveLayout ad) currentPlotDevice)


    )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;; Visual LISP Printer/Plotter Functions ;;;;;;;;;;;;;;;;;;;;;; 
    (defun GetActivePlotDevice (ad)
      (vla-get-ConfigName
        (vla-get-ActiveLayout ad)
      )
    )

    ;;;;;;;;;;;;;;;;
    (defun GetCanonicalMediaNames (ad)
      (vla-RefreshPlotDeviceInfo
        (vla-get-activelayout ad)
      )
      (vlax-safearray->list
        (vlax-variant-value
          (vla-GetCanonicalMediaNames
            (vla-item (vla-get-layouts ad) "Model")
          )
        )
      )
    )

    (defun sPrintHelp ()

      (if (/= (strcase (getvar "PRODUCT")) "AUTOCAD")
        (command "URL" (strcat $septicadPathPDF "PRINT_HELP.pdf"))
      )
      (if (= (strcase (getvar "PRODUCT")) "AUTOCAD")
        (command "BROWSER" (strcat $septicadPathPDF "PRINT_HELP.pdf"))
      )
    )
    
          
      ;;;---- if DELETE, update the list
      (defun sPrintDeleteDCL (/ count tempList)

        ; loop to change printerList
        (setq count 0)
        (setq tempList (list))
        (while (< count (length printerList))
          (if (/= count sPrintPrinter)
            (progn
              (setq tempList (append tempList (list (nth count printerList))))
            )
      ; progn
          )
      ; if
          (setq count (+ count 1))
        )
      ; end while
        (setq printerList tempList)

        (setq count 0)
        (setq tempList (list))
        (while (< count (length paperList))
          (if (/= count sPrintPrinter)
            (progn
              (setq tempList (append tempList (list (nth count paperList))))
            )  ; progn
          )    ; if
          (setq count (+ count 1))
        )      ; end while
        (setq paperList tempList)

          ; update DCL TILE
        (start_list "PRINTER" 3)
        (mapcar 'add_list printerList)
        (end_list)

        (princ)

      )        ; defun sPrintDeleteDCL


      (defun sPrintAddDCL (/ count tempList altstr)

        (setq altstr "")

        (if (and (/= sPrintPaperField "") (/= sPrintPrinterField ""))
          (progn

            (setq count 0)
            (setq tempList (list))
            (foreach item printerList
              (progn
                (if (= count sPrintPrinter)
                  (setq tempList (append tempList (list (nth (atoi sPrintPrinterField) plotDeviceList))))
                )
                (setq count (+ count 1))
                (setq tempList (append tempList (list item)))
              )
            )


            (setq printerList tempList)

            (setq count 0)
            (setq tempList (list))
            (foreach item paperList
              (progn
                (if (= count sPrintPrinter)
                  (setq tempList (append tempList (list (nth (atoi sPrintPaperField) paperTypeList))))
                )
                (setq count (+ count 1))
                (setq tempList (append tempList (list item)))
              )
            )

            (setq paperList tempList)

            ; this code is for teh circumstance when no printers are in the list
            (if (= (length printerlist) 0)
              (progn
                (setq printerList (append printerList (list (nth (atoi sPrintPrinterField) plotDeviceList))))
                (setq paperList (append paperList (list (nth (atoi sPrintPaperField) paperTypeList))))
              )
            )


            ; UPDATE DCL TILE
            (start_list "PRINTER" 3)
            (mapcar 'add_list printerList)
            (end_list)

          )
        )
        ; end if


        (if (= sPrintPrinterField "")
          (setq altstr (strcat altstr "A PRINTER must be entered.\n"))
        )

        (if (= sPrintPaperField "")
          (setq altstr (strcat altstr "A PAPER SIZE must be entered.\n"))
        )

        (if (/= altstr "")
          (Alert (strcat "ERROR - Invalid input\n\n" altstr "\n\nPress the HELP button for instructions"))
        )


        (princ)


      )
      ; defun sPrintAddDCL
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (defun sPrintMoveUp (/ item tempList count)

        (if (AND (/= (get_tile "PRINTER") "0") (/= (get_tile "PRINTER") ""))
          (progn

            ; change list of printers
            (setq count 0)
            (setq tempList (list))
            ; add the first part of the list
            (setq item sPrintPrinter)
            (while (< count (- item 1))
              (setq tempList (append tempList (list (nth count printerList))))
              (setq count (+ count 1))
            )  ; while

            (setq tempList (append tempList (list (nth item printerList))))
            (setq tempList (append tempList (list (nth (- item 1) printerList))))

            (setq count (+ count 2))

            (while (< count (length printerList))
              (setq tempList (append tempList (list (nth count printerList))))
              (setq count (+ count 1))
            )  ; while

            (setq printerList tempList)


            ; change list of Paper sizes
            (setq count 0)
            (setq tempList (list))
            ; add the first part of the list
            (setq item sPrintPrinter)
            (while (< count (- item 1))
              (setq tempList (append tempList (list (nth count paperList))))
              (setq count (+ count 1))
            )  ; while

            (setq tempList (append tempList (list (nth item paperList))))
            (setq tempList (append tempList (list (nth (- item 1) paperList))))

            (setq count (+ count 2))

            (while (< count (length paperList))
              (setq tempList (append tempList (list (nth count paperList))))
              (setq count (+ count 1))
            )  ; while

            (setq paperList tempList)


            ; update DCL TILE
            (start_list "PRINTER" 3)
            (mapcar 'add_list printerList)
            (end_list)
            ; select last selection

            (set_tile "PRINTER" (rtos (- item 1) 2 0))

          )    ; progn
        )      ; if


      )        ; end sPrintMoveUp

      (defun sPrintMoveDown (/ item tempList count itemLocation)

        (if (AND (/= (atoi (get_tile "PRINTER")) (- (length printerList) 1)) (/= (get_tile "PRINTER") ""))
          (progn
            (setq itemLocation 0)
            (setq count 0)
            (setq tempList (list))
            ; add the first part of the list
            (while (< count sPrintPrinter)
              (setq tempList (append tempList (list (nth count printerList))))
              (setq count (+ count 1))
            )  ; while
            (setq itemLocation (+ count 1))
            (setq tempList (append tempList (list (nth (+ sPrintPrinter 1) printerList))))
            (setq tempList (append tempList (list (nth sPrintPrinter printerList))))

            (setq count (+ count 2))

            (while (< count (length printerList))
              (setq tempList (append tempList (list (nth count printerList))))
              (setq count (+ count 1))
            )  ; while

            (setq printerList tempList)


            ; update paperList
            (setq count 0)
            (setq tempList (list))
            ; add the first part of the list
            (while (< count sPrintPrinter)
              (setq tempList (append tempList (list (nth count paperList))))
              (setq count (+ count 1))
            )  ; while
            (setq itemLocation (+ count 1))
            (setq tempList (append tempList (list (nth (+ sPrintPrinter 1) paperList))))
            (setq tempList (append tempList (list (nth sPrintPrinter paperList))))

            (setq count (+ count 2))

            (while (< count (length paperList))
              (setq tempList (append tempList (list (nth count paperList))))
              (setq count (+ count 1))
            )  ; while

            (setq paperList tempList)

            ; update DCL TILE
            (start_list "PRINTER" 3)
            (mapcar 'add_list printerList)
            (end_list)

            ; select last selection
            (set_tile "PRINTER" (rtos itemLocation 2 0))

          )    ; progn
        )      ; if


      )        ; end sPrintMoveDown

      (defun sPrintAddPrinterVars ()
        (setq sPrintPrinter (atoi (get_tile "PRINTER")))
        (setq sPrintPrinterField (get_tile "PRINTER_FIELD"))
        (setq sPrintPaperField (get_tile "PAPER_FIELD"))
      )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;; end subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



       ;;;--- Load the DCL file
  (setq dcl_id (load_dialog (strcat $septicadPathDCL "sPrintAdd.dcl")))

      ;;;--- See if the dialog box is already loaded
  (if (not (new_dialog "sPrintAdd" dcl_id))
    (progn
      (alert "The sPrintAdd.DCL file was not found!")
      (exit)
    )
  )

      ;;;--- If an action event occurs, do this function
  (action_tile "SAVE" "(setq ssdiag -99)(done_dialog)")
  (action_tile "CANCEL" "(setq ssdiag 0)(done_dialog)")
  (action_tile "ADD" "(sPrintAddPrinterVars)(sPrintAddDCL)")
  (action_tile "DELETE" "(sPrintAddPrinterVars)(sPrintDeleteDCL)")
  (action_tile "HELP" "(sPrintHelp)")
  (action_tile "MOVEUP" "(sPrintAddPrinterVars)(sPrintMoveUp)")
  (action_tile "MOVEDOWN" "(sPrintAddPrinterVars)(sPrintMoveDown)")

  (setq ad (vla-get-activedocument (vlax-get-acad-object)))
  (setq plotDeviceList (vlax-safearray->list (vlax-variant-value (vla-getplotdevicenames (vla-item (vla-get-layouts ad) "Model")))))
  (setq paperTypeList (GetCanonicalMediaNames ad))

  (start_list "PRINTER_FIELD" 3)
  (mapcar 'add_list plotDeviceList)
  (end_list)

  (start_list "PAPER_FIELD" 3)
  (mapcar 'add_list paperTypeList)
  (end_list)


  (start_list "PRINTER" 3)
  (mapcar 'add_list printerList)
  (end_list)

     ; sets tile to current printer
  (setq plotDeviceCurrent (GetActivePlotDevice ad))
  (setq a nil count 0)
  (foreach item plotDeviceList
    (if (= plotDeviceCurrent item)
      (set_tile "PRINTER_FIELD" (rtos count 2 0))
    )
    (setq count (+ count 1))
  )


      ;;;--- Display the dialog box
  (start_dialog)


      ;;;--- Unload the dialog box
  (unload_dialog dcl_id)

;;;--- If the Cancel button was pressed
  (if (= ssdiag 0)
    (progn
      (princ)
      (print "Canceled ----> SeptiCAD - Add Printers...")
    )
  )


;;;---- if SAVE, print printerList and paperList changes to the file
  (if (= ssdiag -99)
    (progn
      ;;;--- Open the text file
      (setq f (open (strcat $septicadPathText "sPrint.txt") "w"))
      (write-line " Every printer has a NAME and PAPER SIZE ---do not delete" f)
      (write-line " Every two lines represents a Printer ---do not delete" f)
      (write-line " To Add a Printer type command sPrint at the command line or modify this text file ---do not delete" f)

      ; loop to write list to file
      (setq count 0)
      (while (< count (length printerList))
        (write-line (nth count printerList) f)
        (write-line (nth count paperList) f)
        (setq count (+ count 1))
      )  ; end foreach

      ;;;--- Close the file.
      (close f)

      ;;;--- Set an exit message
      (print "SeptiCAD Printer List Changes Saved.")

      ;;;--- Open sPrint window
      (c:sPrint)
    )    ; progn
  )      ; if


  (princ)




)
; END sPrintAddPrinter

;;;; USAGE OF THESE FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Load the Visual LISP library
  ;(vl-load-com)
  ;(print "Printing Current Plot Device Information")
  ;(setq ad (vla-get-activedocument (vlax-get-acad-object)))

  ; list of plot devices
  ; (setq plotDeviceList (vlax-safearray->list (vlax-variant-value (vla-getplotdevicenames (vla-item (vla-get-layouts ad) "Model")))))
  ; (print "PRINTER LIST:")
  ; (print plotDeviceList)

  ;  current plot device
  ; (setq plotDeviceCurrent (GetActivePlotDevice ad))
  ; (print (strcat "CURRENT PRINTER: " plotDeviceCurrent))

  ; list of color/style tables
  ;  (setq StyleColorTableList (GetPlotStyleTableNames ad))
  ;  (setq StyleColorTableCurrent (vlax-get-property (vla-get-ActiveLayout ad) "StyleSheet"))
  ;  (print "COLOR/STYLE TABLE LIST:")
  ;  (print StyleColorTableList)
  ;  (print (strcat "CURRENT COLOR/STYLE TABLE: " StyleColorTableCurrent))

  ; list of current paper types
  ;  (setq paperTypeList (GetCanonicalMediaNames ad))
  ;   (print "PAPER TYPE LIST:")
  ;   (print paperTypeList)


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Visual LISP Printer/Plotter Functions ;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;























;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:dswale ()
  (if u1_L
    (dswale "TOOL")
    (alert "Diversion Swale Tool only works when a design is in memory")
  )
)
;this is a test for a diversion swale drain drawing function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dswale (howUsed / p1 p2 p3 p4 p5 p6 p7 p8 aPt tPt DIMASZ_DEF DIMTXT_DEF lineWidth lineType ssetRight ssetLeft offset cLayout)

; howUsed = INLINE or TOOL, when used as a tool the snaps might be on

;  (command "UNDO" "E")
  (command "UNDO" "BE")
  (if (= howUsed "TOOL") (toggle-OSNAP-Vars-OFF))

  (setq cLayout (getvar "CTAB"))
  (setvar "CTAB" "MODEL")

	; set linewidth and linetype of fill extension
  (setq lineType "FLOWLINE")
  (setq lineWidth 70)
  (setq offset 3)
	; create empty selection set
  (setq ssetRight (ssadd))
  (setq ssetLeft (ssadd))

  (SETQ DIMASZ_DEF (GETVAR "DIMASZ"))
  (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))


	; the points for drawing the fill extension
        ; diversion swale is offset and the same variables are used
	;     p2       p3
	;  p1 ----------  p4
	;     |        |
	;  p8 ----------  p5
	;     p7      p6
	;

  (setq p1 (polar c_u1 %LEFT (+ offset (/ u1_L 12.0))))
  (setq p2 (polar c_u1 %UP (+ offset (/ u1_U 12.0))))
  (setq p3 (polar c_u2 %UP (+ offset (/ u2_U 12.0))))
  (setq p4 (polar c_u2 %RIGHT (+ offset (/ u2_R 12.0))))
  (setq p5 (polar c_d2 %RIGHT (+ offset (/ d2_R 12.0))))
  (setq p6 (polar c_d2 %DOWN (+ offset (/ d2_d 12.0))))
  (setq p7 (polar c_d1 %DOWN (+ offset (/ d1_d 12.0))))
  (setq p8 (polar c_d1 %LEFT (+ offset (/ d1_L 12.0))))

; decide where the drainage divide will be
; either left, right or center

	; if center
  (if (= u1_U u2_U)
    (progn
      ; create the divide point
      (setq divide (polar p2 %RIGHT (/ (distance p2 p3) 2.0)))

      ; create the lines, adding mid points in between so spline fit works later
      ; when turning the corner the point is adjusted so it works with spline fit later 
      (command "PLINE"
               p5
               (retMidPt p4 p5)
               p4
               (polar (retMidPt p3 p4) (angle c_d1 (retMidPt p3 p4)) (/ (distance p3 p4) 4.0))
               p3
               (retMidPt divide p3)
               divide ""
      )
      (ssadd (entlast) ssetRight)

      (command "PLINE"
               p8
               (retMidPt p1 p8)
               p1
               (polar (retMidPt p2 p1) (angle c_u1 (retMidPt p2 p1)) (/ (distance p1 p2) 4.0))
               p2
               (retMidPt divide p2)
               divide ""
      )
      (ssadd (entlast) ssetLeft)

    )
  )


	; if right side is the lower point (longer fill extension on upslope side)
  (if (< u1_U u2_U)
    (progn
;	(print "left is high point")
      ; set the divide point above left top of system

      ; create the lines, adding mid points in between so spline fit works later
      ; when turning the corner the point is adjusted so it works with spline fit later 
      (command "PLINE"
               p5
               (retMidPt p4 p5)
               p4
               (polar (retMidPt p3 p4) (angle c_d1 (retMidPt p3 p4)) (/ (distance p3 p4) 2.5))
               p3
               (retMidPt p3 p2)
               p2 ""
      )
      (ssadd (entlast) ssetRight)

      (command "PLINE"
               p8
               (retMidPt p1 p8)
               p1
               (polar (retMidPt p2 p1) (angle c_u1 (retMidPt p2 p1)) (/ (distance p1 p2) 2.5))
               p2 ""
      )
      (ssadd (entlast) ssetLeft)

    )
  )



	; if left side is the lower point (longer fill extension on upslope side)
  (if (> u1_U u2_U)
    (progn
;	(print "right is high point")
      ; create the lines, adding mid points in between so spline fit works later
      ; when turning the corner the point is adjusted so it works with spline fit later 
      (command "PLINE"
               p5
               (retMidPt p4 p5)
               p4
               (polar (retMidPt p3 p4) (angle c_d1 (retMidPt p3 p4)) (/ (distance p3 p4) 2.5))
               p3
               ""
      )
      (ssadd (entlast) ssetRight)

      (command "PLINE"
               p8
               (retMidPt p1 p8)
               p1
               (polar (retMidPt p2 p1) (angle c_u1 (retMidPt p2 p1)) (/ (distance p1 p2) 2.5))
               p2
               (retMidPt p2 p3)
               p3 ""
      )
      (ssadd (entlast) ssetLeft)

    )
  )

   ; poly line edit command to turn to spline and turn linetype generation on
  (command "._PEDIT" ssetLeft "S" "L" "ON" "")
  (command "._PEDIT" ssetRight "S" "L" "ON" "")
   ; modifies line type     
  (modifySS ssetRight 6 lineType)
  (modifySS ssetLeft 6 lineType)

  (SETVAR "DIMASZ" (* page3TextSize scaleArrowSize))
  (SETVAR "DIMTXT" page3TextSize)

  (setq aPt (polar p4 (angle p4 p5) (/ (distance p4 p5) 1.5)))
  (setq tPt (polar aPt %RIGHT (/ page3scale 2)))


	;%D-SWALE%
  (COMMAND "LEADER" aPt tPt "" (cdr (assoc "%D-SWALE%" $customSysList)) "")

  (setvar "CTAB" cLayout)
  (SETVAR "DIMASZ" DIMASZ_DEF)
  (SETVAR "DIMTXT" DIMTXT_DEF)

  (if (= howUsed "TOOL") (toggle-OSNAP-Vars-RESET))
  (command "UNDO" "E")

  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;











(defun c:mb (/ overWrite blockName ex osdef base sset1 p1)

;sets block insert unit vars
  (setBlockUnitsVars)

  (setvar "cmdecho" 0)

  (setq osdef (getvar "osmode"))
  (setvar "osmode" 877)

  (setq ex (getvar "Expert"))
  (setvar "Expert" 2)

  (setq overWrite "NO")
  (setq blockName (getstring "Block Name"))

  (while (and (= overwrite "NO") (/= (tblsearch "BLOCK" blockName T) nil))
    (progn
      (initget 2 "Yes No")
      (setq choice (getkword "Block Name already Exists.  Do you want to overwrite"))
      (if (= choice "Yes")
        (setq overWrite "YES")  ; if overwrite is okay
        (setq blockName (getstring "Block Name"))  ; else get new name
      )
    )        ; progn
  )          ; while



  (if (= (last (ssgetfirst)) nil) (princ "\nSelect Entities For Block"))
  (setq sset1 (ssget))

	; get points
  (setq base (getpoint "\nChoose the insertion basepoint: "))

  (command "_-BLOCK" blockName BASE sset1 "")
  (command "_-INSERT" "" BASE "1" "1" "0")

  (setvar "osmode" osdef)
  (setvar "Expert" ex)
;reset block insert unit vars
  (resetBlockUnitsVars)

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun AUX_ERROR (msg)
       (setq *error* tempER)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;---------------------------------------------------------
;;;--- Freeze selected layer - works if entity is in xRef --
;;;---------------------------------------------------------
(defun c:FL (/ $CMDECHO lname ent1)

  (setq $CMDECHO (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

	;Use nentsel to choose layer, even it in xRef
  (if (setq ent1 (car (nentsel "\nSelect Entity on Layer to Freeze: ")))
    (progn
      (setq lname (cdr (assoc 8 (entget ent1))))
      (if (/= (strcase lname) (getvar "CLAYER"))
        (progn
          (command "-layer" "FREEZE" lname "")
          (princ (strcat "--> " lname))
          (command "redraw")
        )
        (princ (strcat lname " is the CURRENT layer. It can not be frozen"))
      )
    )
    (princ "Cancel")
  )

  (setvar "CMDECHO" $CMDECHO)

	; clean exit
  (princ)
)
;;;---------------------------------------------------------
;;;---------------------------------------------------------
;;;---------------------------------------------------------


;;;---------------------------------------------------------
;;;--- Change Color of Selectedlayer - works on xRef -------
;;;---------------------------------------------------------
(defun c:LC (/ $CMDECHO lname ent1 $color)

  (setq tempER *error*)
  (setq *error* AUX_ERROR)

	;Use nentsel to choose layer, even it in xRef
  (setq ent1 nil)
  (setq ent1 (nentsel "\nSelect Entity on Layer to Change Color: "))
  (if (/= ent1 nil)
    (progn
      (setq ent1 (car ent1))
      (if (setq $color (acad_colordlg 1 nil))
        (progn
          (setq $CMDECHO (getvar "CMDECHO"))
          (setvar "CMDECHO" 0)
          (setq lname (cdr (assoc 8 (entget ent1))))
          (command "-layer" "COLOR" $color lname "")
          (command "redraw")
          (setvar "CMDECHO" $CMDECHO)
        )
        (princ "Cancel")
      )
    )
    (princ "Cancel")
  )

  (setq *error* tempER)

	; clean exit
  (princ)
)
;;;---------------------------------------------------------
;;;---------------------------------------------------------
;;;---------------------------------------------------------



;;;---------------------------------------------------------
;;;--- Toggle's background for mText entities --------------
;;;---------------------------------------------------------
(defun c:mtw (/ $CMDECHO ent obj sset1 sset2 newent cnt cnt2 cnt3)
  (vl-load-com)
;; get big selection set
  (setq sset1 nil)
  (if (= (last (ssgetfirst)) nil)
    ; IF
    (progn
      (prompt "\nSelect entities to Toggle Background Fill [only mText will be modified]:")
      (setq sset1 (ssget))
    )
    ; ELSE
    (setq sset1 (last (ssgetfirst)))
  )

  (if (/= sset1 nil)
    (progn

      (setq $CMDECHO (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)

      ;; run through each entity selected
      (setq cnt -1 cnt2 0 cnt3 0)
      (while (< (setq cnt (1+ cnt)) (sslength sset1))
        (setq obj (vlax-ename->vla-object (ssname sset1 cnt))
              ObjType (vla-get-ObjectName obj)
        )
        (cond
          ((= ObjType "AcDbMText")
              (setq cnt2 (1+ cnt2))
              (setq ent (ssname sset1 cnt))
            (if (= (vla-get-BackgroundFill obj) :VLAX-TRUE)
              ;;;--- IF TRUE - already filled
              (progn
                (vla-put-BackgroundFill obj :VLAX-FALSE)
                (vla-update obj)
                (setq cnt3 (1+ cnt3))
              )
              (progn
                (vla-put-BackgroundFill obj :VLAX-TRUE)
                (vla-update obj)
              )
              ;;;--- IF FALSE - no fill
            )

          )  ; end COND
        )    ; end COND
      )      ; end WHILE

      (prompt
        (strcat "\nmText Background(s) Modified <"
                (rtos (- cnt2 cnt3) 2 0) " turned ON> "
                "<" (rtos cnt3 2 0) " turned OFF>\n"
        )
      )

      (setvar "CMDECHO" $CMDECHO)

    )        ; end progn
  )          ; end if

  (princ)
)            ; end MTW
;;;---------------------------------------------------------
;;;---------------------------------------------------------


(defun c:mp3 ()
  (if (= page3TextSize nil) (getUserVars))
  (if (= page3Scale nil) (setscales))
  (changeLabelSize page3TextSize scaleArrowSize "Page 3" page3Scale)
  (princ)
)

(defun c:mp2 ()
  (if (= page2TextSize nil) (getUserVars))
  (if (= page2Scale nil) (setscales))
  (changeLabelSize page2TextSize scaleArrowSize "Page 2" page2Scale)
  (princ)
)

(defun c:mpp ()
  (if (= page2TextSize nil) (getUserVars))
  (if (= profileScale nil) (setscales))
  (changeLabelSize profileTextSize scaleArrowSize "Cross Section" profileScale)
  (princ)
)


;; Changes entities [DIMENSION, LEADER, MTEXT, TEXT] textHeight and arrowHeadSize
(defun changeLabelSize (textSize arrowSizeFactor str pageScale / cnt sset1 obj cntRotDim cntLeader cntMtext cntText)

  (vl-load-com)

  ;; get big selection set

  (if (= (car (cdr (ssgetfirst))) nil) (princ (strcat "\nUpdate Labels to " str " Scale [1\"=" (rtos pageScale 2 0) "']: ")))

  (if (setq sset1 (ssget))
    (progn
      ;; create selection sets for different entity types, ignoring other types
      (setq cnt -1 cntRotDim 0 cntLeader 0 cntMtext 0 cntText 0 modstr "")

      (while (< (setq cnt (1+ cnt)) (sslength sset1))
        (setq obj (vlax-ename->vla-object (ssname sset1 cnt))
              ObjType (vla-get-ObjectName obj)
        )
        (cond
          ((= ObjType ' "AcDbRotatedDimension")
              (vlax-put-property obj 'TextHeight textSize)
              (vlax-put-property
                obj
                'ArrowheadSize
                (* arrowSizeFactor textSize)
              )
             (setq cntRotDim (1+ cntRotDim))
             (vla-update obj)
          )
          ((= ObjType "AcDbLeader")
              (vlax-put-property
                obj
                'ArrowheadSize
                (* arrowSizeFactor textSize)
              )
             (vla-update obj)
             (setq cntLeader (1+ cntLeader))
          )
          ((= ObjType "AcDbMText")
              (vlax-put-property obj 'Height textSize)
              (vla-update obj)
              (setq cntMtext (1+ cntMtext))
          )
          ((= ObjType "AcDbText")
              (vlax-put-property obj 'Height textSize)
              (vla-update obj)
              (setq cntText (1+ cntText))
          )
        )
      )      ; end while


      (if (> cntRotDim 0)
        (setq modStr (strcat modStr "<" (rtos cntRotDim 2 0) " Rotated Dimension> "))
      )
      (if (> cntLeader 0)
        (setq modStr (strcat modStr "<" (rtos cntLeader 2 0) " Leader> "))
      )
      (if (> cntmText 0)
        (setq modStr (strcat modStr "<" (rtos cntMtext 2 0) " mText> "))
      )
      (if (> cntText 0)
        (setq modStr (strcat modStr "<" (rtos cntText 2 0) " Text> "))
      )
      (if (= modStr "")
        (princ "No entities modified - Only works with: mText, Text, Leader & Rotated Dimension entities.")
        (princ (strcat "\nLabels updated to " str " Scale [1\"=" (rtos pageScale 2 0) "']: " modStr))
      )

    )        ; end progn
  )          ; end if


)



























;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:scalc (/ tempList)




  
  ;;; start subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          
        ; function calculates length and width of system prints to the screen
        (defun sCalc-calculate (/ numChambers str length1 width system num_rows row_sep num_units)

          (setq system (car (nth (atoi (get_tile "SYSTEM")) sysList)))
          (setq NUM_ROWS (atoi (get_tile "NUM_ROWS")))
          (setq ROW_SEP (atoi (get_tile "ROW_SEP")))
          (setq NUM_UNITS (atoi (get_tile "NUM_UNITS")))
          (setq sys_type system)

          (setq stoneBeside 0)

          (if (= (substr sys_type 5 16) "CONCRETE_CLUSTER")
            (progn
              (setq stoneBeside (atoi (get_tile "STONE_BESIDE")))
              (setq row_sep 0.0)
            )
          )

          (RUN-DIM_SYSTEM)


          (if (/= system "STONE")
            (progn

              (if (= $defaultSpacing "1")
                (progn
                  (setq width (+ (* ROW_SEP (- NUM_ROWS 1)) (* NUM_ROWS chamber_W)))
                  (if (OR (= SYS_TYPE "MOUNDBUSTER") (= SYS_TYPE "ENVIRO") (= SYS_TYPE "TRENCH2") (= SYS_TYPE "TRENCH3")) (setq length1 (* 12 NUM_UNITS)))
                  (if (AND (/= SYS_TYPE "MOUNDBUSTER") (/= SYS_TYPE "ENVIRO") (/= SYS_TYPE "TRENCH2") (/= SYS_TYPE "TRENCH3")) (setq length1 (* NUM_UNITS chamber_L)))
                )
              )
              (if (= $defaultSpacing "0")
                (progn
                  (setq width (+ (* ROW_SEP (- NUM_ROWS 1)) chamber_W))
                  (if (OR (= SYS_TYPE "MOUNDBUSTER") (= SYS_TYPE "ENVIRO") (= SYS_TYPE "TRENCH2") (= SYS_TYPE "TRENCH3")) (setq length1 (* NUM_UNITS 12)))
                  (if (AND (/= SYS_TYPE "MOUNDBUSTER") (/= SYS_TYPE "ENVIRO") (/= SYS_TYPE "TRENCH2") (/= SYS_TYPE "TRENCH3")) (setq length1 (/ (* NUM_UNITS chamber_L) 12.0)))
                )
              )
              (if (= (substr sys_type 5 16) "CONCRETE_CLUSTER")
                (progn
                  (setq length1 (+ length1 (* stonebeside 2.0)))
                  (setq width (+ width (* stonebeside 2.0)))
                )
              )
              (if (= (substr sys_type 5 15) "CONCRETE_TRENCH")
                (setq length1 (+ length1 24))
              )

              (setq length1 (rtos length1 3 0))
              (setq width (rtos width 3 0))
              (setq numChambers (rtos (* NUM_ROWS NUM_UNITS) 2 0))

              (if (OR (= SYS_TYPE "MOUNDBUSTER") (OR (= SYS_TYPE "ENVIRO") (= SYS_TYPE "GEOFLOW") (= SYS_TYPE "TRENCH2") (= SYS_TYPE "TRENCH3")))
                (setq str (strcat numChambers " l.f. - " width " feet wide x " length1 " feet long"))
              )

              (if (AND (/= SYS_TYPE "MOUNDBUSTER") (OR (/= SYS_TYPE "ENVIRO") (/= SYS_TYPE "GEOFLOW") (/= SYS_TYPE "TRENCH2") (/= SYS_TYPE "TRENCH3")))
                (setq str (strcat numChambers " units - " width " feet wide x " length1 " feet long"))
              )
              (if (= (substr sys_type 5 8) "CONCRETE")
                (setq str (strcat str " (including stone beside chambers)"))
              )
            )
          )

          (if (= system "STONE")
            (progn
              (setq width NUM_ROWS)
              (setq length1 NUM_UNITS)
              (setq length1 (rtos length1 2 0))
              (setq width (rtos width 2 0))
              (setq str (strcat width " feet wide x " length1 " feet long"))
            )
          )

        ; change to feet

          (set_tile "DIMENSIONS" str)
        )
          
        

      (defun sCalcAutoFill (/ system text str1)

        (setq system (car (nth (atoi (get_tile "SYSTEM")) sysList)))
        (setq NUM_ROWS (atoi (get_tile "NUM_ROWS")))
        (setq ROW_SEP (atoi (get_tile "ROW_SEP")))
        (setq NUM_UNITS (atoi (get_tile "NUM_UNITS")))
        (setq sys_type system)
        (setq stonebeside (atoi (get_tile "STONE_BESIDE")))

        (if (= (substr sys_type 5 16) "CONCRETE_CLUSTER")
          (progn
            (setq stonebeside (atoi (get_tile "STONE_BESIDE")))
            (mode_tile "STONE_BESIDE" 0)
            (mode_tile "ROW_SEP" 1)
            (setq row_sep 0.0)
            (COND
              ((= system "4x8_CONCRETE_CLUSTER") (setq text "# of Units per Row [8' long]-> "))
            )
            (COND
              ((= system "8x4_CONCRETE_CLUSTER") (setq text "# of Units per Row [4' long]-> "))
            )
          )
          (progn
            (mode_tile "STONE_BESIDE" 1)
            (mode_tile "ROW_SEP" 0)
          )
        )

        (RUN-DIM_SYSTEM)

        (if (= (substr sys_type 5 16) "CONCRETE_CLUSTER")
          (progn
            (setq str1 (strcat "Each " (cdr (assoc system syslist)) " is " (rtos chamber_W 3 0) " wide x " (rtos chamber_H 2 0) "\" high x " (rtos chamber_L 3 1) " long"))
            (set_tile "DETAILS" str1)
          )
        )

        (if (= $defaultSpacing "0") (set_tile "ROW_SEP_TEXT" "Center to Center Row Spacing [in]-> "))
        (if (= $defaultSpacing "1") (set_tile "ROW_SEP_TEXT" "Edge to Edge Row Spacing [in]-> "))

        (if (and (/= system "STONE") (/= (substr system 5 16) "CONCRETE_CLUSTER"))
          (progn
      ;		(mode_tile "ROW_SEP" 0)
            (COND
              ((= system "ENVIRO") (setq text "Total Length of System [ft]-> "))
            )
            (COND
              ((= system "GEOFLOW") (setq text "Total Length of System [ft]-> "))
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
              ((= system "MOUNDBUSTER") (setq text "Length of Perforated Pipes [ft] -> "))
            )
            (COND
              ((= system "4x8_CONCRETE_TRENCH") (setq text "# of Units per Row [8' long]-> "))
            )
            (COND
              ((= system "8x4_CONCRETE_TRENCH") (setq text "# of Units per Row [4' long]-> "))
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

            (set_tile "NUM_UNITS_TEXT" text)

            (set_tile "NUM_ROWS_TEXT" "Number of Rows -> ")

            (if (= system "ENVIRO")
              (setq str1 (strcat "Each " (cdr (assoc system syslist)) " 12\" high and 12\" wide"))
            )
            (if (= system "MOUNDBUSTER")
              (setq str1 (strcat "Each " (cdr (assoc system syslist)) " 4\" high and 4\" wide"))
            )

            (if (= system "TRENCH2")
              (setq str1 (strcat "Each " (cdr (assoc system syslist)) " 17\" high and 2' wide"))
            )
            (if (= system "TRENCH3")
              (setq str1 (strcat "Each " (cdr (assoc system syslist)) " 17\" high and 3' wide"))
            )
      ; (= (substr system 1 6) "TRENCH")


            (if (OR (= system "8x4_CONCRETE_TRENCH") (= system "4x8_CONCRETE_TRENCH"))
              (setq str1 (strcat "Each " (cdr (assoc system syslist)) " is " (rtos chamber_W 3 1) " wide (including stone beside) x " (rtos chamber_H 2 0) "\" high x " (rtos chamber_L 3 1) " long"))
            )

            (if (AND (/= system "ENVIRO") (/= (substr system 1 6) "TRENCH") (/= system "MOUNDBUSTER") (/= system "8x4_CONCRETE_TRENCH") (/= system "4x8_CONCRETE_TRENCH"))
              (setq str1 (strcat "Each " (cdr (assoc system syslist)) " is " (rtos chamber_W 2 0) "\" wide x " (rtos chamber_H 2 0) "\" high x " (rtos chamber_L 3 1) " long"))
            )

            (set_tile "DETAILS" str1)
          )
        )


        (if (= system "STONE")
          (progn
            (set_tile "NUM_UNITS_TEXT" "Length of Stone Bed [ft]-> ")
            (set_tile "NUM_ROWS_TEXT" "Width of Stone Bed [ft]-> ")
            (mode_tile "ROW_SEP" 1)
            (set_tile "DETAILS" "Stone Bed")
          )
        )

        (if (and
              (/= (get_tile "NUM_ROWS") "")
              (/= (get_tile "ROW_SEP") "")
              (/= (get_tile "NUM_UNITS") "")
            )
          (sCalc-calculate)
        )
      )
      ; end 
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; end subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;










   ;;;;START
    ;;;;; CHOOSE TO PUT STONE AROUND CHAMBERS
  (getUserVars)
  (createSysList)

  (setq tempList (list))
  (foreach item sysList
    (setq tempList (append tempList (list (cdr item))))
  )
		; end foreach

       ;;;--- Load the DCL file
  (setq dcl_id (load_dialog (strcat $septicadPathDCL "scalc.dcl")))

      ;;;--- See if the dialog box is already loaded
  (if (not (new_dialog "SCALC" dcl_id))
    (progn
      (alert "The SCALC.DCL file was not found!")
      (exit)
    )
  )

      ;;;--- If an action event occurs, do this function
  (action_tile "calculate" "(sCalc-calculate)")
  (action_tile "close" "(done_dialog)")

  (start_list "SYSTEM" 3)
  (mapcar 'add_list tempList)
  (end_list)

  (sCalcAutoFill)

      ;;;--- Display the dialog box
  (start_dialog)

      ;;;--- Unload the dialog box
  (unload_dialog dcl_id)

  (princ)
  
  

  
  
  
  
  
  
  
)
;;;;;;;;;;;;;;;;










(defun c:tankElv ()
  (doTankInvertElevation)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun doTankInvertElevation (/ ddiag fieldInvert pipeElvLoss)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; start subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun doTankCalculations (/ tankNote1 tankNote2)
      (setq dBoxDrop (ATOF (get_tile "DBOXDROP")))
      (setq pipeLength (ATOF (get_tile "PIPELENGTH")))

      (setq fieldInvert (+ (cadr row_start) invertHeight dBoxDrop))
      (setq pipeElvLoss (* (/ 1.0 8.0) pipeLength))
      (setq tankInvert (+ fieldInvert pipeElvLoss))

      (if (= dBoxDrop 0)
        (progn
          (setq TankNote1 (strcat "If the septic tank is located " (rtos pipeLength 2 1) " feet from the disposal field and a D-box is not used,"))

          (if (= (+ tankInvert 4) 0)
            (setq TankNote2 (strcat "the top of the septic tank outlet pipe elevation must be +/-" (rtos (+ tankInvert 4) 2 1) " inches or higher."))
          )

          (if (AND (> (+ tankInvert 4) 0) (< (+ tankInvert 4) 1))
            (setq TankNote2 (strcat "the top of the septic tank outlet pipe elevation must be +0" (rtos (+ tankInvert 4) 2 1) " inches or higher."))
          )
          (if (AND (< (+ tankInvert 4) 0) (> (+ tankInvert 4) -1))
            (setq TankNote2 (strcat "the top of the septic tank outlet pipe elevation must be -0" (rtos (abs (+ tankInvert 4)) 2 1) " inches or higher."))
          )

          (if (>= (+ tankInvert 4) 1)
            (setq TankNote2 (strcat "the top of the septic tank outlet pipe elevation must be +" (rtos (+ tankInvert 4) 2 1) " inches or higher."))
          )
          (if (<= (+ tankInvert 4) -1)
            (setq TankNote2 (strcat "the top of the septic tank outlet pipe elevation must be " (rtos (+ tankInvert 4) 2 1) " inches or higher."))
          )

        )
      )
      (if (/= dBoxDrop 0)
        (progn
          (setq TankNote1 (strcat "If the septic tank is located " (rtos pipeLength 2 1) " feet from the disposal field and a D-box is used,"))
          (setq TankNote2 (strcat "the top of the septic tank outlet pipe elevation must be " (rtos (+ tankInvert 4.0) 2 1) " inches or higher."))
        )
      )

      (set_tile "NOTE1" tankNote1)
      (set_tile "NOTE2" tankNote2)
      (setq tankNote (strcat tankNote1 tankNote2))
    )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; end subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


   ; invertHeight variable set in dim_* functions

    ; note must be nil, because page 3 note is inserted regardless of whether its cancelled or not.
  (SETQ tankNote nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;; START DCL DIALOG - Choose Chamber Name to Display
  (setq dcl_id (load_dialog (strcat $septicadPathDCL "tankElevation.dcl")))

      ;;;--- See if the dialog box is already loaded
  (if (not (new_dialog "tankElevation" dcl_id))
    (progn
      (alert "The tankElevation.DCL file was not found!")
      (exit)
    )
  )

      ;;;--- If an action event occurs, do this function
  (action_tile "CLOSE" "(setq ddiag 1)(done_dialog)")
  (action_tile "CALCULATE" "(doTankCalculations)")
  (action_tile "LABEL" "(doTankCalculations)(done_dialog)")


  (set_tile "CHAMBERINVERT" (rtos (+ 4.0 (cadr row_start) invertHeight) 2 2))


      ;;;--- Display the dialog box
  (start_dialog)

      ;;;--- Unload the dialog box
  (unload_dialog dcl_id)

  (if (= ddiag 1)
    (setq TankNote nil)
  )

    ;;; END DCL DIALOG
    ;;; chamberName variable is now set

; global variable - tankNote


)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun c:goMapquest ()
  (SETVAR "CMDECHO" 0)
 ; if no data in memory then attempt to download data from PAGE2_GUIDE xData
  (if (= mapquestURL nil)
    (xDataSiteLocationStringLoad)
  )

  (if (/= mapquestURL nil)
    (progn
      (if (/= (strcase (getvar "PRODUCT")) "AUTOCAD")
        (progn
          (command "URL" MEgisURL)
          (command "URL" googleURL)
          (command "URL" mapQuestURL)
        )
      )
      (if (= (strcase (getvar "PRODUCT")) "AUTOCAD")
        (progn
          (ACADbrowser MEgisURL)
          (ACADbrowser googleURL)
          (ACADbrowser mapQuestURL)
        )
      )
    )
    (alert "Latitude and Longitude Must be entered first")
  )

  (SETVAR "CMDECHO" 1)
  (princ)
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gEarth (applicationFile / CMD-DEF altstr)
 ;Sets command echo off
  (setq CMD-DEF (getvar "CMDECHO"))
  (SETVAR "CMDECHO" 0)
  (setq altStr "")

 ; if no data in memory then attempt to download data from PAGE2_GUIDE xData
  (if (= $septicad-latDD nil)
    (xDataSiteLocationStringLoad)
  )

  (if $septicad-latDD
    ; true - variable is not nil
    (progn
      (googleEarthKML)
      (if (findfile applicationFile)
        (startapp applicationFile (strcat $septicadPathRoot "SeptiCAD-GoogleEarth.KML"))
        (setq altstr (strcat altstr "Google Earth Application File Not Found [" applicationFile "]\n\nInstall Google Earth, or change path for Google Earth application in toolbar\n\n[NOTE: All backslashs must be forwardslashs]"))
      )
    )
    ; else - variable is nil
    (progn
      (setq altstr (strcat altstr "\n\nSite location information not in Memory"))
    )
  )
  (if (/= altstr "") (alert altstr))
  (SETVAR "CMDECHO" CMD-DEF)
  (princ)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; converts Degrees Minutes Seconds to Decimal Degrees
; returns global variables -> $septicad-longDD, $septicad-latDD
(defun DMS->DD (longD longM longS latD latM latS)
  (if (< longD 0.0)
    (progn
      (setq $septicad-longDD (-
                               longD
                               (* longM (/ 1.0 60.0))
                               (* longS (/ 1.0 60.0) (/ 1.0 60.0))
                             )
      )
    )
  )

  (if (>= longD 0.0)
    (progn
      (setq $septicad-longDD (+
                               longD
                               (* longM (/ 1.0 60.0))
                               (* longS (/ 1.0 60.0) (/ 1.0 60.0))
                             )
      )
    )
  )

  (if (= latD 0.0)
    (progn
      (setq $septicad-latDD (-
                              latD
                              (* latM (/ 1.0 60.0))
                              (* latS (/ 1.0 60.0) (/ 1.0 60.0))
                            )
      )
    )
  )

  (if (>= latD 0.0)
    (progn
      (setq $septicad-latDD (+
                              latD
                              (* latM (/ 1.0 60.0))
                              (* latS (/ 1.0 60.0) (/ 1.0 60.0))
                            )
      )
    )
  )
  (PRINC)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun googleEarthKML (/ nameVar fil)

  (if (cdr (assoc "A1" pg1Data))
    (setq nameVar (strcat "Site-> " (cdr (assoc "A1" pg1Data))))  ;if
    (setq nameVar "Site")  ;else
  )

;;; $septicad-longDD and $septicad-latDD set in (HHE-200_PAGE1C_DCL_SAVEVARS) - or retrieved from xData

;;;; Create googleEarth.KML in SeptiCAD Folder -  always overwrite the previous

  (setq fil (open (strcat $septicadPathRoot "SeptiCAD-GoogleEarth.KML") "w"))

  (write-line "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" fil)

  (write-line "<kml xmlns=\"http://earth.google.com/kml/2.2/\">" fil)
  (write-line "<Document>" fil)
  (write-line "<Placemark>" fil)

	; WAYPOINT NAME
  (setq str1 (strcat "<name>" nameVar "</name>"))
  (write-line str1 fil)

	;  WAYPOINT COORDINATE
  (write-line "<Point>" fil)

  (setq str1 (strcat "<coordinates>" (rtos $septicad-longDD 2 8) "," (rtos $septicad-latDD 2 8) ",0" "</coordinates>"))
  (write-line str1 fil)

  (write-line "</Point>" fil)

	;;; POINT TO LOOKAT

  (write-line "<LookAt>" fil)

  (setq str1 (strcat "<longitude>" (rtos $septicad-longDD 2 8) "</longitude>"))
  (write-line str1 fil)

  (setq str1 (strcat "<latitude>" (rtos $septicad-latDD 2 8) "</latitude>"))
  (write-line str1 fil)

  (write-line "<altitude>0</altitude>" fil)
  (write-line "<range>1000</range>" fil)
  (write-line "<tilt>0</tilt>" fil)
  (write-line "<heading>0</heading>" fil)


  (write-line "</LookAt>" fil)


  (write-line "</Placemark>" fil)
  (write-line "</Document>" fil)
  (write-line "</kml>" fil)


  ;;;--- Close the file.
  (close fil)
  (PRINC)
)
; end googleEarthKML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:swingtie ()
  (SETVAR "CMDECHO" 0)
  ; Zoom to Model Space and Center on Page 3
  (COMMAND "LAYOUT" "S" "MODEL")
  (if cpage3 (command "ZOOM" "c" cPage3 (* sPage3 (/ (last (getvar "screensize")) 10.0))))
  (command "REGEN")
  (swingTieMain)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun swingTieMain (/ RP_TYPE mySlideName myKey)



;;;;;;; start subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (defun swingTieGetCorners ()


          ; return non-nil if perpendicular to the desire precision - precision must be set for any rotated rectangles
          (defun checkPerpendicular (p1 p2 p3)
            (setq ret nil)
            (setq prec 10000000)
            (setq angL (* (angle p1 p2) prec))
            (setq angW (* (angle p3 p2) prec))

            (if (= (fix (- angW angL)) (fix (* (/ pi 2.0) prec))) (setq ret "PERP") (setq ret nil))
            ret
          )
          ;;;


        (toggle-OSNAP-Vars-OFF)
        (setvar "OSMODE" 1)

        (Alert "Select three of the system corners beginning with the TOP LEFT corner and proceeding in a clockwise direction")
        (setq p1 (getpoint "Top Left Corner Location"))
        (setq p2 (getpoint p1 "Top Right Corner Location"))
        (setq p3 (getpoint p2 "Bottom Right Corner Location"))

        (if (/= (checkPerpendicular p1 p2 p3) nil)
          (progn
            (setq c_U1 p1)
            (setq c_U2 p2)
            (setq c_D2 p3)

            (setq %LEFT (angle c_U2 c_U1))
            (setq %RIGHT (angle c_U1 c_U2))
            (setq %UP (+ (/ pi 2.0) %RIGHT))
            (setq %DOWN (+ (/ (* 3.0 pi) 2.0) %RIGHT))

            (setq c_D1 (polar C_D2 %LEFT (distance c_u1 c_u2)))

            (setq t_U1 c_U1)
            (setq t_U2 c_U2)
            (setq t_D1 c_D1)
            (setq t_D2 c_D2)

            (setq sys_W (* 12.0 (distance c_U1 C_D1)))
            (setq sys_L (* 12.0 (distance c_U1 C_U2)))
            (toggle-OSNAP-Vars-RESET)
          )
          (progn
            (alert "The System Corners are not Perpendicular, Bottom Right Corner needs to be adjusted")
            (toggle-OSNAP-Vars-RESET)
            (exit)
          )
        )



      )
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (defun swingtieToggleDCL (tile / item turnOff)

        (if (= tile "RIGHT")
          (setq turnOff (list "LEFT" "UP" "DOWN"))
        )

        (if (= tile "LEFT")
          (setq turnOff (list "RIGHT" "UP" "DOWN"))
        )

        (if (= tile "UP")
          (setq turnOff (list "LEFT" "RIGHT" "DOWN"))
        )

        (if (= tile "DOWN")
          (setq turnOff (list "LEFT" "UP" "RIGHT"))
        )

        (foreach item turnoff
          (set_tile item "0")
        )


      )
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;








      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (defun swingTieCheckInput (/ len wid tst alert_string)

        (setq len (/ sys_l 12.0))
        (setq wid (/ sys_w 12.0))
        (setq alert_string "")

        ; if no check box
        ; if not 2 boxes selected
        (setq tst 0)
        (foreach item (list TR TL BR BL)
          (if (= item 0) (setq tst (1+ tst)))
        )
        (if (= tst 4) (setq alert_string (strcat alert_string "Invalid Input - No corner distances were entered.\nTwo swing ties from adjacent corners must be entered.")))
        (if (= tst 3) (setq alert_string (strcat alert_string "Invalid Input - One corner distances were entered.\nTwo swing ties from adjacent corners must be entered.")))
        (if (= tst 1) (setq alert_string (strcat alert_string "Invalid Input - Three corner distances were entered.\nTwo swing ties from adjacent corners must be entered.")))
        (if (= tst 0) (setq alert_string (strcat alert_string "Invalid Input - Four corner distances were entered.\nTwo swing ties from adjacent corners must be entered.")))

        (if (= swingSide nil) (setq alert_string (strcat alert_string "\nA checkbox identifing the system of the system must be chosen")))


        ; start BIG IF #1
        ; now alert_string shoud be "" if these tests were passed
        ; and it can be assumed that two distances were entered.
        (if (= alert_string "")
          (progn
            ; now check to see if adjacent corners were selected
            (if (and (= BL 0) (= TR 0)) (setq alert_string (strcat alert_string "\nInvalid Input - Swing ties from adjacent corners must be entered.")))
            (if (and (= TL 0) (= BR 0)) (setq alert_string (strcat alert_string "\nInvalid Input - Swing ties from adjacent corners must be entered.")))


            ; start BIG IF #2
            ; now alert_string shoud be "" if these tests were passed
            ; and it can be assumed that two distances were entered on adjacent corners.
            (if (= alert_string "")
              (progn
                ; Now determine what side, top, bottom, left or right
                (if (AND (/= TL 0.0) (/= TR 0.0)) (setq side "TOP"))
                (if (AND (/= BL 0.0) (/= BR 0.0)) (setq side "BOTTOM"))
                (if (AND (/= TL 0.0) (/= BL 0.0)) (setq side "LEFT"))
                (if (AND (/= TR 0.0) (/= BR 0.0)) (setq side "RIGHT"))

                ; Check to see if things the circles would intersect or not
                (if (= side "TOP")
                  (progn
                    (if (> len (+ TR TL)) (setq alert_string (strcat alert_string " Invalid Input - Swing ties do not intersect\nThe system length is " (rtos len 2 4) " feet")))
                    (if (> TR (+ TL len)) (setq alert_string (strcat alert_string " Invalid Input - Swing ties do not intersect\nThe system length is " (rtos len 2 4) " feet")))
                    (if (> TL (+ TR len)) (setq alert_string (strcat alert_string " Invalid Input - Swing ties do not intersect\nThe system length is " (rtos len 2 4) " feet")))
                  )
                )

                (if (= side "BOTTOM)
               (progn
                                    (if (> len (+ BR BL))(setq alert_string (strcat alert_string " Invalid Input - Swing ties do not intersect\nThe system length is " (rtos len 2 4) " feet ")))
                                                              (if (> BR (+ BL len))(setq alert_string (strcat alert_string " Invalid Input - Swing ties do not intersect\nThe system length is " (rtos len 2 4) " feet ")))
                                                                                                  (if (> BL (+ BR len))(setq alert_string (strcat alert_string " Invalid Input - Swing ties do not intersect\nThe system length is " (rtos len 2 4) " feet ")))
               )
            )




            (if (= side " RIGHT)
                      (progn
                        (if (> wid (+ TR BR)) (setq alert_string (strcat alert_string " Invalid Input - Swing ties do not intersect\nThe system width is " (rtos wid 2 4) " feet")))
                        (if (> TR (+ BR wid)) (setq alert_string (strcat alert_string " Invalid Input - Swing ties do not intersect\nThe system width is " (rtos wid 2 4) " feet")))
                        (if (> BR (+ TR wid)) (setq alert_string (strcat alert_string " Invalid Input - Swing ties do not intersect\nThe system width is " (rtos wid 2 4) " feet")))
                      )
                    )

                  (if (= side "LEFT")
                    (progn
                      (if (> wid (+ TL BL)) (setq alert_string (strcat alert_string " Invalid Input - Swing ties do not intersect\nThe system width is " (rtos wid 2 4) " feet")))
                      (if (> TL (+ BL wid)) (setq alert_string (strcat alert_string " Invalid Input - Swing ties do not intersect\nThe system width is " (rtos wid 2 4) " feet")))
                      (if (> BL (+ TL wid)) (setq alert_string (strcat alert_string " Invalid Input - Swing ties do not intersect\nThe system width is " (rtos wid 2 4) " feet")))
                    )
                  )
                )
              )
              ; end BIG IF #2


            )
          )
        ; end BIG IF #1

          (if (= alert_string "")
            (progn  ;if no alerts then proceed
              (setq ddiag 2) (done_dialog)
            )
            (progn  ; else alert and wait for good input
              (alert alert_string)
            )
          )

        )           ; end swingTieCheckInput









      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (defun saveSwingTieVars ()

        (setq TL (ATOF (get_tile "TL")))
        (setq TR (ATOF (get_tile "TR")))
        (setq BL (ATOF (get_tile "BL")))
        (setq BR (ATOF (get_tile "BR")))
        (setq RP_TYPE (get_tile "RP_TYPE"))

        (setq swingSide nil)
        (foreach item (list "RIGHT" "LEFT" "UP" "DOWN")
          (if (= (get_tile item) "1") (setq swingSide item))
        )

        (setq txt1 (get_tile "TXT1"))
        (setq txt2 (get_tile "TXT2"))


      )  ; end saveSwingTieVars





(defun swingTieDraw&Label (/ a x c1 c2 cInt len ang term1 term2 term12 termrad leadPt intPt intPt2 tSize changeY changeX c_u1-TEMP c_u2-TEMP c_d1-TEMP c_d2-TEMP t_u1-TEMP t_u2-TEMP t_d1-TEMP t_d2-TEMP sys_L-TEMP sys_W-TEMP)





    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun doLeader (arrowPt textPt txt1 txt2)

      (if (AND (/= txt1 "") (= $ALLCAPS "YES"))
        (setq txt1 (STRCASE txt1))
      )
      (if (AND (/= txt2 "") (= $ALLCAPS "YES"))
        (setq txt2 (STRCASE txt2))
      )


      (if (AND (/= txt1 "") (/= txt2 ""))
        (COMMAND "LEADER" arrowPt textPt "" txt1 txt2 "")
      )

      (if (AND (/= txt1 "") (= txt2 ""))
        (COMMAND "LEADER" arrowPt textPt "" txt1 "")
      )

      (if (AND (= txt1 "") (/= txt2 ""))
        (COMMAND "LEADER" arrowPt textPt "" txt2 "")
      )

    )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun doDim (pnt1 pnt2)
      (setq ANGDIR_DEF (GETVAR "ANGDIR"))
      (setvar "ANGDIR" 0)
      (SETQ ANGBASE_DEF (GETVAR "ANGBASE"))
      (setvar "ANGBASE" 0)

      (setq midDimPt (polar pnt1 (angle pnt1 pnt2) (/ (distance pnt1 pnt2) 2.0)))
      (COMMAND "DIM" "ro" pnt1 pnt2 pnt1 pnt2 midDimPt "" "EXIT")
      (setvar "ANGDIR" ANGDIR_DEF)
      (SETVAR "ANGBASE" ANGBASE_DEF)
    )





  (setq c1 (list 0.0 0.0 0.0))
  (SETQ DIMASZ_DEF (GETVAR "DIMASZ"))
  (SETVAR "DIMASZ" (* page3TextSize scaleArrowSize))
  (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
  (SETVAR "DIMTXT" page3TextSize)

	;sets block insert unit vars
  (setBlockUnitsVars)
  (toggle-OSNAP-Vars-OFF)


   ; this will temporarily adjust corner locations and system width
   ; for doing swing ties to edge of concrete chambers
  (if (or (= SYS_TYPE "4x8_CONCRETE_TRENCH")
          (= SYS_TYPE "8x4_CONCRETE_TRENCH")
      )
    (progn
      ; page three plan corners
      ; c_u1    c_u2
      ; c_d1    c_d2

      ; page two plan corners
      ; t_u1    t_u2
      ; t_d1    t_d2

      (setq c_u1-TEMP c_u1)
      (setq c_u2-TEMP c_u2)
      (setq c_d1-TEMP c_d1)
      (setq c_d2-TEMP c_d2)

      (setq t_u1-TEMP t_u1)
      (setq t_u2-TEMP t_u2)
      (setq t_d1-TEMP t_d1)
      (setq t_d2-TEMP t_d2)

      (setq sys_L-TEMP sys_L)
      (setq sys_W-TEMP sys_W)

      (setq c_u1 (polar (polar c_u1 %RIGHT 1.0) %DOWN 1.0))
      (setq c_u2 (polar (polar c_u2 %LEFT 1.0) %DOWN 1.0))
      (setq c_d1 (polar (polar c_d1 %RIGHT 1.0) %UP 1.0))
      (setq c_d2 (polar (polar c_d2 %LEFT 1.0) %UP 1.0))

      (setq t_u1 (polar (polar t_u1 %RIGHT 1.0) %DOWN 1.0))
      (setq t_u2 (polar (polar t_u2 %LEFT 1.0) %DOWN 1.0))
      (setq t_d1 (polar (polar t_d1 %RIGHT 1.0) %UP 1.0))
      (setq t_d2 (polar (polar t_d2 %LEFT 1.0) %UP 1.0))

      (setq sys_L (* (distance (list (car c_u1) (cadr c_u1) 0.0) (list (car c_u2) (cadr c_u2) 0.0)) 12.0))
      (setq sys_W (* (distance (list (car c_u1) (cadr c_u1) 0.0) (list (car c_d1) (cadr c_d1) 0.0)) 12.0))
    )
  )


       ; now i have swingSide ("UP", "DOWN" "LEFT" "RIGHT") and TL, TR, BL, BR as real number distances

	; Check to see what corners have been selected
  (if (AND (/= TR 0.0) (/= BR 0.0))
    (progn
      (setq r1 TR r2 BR)       ; radii of circles
      (setq d (/ sys_w 12.0))  ; separation between circle centers
      (setq c2 (list (/ sys_w 12.0) 0.0))
    )
  )

  (if (AND (/= TL 0.0) (/= BL 0.0))
    (progn
      (setq r1 BL r2 TL)       ; radii of circles
      (setq d (/ sys_w 12.0))  ; separation between circle centers
      (setq c2 (list (/ sys_w 12.0) 0.0))
    )
  )

  (if (AND (/= TR 0.0) (/= TL 0.0))
    (progn
      (setq r1 TL r2 TR)       ; radii of circles
      (setq d (/ sys_l 12.0))  ; separation between circle centers
      (setq c2 (list (/ sys_L 12.0) 0.0))
    )
  )
  (if (AND (/= BR 0.0) (/= BL 0.0))
    (progn
      (setq r1 BR r2 BL)       ; radii of circles
      (setq d (/ sys_l 12.0))  ; separation between circle centers
      (setq c2 (list (/ sys_L 12.0) 0.0))
    )
  )


  (setq x (/ (+ (- (* d d) (* r2 r2)) (* r1 r1)) (* 2.0 d)))

  (setq term1 (* 4.0 (* (* d d) (* r1 r1))))
  (setq term2 (+ (- (* d d) (* r2 r2)) (* r1 r1)))
  (setq term12 (- term1 (* term2 term2)))
  (setq termrad (sqrt term12))
  (setq a (* (/ 1.0 d) termrad))

  (setq cInt (polar (polar c1 0.0 x) (/ pi 2.0) (/ a 2.0)))
  (setq len (distance cInt c1))
  (setq ang (angle c1 cInt))

  (setq changeX (car cInt))
  (setq changeY (cadr cInt))

	; Top side intersection
  (if (AND (/= TL 0.0) (/= TR 0.0))
    (progn
      (setq intPt2 (polar t_u1 %RIGHT changeX))
      (setq intPt (polar c_u1 %RIGHT changeX))
      (if (or (= swingSide "UP") (= swingSide "LEFT") (= swingSide "RIGHT"))
        (progn
          (setq intPt (polar intPt %UP changeY)) (setq intPt2 (polar intPt2 %UP changeY))
        )
        (progn
          (setq intPt (polar intPt %DOWN changeY)) (setq intPt2 (polar intPt2 %DOWN changeY))
        )
      )
      (doDim c_u1 intPt)
      (doDim c_u2 intPt)

    )
  )

	; Bottom side intersection
  (if (AND (/= BL 0.0) (/= BR 0.0))
    (progn
      (setq intPt2 (polar t_d2 %LEFT changeX))
      (setq intPt (polar c_d2 %LEFT changeX))
      (if (or (= swingSide "UP") (= swingSide "LEFT") (= swingSide "RIGHT"))
        (progn
          (setq intPt (polar intPt %UP changeY)) (setq intPt2 (polar intPt2 %UP changeY))
        )
        (progn
          (setq intPt (polar intPt %DOWN changeY)) (setq intPt2 (polar intPt2 %DOWN changeY))
        )
      )
      (doDim c_d1 intPt)
      (doDim c_d2 intPt)

    )
  )


	; right hand side intersection
  (if (AND (/= TR 0.0) (/= BR 0.0))
    (progn
      (setq intPt2 (polar t_u2 %DOWN changeX))
      (setq intPt (polar c_u2 %DOWN changeX))
      (if (or (= swingSide "UP") (= swingSide "LEFT") (= swingSide "DOWN"))
        (progn
          (setq intPt (polar intPt %LEFT changeY)) (setq intPt2 (polar intPt2 %LEFT changeY))
        )
        (progn
          (setq intPt (polar intPt %RIGHT changeY)) (setq intPt2 (polar intPt2 %RIGHT changeY))
        )
      )
      (doDim c_u2 intPt)
      (doDim c_d2 intPt)

    )
  )


	; Left side intersection
  (if (AND (/= TL 0.0) (/= BL 0.0))
    (progn
      (setq intPt2 (polar t_d1 %UP changeX))
      (setq intPt (polar c_d1 %UP changeX))
      (if (or (= swingSide "UP") (= swingSide "LEFT") (= swingSide "DOWN"))
        (progn
          (setq intPt (polar intPt %LEFT changeY)) (setq intPt2 (polar intPt2 %LEFT changeY))
        )
        (progn
          (setq intPt (polar intPt %RIGHT changeY)) (setq intPt2 (polar intPt2 %RIGHT changeY))
        )
      )
      (doDim c_u1 intPt)
      (doDim c_d1 intPt)

    )
  )


  (setq ex (getvar "Expert"))
  (setvar "Expert" 2)          ; suppresses "Block already defined. Redefined it? ALERT"

  (if (= RP_TYPE "1") (setq blk (strcat $septicadPathBlockRequired "SURVEY_PIN.DWG")))
  (if (= RP_TYPE "2") (setq blk (strcat $septicadPathBlockRequired "SURVEY_MON.DWG")))
  (if (= RP_TYPE "3") (setq blk (strcat $septicadPathBlockRequired "TREE_E.DWG")))
  (if (= RP_TYPE "4") (setq blk (strcat $septicadPathBlockRequired "TREE_D.DWG")))
  (if (= RP_TYPE "5") (setq blk (strcat $septicadPathBlockRequired "UTILITY.DWG")))
  (if (= RP_TYPE "6") (setq blk (strcat $septicadPathBlockRequired "CORNER.DWG")))

	; insert Block and Leader
  (if (= RP_TYPE "1") (command "-insert" blk intPt (/ page3scale 10.0) (/ page3scale 10.0) "0"))
  (if (= RP_TYPE "2") (command "-insert" blk intPt (/ page3scale 10.0) (/ page3scale 10.0) "0"))
  (if (= RP_TYPE "3") (command "-insert" blk intPt (/ page3scale 10.0) (/ page3scale 10.0) "0"))
  (if (= RP_TYPE "4") (command "-insert" blk intPt (/ page3scale 10.0) (/ page3scale 10.0) "0"))
  (if (= RP_TYPE "5") (command "-insert" blk intPt (/ page3scale 10.0) (/ page3scale 10.0) "0"))
  (if (= RP_TYPE "6") (command "-insert" blk intPt (/ page3scale 10.0) (/ page3scale 10.0) "0"))


    ; if flag is nil then this is an inline function and blocks are inserted into page2 plan
  (if (and (/= fill_top nil) (= $flag nil))
    (progn
      (if (= RP_TYPE "1") (command "-insert" blk intPt2 (/ page2scale 10.0) (/ page2scale 10.0) "0"))
      (if (= RP_TYPE "2") (command "-insert" blk intPt2 (/ page2scale 10.0) (/ page2scale 10.0) "0"))
      (if (= RP_TYPE "3") (command "-insert" blk intPt2 (/ page2scale 10.0) (/ page2scale 10.0) "0"))
      (if (= RP_TYPE "4") (command "-insert" blk intPt2 (/ page2scale 10.0) (/ page2scale 10.0) "0"))
      (if (= RP_TYPE "5") (command "-insert" blk intPt2 (/ page2scale 10.0) (/ page2scale 10.0) "0"))
      (if (= RP_TYPE "6") (command "-insert" blk intPt2 (/ page2scale 10.0) (/ page2scale 10.0) "0"))
    )
  )


  (setvar "Expert" ex)         ; resets variable


	; Calculate Leadpt based on 
  (if (= swingSide "UP") (setq leadPt (polar intPt (* 3.0 (/ pi 4.0)) 20.0)))
  (if (= swingSide "DOWN") (setq leadPt (polar intPt (* 7.0 (/ pi 4.0)) 20.0)))
  (if (= swingSide "RIGHT") (setq leadPt (polar intPt (* 7.0 (/ pi 4.0)) 20.0)))
  (if (= swingSide "LEFT") (setq leadPt (polar intPt (* 3.0 (/ pi 4.0)) 20.0)))

  (if (= swingSide "UP") (setq leadPt2 (polar intPt2 (* 3.0 (/ pi 4.0)) 30.0)))
  (if (= swingSide "DOWN") (setq leadPt2 (polar intPt2 (* 7.0 (/ pi 4.0)) 30.0)))
  (if (= swingSide "RIGHT") (setq leadPt2 (polar intPt2 (* 7.0 (/ pi 4.0)) 30.0)))
  (if (= swingSide "LEFT") (setq leadPt2 (polar intPt2 (* 3.0 (/ pi 4.0)) 30.0)))


    ; print to page 3 with default dimensions
  (SETQ DIMASZ_DEF (GETVAR "DIMASZ"))
  (SETVAR "DIMASZ" (* page3TextSize scaleArrowSize))
  (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
  (SETVAR "DIMTXT" page3TextSize)
  (doleader intPt leadPt txt1 txt2)
  (SETVAR "DIMASZ" DIMASZ_DEF)
  (SETVAR "DIMTXT" DIMTXT_DEF)


    ; if flag is nil then this is an inline function and blocks are inserted into page2 plan
  (if (and (/= fill_top nil) (= $flag nil))
    (progn
      ; print to page 2 with set dimensions
      (SETVAR "DIMASZ" (* page2TextSize scaleArrowSize))
      (SETVAR "DIMTXT" page2TextSize)
      (doleader intPt2 leadPt2 txt1 txt2)
      (SETVAR "DIMASZ" DIMASZ_DEF)
      (SETVAR "DIMTXT" DIMTXT_DEF)
    )
  )


    ;reset block insert unit vars
  (resetBlockUnitsVars)
  (toggle-OSNAP-Vars-RESET)

   ; this will temporarily adjust corner locations and system width
   ; for doing swing ties to edge of concrete chambers

  (if (or (= SYS_TYPE "4x8_CONCRETE_TRENCH")
          (= SYS_TYPE "8x4_CONCRETE_TRENCH")
      )
    (progn
      (setq c_u1 c_u1-TEMP)
      (setq c_u2 c_u2-TEMP)
      (setq c_d1 c_d1-TEMP)
      (setq c_d2 c_d2-TEMP)
      (setq t_u1 t_u1-TEMP)
      (setq t_u2 t_u2-TEMP)
      (setq t_d1 t_d1-TEMP)
      (setq t_d2 t_d2-TEMP)
      (setq sys_L sys_L-TEMP)
      (setq sys_W sys_W-TEMP)
    )
  )
  
  
  
  

  
  
  
)

;;;;;;; end subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



     ; $flag is used to determine whether being used inline with system flow, or whether on recently opened document with no info in memory  three points are obtained.
  (setq $flag nil)
; get system corners if not define and only do one set of labels
   ; if this is true than this is a re-opened document
  (if (= c_u1 nil)
    (progn
      (GetUserVars)
      (setScales)
      (swingTieGetCorners)
      (setq $flag 1)
    )
  )


  (command "UNDO" "BE")
  (toggle-OSNAP-Vars-OFF)

  (setq TR nil TL nil BR nil BL nil ddiag nil)

  (setq RPList (list "" "Pin" "Monument" "Evergreen Tree" "Deciduous Tree" "Utility Pole" "Corner of Building"))

       ;;;--- Load the DCL file

  (if (= (strcase (getvar "PRODUCT")) "AUTOCAD")
    (setq dcl_id (load_dialog (strcat $septicadPathDCL "swingtie-ACAD.dcl")))
    (setq dcl_id (load_dialog (strcat $septicadPathDCL "swingtie-BCAD.dcl")))
  )


      ;;;--- See if the dialog box is already loaded
  (if (not (new_dialog "swingtie" dcl_id))
    (progn
      (alert "The swingtie.DCL file was not found!")
      (exit)
    )
  )

      ;;;--- Add the label names to the dialog box
  (start_list "RP_TYPE" 3)
  (mapcar 'add_list RPList)
  (end_list)



	;;;;;--------------- in LSP after new_dialog
	;;;--- First we need a slide name 
  (setq mySlideName (strcat $septicadPathDCL "erp_hrp.sld"))      ; can be WMF, SLD or EMF

	;;;--- Second we need the key to the image control 
  (setq myKey "field")

	;;;--- Next we send the slide name and the key to the update function 
  (upDateImage mySlideName myKey)

      ;;;--- If an action event occurs, do this function
  (action_tile "cancel" "(setq ddiag 1)(done_dialog)")
  (action_tile "okay" "(saveSwingTieVars)(swingTieCheckInput)")

  (set_tile "DESCRIPTION1" "Swing Tie Labeler")
  (set_tile "DESCRIPTION2" "Use as many times as you want to locate feature with swing ties to")
  (set_tile "DESCRIPTION3" "the disposal field corners. Click <Cancel/Skip> button to continue")

     ;;;--- Display the dialog box
  (start_dialog)

      ;;;--- Unload the dialog box
  (unload_dialog dcl_id)







      ;;;--- If the "Okay" button was pressed
  (if (= ddiag 2)
    (progn
      (swingTieDraw&Label)
      (setq alert_string nil valid nil TR nil TL nil BR nil BL nil rp_type nil swingType nil)
      (swingtieMain)
    )
  )

     ; upon exiting a sequence of swing ties, nil these variables so that the function will start up correctly (prompt for points) when run next
  (if (or (= $flag 1) (= fill_top nil)) (setq $flag nil c_U1 nil c_U2 nil c_D1 nil c_D2 nil sys_W nil sys_L nil))

  (toggle-OSNAP-Vars-RESET)

  (command "UNDO" "E")
  (princ)



)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;