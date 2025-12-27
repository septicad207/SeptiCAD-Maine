;;; this is the code of the scarification tools



(defun c:scarh (/ s i e l cm)
  (setq cm (getvar 'CMDECHO))
  (setvar 'CMDECHO 0)
  (toggle-OSNAP-Vars-OFF)


  (if (= (last (ssgetfirst)) nil)
    ; IF
    (progn
      (prompt "\nSelect polyline entities to add scarification hatch too:")
      (setq s (ssget))
    )
    ; ELSE
    (setq s (last (ssgetfirst)))
  )

  (if s
    (progn
      ;; run through each entity selected
      (setq i 0)
      (setq l (sslength s))
      (command "UNDO" "BE")
      (repeat l
        (setq e (ssname s i)
              i (1+ i)
        )
        (add-scarh e)
      )
    )
  )

  (setvar 'CMDECHO cm)
  (toggle-OSNAP-Vars-RESET)

; if $customSysList is defined the ask user for adding note
  (if $customSysList
    (progn
      ; set three scarification notes
      (setq $scarifyNote (list (cdr (assoc "%SCAR-1%" $customSysList))
                               (cdr (assoc "%SCAR-2%" $customSysList))
                               (cdr (assoc "%SCAR-3%" $customSysList))
                         )
      )

      ;;;--- Load the DCL file
      (setq dcl_id (load_dialog (strcat $septicadPathDCL "scarifySimple.dcl")))

      ;;;--- See if the dialog box is already loaded
      (if (not (new_dialog "scarifySimple" dcl_id))
        (progn
          (alert "The scarifySimple.DCL file was not found!")
          (exit)
        )
      )

      (setq $scarifyNote (list (cdr (assoc "%SCAR-1%" $customSysList))
                               (cdr (assoc "%SCAR-2%" $customSysList))
                               (cdr (assoc "%SCAR-3%" $customSysList))
                         )
      )

      ;;;--- Add the label names to the dialog box
      (start_list "NOTE" 3)
      (mapcar 'add_list $scarifyNote)
      (end_list)


      ;;;--- If an action event occurs, do this function

      (action_tile "label" "(scarifySaveVars)(setq bdiag 0)")
      (action_tile "cancel" "(setq bdiag 1)(done_dialog)")


      ;;;--- Display the dialog box
      (start_dialog)

      ;;;--- Unload the dialog box
      (unload_dialog dcl_id)

      ;;;--- If the label button was pressed
      (if (= bdiag 0)
        (progn
          ; $scarifyNote is the not selected
          (SETQ DIMASZ_DEF (GETVAR "DIMASZ"))
          (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
          (SETVAR "DIMTXT" profileTextSize)
          (SETVAR "DIMASZ" (* profileTextSize scaleArrowSize))


          (COMMAND "LEADER" (getpoint "Select Point for Arrowhead") (getpoint "Select Point for Label") "" $scarifyNote "")

          (setq ss (ssadd (entlast)))
          ; changed mText Width
          (modifySS ss 41 (* profilescale 27.2))


          (SETVAR "DIMASZ" DIMASZ_DEF)
          (SETVAR "DIMTXT" DIMTXT_DEF)
          ;  print the label here
        )
      )

      ;;;--- If the cancel button was pressed
      (if (= bdiag 1)
        (progn
          (princ "Function Cancelled - No Label Printed")
        )
      )

      (command "UNDO" "E")
      (setq $scarifyNote nil)
      (princ)


    )
  )


  (princ)
)



(defun add-scarh (ent / obj hobj objType mspace ent sset1 hatchName hatchScale hatchlineweight offsetDist ptlist firstX lastX pt cm L1-ptlist L2-ptlist offline pline)
  (vl-load-com)
  (setq mspace (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object))))

  (setq obj (vlax-ename->vla-object ent)
        ObjType (vla-get-ObjectName obj)
        sset1 (ssadd)
        hatchName "ANSI31"
        hatchScale 30
        hatchAngle 0
        hatchlineweight 256
        offsetDist 6
  )
  (if (and
        (= objType "AcDbPolyline")
        (= (vla-get-closed obj) ':VLAX-FALSE)
      )
    (progn

      ; first make sure that the vertices are listed from left to right
      (setq ptlist (get_pline_cor (entget ent)))
      (setq firstX (car (car ptlist)))
      (setq lastX (car (last ptlist)))
      (if (> firstX lastX)
        (reverse_pline ent)
      )

      ; create copy of selected polyline, so original line will never by modified
      (setq pt (list 0 0 0)
            cm (getvar "COPYMODE")
      )
      (setvar "COPYMODE" 1)
      (command "._copy" (ssadd ent) "" pt pt)
      (setvar "COPYMODE" cm)
      (ssadd (entlast) sset1)

      (setq L1-ptlist (get_pline_cor (entget (entlast))))  ; get list of point for selected polyline


      (setq offLine (vla-Offset obj offsetDist))           ; create offset
      (ssadd (entlast) sset1)
      (setq L2-ptlist (get_pline_cor (entget (entlast))))

      (setq leftLine (vla-addline mspace (vlax-3d-point (car L1-ptList)) (vlax-3d-point (car L2-ptList))))
      (ssadd (entlast) sset1)

      (setq rightLine (vla-addline mspace (vlax-3d-point (last L1-ptList)) (vlax-3d-point (last L2-ptList))))
      (ssadd (entlast) sset1)

      ; joins polylines in sset and resets sset
      (polylinejoin sset1)
      (setq obj (vlax-ename->vla-object (entlast))
            sset1 (ssadd)
      )
      ; add joined polyline to sset - to be deleted later
      (ssadd (entlast) sset1)

      ;; Create the make sure it is closed
      (vla-put-closed obj :vlax-true)
      (setq ptlist (get_pline_cor (entget (entlast))))
      (setq Polypoints (apply 'append ptlist))

      (setq VLADataPts (Polylist->variantArray polypoints))

      (setq pline (vla-addLightweightPolyline
                    mspace      ; Global Definition for Model Space
                    VLADataPts  ; vertices of path boundary
                  ) ;_ end of vla-addLightweightPolyline
      )
      (vla-put-closed pline :vlax-true)
      ; add polyline to sset - to be deleted later
      (ssadd (entlast) sset1)


      ;; Add the Hatch Object:
      (command "_-hatch" "_select" "_last" "" "properties" "ANSI31" hatchScale hatchAngle "")

      ;erase all the temporary objects
      (command "._erase" sset1 "")
    )  ; end if
    ; else
    (princ (strcat "\n<<" objType " Entity Ignored>> Function only works with open polylines"))
  )

  (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; allocate space for an array of 2d points stored as doubles
; send on list wtih a list of numbers represent in x y coords and it will make a safearray
; that is sufficient for creating polyline and making hatches.
(defun Polylist->variantArray (ptList / arraySpace sArray)
  (setq arraySpace (vlax-make-safearray
                     vlax-vbdouble  ; element type
                     (cons 0
                           (- (length ptList) 1)
                     )
                   )
  )
  (setq sArray (vlax-safearray-fill arraySpace ptList))  ; return array variant
  (vlax-make-variant sArray)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;--------------------------------------------------------------------
  ;;  get pline vertex list for LWPline only
(defun get_pline_cor (elst)
  (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= 10 (car x))) elst))
)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; R_PLINE -Gilles Chanteau-
;;; Reverse vertices order.
;;; Works with lines, lwpolylines, 2D and 3D polylines.
;;; Keeps the pline properties (arcs, widthes ...)

(defun reverse_pline (ent / e_lst typ vtx v_lst p_lst l_vtx)
  (setq e_lst (entget ent)
        typ (cdr (assoc 0 e_lst))
  )
  (if (= typ "LINE")
    ((lambda (p1 p2)
             (entmod
               (subst (cons 10 (cdr p2))
                      p1
                      (subst (cons 11 (cdr p1)) p2 elst)
               )
             )
     )
       (assoc 10 e_lst)
       (assoc 11 e_lst)
    )
    (progn
      (cond
        ((= typ "POLYLINE")
            (setq vtx (entnext ent))
          (while (= (cdr (assoc 0 (entget vtx))) "VERTEX")
            (setq v_lst (cons (entget vtx) v_lst)
                  vtx (entnext vtx)
            )
          )
        )
        ((= typ "LWPOLYLINE")
            (setq p_lst (vl-remove-if-not
                          (function
                            (lambda (x)
                                    (member (car x) '(10 40 41 42))
                            )
                          )
                                          e_lst
                        )
                  e_lst (vl-remove-if
                          (function
                            (lambda (x)
                                    (member x p_lst)
                            )
                          )
                                      e_lst
                        )
            )
           (while p_lst
             (setq v_lst (cons
                           (list (car p_lst)
                                 (cadr p_lst)
                                 (caddr p_lst)
                                 (cadddr p_lst)
                           )
                               v_lst
                         )
                   p_lst (member (assoc 10 (cdr p_lst)) (cdr p_lst))
             )
           )
        )
      )
      (setq l_vtx (last v_lst)
            l_vtx (subst (cons 40 (cdr (assoc 41 (car v_lst))))
                         (assoc 40 l_vtx)
                         l_vtx
                  )
            l_vtx (subst (cons 41 (cdr (assoc 40 (car v_lst))))
                         (assoc 41 l_vtx)
                         l_vtx
                  )
            l_vtx (subst (cons 42 (- (cdr (assoc 42 (car v_lst)))))
                         (assoc 42 l_vtx)
                         l_vtx
                  )
      )
      (setq v_lst
            (mapcar
              (function
                (lambda (x y)
                        (setq x (subst (cons 40 (cdr (assoc 41 y))) (assoc 40 x) x)
                              x (subst (cons 41 (cdr (assoc 40 y))) (assoc 41 x) x)
                              x (subst (cons 42 (- (cdr (assoc 42 y)))) (assoc 42 x) x)
                        )
                )
              )
                    v_lst
                    (cdr v_lst)
            )
      )
      (if (= (logand 1 (cdr (assoc 70 e_lst))) 1)
        (setq v_lst (append (list l_vtx) v_lst))
        (setq v_lst (append v_lst (list l_vtx)))
      )
      (cond
        ((= (cdr (assoc 0 e_lst)) "POLYLINE")
            (mapcar 'entmake
                    (append (list e_lst) v_lst (list (entget vtx)))
            )
           (entdel ent)
        )
        ((= (cdr (assoc 0 e_lst)) "LWPOLYLINE")
            (setq e_lst (append e_lst (apply 'append v_lst)))
            (entmod e_lst)
        )
      )
    )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







(defun c:scarAuto (/ codeList noteList conList cnt dcl_id)

  (if (/= U1 nil)
    (progn

      (setvar "cmdecho" 0)

      ;;;--- Load the DCL file
      (setq dcl_id (load_dialog (strcat $septicadPathDCL "scarify.dcl")))

      ;;;--- See if the dialog box is already loaded
      (if (not (new_dialog "scarify" dcl_id))
        (progn
          (alert "The scarify.DCL file was not found!")
          (exit)
        )
      )

      (setq $scarifyNote (list (cdr (assoc "%SCAR-1%" $customSysList))
                               (cdr (assoc "%SCAR-2%" $customSysList))
                               (cdr (assoc "%SCAR-3%" $customSysList))
                         )
      )

      ;;;--- Add the label names to the dialog box
      (start_list "NOTE" 3)
      (mapcar 'add_list $scarifyNote)
      (end_list)


      ; turn platform off if one of these types of systems
      (if (OR (= sys_type "STONE")
              (= sys_type "4x8_CONCRETE_CLUSTER")
              (= sys_type "8x4_CONCRETE_CLUSTER")
              (= sys_type "ELJEN")
              (= sys_type "ELJEN_INVERTED")
              (= sys_type "ENVIRO")
          )

        (mode_tile "trench" 1)

      )



      ;;;--- If an action event occurs, do this function

      (action_tile "platform" "(scarifySaveVars)(setq bdiag 0)")
      (action_tile "trench" "(scarifySaveVars)(setq bdiag 1)")
      (action_tile "cancel" "(setq bdiag 2)(done_dialog)")
      (action_tile "customize" "(setq bdiag 3)(done_dialog)")

      (upDateImage (strcat $septicadPathDCL "scar-trench.sld") "trench")
      (upDateImage (strcat $septicadPathDCL "scar-platform.sld") "platform")


      ;;;--- Display the dialog box
      (start_dialog)

      ;;;--- Unload the dialog box
      (unload_dialog dcl_id)
      (command "UNDO" "BE")
      ;;;--- If the platform button was pressed
      (if (= bdiag 0)
        (progn
          (autoscarify "PLATFORM" $scarifyNote)
        )
      )

      ;;;--- If the trench button was pressed
      (if (= bdiag 1)
        (progn
          (autoscarify "TRENCH" $scarifyNote)
        )
      )


      ;;;--- If the cancel button is pressed
      (if (= bdiag 2)
        (progn
          (princ "\nCancelled SeptiCAD --- Scarify / Transitional Horizon Tool \n")
        )
      )

      ;;;--- If the customize button is pressed
      (if (= bdiag 3)
        (progn
          (alert "Three Scarification / Transitional Horizon notes are possible for each system.\n
			Each of the choices are stored in the system preference under the %SCAR-1%, %SCAR-2%, %SCAR-3% codes.\n

      			The System Customization Menu will open when you press okay."
          )

          (customSystem)
        )
      )
      (command "UNDO" "E")
      (setq $scarifyNote nil)
      (princ)

    )
    (alert "Function only Available When Design in Memory")
  )                 ; end big if

)
; scarify

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun scarifySaveVars ()
  (if (/= (get_tile "NOTE") "")
    (progn
      (setq $scarifyNote (nth (atoi (get_tile "NOTE")) $scarifyNote))
      (done_dialog)
    )
    (progn
      (alert " A Note must be selected ")
    )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun autoScarify (type scarNote / ss DIMASZ_DEF DIMTXT_DEF down up right left upLeft upRight upPt upPt1 upPt2 downPt downPt1 downPt2 tempChamberPtList pt item sPtLeft sPtRight cPtLeft cPtRight sPtList cPtList ptList cnt belowGradeList lastPt)
;(defun autoScarify (type scarNote)

  (command "UNDO" "BE")

;;;--- Set default directions
  (setq down (/ (* 3.0 pi) 2.0))
  (setq up (/ pi 2.0))
  (setq right 0.0)
  (setq left pi)

	; color for line denoting excavation and hatch
  (setq lineColor 256
        hatchColor 256
  )

	; angle for trench excavations, near vertical
  (setq upLeft (+ up 0.2))
  (setq upRight (- up 0.2))

	; these are the variable for the cross sections
	;u1u2 u1u1 (list 0.0 U1) (list sys_w D1) d1d1 d1d2 ; left
	;u2u2 u2u1 (list 0.0 U2) (list sys_w D2) d2d1 d2d2 ; right
	; UP_INT and DOWN_INT ; in common

  (if (= crossSection "LEFT")
    (progn
      (setq upPt (list 0.0 U1 0.0))
      (setq upPt1 u1u1)
      (setq upPt2 u1u2)
      (setq downPt (list sys_W d1 0.0))
      (setq downPt1 d1d1)
      (setq downPt2 d1d2)
    )
  )
  (if (= crossSection "RIGHT")
    (progn
      (setq upPt (list 0.0 U2 0.0))
      (setq upPt1 u2u1)
      (setq upPt2 u2u2)
      (setq downPt (list sys_W d2 0.0))
      (setq downPt1 d2d1)
      (setq downPt2 d2d2)
    )
  )

        ; if shelfs are used, then massage list so it in same format as if in no shelf mode and leave chamberPtList alone
  (setq tempChamberPtList ChamberPtList)
  (if (= shelfToggle "1")
    (progn
      (setq tempChamberPtList (list))
      (foreach item ChamberPtList
        (foreach pt item
          (setq tempChamberPtList (append tempChamberPtList (list (list pt))))
        )
      )
    )
  )

	;  Where is the up_int along the line
	;  using the inters function will work as it is specific to endpoints

  (setq upPtList (list))
  (setq downPtList (list))

	; upslope intersections, add list of points, from left to right
	;  if first segment upslope
  (if (inters upPt upPt1 (polar up_int down 999999) (polar up_int up 999999))
    (setq upPtList (list UP_INT))
  )

	; if second segment upslope
  (if (inters upPt1 upPt2 (polar up_int down 999999) (polar up_int up 999999))
    (setq upPtList (list UP_INT upPt1))
  )

	; if extended segment upslope
  (if (inters upPt2 (polar upPt2 (angle upPt1 upPt2) 99999999) (polar up_int down 999999) (polar up_int up 999999))
    (setq upPtList (list UP_INT upPt2 upPt1))
  )

	;if first segment downslope
  (if (inters downPt downPt1 (polar down_int down 999999) (polar down_int up 999999))
    (setq downPtList (list DOWN_INT))
  )

	; if second segment downslope
  (if (inters downPt1 downPt2 (polar down_int down 999999) (polar down_int up 999999))
    (setq downPtList (list downPt1 DOWN_INT))
  )

	; extended segment downslope
  (if (inters downPt2 (polar downPt2 (angle downPt1 downPt2) 99999999) (polar down_int down 999999) (polar down_int up 999999))
    (setq downPtList (list downPt1 downPt2 DOWN_INT))
  )


;;;--- START VERSION FOR STONE BED AND CLUSTER CONCRETE
; if stone then do row_start and row_end and what ever the intersection is under the stone if present
  (if (OR (= sys_type "STONE")
          (= sys_type "4x8_CONCRETE_CLUSTER")
          (= sys_type "8x4_CONCRETE_CLUSTER")
      )
    (progn
      (setq ptList (list))

      ; if cluster concrete temporily adjust row_start and row_end
      (if (OR (= sys_type "4x8_CONCRETE_CLUSTER")
              (= sys_type "8x4_CONCRETE_CLUSTER")
          )
        (progn
          (setq row_startTemp row_start)
          (setq row_endTemp row_end)
          (setq row_start (polar row_start left stoneBeside))
          (setq row_start (polar row_start down stonebelow))
          (setq row_end (polar row_end right stoneBeside))
          (setq row_end (polar row_end down stonebelow))
        )
      )


      ; if completely below grade
      (if (and (> (cadr downPt) (+ (cadr row_end) chamber_H fill_above))
               (> (cadr upPt) (+ (cadr row_start) chamber_H fill_above))
          )
        (setq allBelowGrade "YES")
        (setq allBelowGrade "NO")
      )



      (if (= allBelowGrade "YES")
        (progn

          (setq ptList (list
                         (lineIntersection upPt1 upPt (polar row_start left 12) (polar row_start upLeft 999))
                         (polar row_start left 12)
                         (polar row_end right 12)
                         (lineIntersection downPt1 downPt (polar row_end right 12) (polar row_end upRight 999))
                       )
          )

          (setq ptList (list (polar row_start left 12) (polar row_end right 12)))

          (drawPolyLine ptList)
          (setq ss (ssadd (entlast)))
          (modifySS ss 62 lineColor)
          (command "draworder" ss "" "B")

          (add-scarh (entlast))
          (setq ss (ssadd (entlast)))
          (modifySS ss 62 hatchColor)
          (command "draworder" ss "" "B")

          (setq arrowPoint (POLAR (polar ROW_END down 5) PI (* SYS_W 0.1)))
          (SETQ textPoint (POLAR arrowPoint 5.0 (* (/ profileScale 5.0) 35)))

        )
      )


      ; start if not all below grade
      (if (= allBelowGrade "NO")
        (progn

          ; if stone is below grade on upslope side do intersection upLeft row_start, else use upPt
          (if (< (cadr row_start) (cadr upPt))
            (progn
              (setq ptList (append ptList
                                   (list (lineIntersection upPt1 upPt row_start (polar row_start upLeft 999)))
                           )
              )
              (setq ptList (append ptList (list row_start)))
            )
            (progn
              (setq ptList (append ptList (list upPt)))
            )
          )

          ; if stone is below grade on downslope side do row_end and interseciton upRigt -ELSE- add intersection point along bottom of bed and downPt
          (if (<= (cadr row_end) (cadr downPt))
            (progn  ; if
              (setq ptList (append ptList (list row_end)))
              (setq ptList (append ptList
                                   (list (lineIntersection downPt1 downPt row_end (polar row_end upRight 999)))
                           )
              )

            )       ; end if

            (progn  ; else
                    (setq ptList (append ptList
                                         (list (lineIntersection upPt downPt row_start row_end))
                                 )
                    )
                    ; add second point
              (setq ptList (append ptList (list downPt)))
            )       ; end else
          )

          ;;;; now ptList has intersections below stone, upslope and downslope fill extensions need to be added in the appropriate order
          (setq ptList (append upPtList ptList))
          (setq ptList (append ptList downPtList))


          (if (> (length ptList) 0)
            (progn

              (drawPolyLine ptList)
              (setq ss (ssadd (entlast)))
              (modifySS ss 62 lineColor)
              (command "draworder" ss "" "B")

              (add-scarh (entlast))
              (setq ss (ssadd (entlast)))
              (modifySS ss 62 hatchColor)
              (command "draworder" ss "" "B")
            )
          )

          (setq arrowPoint (POLAR (polar downPt down 5) PI (* SYS_W 0.1)))
          (SETQ textPoint (POLAR arrowPoint 5.0 (* (/ profileScale 5.0) 35)))

        )
      )
      ; end if not all below grade

      ; reset row_start and row_end
      (if (OR (= sys_type "4x8_CONCRETE_CLUSTER")
              (= sys_type "8x4_CONCRETE_CLUSTER")
          )
        (progn
          (setq row_start row_startTemp)
          (setq row_end row_endTemp)
        )
      )

    )
  )
;;;--- END VERSION FOR STONE BED AND CLUSTER CONCRETE





;;;--- START VERSION FOR ENVIRO SEPTIC
  (if (= sys_type "ENVIRO")
    (progn

      (setq ptList (list)
            sPtList (list)
            cPtList (list)
            cnt 1
      )

      ; sysAngle is used to project slope of base of sand on downslope side of the system
      (setq sysAngle (angle row_start (polar row_end left chamber_W)))

      ; slope in percent
      (setq sysSlope (/ (- (last row_start) (last row_end)) (- sys_w chamber_W)))

      (setq cPtList (list))
      (foreach item tempChamberPtList
        (setq cPtList (append cPtList (list (polar (polar (car item) down 6.0) right (/ chamber_W 2.0)))))
      )


      ; add the point for the bottom of the sand upslope side
      (setq cPtList (append (list (polar (nth 0 cPtList) left (+ (/ chamber_W 2.0) 12.0))) cPtList))

      ; add the point for the bottom of the sand downslope side
      (setq cPtList (append cPtList (list (polar (nth (- (length cPtList) 1) cPtList) sysAngle (+ (/ chamber_W 2.0) 12.0)))))

      ; add the point for the bottom if >10% slope, add point 36 inches from standard end of system sand. 36 inches farther from chamber

      (if (>= sysSlope 0.1)
        (setq cPtList (append cPtList (list (polar (nth (- (length cPtList) 2) cPtList) sysAngle (+ (/ chamber_W 2.0) 48.0)))))
      )


      ; slope intersections points associated with each bottom of sand below enviroseptic sand 
      (foreach item cPtList

        ; if the first point
        ; the first chamber left point intersects outside the foot print of the system
        ; to simplify the code i assume no one will put in a crazy close number 
        (if (= cnt 1)
          (setq sPt (lineIntersection upPt1 upPt item (polar item up 999)))      ; if - special case
        )

        ; not the first and not the last point
        (if (and (/= cnt 1) (/= cnt (length cPtList)))
          (setq sPt (lineIntersection upPt downPt item (polar item up 999)))     ; else - normal
        )


        ; the last chamber right point intersection outside the footprint of the system
        ; to simplify the code i assume no one will put in a crazy close number 
        (if (= cnt (length cPtList))
          (setq sPt (lineIntersection downPt downPt1 item (polar item up 999)))  ; if - special case
        )


        (setq sPtList (append sPtList (list sPt)))

        (setq cnt (1+ cnt))
      )

      (setq belowGrade nil cnt 0)
      ; if any sand below grade then trip 
      (while (< cnt (length sPtList))
        (if (> (car (cdr (nth cnt sPtList)))
               (car (cdr (nth cnt cPtList)))
            )
          (setq belowGrade "YES")
        )
        (setq cnt (+ cnt 1))
      )


      (if (= belowGrade nil)
        (progn
          (setq ptList (append upPtList sPtList))
          (setq ptList (append ptList downPtList))
        )
      )

      (if (= belowGrade "YES")
        (progn

          ; create while loop to go through each item in selection set
          (setq cnt 0
                belowGradeList (list)
                flag nil
          )
          (while (< cnt (length sPtList))
            (if (> (car (cdr (nth cnt sPtList)))
                   (car (cdr (nth cnt cPtList)))
                )

              ; if sand is below grade, select chamber pt
              (progn
                ; if the first one is below slope, add special starting point
                (if (= cnt 0)
                  (progn
                    (setq int (slopeCutInt
                                (nth 0 cPtList)
                                (polar (nth 0 cPtList) upLeft 999)
                                upPt
                                upPt1
                                upPt2
                              )
                    )     ; if - special case
                    (setq belowGradeList (append belowGradeList (list int)))
                  )
                )

                ; add normal point
                (setq belowGradeList (append belowGradeList (list (nth cnt cPtList))))


                ; if the last row is below slope, add special starting point
                (if (= cnt (- (length sPtList) 1))
                  (progn
                    (setq int (slopeCutInt
                                (nth cnt cPtList)
                                (polar (nth cnt cPtList) upRight 999)
                                downPt
                                downPt1
                                downPt2
                              )
                    )     ; if - special case
                    (setq belowGradeList (append belowGradeList (list int)))
                  )
                )

              )

              ; if sand is above or at grade, select slope pt
              (progn

                ; if first time and not first pnt then we have just jumped and intersecting point
                (if (and (= flag nil) (/= cnt 0))
                  (progn  ; if transitional point, add intersection and regular slope point
                    (setq flag cnt)
                    (setq int (lineIntersection
                                (nth (- cnt 1) sPtList)
                                (nth cnt sPtList)
                                (nth (- cnt 1) cPtList)
                                (nth cnt cPtList)
                              )
                    )
                    (setq belowGradeList (append belowGradeList (list int)))
                    (setq belowGradeList (append belowGradeList (list (nth cnt sPtList))))
                  )
                  (progn  ; if standard point
                    (setq belowGradeList (append belowGradeList (list (nth cnt sPtList))))
                  )
                )
              )
            )
            (setq cnt (1+ cnt))
          )               ; end while


;;;;; it gets complicated with additional length not accounted for, the 
          (setq ptList (list))
          (setq ptList (append upPtList belowGradeList))
          (setq ptList (append ptList downPtList))
        )
      )


      (setq arrowPoint (polar row_end down 9))
      (SETQ textPoint (POLAR arrowPoint 5.0 (* (/ profileScale 5.0) 35)))

      (if (> (length ptList) 0)
        (progn
          (drawPolyLine ptList)
          (setq ss (ssadd (entlast)))
          (modifySS ss 62 lineColor)
          (command "draworder" ss "" "B")

          (add-scarh (entlast))
          (setq ss (ssadd (entlast)))
          (modifySS ss 62 hatchColor)
          (command "draworder" ss "" "B")
        )
      )


    )
  )
;;;--- END VERSION FOR ENVIRO SEPTIC









;;;--- START VERSION FOR ELJEN
  (if (or (= sys_type "ELJEN") (= sys_type "ELJEN_INVERTED"))
    (progn

      (setq ptList (list)
            sPtList (list)
            cPtList (list)
            cnt 1
      )

      ; sysAngle is used to project slope of base of sand on downslope side of the system
      (setq sysAngle (angle row_start (polar row_end left chamber_W)))

      ; slope in percent
      (setq sysSlope (/ (- (last row_start) (last row_end)) (- sys_w chamber_W)))

      (setq cPtList (list
                      (polar (polar row_start down 6.0) left 9.0)
                      (polar row_start down 6.0)
                      (polar (polar row_end left chamber_w) down 6.0)
                      (polar row_end down 6.0)
                      (polar (polar row_end right 9.0) down 6.0)
                    )
      )

      ; slope intersections points associated with each bottom of sand
      (foreach item cPtList

        ; if the first point
        ; the first chamber left point intersects outside the foot print of the system
        ; to simplify the code i assume no one will put in a crazy close number 
        (if (= cnt 1)
          (setq sPt (lineIntersection upPt1 upPt item (polar item up 999)))      ; if - special case
        )

        ; not the first and not the last point
        (if (and (/= cnt 1) (/= cnt (length cPtList)))
          (setq sPt (lineIntersection upPt downPt item (polar item up 999)))     ; else - normal
        )


        ; the last chamber right point intersection outside the footprint of the system
        ; to simplify the code i assume no one will put in a crazy close number 
        (if (= cnt (length cPtList))
          (setq sPt (lineIntersection downPt downPt1 item (polar item up 999)))  ; if - special case
        )


        (setq sPtList (append sPtList (list sPt)))

        (setq cnt (1+ cnt))
      )

      (setq belowGrade nil cnt 0)
      ; if any sand below grade then trip 
      (while (< cnt (length sPtList))
        (if (> (car (cdr (nth cnt sPtList)))
               (car (cdr (nth cnt cPtList)))
            )
          (setq belowGrade "YES")
        )
        (setq cnt (+ cnt 1))
      )




      (if (= belowGrade nil)
        (progn
          (setq ptList (append upPtList sPtList))
          (setq ptList (append ptList downPtList))
        )
      )

      (if (= belowGrade "YES")
        (progn

          ; create while loop to go through each item in selection set
          (setq cnt 0
                belowGradeList (list)
                flag nil
          )
          (while (< cnt (length sPtList))
            (if (> (car (cdr (nth cnt sPtList)))
                   (car (cdr (nth cnt cPtList)))
                )

              ; if sand is below grade, select chamber pt
              (progn
                ; if the first one is below slope, add special starting point
                (if (= cnt 0)
                  (progn
                    (setq int (slopeCutInt
                                (nth 0 cPtList)
                                (polar (nth 0 cPtList) upLeft 999)
                                upPt
                                upPt1
                                upPt2
                              )
                    )     ; if - special case
                    (setq belowGradeList (append belowGradeList (list int)))
                  )
                )

                ; add normal point
                (setq belowGradeList (append belowGradeList (list (nth cnt cPtList))))


                ; if the last row is below slope, add special starting point
                (if (= cnt (- (length sPtList) 1))
                  (progn
                    (setq int (slopeCutInt
                                (nth cnt cPtList)
                                (polar (nth cnt cPtList) upRight 999)
                                downPt
                                downPt1
                                downPt2
                              )
                    )     ; if - special case
                    (setq belowGradeList (append belowGradeList (list int)))
                  )
                )

              )

              ; if sand is above or at grade, select slope pt
              (progn

                ; if first time and not first pnt then we have just jumped and intersecting point
                (if (and (= flag nil) (/= cnt 0))
                  (progn  ; if transitional point, add intersection and regular slope point
                    (setq flag cnt)
                    (setq int (lineIntersection
                                (nth (- cnt 1) sPtList)
                                (nth cnt sPtList)
                                (nth (- cnt 1) cPtList)
                                (nth cnt cPtList)
                              )
                    )
                    (setq belowGradeList (append belowGradeList (list int)))
                    (setq belowGradeList (append belowGradeList (list (nth cnt sPtList))))
                  )
                  (progn  ; if standard point
                    (setq belowGradeList (append belowGradeList (list (nth cnt sPtList))))
                  )
                )
              )
            )
            (setq cnt (1+ cnt))
          )               ; end while


;;;;; it gets complicated with additional length not accounted for, the 
          (setq ptList (list))
          (setq ptList (append upPtList belowGradeList))
          (setq ptList (append ptList downPtList))
        )
      )


      (setq arrowPoint (polar row_end down 9))
      (SETQ textPoint (POLAR arrowPoint 5.0 (* (/ profileScale 5.0) 35)))

      (if (> (length ptList) 0)
        (progn
          (drawPolyLine ptList)
          (setq ss (ssadd (entlast)))
          (modifySS ss 62 lineColor)
          (command "draworder" ss "" "B")

          (add-scarh (entlast))
          (setq ss (ssadd (entlast)))
          (modifySS ss 62 hatchColor)
          (command "draworder" ss "" "B")
        )
      )


    )
  )
;;;--- END VERSION FOR ELJEN





;;;--- START VERSION FOR STANDARD CHAMBERS
  (if (AND (/= sys_type "STONE")
           (/= sys_type "4x8_CONCRETE_CLUSTER")
           (/= sys_type "8x4_CONCRETE_CLUSTER")
           (/= sys_type "ELJEN")
           (/= sys_type "ELJEN_INVERTED")
           (/= sys_type "ENVIRO")
      )
    (progn

      (setq ptList (list)
            sPtList (list)
            cPtList (list)
            cnt 1
      )

      (foreach item tempChamberPtList

        ; slope intersections points associated with each trench 
        ; the first chamber left point intersects outside the foot print of the system
        ; to simplify the code i assume no one will put in a crazy close number 
        (if (= cnt 1)
          (setq sPtLeft (lineIntersection upPt1 upPt (car item) (polar (car item) upLeft 999)))   ; if - special case
          (setq sPtLeft (lineIntersection upPt downPt (car item) (polar (car item) upLeft 999)))  ; else - normal
        )

        ; the last chamber right point intersection outside the footprint of the system
        ; to simplify the code i assume no one will put in a crazy close number 
        (if (= cnt (length tempChamberPtList))
          (setq sPtRight (lineIntersection downPt downPt1 (polar (car item) right chamber_w) (polar (polar (car item) right chamber_w) upRight 999)))  ; if - special case
          (setq sPtRight (lineIntersection upPt downPt (polar (car item) right chamber_w) (polar (polar (car item) right chamber_w) upRight 999)))     ; else - normal
        )

        (setq cPtLeft (car item))
        (setq cPtRight (polar (car item) right chamber_W))

        (setq ptList (append ptList (list sPtLeft)))
        (setq ptList (append ptList (list cPtLeft)))
        (setq ptList (append ptList (list cPtRight)))
        (setq ptList (append ptList (list sPtRight)))

        (setq sPtList (append sPtList (list sPtLeft)))
        (setq sPtList (append sPtList (list sPtRight)))

        (setq cPtList (append cPtList (list cPtLeft)))
        (setq cPtList (append cPtList (list cPtRight)))

        (setq cnt (1+ cnt))
      )


      ; create while loop to go through each item in selection set
      (setq cnt 0
            belowGradeList (list)
      )
      (while (< cnt (length sPtList))
        (if (> (car (cdr (nth cnt sPtList))) (car (cdr (nth cnt cPtList))))
          (setq belowGradeList (append belowGradeList (list (nth cnt sPtList))))
        )
        (setq cnt (1+ cnt))
      )      ; end while

      (setq cnt 0
            ptList (list)
      )

      ; if want to draw TRENCH cuts for chambers - typical for high capacity plastic chambers
      (if (= type "TRENCH")
        (progn
          ; now add the chamber points and slope points in order	
          (while (< cnt (length belowGradeList))
            (if (= (oddOrEven cnt) "EVEN")
              (progn
                (setq ptList (append ptList (list (nth cnt belowGradeList))))
                (setq ptList (append ptList (list (nth cnt cPtList))))
              )
              (progn
                (setq ptList (append ptList (list (nth cnt cPtList))))
                (setq ptList (append ptList (list (nth cnt belowGradeList))))
              )
            )
            (setq cnt (1+ cnt))
          )  ; end while


          ; this is true if there are odd number of points and the last point need to be added because half of a chamber is below grade / half above
          (if (= (oddOrEven (length belowGradeList)) "ODD")
            (progn
              (setq lastPt (nth (- (length belowGradeList) 1) cPtList))
              (setq lastPt (lineIntersection lastPt (polar lastPt right chamber_W) upPt downPt))
              (setq ptList (append ptList (list lastPt)))
            )
          )

          ; if last chamber right bottom point is above slope then add downPt
          (if (>= (cadr (last cptlist)) (cadr downPt))
            (setq ptList (append ptList (list downPt)))
          )

          ;;;; now ptList has intersections below the chambers, upslope and downslope fill extensions need to be added in the appropriate order
          (setq ptList (append upPtList ptList))
          (setq ptList (append ptList downPtList))
        )
      )

      ; if want to draw one cut connecting bottom of chambers
      (if (= type "PLATFORM")
        (progn
          (setq ptList (append ptList (list (nth 0 sPtList))))
          (foreach item belowGradeList
            (setq ptList (append ptList (list (nth cnt cPtList))))
            (setq cnt (1+ cnt))
          )

          ; this is true if there are odd number of points and the last point need to be added because half of a chamber is below grade / half above
          (if (= (oddOrEven (length belowGradeList)) "ODD")
            (progn
              (setq lastPt (nth (- (length belowGradeList) 1) cPtList))
              (setq lastPt (lineIntersection lastPt (polar lastPt right chamber_W) upPt downPt))
              (setq ptList (append ptList (list lastPt)))
            )
          )

          ; if last chamber right bottom point is above slope then add downPt
          (if (>= (cadr (last cptlist)) (cadr downPt))
            (setq ptList (append ptList (list downPt)))
          )

          ; if last chamber right bottom point is below the slope then add slightly sloped line           
          (if (< (cadr (last cPtList)) (cadr downPt))
            (setq ptList (append ptList (list (lineIntersection downPt downPt1 (last cPtList) (polar (last cPtList) upRight 999)))))
          )

          ;;;; now ptList has intersections below the chambers, upslope and downslope fill extensions need to be added in the appropriate order
          (setq ptList (append upPtList ptList))
          (setq ptList (append ptList downPtList))
        )
      )


      (setq arrowPoint (polar (polar row_end down 3) left (/ chamber_w 3)))
      (SETQ textPoint (POLAR arrowPoint 5.0 (* (/ profileScale 5.0) 35)))

      (if (> (length ptList) 0)
        (progn
          (drawPolyLine ptList)
          (setq ss (ssadd (entlast)))
          (modifySS ss 62 lineColor)
          (command "draworder" ss "" "B")

          (add-scarh (entlast))
          (setq ss (ssadd (entlast)))
          (modifySS ss 62 hatchColor)
          (command "draworder" ss "" "B")
        )
      )

    )
  )
;;;--- END VERSION FOR STANDARD CHAMBERS

  (SETQ DIMASZ_DEF (GETVAR "DIMASZ"))
  (SETQ DIMTXT_DEF (GETVAR "DIMTXT"))
  (SETVAR "DIMTXT" profileTextSize)
  (SETVAR "DIMASZ" (* profileTextSize scaleArrowSize))

  (COMMAND "LEADER" arrowPoint textPoint "" scarNote "")

  (setq ss (ssadd (entlast)))
  (modifySS ss 41 (* profilescale 27.2)) ; changed mText Width

  (SETVAR "DIMASZ" DIMASZ_DEF)
  (SETVAR "DIMTXT" DIMTXT_DEF)

  (command "UNDO" "E")
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun SlopeCutInt (pt1 pt2 ptA ptB ptC)
  (setq int nil)
  (if (inters pt1 pt2 ptA ptB) (setq int (inters pt1 pt2 ptA ptB)))
  (if (inters pt1 pt2 ptB ptC) (setq int (inters pt1 pt2 ptB ptC)))
  (if (inters pt1 pt2 ptC (polar ptC (angle ptB ptC) 999999)) (setq int (inters pt1 pt2 ptC (polar ptC (angle ptB ptC) 999999))))
  int
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; return odd or even for an integer
(defun oddOrEven (num)
  (if (= (fix (/ num 2.0)) (/ num 2.0))
    (setq val "EVEN")
    (setq val "ODD")
  )
  val
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; return the y value for a point on a line
(defun drawPolyLine (ptList)
  (command "PLINE")
  (foreach item ptList
    (command item)
  )
  (command "")
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


