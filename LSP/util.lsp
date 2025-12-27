
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; | ----------------------------------------------------------------------------
;; | ST_str2lst
;; | ----------------------------------------------------------------------------
;; | Function : Convert String to List
;; | Argument : 'strng'  - String to convert
;; |            'delim'  - Delimiter to use to break down the string to a list
;; | Return   : The converted list
;; | Updated  : February 5, 1999
;; | e-mail   : rakesh.rao@4d-technologies.com 
;; | Web      : www.4d-technologies.com
;; | ----------------------------------------------------------------------------


(defun ST_str2lst (strng delim / stlist st pos)

  (setq
    strng (strcat strng delim)
    pos T
    stlist '()
  )
  (while pos
    (setq pos (ST_fsrch strng delim))
    (if pos
      (setq
        st (substr strng 1 (1- pos))
        stlist (append stlist (list st))
        strng (substr strng (1+ pos))
      )
    )
  )
  stlist
)
;; | ----------------------------------------------------------------------------
;; | ST_fsrch
;; | ----------------------------------------------------------------------------
;; | Function : Search for the first location of a character in a string
;; | Argument : 'str'   - String to search
;; |            'delim' - Character to search
;; | Return   : The numeric first position from start where the specified character
;; |            occurs
;; | Updated  : February 5, 1999
;; | e-mail   : rakesh.rao@4d-technologies.com 
;; | Web      : www.4d-technologies.com
;; | ----------------------------------------------------------------------------


(defun ST_fsrch (str delim / len pos more cnt ch)
  (setq
    len (strlen str)
    pos nil
    more T
  )
  (if (> len 0)
    (progn
      (setq cnt 1)
      (while (and more (<= cnt len))
        (setq
          ch (substr str 1 1)
          str (substr str 2)
        )
        (if (= ch delim)
          (setq
            pos cnt
            more nil
          )
        )
        (setq cnt (1+ cnt))
      )
    )
  )
  pos
)



;;;;;;;;;;;;;;;;;;





;;; used for creating elevation table
;;; a list of numbers will be converted to list of strings with zeros truncated
;;; each number is increased by "num" - so chamber_H can be pasted to the funciton as well.
(defun convertListNumbersToText (lst / cnt newList)
  (setq cnt 0 newList (list))
  (foreach item lst
    (setq newList (append newList (list (dropTrailingZero item))))
    (setq cnt (+ cnt 1))
  )
  newList
)

; return a number as text with trailing zeros removed when at one decimal point
(defun dropTrailingZero (num / ret)
  (if (= (/ (fix num) num) 1)(setq ret (RTOS num 2 0)))
  (if (/= (/ (fix num) num) 1)(setq ret (RTOS num 2 1)))
  ret
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:openSupportPath ()
  (ExplorerFolder (strcat (getvar "roamablerootprefix") "support\\"))
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; return mid point between two points
(defun retMidPt (p1 p2)
  (polar p1 (angle p1 p2) (/ (distance p1 p2) 2.0))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; find occurent of pattern
(defun replaceString (str pattern newTxt)
  ; set wildcards around pattern for checking presence
  ; if present then do find replace, check again, until not match
  (setq searchStr (strcat "*" pattern "*"))
  (while (wcmatch str searchStr)
    (setq str (vl-string-subst newTxt pattern str 0))
  )
  str
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; searches a list of strings for a string and returns "T" if found 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun searchListStr (lst str / inList item)
  (setq inList nil)
  (foreach item lst (if (= item str) (setq inList "T")))
  inList
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun modifyAssocListItem (lst code newValue / cnt)

   ; create while loop to go through each item in selection set
  (setq cnt 0)
  (while (< cnt (length lst))

    (if (/= (assoc code lst) nil)
      (setq lst (subst (cons code newValue) (assoc code lst) lst))
    )

    (if (= (assoc code lst) nil)
      (setq lst (append lst (list (cons code newValue))))
    )

    (setq cnt (1+ cnt))

  )
   ; end while

  lst
)
; end modifyAssocListItem
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Hatch Pattern List - Lee Mac
;; Returns a list of all hatch patterns defined in all PAT files found in the
;; support file search paths & working directory.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hatchPatternList (/ lst)

  
  ;;SUBROUTINE START;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun unique (lst)
    (if lst (cons (car lst) (unique (vl-remove (car lst) (cdr lst)))))
  )

  (defun fixdir (str)
    (vl-string-right-trim "\\" (vl-string-translate "/" "\\" str))
  )

  (defun parsesupportpaths (str / pos)
    (if (setq pos (vl-string-position 59 str))
      (vl-remove "" (cons (substr str 1 pos) (parsesupportpaths (substr str (+ pos 2)))))
      (list str)
    )
  )

  (defun parsepatfile (fn / fd hp ln)
    (if
      (and
        (setq fn (findfile fn))
        (setq fd (open fn "r"))
      )
      (progn
        (while (setq ln (read-line fd))
          (if (wcmatch ln "`**`,*")
            (setq hp (cons (strcase (substr ln 2 (1- (vl-string-position 44 ln)))) hp))
          )
        )
        (close fd)
        (reverse hp)
      )
    )
  )
  ;;end subs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  
    (foreach dir (cons (getvar 'dwgprefix) (parsesupportpaths (getenv "ACAD")))
      (foreach pat (vl-directory-files dir "*.pat" 1)
        (setq lst (cons (parsepatfile (strcat (fixdir dir) "\\" pat)) lst))
      )
    )
    (vl-sort (unique (apply 'append lst)) '<)
  
  
)



(defun gravelHatchName (/ lst found item)
  (setq lst (hatchPatternList))
  (foreach item lst
    (if (= (strcase (substr item 1 4)) "GRAV")
      (setq found item)
    )
  )
  found
)              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;sets block insert unit vars for compatablity with vertical ACAD application
(defun setBlockUnitsVars ()
  (SETQ DEF_INSUNITS (GETVAR "INSUNITS"))
  (SETVAR "INSUNITS" 0)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;reset block insert unit vars that were originally modified by (setBlockUnitVars)
(defun resetBlockUnitsVars ()
  (SETVAR "INSUNITS" DEF_INSUNITS)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun c:pj ()
  (PolylineJoin (ssget '((0 . "LINE,ARC,*POLYLINE"))))
)

;;  PolylineJoin.lsp [command name: PJ]
;;  Joins viable objects [Lines/Arcs/LWPolylines/not-splined-or-fitted 2DPolylines]
;;    into connected LWPolylines.
;;  Rejects splined/fitted 2DPolylines, [joining them to other objects removes their
;;    spline/fit curvature] and 3DPolylines [can't be joined].
;;  If one viable object is selected, joins all other possible objects [contiguous at ends to
;;    it and to each other] to it, into one LWPolyline.
;;  If multiple viable objects are selected, joins them into as many LWPolylines as
;;    appropriate, not including anything contiguous at ends that was not selected.
;;  Leaves selected Lines/Arcs that are not contiguous to anything else as Lines/Arcs,
;;    not converted into one-segment Polylines.
;;  Concept from c:pljoin by beaufordt on AutoCAD Customization Discussion Group
;;  Streamlined by Kent Cooper, June 2011; expanded capabilities July 2012
;;  Last edited 16 July 2012
; version modified by marcotte so that a sset is sent
(defun PolylineJoin (pjss / *error* cmde peac nextent pjinit inc edata pjent)


;  (princ "\nTo join objects into Polyline(s) [pick 1 to join all possible to it],")
  (setq
;    pjss (ssget '((0 . "LINE,ARC,*POLYLINE")))
    cmde (getvar 'cmdecho)
    peac (getvar 'peditaccept)
    nextent (entlast)  ; starting point for checking new entities
  )  ; setq
  (repeat (setq pjinit (sslength pjss) inc pjinit)  ; PJ INITial-selection quantity & incrementer
    (if
      (and
        (=
          (cdr (assoc 0 (setq edata (entget (setq pjent (ssname pjss (setq inc (1- inc))))))))
          "POLYLINE"  ; 2D "heavy" or 3D Polyline
        )  ; =
           (or
             (= (cdr (assoc 100 (reverse edata))) "AcDb3dPolyline")  ; 3D
             (member (boole 1 6 (cdr (assoc 70 edata))) '(2 4))      ; splined or fitted 2D
           )  ; or
      )       ; and
      (ssdel pjent pjss)  ; remove 3D, splined/fitted 2D from set
    )  ; if
  )    ; repeat
  (setvar 'cmdecho 0)
  (command "_.undo" "_begin")
  (setvar 'peditaccept 1)
  (setvar 'plinetype 2)  ; [just in case; assumes no desire to save and set back if different]
  (if pjss  ; selected qualifying object(s)
    (cond   ; then
           ((= pjinit (sslength pjss) 1)  ; selected only one, and it qualifies
               (command "_.pedit" pjss "_join" "_all" "" "")  ; join everything possible to it
           )  ; single-selection condition
      ((> (sslength pjss) 1)  ; more than one qualifying object
          (command "_.pedit" "_multiple" pjss "" "_join" "0.0" "")
      )  ; multiple qualifying condition
      ((prompt "\nSingle object not viable, or <= 1 of multiple selection viable."))
    )    ; cond
    (prompt "\nNothing viable selected.")
  )      ; outer if
  (while (setq nextent (entnext nextent))  ; start with first newly-created Pline, if any
    (if        ; revert any un-joined Lines/Arcs back from Pline conversion
         (and  ; newly-created single-segment Pline from unconnected Line/Arc
               (= (cdr (assoc 90 (entget nextent))) 2)
               (not (vlax-curve-isClosed nextent))
         )     ; and
      (command "_.explode" nextent)
    )          ; if
  )            ; while
  (setvar 'peditaccept peac)
  (command "_.undo" "_end")
  (setvar 'cmdecho cmde)
  (princ)
  
  
    (defun *error* (errmsg)
    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
      (princ (strcat "\nError: " errmsg))
    )  ; if
    (setvar 'peditaccept peac)
    (command "_.undo" "_end")
    (setvar 'cmdecho cmde)
    (princ)
  )    ; defun - *error*
  
  
)              ; defun





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Converts selected text or mtext strings sent to this function to all upper case characters
(defun ALLCAPS (ss / n txt strg)
  (setq n (sslength ss))  ; Count items.
  (while (> n 0)
    (setq n (1- n))
    (setq obj (vlax-ename->vla-object (ssname ss n)))
    (setq txt (STRCASE (vla-get-textstring obj)))
    (vla-put-textstring obj txt)
  )
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;  Prints all xData for Selected Entity - CAB 01.17.08 ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:GetXData (/ ent dic result massoc listdictionaries)
  (if (setq ent (car (entsel "\nSelect object to get dictionary.")))
    (print (assoc -3 (entget ent '( "*"))))
  )
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; ---     Attachs xData to ent   ---;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;; (-3 (appName (dataType . textTag)))---;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xDataTag (ent textTag dataType appName / e e1)

  (setq e (entget ent))

    ;if not register, register it
  (if (not (tblsearch "APPID" appName))
    (regapp appName)
  )

    ;create an list of xdata list
  (setq e1 (list (list -3 (list appName (cons dataType textTag)))))

    ;append to to the main list
  (setq e (append e e1))

    ;modify the entity
  (entmod e)

  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun toggle-OSNAP-Vars-OFF ()

  (setq $OSMODE_DEF (getvar "OSMODE")
        $OSNAPCOORD_DEF (getvar "OSNAPCOORD")
  )

  (setvar "OSMODE" 0)
  (setvar "OSNAPCOORD" 1)
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun toggle-OSNAP-Vars-RESET ()
  (setvar "OSMODE" $OSMODE_DEF)
  (setvar "OSNAPCOORD" $OSNAPCOORD_DEF)
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;(ACADbrowser (strcat $septicadPathPDF "ELEVATIONS.pdf"))
(defun ACADbrowser (url)
  (setq url (strcat "\"" (vl-string-translate "/" "\\" url) "\""))
  (vl-some '(lambda (exe) (and (findfile exe) (startapp exe url)))
           (list
             (strcat (getenv "programfiles") "\\Mozilla Firefox\\firefox.exe")
             (strcat (getenv "localappdata") "\\Google\\Chrome\\Application\\chrome.exe")
             (strcat (getenv "programfiles") "\\Internet Explorer\\iexplore.exe")
           )
  )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun modifySS (sset1 code newValue / cnt ent1)

   ; create while loop to go through each item in selection set
  (setq cnt 0)
  (while (< cnt (sslength sset1))

    (setq ent1 (entget (ssname sset1 cnt)))

    (if (/= (assoc code ent1) nil)
      (setq ent1 (subst (cons code newValue) (assoc code ent1) ent1))
    )

    (if (= (assoc code ent1) nil)
      (setq ent1 (append ent1 (list (cons code newValue))))
    )

    ; make changes to ent1
    (entmod ent1)

    (setq cnt (1+ cnt))

  )
   ; end while


)
; end modifySS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;    START     ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; VPort LOCK-UNLOCK Functions IntelliCAD & AutoCAD ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vPortLock (flag / sset1 cnt ent1)
  (vl-load-com)
    ; flag
	; if flag = "VERBOSE" will princ at function end
	; if flag /= "VERBOSE" silent at function end


    ;  create selection set of all VIEWPORTS
  (if (setq sset1 (ssget "x" (list (cons 0 "VIEWPORT"))))
    (progn
      (setvar "CMDECHO" 0)

      ; UNLOCK the LAYER with the VIEWPORTS
      (if (Tblsearch "LAYER" "FORM_HHE-200")
        (command "-layer" "UNLOCK" "FORM_HHE-200" "")
      )

      ;;;; if BCAD 7
      (if (= (strcase (getvar "PRODUCT")) "BRICSCAD")
        (progn
          ;;;;;;;;;;;;START BCAD 7
          (if (= (substr (getvar "_VERNUM") 1 1) "7")
            (progn
              ; create while loop to go through each item in selection set
              (setq cnt 0)
              (while (< cnt (sslength sset1))
                (setq ent1 (entget (ssname sset1 cnt)))
                (setq ent1 (subst (cons 90 (boole 7 16384 (cdr (assoc 90 ent1)))) (assoc 90 ent1) ent1))
                (entmod ent1)
                (setq cnt (1+ cnt))
              )
              ; end while
            )
          )
          ;;;;;;;;;;;;END BCAD 7

          ;;;;;;;;;;;;START BCAD =8
          (if (= (atoi (substr (getvar "_VERNUM") 1 1)) 8)
            (progn
              (setq cnt 0)
              (while (< cnt (sslength sset1))
                (setq ent1 (ssname sset1 cnt))
                (setq obj1 (vlax-ename->vla-object ent1))
                (vlax-put-property obj1 "DisplayLocked" T)
                (setq cnt (1+ cnt))
              )
              ; end while
            )
          )
          ;;;;;;;;END BCAD 8

          ;;;;;;;;;;;;START BCAD 9
          (if (>= (atoi (substr (getvar "_VERNUM") 1 2)) 9)
            (command "-vports" "LOCK" "ON" sset1 "")
          )
          ;;;;;;;;END BCAD 9

        )
      )
      ;;;;;;;;;END BCAD

      ;;;;;;;;;START ACAD
      (if (= (strcase (getvar "PRODUCT")) "AUTOCAD")
        (command "-vports" "LOCK" "ON" sset1 "")
      )
      ;;;;;;;;END ACAD

      (if (Tblsearch "LAYER" "FORM_HHE-200")
        (command "-layer" "LOCK" "FORM_HHE-200" "")
      )

      (if (= flag "VERBOSE")
        (princ "\nThe scale in all viewports are now locked - Use command VPORTUNLOCK to unlock all viewports.")
      )
      (princ)
    )
  )

)
;;-- end function

(defun vPortUnlock (flag / sset1 cnt ent1)
  (vl-load-com)

    ; flag
	; if flag = "VERBOSE" will princ at function end
	; if flag /= "VERBOSE" silent at function end

    ;  create selection set of all VIEWPORTS
  (if (setq sset1 (ssget "x" (list (cons 0 "VIEWPORT"))))
    (progn
      (setvar "CMDECHO" 0)

      (if (Tblsearch "LAYER" "FORM_HHE-200")
        (command "-layer" "UNLOCK" "FORM_HHE-200" "")
      )

      ;;;; if BCAD 7
      (if (= (strcase (getvar "PRODUCT")) "BRICSCAD")
        (progn
          ;;;;;;;;;;;;START BCAD 7
          (if (= (substr (getvar "_VERNUM") 1 1) "7")
            (progn
              ; create while loop to go through each item in selection set
              (setq cnt 0)
              (while (< cnt (sslength sset1))
                (setq ent1 (entget (ssname sset1 cnt)))
                (setq ent1 (subst (cons 90 (- (cdr (assoc 90 ent1)) 16384)) (assoc 90 ent1) ent1))
                (entmod ent1)
                (setq cnt (1+ cnt))
              )
              ; end while
            )
          )
          ;;;;;;;;;;;;END BCAD 7

          ;;;;;;;;;;;;START BCAD =8
          (if (= (atoi (substr (getvar "_VERNUM") 1 1)) 8)
            (progn
              (setq cnt 0)
              (while (< cnt (sslength sset1))
                (setq ent1 (ssname sset1 cnt))
                (setq obj1 (vlax-ename->vla-object ent1))
                (vlax-put-property obj1 "DisplayLocked" F)
                (setq cnt (1+ cnt))
              )
              ; end while
            )
          )
          ;;;;;;;;END BCAD 8

          ;;;;;;;;;;;;START BCAD 9
          (if (>= (atoi (substr (getvar "_VERNUM") 1 2)) 9)
            (command "-vports" "LOCK" "OFF" sset1 "")
          )
          ;;;;;;;;END BCAD 9
        )
      )
      ;;;;;;;;;END BCAD

      ;;;;;;;;START ACAD
      (if (= (strcase (getvar "PRODUCT")) "AUTOCAD")
        (command "-vports" "LOCK" "OFF" sset1 "")
      )
      ;;;;;;;;END ACAD

      (if (Tblsearch "LAYER" "FORM_HHE-200")
        (command "-layer" "LOCK" "FORM_HHE-200" "")
      )

      (if (= flag "VERBOSE")
        (princ "\nThe scale in all viewports are now unlocked - Use command VPORTLOCK to lock all viewports.")
      )

      (princ)
    )
  )


)
;;-- end function
;;;;;;;;;;;;;;;;;;;;;;;;;;     END      ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; VPort LOCK-UNLOCK Functions IntelliCAD & AutoCAD ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
