;; functions that set global variables depending on system type are defined here 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun RUN-DIM_SYSTEM ()

  (COND
    ((= SYS_TYPE "ENVIRO") (DIM_ENVIRO))
  )
  (COND
    ((= SYS_TYPE "QUICK4_HI_CAP") (DIM_QUICK4_HI_CAP))
  )
  (COND
    ((= SYS_TYPE "ADS_HI_CAP") (DIM_ADS_HICAP))
  )
  (COND
    ((= SYS_TYPE "ELJEN") (DIM_ELJEN))
  )
  (COND
    ((= SYS_TYPE "ELJEN_INVERTED") (DIM_ELJEN_INVERT))
  )
  (COND
    ((= SYS_TYPE "ADS_BIO2") (DIM_ADS_BIO2))
  )
  (COND
    ((= SYS_TYPE "QUICK4_EQ24") (DIM_QUICK4_EQ24))
  )
  (COND
    ((= sys_type "STONE") (DIM_STONE))
  )
  (COND
    ((= SYS_TYPE "4x8_CONCRETE_CLUSTER") (DIM_4x8_CONCRETE_CLUSTER))
  )
  (COND
    ((= SYS_TYPE "8x4_CONCRETE_CLUSTER") (DIM_8x4_CONCRETE_CLUSTER))
  )
  (COND
    ((= SYS_TYPE "MOUNDBUSTER") (DIM_MOUNDBUSTER))
  )
  (COND
    ((= SYS_TYPE "4x8_CONCRETE_TRENCH") (DIM_4x8_CONCRETE_TRENCH))
  )
  (COND
    ((= SYS_TYPE "8x4_CONCRETE_TRENCH") (DIM_8x4_CONCRETE_TRENCH))
  )

  (COND
    ((= sys_type "QUICK4_STD") (DIM_QUICK4_STD))
  )
  (COND
    ((= sys_type "ARC18") (DIM_ARC18))
  )
  (COND
    ((= sys_type "ARC36HC") (DIM_ARC36HC))
  )
  (COND
    ((= sys_type "ARC36") (DIM_ARC36))
  )
  (COND
    ((= sys_type "ADS_STD") (DIM_ADS_STD))
  )
  (COND
    ((= sys_type "INF_STD") (DIM_INF_STD))
  )
  (COND
    ((= sys_type "INF_HI_CAP") (DIM_INF_HI_CAP))
  )
  (COND
    ((= sys_type "INF_EQ24") (DIM_INF_EQ24))
  )
  (COND
    ((= sys_type "TRENCH2") (DIM_TRENCH2))
  )
  (COND
    ((= sys_type "TRENCH3") (DIM_TRENCH3))
  )

  (COND
    ((= sys_type "ARC24") (DIM_ARC24))
  )
  (COND
    ((= sys_type "Q4_PLUS_EQ_36_LP") (DIM_Q4_PLUS_EQ_36_LP))
  )
  (COND
    ((= sys_type "Q4_PLUS_HC") (DIM_Q4_PLUS_HC))
  )
  (COND
    ((= sys_type "Q4_PLUS_STD") (DIM_Q4_PLUS_STD))
  )
  (COND
    ((= sys_type "Q4_PLUS_STD_LP") (DIM_Q4_PLUS_STD_LP))
  )
  (COND
    ((= sys_type "QUICK4_EQ24LP") (DIM_QUICK4_EQ24LP))
  )
  (COND
    ((= sys_type "QUICK4_EQ36") (DIM_QUICK4_EQ36))
  )


)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun DIM_ENVIRO ()
       (SETQ CHAMBER_H 12.0)
       (SETQ CHAMBER_W 12.0)

  ; total length of system in inches
       (setq sys_L (* num_units 12.0))
  ; total width of system in inches
       (setq sys_W 0)

  (if (/= isShelf "YES")
    (setq sys_W (+ (* (- num_rows 1) row_sep) (* num_rows chamber_W)))
  )

  (if (= isShelf "YES")
    (progn
      (foreach shelf shelfList
        (setq sys_W (+ sys_W (* shelf chamber_W) (* (- shelf 1) row_sep)))
      )
      (setq sys_W (+ sys_W (* (- (length shelfList) 1) shelf_sep)))
    )
  )
  ; height of invert above base of system  
       (setq invertHeight 7)
)

















(defun DIM_4x8_CONCRETE_CLUSTER ()
       (SETQ CHAMBER_H 13.0)
       (SETQ CHAMBER_W 48.0)
       (SETQ CHAMBER_L 96.0)

  ; SYSTEM WIDTH AND LENGTH ARE CALCULATED
       (setq row_sep 0.0)
       (setq sys_L (+ (* stonebeside 2.0) (* num_units chamber_L)))
       (setq sys_W (+ (* stonebeside 2.0) (* (- num_rows 1) row_sep) (* num_rows chamber_W)))
  ; height of invert above base of system  
       (setq invertHeight 7)
)

(defun DIM_8x4_CONCRETE_CLUSTER ()
       (SETQ CHAMBER_H 13.0)
       (SETQ CHAMBER_W 96.0)
       (SETQ CHAMBER_L 48.0)

  ; SYSTEM WIDTH AND LENGTH ARE CALCULATED
       (setq row_sep 0.0)
       (setq sys_L (+ (* stonebeside 2.0) (* num_units chamber_L)))
       (setq sys_W (+ (* stonebeside 2.0) (* (- num_rows 1) row_sep) (* num_rows chamber_W)))
  ; height of invert above base of system  
       (setq invertHeight 7)
)


(defun DIM_4x8_CONCRETE_TRENCH ()
       (SETQ CHAMBER_H 13.0)
       (SETQ CHAMBER_W 72.0)
       (SETQ CHAMBER_L 96.0)

  ; SYSTEM WIDTH AND LENGTH ARE CALCULATED
       (setq sys_L (+ 24 (* num_units chamber_L)))
       (setq sys_W 0)

  (if (/= isShelf "YES")
    (setq sys_W (+ (* (- num_rows 1) row_sep) (* num_rows chamber_W)))
  )

  (if (= isShelf "YES")
    (progn
      (foreach shelf shelfList
        (setq sys_W (+ sys_W (* shelf chamber_W) (* (- shelf 1) row_sep)))
      )
      (setq sys_W (+ sys_W (* (- (length shelfList) 1) shelf_sep)))
    )
  )

  ; height of invert above base of system  
       (setq invertHeight 7)
)

(defun DIM_8x4_CONCRETE_TRENCH ()
       (SETQ CHAMBER_H 13.0)
       (SETQ CHAMBER_W 120.0)
       (SETQ CHAMBER_L 48.0)

  ; SYSTEM WIDTH AND LENGTH ARE CALCULATED
       (setq sys_L (+ 24 (* num_units chamber_L)))
       (setq sys_W 0)
  (if (/= isShelf "YES")
    (setq sys_W (+ (* (- num_rows 1) row_sep) (* num_rows chamber_W)))
  )

  (if (= isShelf "YES")
    (progn
      (foreach shelf shelfList
        (setq sys_W (+ sys_W (* shelf chamber_W) (* (- shelf 1) row_sep)))
      )
      (setq sys_W (+ sys_W (* (- (length shelfList) 1) shelf_sep)))
    )
  )

  ; height of invert above base of system  
       (setq invertHeight 7)
)



(defun DIM_STONE ()
  ; height of stone
       (SETQ CHAMBER_H 12.0)
  ; dummy variables chamber_W and chamber_L
       (SETQ CHAMBER_W 0.0)
       (SETQ CHAMBER_L 0.0)

  ;;; pipe spacing 
       (setq pipeSpacing 60.0)
  ;;; shoulder width 
       (setq shStone 60.0)
  ; label in DCL changes - not exactly sure why?
       (setq sys_L (* num_units 12.0))
       (setq sys_W (* num_rows 12.0))
  ; height of invert above base of system  
       (setq invertHeight 7)
)


(defun DIM_ADS_HICAP ()
       (SETQ CHAMBER_H 16.0)
       (SETQ CHAMBER_W 36.0)
       (SETQ CHAMBER_L 76.0)

  ; SYSTEM WIDTH AND LENGTH ARE CALCULATED
       (setq sys_L (* num_units chamber_L))
       (setq sys_W 0)

  (if (/= isShelf "YES")
    (setq sys_W (+ (* (- num_rows 1) row_sep) (* num_rows chamber_W)))
  )

  (if (= isShelf "YES")
    (progn
      (foreach shelf shelfList
        (setq sys_W (+ sys_W (* shelf chamber_W) (* (- shelf 1) row_sep)))
      )
      (setq sys_W (+ sys_W (* (- (length shelfList) 1) shelf_sep)))
    )
  )
  ; height of invert above base of system  
       (setq invertHeight 11.3)
)






(defun DIM_ADS_BIO2 ()
       (SETQ CHAMBER_H 12.0)
       (SETQ CHAMBER_W 15.0)
       (SETQ CHAMBER_L 87.0)

  ; SYSTEM WIDTH AND LENGTH ARE CALCULATED
       (setq sys_L (* num_units chamber_L))
       (setq sys_W 0)

  (if (/= isShelf "YES")
    (setq sys_W (+ (* (- num_rows 1) row_sep) (* num_rows chamber_W)))
  )

  (if (= isShelf "YES")
    (progn
      (foreach shelf shelfList
        (setq sys_W (+ sys_W (* shelf chamber_W) (* (- shelf 1) row_sep)))
      )
      (setq sys_W (+ sys_W (* (- (length shelfList) 1) shelf_sep)))
    )
  )
  ; height of invert above base of system  
       (setq invertHeight 7.6)
)


(defun DIM_QUICK4_EQ24 ()
       (SETQ CHAMBER_H 11.0)
       (SETQ CHAMBER_W 16.0)
       (SETQ CHAMBER_L 48.0)

  ; SYSTEM WIDTH AND LENGTH ARE CALCULATED
       (setq sys_L (* num_units chamber_L))
       (setq sys_W 0)

  (if (/= isShelf "YES")
    (setq sys_W (+ (* (- num_rows 1) row_sep) (* num_rows chamber_W)))
  )

  (if (= isShelf "YES")
    (progn
      (foreach shelf shelfList
        (setq sys_W (+ sys_W (* shelf chamber_W) (* (- shelf 1) row_sep)))
      )
      (setq sys_W (+ sys_W (* (- (length shelfList) 1) shelf_sep)))
    )
  )


  ; height of invert above base of system  
       (setq invertHeight 6)
)

(defun DIM_MoundBuster ()
       (SETQ CHAMBER_H 4.0)
       (SETQ CHAMBER_W 4.0)

  ; system length is user input in Feet multiplied by 12.0 to get inches
       (setq sys_L (* num_units 12.0))

       (setq sys_W 0)
  (if (/= isShelf "YES")
    (setq sys_W (+ (* (- num_rows 1) row_sep) (* num_rows chamber_W)))
  )

  (if (= isShelf "YES")
    (progn
      (foreach shelf shelfList
        (setq sys_W (+ sys_W (* shelf chamber_W) (* (- shelf 1) row_sep)))
      )
      (setq sys_W (+ sys_W (* (- (length shelfList) 1) shelf_sep)))
    )
  )


;  (SETQ CHAMBER_L sys_L)
  ; height of invert above base of system  
       (setq invertHeight 0)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun DIM_QUICK4_STD ()
       (SETQ CHAMBER_H 12.0)
       (SETQ CHAMBER_W 36.0)
       (SETQ CHAMBER_L 48.0)

  ; SYSTEM WIDTH AND LENGTH ARE CALCULATED
       (setq sys_L (* num_units chamber_L))  ; total length of system
       (setq sys_W 0)

  (if (/= isShelf "YES")
    (setq sys_W (+ (* (- num_rows 1) row_sep) (* num_rows chamber_W)))
  )

  (if (= isShelf "YES")
    (progn
      (foreach shelf shelfList
        (setq sys_W (+ sys_W (* shelf chamber_W) (* (- shelf 1) row_sep)))
      )
      (setq sys_W (+ sys_W (* (- (length shelfList) 1) shelf_sep)))
    )
  )
  ; height of invert above base of system  
       (setq invertHeight 8)

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun DIM_ARC18 ()
       (SETQ CHAMBER_H 12.0)
       (SETQ CHAMBER_W 16.0)
       (SETQ CHAMBER_L 60.0)

  ; SYSTEM WIDTH AND LENGTH ARE CALCULATED
       (setq sys_L (* num_units chamber_L))  ; total length of system
       (setq sys_W 0)

  (if (/= isShelf "YES")
    (setq sys_W (+ (* (- num_rows 1) row_sep) (* num_rows chamber_W)))
  )

  (if (= isShelf "YES")
    (progn
      (foreach shelf shelfList
        (setq sys_W (+ sys_W (* shelf chamber_W) (* (- shelf 1) row_sep)))
      )
      (setq sys_W (+ sys_W (* (- (length shelfList) 1) shelf_sep)))
    )
  )
  ; height of invert above base of system  
       (setq invertHeight 6.1)

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun DIM_ARC36HC ()
       (SETQ CHAMBER_H 16.0)
       (SETQ CHAMBER_W 36.0)
       (SETQ CHAMBER_L 60.0)

  ; SYSTEM WIDTH AND LENGTH ARE CALCULATED
       (setq sys_L (* num_units chamber_L))  ; total length of system
       (setq sys_W 0)

  (if (/= isShelf "YES")
    (setq sys_W (+ (* (- num_rows 1) row_sep) (* num_rows chamber_W)))
  )

  (if (= isShelf "YES")
    (progn
      (foreach shelf shelfList
        (setq sys_W (+ sys_W (* shelf chamber_W) (* (- shelf 1) row_sep)))
      )
      (setq sys_W (+ sys_W (* (- (length shelfList) 1) shelf_sep)))
    )
  )
  ; height of invert above base of system  
       (setq invertHeight 10.3)

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun DIM_ARC36 ()
       (SETQ CHAMBER_H 12.0)
       (SETQ CHAMBER_W 36.0)
       (SETQ CHAMBER_L 60.0)

  ; SYSTEM WIDTH AND LENGTH ARE CALCULATED
       (setq sys_L (* num_units chamber_L))  ; total length of system
       (setq sys_W 0)

  (if (/= isShelf "YES")
    (setq sys_W (+ (* (- num_rows 1) row_sep) (* num_rows chamber_W)))
  )

  (if (= isShelf "YES")
    (progn
      (foreach shelf shelfList
        (setq sys_W (+ sys_W (* shelf chamber_W) (* (- shelf 1) row_sep)))
      )
      (setq sys_W (+ sys_W (* (- (length shelfList) 1) shelf_sep)))
    )
  )
  ; height of invert above base of system  
       (setq invertHeight 7.1)

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun DIM_ADS_STD ()
       (SETQ CHAMBER_H 11.0)
       (SETQ CHAMBER_W 36.0)
       (SETQ CHAMBER_L 76.0)

  ; SYSTEM WIDTH AND LENGTH ARE CALCULATED
       (setq sys_L (* num_units chamber_L))  ; total length of system
       (setq sys_W 0)

  (if (/= isShelf "YES")
    (setq sys_W (+ (* (- num_rows 1) row_sep) (* num_rows chamber_W)))
  )

  (if (= isShelf "YES")
    (progn
      (foreach shelf shelfList
        (setq sys_W (+ sys_W (* shelf chamber_W) (* (- shelf 1) row_sep)))
      )
      (setq sys_W (+ sys_W (* (- (length shelfList) 1) shelf_sep)))
    )
  )
  ; height of invert above base of system  
       (setq invertHeight 6.5)

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun DIM_INF_STD ()
       (SETQ CHAMBER_H 12.0)
       (SETQ CHAMBER_W 36.0)
       (SETQ CHAMBER_L 75.0)

  ; SYSTEM WIDTH AND LENGTH ARE CALCULATED
       (setq sys_L (* num_units chamber_L))  ; total length of system
       (setq sys_W 0)

  (if (/= isShelf "YES")
    (setq sys_W (+ (* (- num_rows 1) row_sep) (* num_rows chamber_W)))
  )

  (if (= isShelf "YES")
    (progn
      (foreach shelf shelfList
        (setq sys_W (+ sys_W (* shelf chamber_W) (* (- shelf 1) row_sep)))
      )
      (setq sys_W (+ sys_W (* (- (length shelfList) 1) shelf_sep)))
    )
  )
  ; height of invert above base of system  
       (setq invertHeight 6.5)

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun DIM_INF_HI_CAP ()
       (SETQ CHAMBER_H 16.0)
       (SETQ CHAMBER_W 36.0)
       (SETQ CHAMBER_L 75.0)

  ; SYSTEM WIDTH AND LENGTH ARE CALCULATED
       (setq sys_L (* num_units chamber_L))  ; total length of system
       (setq sys_W 0)

  (if (/= isShelf "YES")
    (setq sys_W (+ (* (- num_rows 1) row_sep) (* num_rows chamber_W)))
  )

  (if (= isShelf "YES")
    (progn
      (foreach shelf shelfList
        (setq sys_W (+ sys_W (* shelf chamber_W) (* (- shelf 1) row_sep)))
      )
      (setq sys_W (+ sys_W (* (- (length shelfList) 1) shelf_sep)))
    )
  )
  ; height of invert above base of system  
       (setq invertHeight 10.2)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun DIM_INF_EQ24 ()
       (SETQ CHAMBER_H 11.0)
       (SETQ CHAMBER_W 15.0)
       (SETQ CHAMBER_L 100.0)

  ; SYSTEM WIDTH AND LENGTH ARE CALCULATED
       (setq sys_L (* num_units chamber_L))  ; total length of system
       (setq sys_W 0)

  (if (/= isShelf "YES")
    (setq sys_W (+ (* (- num_rows 1) row_sep) (* num_rows chamber_W)))
  )

  (if (= isShelf "YES")
    (progn
      (foreach shelf shelfList
        (setq sys_W (+ sys_W (* shelf chamber_W) (* (- shelf 1) row_sep)))
      )
      (setq sys_W (+ sys_W (* (- (length shelfList) 1) shelf_sep)))
    )
  )
  ; height of invert above base of system  
       (setq invertHeight 6.5)

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun DIM_TRENCH2 ()
       (SETQ CHAMBER_H 17.0)
       (SETQ CHAMBER_W 24.0)

  ; total length of system in inches
       (setq sys_L (* num_units 12.0))
  ; total width of system in inches
       (setq sys_W 0)

  (if (/= isShelf "YES")
    (setq sys_W (+ (* (- num_rows 1) row_sep) (* num_rows chamber_W)))
  )

  (if (= isShelf "YES")
    (progn
      (foreach shelf shelfList
        (setq sys_W (+ sys_W (* shelf chamber_W) (* (- shelf 1) row_sep)))
      )
      (setq sys_W (+ sys_W (* (- (length shelfList) 1) shelf_sep)))
    )
  )
  ; height of invert above base of system  
       (setq invertHeight 12)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun DIM_TRENCH3 ()
       (SETQ CHAMBER_H 17.0)
       (SETQ CHAMBER_W 36.0)

  ; total length of system in inches
       (setq sys_L (* num_units 12.0))
  ; total width of system in inches
       (setq sys_W 0)

  (if (/= isShelf "YES")
    (setq sys_W (+ (* (- num_rows 1) row_sep) (* num_rows chamber_W)))
  )

  (if (= isShelf "YES")
    (progn
      (foreach shelf shelfList
        (setq sys_W (+ sys_W (* shelf chamber_W) (* (- shelf 1) row_sep)))
      )
      (setq sys_W (+ sys_W (* (- (length shelfList) 1) shelf_sep)))
    )
  )
  ; height of invert above base of system  
       (setq invertHeight 12)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun DIM_QUICK4_HI_CAP ()
       (SETQ CHAMBER_H 16.0)
       (SETQ CHAMBER_W 36.0)
       (SETQ CHAMBER_L 48.0)

  ; SYSTEM WIDTH AND LENGTH ARE CALCULATED
       (setq sys_L (* num_units chamber_L))  ; total length of system
       (setq sys_W 0)

  (if (/= isShelf "YES")
    (setq sys_W (+ (* (- num_rows 1) row_sep) (* num_rows chamber_W)))
  )

  (if (= isShelf "YES")
    (progn
      (foreach shelf shelfList
        (setq sys_W (+ sys_W (* shelf chamber_W) (* (- shelf 1) row_sep)))
      )
      (setq sys_W (+ sys_W (* (- (length shelfList) 1) shelf_sep)))
    )
  )
  ; height of invert above base of system  
       (setq invertHeight 11.5)

)


(defun DIM_QUICK4_EQ36 ()
  (SETQ CHAMBER_H 12.0)
  (SETQ CHAMBER_W 16.0)
  (SETQ CHAMBER_L 48.0)

  ; SYSTEM WIDTH AND LENGTH ARE CALCULATED
  (setq sys_L (* num_units chamber_L))
  (setq sys_W 0)

  (if (/= isShelf "YES")
    (setq sys_W (+ (* (- num_rows 1) row_sep) (* num_rows chamber_W)))
  )

  (if (= isShelf "YES")
    (progn
      (foreach shelf shelfList
        (setq sys_W (+ sys_W (* shelf chamber_W) (* (- shelf 1) row_sep)))
      )
      (setq sys_W (+ sys_W (* (- (length shelfList) 1) shelf_sep)))
    )
  )
  ; height of invert above base of system  
  (setq invertHeight 2)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun DIM_QUICK4_EQ24LP ()
  (SETQ CHAMBER_H 8.0)
  (SETQ CHAMBER_W 16.0)
  (SETQ CHAMBER_L 48.0)

  ; SYSTEM WIDTH AND LENGTH ARE CALCULATED
  (setq sys_L (* num_units chamber_L))
  (setq sys_W 0)

  (if (/= isShelf "YES")
    (setq sys_W (+ (* (- num_rows 1) row_sep) (* num_rows chamber_W)))
  )

  (if (= isShelf "YES")
    (progn
      (foreach shelf shelfList
        (setq sys_W (+ sys_W (* shelf chamber_W) (* (- shelf 1) row_sep)))
      )
      (setq sys_W (+ sys_W (* (- (length shelfList) 1) shelf_sep)))
    )
  )
  ; height of invert above base of system  
  (setq invertHeight 2)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun DIM_Q4_PLUS_STD_LP ()
  (SETQ CHAMBER_H 8.0)
  (SETQ CHAMBER_W 36.0)
  (SETQ CHAMBER_L 48.0)

  ; SYSTEM WIDTH AND LENGTH ARE CALCULATED
  (setq sys_L (* num_units chamber_L))
  (setq sys_W 0)

  (if (/= isShelf "YES")
    (setq sys_W (+ (* (- num_rows 1) row_sep) (* num_rows chamber_W)))
  )

  (if (= isShelf "YES")
    (progn
      (foreach shelf shelfList
        (setq sys_W (+ sys_W (* shelf chamber_W) (* (- shelf 1) row_sep)))
      )
      (setq sys_W (+ sys_W (* (- (length shelfList) 1) shelf_sep)))
    )
  )
  ; height of invert above base of system  
  (setq invertHeight 3.3)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun DIM_Q4_PLUS_STD ()
  (SETQ CHAMBER_H 12.0)
  (SETQ CHAMBER_W 36.0)
  (SETQ CHAMBER_L 48.0)

  ; SYSTEM WIDTH AND LENGTH ARE CALCULATED
  (setq sys_L (* num_units chamber_L))
  (setq sys_W 0)

  (if (/= isShelf "YES")
    (setq sys_W (+ (* (- num_rows 1) row_sep) (* num_rows chamber_W)))
  )

  (if (= isShelf "YES")
    (progn
      (foreach shelf shelfList
        (setq sys_W (+ sys_W (* shelf chamber_W) (* (- shelf 1) row_sep)))
      )
      (setq sys_W (+ sys_W (* (- (length shelfList) 1) shelf_sep)))
    )
  )
  ; height of invert above base of system  
  (setq invertHeight 8)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun DIM_Q4_PLUS_HC ()
  (SETQ CHAMBER_H 14.0)
  (SETQ CHAMBER_W 36.0)
  (SETQ CHAMBER_L 48.0)

  ; SYSTEM WIDTH AND LENGTH ARE CALCULATED
  (setq sys_L (* num_units chamber_L))
  (setq sys_W 0)

  (if (/= isShelf "YES")
    (setq sys_W (+ (* (- num_rows 1) row_sep) (* num_rows chamber_W)))
  )

  (if (= isShelf "YES")
    (progn
      (foreach shelf shelfList
        (setq sys_W (+ sys_W (* shelf chamber_W) (* (- shelf 1) row_sep)))
      )
      (setq sys_W (+ sys_W (* (- (length shelfList) 1) shelf_sep)))
    )
  )
  ; height of invert above base of system  
  (setq invertHeight 8)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun DIM_Q4_PLUS_EQ_36_LP ()
  (SETQ CHAMBER_H 8.0)
  (SETQ CHAMBER_W 22.0)
  (SETQ CHAMBER_L 48.0)

  ; SYSTEM WIDTH AND LENGTH ARE CALCULATED
  (setq sys_L (* num_units chamber_L))
  (setq sys_W 0)

  (if (/= isShelf "YES")
    (setq sys_W (+ (* (- num_rows 1) row_sep) (* num_rows chamber_W)))
  )

  (if (= isShelf "YES")
    (progn
      (foreach shelf shelfList
        (setq sys_W (+ sys_W (* shelf chamber_W) (* (- shelf 1) row_sep)))
      )
      (setq sys_W (+ sys_W (* (- (length shelfList) 1) shelf_sep)))
    )
  )
  ; height of invert above base of system  
  (setq invertHeight 3.3)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun DIM_ARC24 ()
  (SETQ CHAMBER_H 12.0)
  (SETQ CHAMBER_W 22.0)
  (SETQ CHAMBER_L 60.0)

  ; SYSTEM WIDTH AND LENGTH ARE CALCULATED
  (setq sys_L (* num_units chamber_L))
  (setq sys_W 0)

  (if (/= isShelf "YES")
    (setq sys_W (+ (* (- num_rows 1) row_sep) (* num_rows chamber_W)))
  )

  (if (= isShelf "YES")
    (progn
      (foreach shelf shelfList
        (setq sys_W (+ sys_W (* shelf chamber_W) (* (- shelf 1) row_sep)))
      )
      (setq sys_W (+ sys_W (* (- (length shelfList) 1) shelf_sep)))
    )
  )
  ; height of invert above base of system  
  (setq invertHeight 6)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN DIM_ELJEN ()
  (SETQ CHAMBER_H 11.0)
  (SETQ CHAMBER_W 36.0)
  (SETQ CHAMBER_L 48.0)

  ; SYSTEM WIDTH AND LENGTH ARE CALCULATED
  (setq sys_L (* num_units chamber_L)) 
  (setq sys_W 0)

  (if (/= isShelf "YES")
    (setq sys_W (+ (* (- num_rows 1) row_sep) (* num_rows chamber_W))) 
  )

  (if (= isShelf "YES")
    (progn
      (foreach shelf shelfList
       (setq sys_W (+ sys_W (* shelf chamber_W) (* (- shelf 1) row_sep)))
      )
      (setq sys_W (+ sys_W (* (- (length shelfList) 1) shelf_sep)))
    )
  )

  ; height of invert above base of system  
  (setq invertHeight 7)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN DIM_ELJEN_INVERT ()
  (SETQ CHAMBER_H 11.0)
  (SETQ CHAMBER_W 48.0)
  (SETQ CHAMBER_L 36.0)

  ; SYSTEM WIDTH AND LENGTH ARE CALCULATED
  (setq sys_L (* num_units chamber_L)) 
  (setq sys_W 0)

  (if (/= isShelf "YES")
    (setq sys_W (+ (* (- num_rows 1) row_sep) (* num_rows chamber_W))) 
  )

  (if (= isShelf "YES")
    (progn
      (foreach shelf shelfList
       (setq sys_W (+ sys_W (* shelf chamber_W) (* (- shelf 1) row_sep)))
      )
      (setq sys_W (+ sys_W (* (- (length shelfList) 1) shelf_sep)))
    )
  )
  ; height of invert above base of system  
  (setq invertHeight 7)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


