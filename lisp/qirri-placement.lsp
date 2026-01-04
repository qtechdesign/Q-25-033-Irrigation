;;; ============================================================================
;;; QIRRI - Sprinkler Placement Algorithms
;;; OPTIMIZED for speed - simplified algorithms
;;; Copyright (c) 2026 QTech Design - www.qtech.hr
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; Area Selection Command
;;; ----------------------------------------------------------------------------

(defun c:QIRRAREA (/ ent vertices area perimeter bounds width-m height-m area-m2)
  "Select and analyze irrigation area"
  
  ;; Make sure units are detected
  (if (not *qirri-scale-factor*) (qtech:detect-units))
  
  (setq ent (qtech:select-closed-polyline "Select closed polyline for irrigation area: "))
  (if ent
    (progn
      (setq *qtech-current-area* ent)
      (setq vertices (qtech:get-pline-vertices ent))
      (setq area (qtech:pline-area ent))
      (setq perimeter (qtech:pline-length ent))
      (setq bounds (qtech:pline-bounds vertices))
      
      ;; Convert to meters
      (setq width-m (qtech:to-meters (- (car (cadr bounds)) (caar bounds))))
      (setq height-m (qtech:to-meters (- (cadr (cadr bounds)) (cadar bounds))))
      (setq area-m2 (* area *qirri-scale-factor* *qirri-scale-factor*))
      (setq perimeter-m (qtech:to-meters perimeter))
      
      ;; Display
      (princ "\n\n=== IRRIGATION AREA ===")
      (princ (strcat "\nUnits: " *qirri-units*))
      (princ (strcat "\nVertices: " (itoa (length vertices))))
      (princ (strcat "\nArea: " (rtos area-m2 2 1) " m2"))
      (princ (strcat "\nPerimeter: " (rtos perimeter-m 2 1) " m"))
      (princ (strcat "\nSize: " (rtos width-m 2 2) " x " (rtos height-m 2 2) " m"))
      
      ;; Store
      (setq *qtech-area-data* 
            (list (cons "entity" ent)
                  (cons "vertices" vertices)
                  (cons "area" area)
                  (cons "area-m2" area-m2)
                  (cons "perimeter" perimeter)
                  (cons "perimeter-m" perimeter-m)
                  (cons "bounds" bounds)
                  (cons "centroid" (qtech:pline-centroid vertices))))
      
      (redraw ent 3)
      (princ "\n\nRun QIRRPLACE to auto-place sprinklers.\n")
    )
    (princ "\nNo valid area selected.\n")
  )
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; SIMPLIFIED Placement - Grid-based approach
;;; ----------------------------------------------------------------------------

(defun c:QIRRPLACE (/ vertices bounds area-m2 nozzle radius-m spacing-m
                      all-heads settings grid-res)
  "Automatic sprinkler placement - FAST grid-based method"
  
  (if (not *qtech-area-data*)
    (progn (princ "\nRun QIRRAREA first.") (princ) (exit))
  )
  
  (if (not *qirri-scale-factor*) (qtech:detect-units))
  
  (setq vertices (cdr (assoc "vertices" *qtech-area-data*)))
  (setq bounds (cdr (assoc "bounds" *qtech-area-data*)))
  (setq area-m2 (cdr (assoc "area-m2" *qtech-area-data*)))
  (if (not area-m2)
    (setq area-m2 (* (cdr (assoc "area" *qtech-area-data*)) 
                     *qirri-scale-factor* *qirri-scale-factor*))
  )
  
  (princ "\n")
  (princ "\n+----------------------------------------------+")
  (princ "\n|     QIRRI AUTOMATIC PLACEMENT                |")
  (princ "\n+----------------------------------------------+")
  (princ (strcat "\nArea: " (rtos area-m2 2 1) " m2"))
  
  (setq settings *qtech-settings*)
  
  ;; Find best nozzle for this area size
  (princ "\n\nFinding optimal sprinkler...")
  
  ;; Use 5m radius for typical residential, adjust based on area
  (setq target-radius 
    (cond
      ((< area-m2 50) 3.0)      ; Small area
      ((< area-m2 200) 4.0)     ; Medium
      ((< area-m2 500) 5.0)     ; Large
      (T 6.0)                    ; Very large
    )
  )
  
  (setq nozzle (qtech:select-best-nozzle 
                 target-radius
                 360  ; Full circle for simplicity
                 (cdr (assoc "pressure" settings))
                 (cdr (assoc "target-precip" settings))
                 (cdr (assoc "preferred-brand" settings))
                 (cdr (assoc "efficiency-priority" settings))))
  
  (if (not nozzle)
    (progn
      (princ "\nNo suitable nozzle found in catalogue!")
      (princ "\nUsing default: 5m radius, 15mm/hr")
      ;; Create dummy nozzle
      (setq nozzle '("Generic" "MP" "5.0" 5.0 0.15 15.0 3.0))
    )
  )
  
  (setq radius-m (qtech:cat-radius nozzle))
  (princ (strcat "\nUsing: " (qtech:cat-full-name nozzle)))
  (princ (strcat "\n  Radius: " (rtos radius-m 2 1) " m"))
  (princ (strcat "\n  Flow: " (rtos (qtech:cat-flow nozzle) 2 3) " m3/h"))
  
  ;; Calculate spacing (55% of diameter for good overlap)
  (setq spacing-m (* radius-m 2 (cdr (assoc "spacing-factor" settings))))
  (princ (strcat "\n  Spacing: " (rtos spacing-m 2 2) " m"))
  
  ;; Place heads on grid
  (princ "\n\nPlacing sprinklers on triangular grid...")
  (setq all-heads (qtech:place-grid-pattern vertices bounds nozzle spacing-m))
  
  (setq *qtech-placements* all-heads)
  
  ;; Summary
  (princ "\n")
  (princ "\n+----------------------------------------------+")
  (princ "\n|           PLACEMENT COMPLETE                 |")
  (princ "\n+----------------------------------------------+")
  (princ (strcat "\n  Heads placed: " (itoa (length all-heads))))
  (princ (strcat "\n  Total flow: " (rtos (qtech:total-flow all-heads) 2 3) " m3/h"))
  (princ (strcat "\n  Coverage: ~" (itoa (fix (* (/ (* (length all-heads) 3.14159 radius-m radius-m) area-m2) 100))) "%"))
  (princ "\n+----------------------------------------------+")
  
  ;; Draw
  (princ "\n\nDrawing heads...")
  (qtech:draw-all-heads all-heads)
  (princ " Done!")
  
  (princ "\n\nNext: QIRRPATTERN to see spray coverage")
  (princ "\n      QIRRVALIDATE for uniformity check\n")
  
  (princ)
)

(defun qtech:total-flow (heads)
  (apply '+ (mapcar '(lambda (h) (cdr (assoc "flow" h))) heads))
)

;;; ----------------------------------------------------------------------------
;;; Grid-Based Placement (Fast)
;;; ----------------------------------------------------------------------------

(defun qtech:place-grid-pattern (vertices bounds nozzle spacing-m / 
                                   heads spacing-dwg minpt maxpt x y row-offset
                                   pt count)
  "Place sprinklers on triangular grid pattern"
  (setq heads '())
  (setq spacing-dwg (qtech:from-meters spacing-m))
  (setq minpt (car bounds))
  (setq maxpt (cadr bounds))
  
  ;; Add margin
  (setq margin-dwg (/ spacing-dwg 2))
  (setq minpt (list (+ (car minpt) margin-dwg) (+ (cadr minpt) margin-dwg)))
  (setq maxpt (list (- (car maxpt) margin-dwg) (- (cadr maxpt) margin-dwg)))
  
  (setq y (cadr minpt))
  (setq row 0)
  (setq count 0)
  
  (while (<= y (cadr maxpt))
    ;; Offset every other row for triangular pattern
    (setq row-offset (if (= (rem row 2) 0) 0 (/ spacing-dwg 2)))
    (setq x (+ (car minpt) row-offset))
    
    (while (<= x (car maxpt))
      (setq pt (list x y))
      ;; Only place if inside polygon
      (if (qtech:point-in-polygon pt vertices)
        (progn
          (setq heads (cons (qtech:create-head-data pt nozzle 360 "grid") heads))
          (setq count (1+ count))
          (if (= (rem count 10) 0) (princ "."))
        )
      )
      (setq x (+ x spacing-dwg))
    )
    (setq y (+ y (* spacing-dwg 0.866)))  ; 0.866 = sin(60°) for equilateral triangle
    (setq row (1+ row))
  )
  
  (reverse heads)
)

;;; ----------------------------------------------------------------------------
;;; Head Data
;;; ----------------------------------------------------------------------------

(defun qtech:create-head-data (position nozzle arc placement-type)
  "Create head data"
  (list
    (cons "position" position)
    (cons "brand" (qtech:cat-brand nozzle))
    (cons "model" (qtech:cat-model nozzle))
    (cons "nozzle" (qtech:cat-nozzle nozzle))
    (cons "radius" (qtech:cat-radius nozzle))
    (cons "flow" (qtech:cat-flow nozzle))
    (cons "precip" (qtech:cat-precip nozzle))
    (cons "pressure" (qtech:cat-pressure nozzle))
    (cons "arc" arc)
    (cons "rotation" 0)
    (cons "type" placement-type)
    (cons "zone" nil)
  )
)

;;; ----------------------------------------------------------------------------
;;; Drawing Functions
;;; ----------------------------------------------------------------------------

(defun qtech:draw-all-heads (heads / count)
  "Draw all sprinkler heads"
  (qtech:set-layer "IRR-SPRINKLER")
  (setq count 0)
  (foreach h heads
    (qtech:draw-head h)
    (setq count (1+ count))
    (if (= (rem count 20) 0) (princ "."))
  )
)

(defun qtech:draw-head (head-data / pos arc symbol-size-dwg)
  "Draw single head"
  (setq pos (cdr (assoc "position" head-data)))
  (setq arc (cdr (assoc "arc" head-data)))
  
  ;; Symbol size 15cm
  (setq symbol-size-dwg (qtech:from-meters 0.15))
  
  ;; Draw circle
  (command "._CIRCLE" pos symbol-size-dwg)
  
  ;; Draw cross
  (command "._LINE" 
           (list (- (car pos) symbol-size-dwg) (cadr pos))
           (list (+ (car pos) symbol-size-dwg) (cadr pos))
           "")
  (command "._LINE"
           (list (car pos) (- (cadr pos) symbol-size-dwg))
           (list (car pos) (+ (cadr pos) symbol-size-dwg))
           "")
)

;;; ----------------------------------------------------------------------------
;;; Manual Placement
;;; ----------------------------------------------------------------------------

(defun c:QIRRMANUAL (/ pt nozzle arc radius-m)
  "Manual placement"
  (princ "\n=== MANUAL PLACEMENT ===")
  (princ (strcat "\nUnits: " *qirri-units*))
  
  (princ "\nRadius in meters [3.0]: ")
  (setq radius-m (getreal))
  (if (not radius-m) (setq radius-m 3.0))
  
  (princ "Arc (90/180/270/360) [360]: ")
  (setq arc (getint))
  (if (not arc) (setq arc 360))
  
  (setq nozzle (qtech:select-best-nozzle 
                 radius-m arc
                 (cdr (assoc "pressure" *qtech-settings*))
                 (cdr (assoc "target-precip" *qtech-settings*))
                 (cdr (assoc "preferred-brand" *qtech-settings*))
                 (cdr (assoc "efficiency-priority" *qtech-settings*))))
  
  (if nozzle
    (progn
      (princ (strcat "\nUsing: " (qtech:cat-full-name nozzle)))
      (qtech:set-layer "IRR-SPRINKLER")
      
      (while (setq pt (getpoint "\nClick to place (ENTER to exit): "))
        (setq new-head (qtech:create-head-data pt nozzle arc "manual"))
        (qtech:draw-head new-head)
        (setq *qtech-placements* (cons new-head *qtech-placements*))
        (princ (strcat " Placed at " (rtos (qtech:to-meters (car pt)) 2 2) "," 
                       (rtos (qtech:to-meters (cadr pt)) 2 2) " m"))
      )
    )
    (princ "\nNo nozzle found.")
  )
  (princ "\nDone.\n")
  (princ)
)

;;; ============================================================================

(princ "\n  qirri-placement.lsp loaded")
(princ)
