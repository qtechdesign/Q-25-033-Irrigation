;;; ============================================================================
;;; QIRRI - Sprinkler Placement Algorithms
;;; Automatic and manual placement functions
;;; NOW WITH UNIT-AWARE dimensions
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
      ;; Store current area
      (setq *qtech-current-area* ent)
      
      ;; Get vertices
      (setq vertices (qtech:get-pline-vertices ent))
      
      ;; Calculate properties (in drawing units)
      (setq area (qtech:pline-area ent))
      (setq perimeter (qtech:pline-length ent))
      (setq bounds (qtech:pline-bounds vertices))
      
      ;; Convert to meters for display
      (setq width-m (qtech:to-meters (- (car (cadr bounds)) (caar bounds))))
      (setq height-m (qtech:to-meters (- (cadr (cadr bounds)) (cadar bounds))))
      (setq area-m2 (* area *qirri-scale-factor* *qirri-scale-factor*))
      (setq perimeter-m (qtech:to-meters perimeter))
      
      ;; Display info
      (princ "\n\n=== IRRIGATION AREA ANALYSIS ===\n")
      (princ (strcat "Drawing Units: " *qirri-units* "\n"))
      (princ (strcat "Vertices: " (itoa (length vertices)) "\n"))
      (princ (strcat "Area: " (qtech:format-area area) "\n"))
      (princ (strcat "Perimeter: " (qtech:format-length perimeter) "\n"))
      (princ (strcat "Bounding box: " (rtos width-m 2 2) " x " (rtos height-m 2 2) " m\n"))
      
      ;; Store area data (keep in drawing units for geometry operations)
      (setq *qtech-area-data* 
            (list (cons "entity" ent)
                  (cons "vertices" vertices)
                  (cons "area" area)               ; Drawing units squared
                  (cons "area-m2" area-m2)         ; In square meters  
                  (cons "perimeter" perimeter)     ; Drawing units
                  (cons "perimeter-m" perimeter-m) ; In meters
                  (cons "bounds" bounds)
                  (cons "centroid" (qtech:pline-centroid vertices))))
      
      ;; Highlight selection
      (redraw ent 3)
      
      (princ "\nArea selected. Run QIRRPLACE to auto-place sprinklers.\n")
    )
    (princ "\nNo valid area selected.\n")
  )
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; Automatic Placement Command
;;; ----------------------------------------------------------------------------

(defun c:QIRRPLACE (/ vertices bounds offset-dist perimeter-heads interior-heads
                      all-heads total-flow settings area-m2 cu du)
  "Automatic sprinkler placement using hybrid greedy algorithm"
  
  ;; Check if area is selected
  (if (not *qtech-area-data*)
    (progn
      (princ "\nNo irrigation area selected. Run QIRRAREA first.")
      (princ)
      (exit)
    )
  )
  
  ;; Ensure units detected
  (if (not *qirri-scale-factor*) (qtech:detect-units))
  
  (setq vertices (cdr (assoc "vertices" *qtech-area-data*)))
  (setq bounds (cdr (assoc "bounds" *qtech-area-data*)))
  (setq area-m2 (cdr (assoc "area-m2" *qtech-area-data*)))
  (if (not area-m2)
    (setq area-m2 (* (cdr (assoc "area" *qtech-area-data*)) 
                     *qirri-scale-factor* *qirri-scale-factor*))
  )
  
  (princ "\n")
  (princ "+--------------------------------------------------------------+\n")
  (princ "|        QTECH HYBRID GREEDY PLACEMENT ALGORITHM               |\n")
  (princ "+--------------------------------------------------------------+\n")
  
  ;; Get settings
  (setq settings *qtech-settings*)
  ;; Offset distance in METERS, will be converted internally
  (setq offset-dist (cdr (assoc "perimeter-offset" settings)))
  
  ;; Generate simulation grid (resolution in meters)
  (princ "\nInitializing simulation grid...")
  (qtech:generate-simulation-grid vertices (cdr (assoc "grid-resolution" settings)))
  
  ;; Phase 1: Perimeter placement (edge handling critical for water savings)
  (princ "\n\n>> PHASE 1: Perimeter-First Placement")
  (princ "\n   Goal: Minimize overspray at boundaries (saves 10-20% water)")
  (princ "\n   Strategy: Short-radius heads hugging edges...")
  (setq perimeter-heads (qtech:place-perimeter vertices offset-dist settings))
  (princ (strcat "\n   Result: " (itoa (length perimeter-heads)) " perimeter heads placed.\n"))
  
  ;; Phase 2: Interior puzzle-like infill
  (princ "\n>> PHASE 2: Inward Puzzle-Like Infill")
  (princ "\n   Goal: Fill coverage gaps from perimeter inward")
  (princ "\n   Strategy: Concentric layers, largest gaps first...")
  (setq interior-heads (qtech:place-interior vertices perimeter-heads bounds settings))
  (princ (strcat "\n   Result: " (itoa (length interior-heads)) " interior heads placed.\n"))
  
  ;; Combine all heads
  (setq all-heads (append perimeter-heads interior-heads))
  (setq *qtech-placements* all-heads)
  
  ;; Calculate metrics
  (setq total-flow (qtech:list-sum (mapcar '(lambda (h) (cdr (assoc "flow" h))) all-heads)))
  
  ;; Run simulation for uniformity check
  (princ "\n>> PHASE 3: Coverage Validation")
  (princ "\n   Running precipitation simulation...")
  (qtech:simulate-coverage all-heads)
  (setq metrics (qtech:calculate-uniformity *qtech-grid*))
  (setq cu (cdr (assoc "cu" metrics)))
  (setq du (cdr (assoc "du" metrics)))
  (setq coverage (cdr (assoc "coverage" metrics)))
  
  ;; Summary
  (princ "\n\n")
  (princ "+--------------------------------------------------------------+\n")
  (princ "|                   PLACEMENT SUMMARY                          |\n")
  (princ "+--------------------------------------------------------------+\n")
  (princ (strcat "|  Area:              " (qtech:string-pad (strcat (rtos area-m2 2 1) " m2") 20 " ") "              |\n"))
  (princ (strcat "|  Total Heads:       " (qtech:string-pad (itoa (length all-heads)) 20 " ") "              |\n"))
  (princ (strcat "|    - Perimeter:     " (qtech:string-pad (itoa (length perimeter-heads)) 20 " ") "              |\n"))
  (princ (strcat "|    - Interior:      " (qtech:string-pad (itoa (length interior-heads)) 20 " ") "              |\n"))
  (princ (strcat "|  Total Flow:        " (qtech:string-pad (strcat (rtos total-flow 2 3) " m3/h") 20 " ") "              |\n"))
  (princ "+--------------------------------------------------------------+\n")
  (princ (strcat "|  Uniformity CU:     " (qtech:string-pad (strcat (rtos cu 2 1) "% " (qtech:cu-rating cu)) 20 " ") "              |\n"))
  (princ (strcat "|  Distribution DU:   " (qtech:string-pad (strcat (rtos du 2 1) "% " (qtech:du-rating du)) 20 " ") "              |\n"))
  (princ (strcat "|  Coverage:          " (qtech:string-pad (strcat (rtos coverage 2 1) "%") 20 " ") "              |\n"))
  (princ "+--------------------------------------------------------------+\n")
  
  ;; Recommendations
  (if (< cu (cdr (assoc "target-cu" settings)))
    (princ "\n>> CU below target - recommend running QIRROPTIMIZE for GA refinement.\n")
  )
  
  ;; Draw heads
  (princ "\nDrawing sprinkler heads...")
  (qtech:draw-all-heads all-heads)
  (princ " Done.\n")
  
  (princ "\nNext steps:")
  (princ "\n  - QIRRPATTERN  - Visualize spray coverage")
  (princ "\n  - QIRROPTIMIZE - GA optimization for better uniformity")
  (princ "\n  - QIRRVALIDATE - Detailed uniformity analysis\n")
  
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; Perimeter Placement
;;; ----------------------------------------------------------------------------

(defun qtech:place-perimeter (vertices offset-dist-m settings / heads n i v0 v1 v2 
                                corner-arc edge-heads spacing nozzle pressure brand eff
                                offset-dwg)
  "Place sprinklers around the perimeter"
  (setq heads '())
  (setq n (length vertices))
  (setq pressure (cdr (assoc "pressure" settings)))
  (setq brand (cdr (assoc "preferred-brand" settings)))
  (setq eff (cdr (assoc "efficiency-priority" settings)))
  (setq spacing-factor (cdr (assoc "spacing-factor" settings)))
  
  ;; Convert offset from meters to drawing units
  (setq offset-dwg (qtech:from-meters offset-dist-m))
  
  ;; Process each vertex (corner) and edge
  (setq i 0)
  (while (< i n)
    ;; Get vertex triplet for corner analysis
    (setq v0 (nth (rem (+ i n -1) n) vertices))  ; Previous
    (setq v1 (nth i vertices))                    ; Current
    (setq v2 (nth (rem (1+ i) n) vertices))       ; Next
    
    ;; Determine corner arc angle
    (setq corner-arc (qtech:calculate-corner-arc v0 v1 v2))
    
    ;; Place corner head
    (if (and corner-arc (> corner-arc 45))
      (progn
        ;; Find best nozzle for corner (radius in meters)
        (setq nozzle (qtech:select-best-nozzle 
                       3.0  ; 3m radius for perimeter
                       (qtech:snap-arc corner-arc)
                       pressure
                       (cdr (assoc "target-precip" settings))
                       brand
                       eff))
        (if nozzle
          (setq heads (append heads 
                        (list (qtech:create-head-data 
                                (qtech:offset-corner v0 v1 v2 offset-dwg)
                                nozzle
                                (qtech:snap-arc corner-arc)
                                "perimeter"))))
        )
      )
    )
    
    ;; Place edge heads between corners
    (setq edge-heads (qtech:place-edge-heads v1 v2 offset-dwg settings))
    (setq heads (append heads edge-heads))
    
    (setq i (1+ i))
  )
  heads
)

(defun qtech:calculate-corner-arc (v0 v1 v2 / ang1 ang2 interior-ang)
  "Calculate the arc angle needed for a corner"
  (setq ang1 (qtech:angle-2d v1 v0))
  (setq ang2 (qtech:angle-2d v1 v2))
  ;; Calculate interior angle
  (setq interior-ang (abs (- ang2 ang1)))
  (if (> interior-ang pi) (setq interior-ang (- (* 2 pi) interior-ang)))
  ;; Convert to spray arc (exterior angle)
  (qtech:radians-to-degrees (- (* 2 pi) interior-ang))
)

(defun qtech:snap-arc (arc)
  "Snap arc angle to standard values (90, 180, 270, 360)"
  (cond
    ((<= arc 110) 90)
    ((<= arc 200) 180)
    ((<= arc 290) 270)
    (T 360)
  )
)

(defun qtech:offset-corner (v0 v1 v2 dist / ang-bisect)
  "Calculate offset position for corner head (dist in drawing units)"
  (setq ang1 (qtech:angle-2d v1 v0))
  (setq ang2 (qtech:angle-2d v1 v2))
  (setq ang-bisect (/ (+ ang1 ang2) 2.0))
  ;; Offset inward along bisector
  (qtech:polar-point v1 ang-bisect dist)
)

(defun qtech:place-edge-heads (v1 v2 offset-dist-dwg settings / edge-len spacing n-heads
                                pts ang nozzle pressure brand eff heads radius-dwg)
  "Place sprinkler heads along an edge"
  (setq heads '())
  (setq edge-len (qtech:distance-2d v1 v2))
  (setq edge-len-m (qtech:to-meters edge-len))
  (setq pressure (cdr (assoc "pressure" settings)))
  (setq brand (cdr (assoc "preferred-brand" settings)))
  (setq eff (cdr (assoc "efficiency-priority" settings)))
  (setq spacing-factor (cdr (assoc "spacing-factor" settings)))
  
  ;; Find appropriate nozzle for edge (4m radius)
  (setq nozzle (qtech:select-best-nozzle 
                 4.0  ; 4m radius for edge
                 180  ; Half-circle for edges
                 pressure
                 (cdr (assoc "target-precip" settings))
                 brand
                 eff))
  
  (if nozzle
    (progn
      ;; Get radius in meters from nozzle, convert to drawing units
      (setq radius-m (qtech:cat-radius nozzle))
      (setq radius-dwg (qtech:from-meters radius-m))
      
      ;; Calculate spacing in drawing units
      (setq spacing-dwg (* radius-dwg spacing-factor 2))
      
      ;; Number of heads needed (excluding corners)
      (setq n-heads (fix (/ edge-len spacing-dwg)))
      (if (< n-heads 1) (setq n-heads 0))
      
      ;; Calculate actual spacing
      (if (> n-heads 0)
        (progn
          (setq actual-spacing (/ edge-len (1+ n-heads)))
          (setq ang (qtech:angle-2d v1 v2))
          (setq perp-ang (+ ang (/ pi 2)))  ; Perpendicular for offset
          
          ;; Place heads along edge
          (setq i 1)
          (while (<= i n-heads)
            (setq edge-pt (qtech:polar-point v1 ang (* i actual-spacing)))
            (setq offset-pt (qtech:polar-point edge-pt perp-ang offset-dist-dwg))
            (setq heads (append heads 
                          (list (qtech:create-head-data 
                                  offset-pt
                                  nozzle
                                  180  ; Half-circle facing outward
                                  "perimeter"))))
            (setq i (1+ i))
          )
        )
      )
    )
  )
  heads
)

;;; ----------------------------------------------------------------------------
;;; Interior Infill Placement
;;; ----------------------------------------------------------------------------

(defun qtech:place-interior (vertices perimeter-heads bounds settings / 
                              grid coverage heads uncovered nozzle grid-res-dwg
                              spacing-dwg)
  "Place interior sprinkler heads to fill coverage gaps"
  (setq heads '())
  
  ;; Get grid resolution in drawing units
  (setq grid-res-m (cdr (assoc "grid-resolution" settings)))
  (setq grid-res-dwg (qtech:from-meters grid-res-m))
  
  ;; Generate coverage grid
  (setq grid (qtech:generate-grid bounds grid-res-dwg))
  (setq grid (qtech:filter-points-in-polygon grid vertices))
  
  ;; Calculate initial coverage from perimeter heads
  (setq coverage (qtech:calculate-coverage grid perimeter-heads))
  
  ;; Find best nozzle for interior (full circle, 5m radius)
  (setq nozzle (qtech:select-best-nozzle 
                 5.0  ; 5m radius for interior
                 360  ; Full circle
                 (cdr (assoc "pressure" settings))
                 (cdr (assoc "target-precip" settings))
                 (cdr (assoc "preferred-brand" settings))
                 (cdr (assoc "efficiency-priority" settings))))
  
  (if nozzle
    (progn
      ;; Get spacing in drawing units
      (setq radius-m (qtech:cat-radius nozzle))
      (setq radius-dwg (qtech:from-meters radius-m))
      (setq spacing-dwg (* radius-dwg (cdr (assoc "spacing-factor" settings)) 2))
      
      ;; Iteratively place heads in uncovered areas
      (setq uncovered (qtech:find-uncovered-areas grid coverage 0.5))
      (setq iteration 0)
      (setq max-iterations 50)
      
      (while (and uncovered (< iteration max-iterations))
        ;; Find best position for new head
        (setq best-pos (qtech:find-best-interior-position 
                         uncovered vertices heads spacing-dwg))
        
        (if best-pos
          (progn
            ;; Add new head
            (setq new-head (qtech:create-head-data best-pos nozzle 360 "interior"))
            (setq heads (append heads (list new-head)))
            
            ;; Recalculate coverage
            (setq coverage (qtech:calculate-coverage grid (append perimeter-heads heads)))
            (setq uncovered (qtech:find-uncovered-areas grid coverage 0.5))
          )
          (setq uncovered nil)  ; Exit if no valid position found
        )
        (setq iteration (1+ iteration))
      )
    )
  )
  heads
)

(defun qtech:calculate-coverage (grid heads / coverage pt)
  "Calculate coverage value for each grid point"
  (mapcar 
    '(lambda (pt)
       (cons pt (qtech:point-coverage pt heads)))
    grid
  )
)

(defun qtech:point-coverage (pt heads / total radius-dwg)
  "Calculate total precipitation at a point from all heads"
  (setq total 0.0)
  (foreach h heads
    (setq head-pos (cdr (assoc "position" h)))
    (setq radius-m (cdr (assoc "radius" h)))  ; Radius stored in meters
    (setq radius-dwg (qtech:from-meters radius-m))
    (setq precip (cdr (assoc "precip" h)))
    (setq arc (cdr (assoc "arc" h)))
    (setq dist (qtech:distance-2d pt head-pos))
    
    ;; Check if point is within range and arc
    (if (and (<= dist radius-dwg)
             (qtech:point-in-arc pt head-pos arc (cdr (assoc "rotation" h))))
      ;; Simple linear falloff model
      (setq total (+ total (* precip (- 1.0 (/ dist radius-dwg)))))
    )
  )
  total
)

(defun qtech:point-in-arc (pt center arc rotation / ang diff)
  "Check if point is within sprinkler arc"
  (if (= arc 360)
    T
    (progn
      (setq ang (qtech:angle-2d center pt))
      (setq ang (qtech:radians-to-degrees (qtech:normalize-angle ang)))
      (setq rot (if rotation rotation 0))
      (setq diff (abs (- ang rot)))
      (if (> diff 180) (setq diff (- 360 diff)))
      (<= diff (/ arc 2.0))
    )
  )
)

(defun qtech:find-uncovered-areas (grid coverage threshold / uncovered)
  "Find grid points with coverage below threshold"
  (vl-remove-if-not
    '(lambda (gc) (< (cdr gc) threshold))
    coverage
  )
)

(defun qtech:find-best-interior-position (uncovered vertices existing-heads spacing-dwg / 
                                           best-pos best-score pt score)
  "Find the best position for a new interior head"
  (setq best-pos nil)
  (setq best-score -1)
  
  (foreach uc uncovered
    (setq pt (car uc))
    ;; Check minimum distance from existing heads
    (if (qtech:min-distance-ok pt existing-heads spacing-dwg)
      (progn
        ;; Score based on how many uncovered points it would cover
        (setq score (qtech:score-interior-position pt uncovered spacing-dwg))
        (if (> score best-score)
          (progn
            (setq best-pos pt)
            (setq best-score score)
          )
        )
      )
    )
  )
  best-pos
)

(defun qtech:min-distance-ok (pt heads min-dist-dwg / ok radius-dwg)
  "Check if point maintains minimum distance from all existing heads"
  (setq ok T)
  (foreach h heads
    ;; Use the head's radius converted to drawing units
    (setq radius-m (cdr (assoc "radius" h)))
    (setq radius-dwg (qtech:from-meters radius-m))
    (if (< (qtech:distance-2d pt (cdr (assoc "position" h))) (* radius-dwg 0.8))
      (setq ok nil)
    )
  )
  ok
)

(defun qtech:score-interior-position (pt uncovered radius-dwg / count)
  "Score a potential interior position by coverage potential"
  (setq count 0)
  (foreach uc uncovered
    (if (<= (qtech:distance-2d pt (car uc)) radius-dwg)
      (setq count (1+ count))
    )
  )
  count
)

;;; ----------------------------------------------------------------------------
;;; Head Data Management
;;; ----------------------------------------------------------------------------

(defun qtech:create-head-data (position nozzle arc placement-type / data)
  "Create a head data association list
   position: In DRAWING UNITS
   nozzle: Catalogue entry (radius in METERS)
   arc: Degrees
   placement-type: 'perimeter', 'interior', 'manual'"
  (list
    (cons "position" position)           ; Drawing units
    (cons "brand" (qtech:cat-brand nozzle))
    (cons "model" (qtech:cat-model nozzle))
    (cons "nozzle" (qtech:cat-nozzle nozzle))
    (cons "radius" (qtech:cat-radius nozzle))  ; Stored in METERS
    (cons "flow" (qtech:cat-flow nozzle))      ; m³/h
    (cons "precip" (qtech:cat-precip nozzle))  ; mm/hr
    (cons "pressure" (qtech:cat-pressure nozzle))  ; bar
    (cons "arc" arc)                     ; Degrees
    (cons "rotation" 0)                  ; Rotation angle for arc orientation
    (cons "type" placement-type)
    (cons "zone" nil)
    (cons "line" nil)
    (cons "locked" nil)
  )
)

;;; ----------------------------------------------------------------------------
;;; Drawing Functions
;;; ----------------------------------------------------------------------------

(defun qtech:draw-all-heads (heads)
  "Draw all sprinkler heads in AutoCAD"
  (qtech:set-layer "IRR-SPRINKLER")
  (foreach h heads
    (qtech:draw-head h)
  )
)

(defun qtech:draw-head (head-data / pos radius arc symbol-size-dwg ang text-size-dwg)
  "Draw a single sprinkler head with directional indicator"
  (setq pos (cdr (assoc "position" head-data)))
  (setq arc (cdr (assoc "arc" head-data)))
  (setq rotation (cdr (assoc "rotation" head-data)))
  (if (not rotation) (setq rotation 0))
  
  ;; Symbol size - 15cm converted to drawing units
  (setq symbol-size-dwg (qtech:from-meters 0.15))
  (setq text-size-dwg (qtech:from-meters 0.10))
  
  ;; Draw base circle for head location
  (command "._CIRCLE" pos symbol-size-dwg)
  
  ;; Add cross marker
  (command "._LINE" 
           (list (- (car pos) symbol-size-dwg) (cadr pos))
           (list (+ (car pos) symbol-size-dwg) (cadr pos))
           "")
  (command "._LINE"
           (list (car pos) (- (cadr pos) symbol-size-dwg))
           (list (car pos) (+ (cadr pos) symbol-size-dwg))
           "")
  
  ;; Add arc indicator for non-360 heads
  (if (/= arc 360)
    (progn
      (setq half-arc (/ arc 2.0))
      (setq start-ang (- rotation half-arc))
      (setq end-ang (+ rotation half-arc))
      
      ;; Draw small arc indicator
      (command "._ARC" "_C" pos
               (qtech:polar-point pos (qtech:degrees-to-radians start-ang) (* symbol-size-dwg 1.5))
               (qtech:polar-point pos (qtech:degrees-to-radians end-ang) (* symbol-size-dwg 1.5)))
    )
  )
  
  ;; Add label with arc degree
  (qtech:set-layer "IRR-TEXT")
  (command "._TEXT" 
           (list (+ (car pos) (* symbol-size-dwg 1.8)) (- (cadr pos) (/ text-size-dwg 2)))
           text-size-dwg 0 
           (strcat (itoa arc) "d"))  ; Use "d" instead of degree symbol for compatibility
  (qtech:set-layer "IRR-SPRINKLER")
)

(defun qtech:draw-head-with-attributes (head-data / pos text-size-dwg)
  "Draw sprinkler head with full block attributes"
  (setq pos (cdr (assoc "position" head-data)))
  (setq text-size-dwg (qtech:from-meters 0.08))
  
  ;; Draw basic symbol
  (qtech:draw-head head-data)
  
  ;; Add attribute text (simplified - full would use INSERT with block)
  (qtech:set-layer "IRR-TEXT")
  (command "._TEXT"
           (list (car pos) (- (cadr pos) (* text-size-dwg 3)))
           text-size-dwg 0
           (strcat (cdr (assoc "model" head-data)) " " (cdr (assoc "nozzle" head-data))))
  (qtech:set-layer "IRR-SPRINKLER")
)

;;; ----------------------------------------------------------------------------
;;; Manual Placement Command
;;; ----------------------------------------------------------------------------

(defun c:QIRRMANUAL (/ pt nozzle arc radius-m model continue)
  "Manual sprinkler placement mode"
  (princ "\n\n=== MANUAL PLACEMENT MODE ===\n")
  (princ (strcat "Drawing units: " *qirri-units* "\n"))
  (princ "Click to place sprinklers. Press ENTER to exit.\n")
  
  ;; Get nozzle selection (in METERS)
  (princ "\nEnter radius in METERS [3.0]: ")
  (setq radius-m (getreal))
  (if (not radius-m) (setq radius-m 3.0))
  
  (princ "Enter arc angle (90/180/270/360) [360]: ")
  (setq arc (getint))
  (if (not arc) (setq arc 360))
  
  ;; Find matching nozzle
  (setq nozzle (qtech:select-best-nozzle 
                 radius-m arc
                 (cdr (assoc "pressure" *qtech-settings*))
                 (cdr (assoc "target-precip" *qtech-settings*))
                 (cdr (assoc "preferred-brand" *qtech-settings*))
                 (cdr (assoc "efficiency-priority" *qtech-settings*))))
  
  (if nozzle
    (progn
      (princ (strcat "\nUsing: " (qtech:cat-full-name nozzle) 
                     " (R=" (rtos (qtech:cat-radius nozzle) 2 1) "m)\n"))
      
      ;; Placement loop
      (setq continue T)
      (qtech:set-layer "IRR-SPRINKLER")
      
      (while continue
        (setq pt (getpoint "\nPlace sprinkler (ENTER to exit): "))
        (if pt
          (progn
            ;; Create and draw head
            (setq new-head (qtech:create-head-data pt nozzle arc "manual"))
            (qtech:draw-head new-head)
            
            ;; Add to placements
            (setq *qtech-placements* (append *qtech-placements* (list new-head)))
            
            ;; Show position in meters
            (princ (strcat "Placed at (" 
                          (rtos (qtech:to-meters (car pt)) 2 2) ", " 
                          (rtos (qtech:to-meters (cadr pt)) 2 2) ") m"))
          )
          (setq continue nil)
        )
      )
    )
    (princ "\nNo suitable nozzle found for specified parameters.")
  )
  
  (princ "\nManual placement complete.\n")
  (princ)
)

;;; ============================================================================
;;; End of qirri-placement.lsp
;;; ============================================================================

(princ "\n  qirri-placement.lsp loaded")
(princ)
