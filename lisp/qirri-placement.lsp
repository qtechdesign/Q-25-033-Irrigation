;;; ============================================================================
;;; QIRRI - Sprinkler Placement Algorithms
;;; Automatic and manual placement functions
;;; Copyright (c) 2026 QTech Design - www.qtech.hr
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; Area Selection Command
;;; ----------------------------------------------------------------------------

(defun c:QIRRAREA (/ ent vertices area perimeter bounds)
  "Select and analyze irrigation area"
  (setq ent (qtech:select-closed-polyline "Select closed polyline for irrigation area: "))
  (if ent
    (progn
      ;; Store current area
      (setq *qtech-current-area* ent)
      
      ;; Get vertices
      (setq vertices (qtech:get-pline-vertices ent))
      
      ;; Calculate properties
      (setq area (qtech:pline-area ent))
      (setq perimeter (qtech:pline-length ent))
      (setq bounds (qtech:pline-bounds vertices))
      
      ;; Display info
      (princ "\n\n=== IRRIGATION AREA ANALYSIS ===\n")
      (princ (strcat "Vertices: " (itoa (length vertices)) "\n"))
      (princ (strcat "Area: " (rtos area 2 2) " m²\n"))
      (princ (strcat "Perimeter: " (rtos perimeter 2 2) " m\n"))
      (princ (strcat "Bounding box: " 
                     (rtos (- (car (cadr bounds)) (caar bounds)) 2 2) " x "
                     (rtos (- (cadr (cadr bounds)) (cadar bounds)) 2 2) " m\n"))
      
      ;; Store area data
      (setq *qtech-area-data* 
            (list (cons "entity" ent)
                  (cons "vertices" vertices)
                  (cons "area" area)
                  (cons "perimeter" perimeter)
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
                      all-heads total-flow settings area cu du)
  "Automatic sprinkler placement using hybrid greedy algorithm"
  
  ;; Check if area is selected
  (if (not *qtech-area-data*)
    (progn
      (princ "\nNo irrigation area selected. Run QIRRAREA first.")
      (princ)
      (exit)
    )
  )
  
  (setq vertices (cdr (assoc "vertices" *qtech-area-data*)))
  (setq bounds (cdr (assoc "bounds" *qtech-area-data*)))
  (setq area (cdr (assoc "area" *qtech-area-data*)))
  
  (princ "\n")
  (princ "╔══════════════════════════════════════════════════════════════╗\n")
  (princ "║        QTECH HYBRID GREEDY PLACEMENT ALGORITHM               ║\n")
  (princ "╚══════════════════════════════════════════════════════════════╝\n")
  
  ;; Get settings
  (setq settings *qtech-settings*)
  (setq offset-dist (cdr (assoc "perimeter-offset" settings)))
  
  ;; Generate simulation grid
  (princ "\nInitializing simulation grid...")
  (qtech:generate-simulation-grid vertices (cdr (assoc "grid-resolution" settings)))
  
  ;; Phase 1: Perimeter placement (edge handling critical for water savings)
  (princ "\n\n▶ PHASE 1: Perimeter-First Placement")
  (princ "\n  Goal: Minimize overspray at boundaries (saves 10-20% water)")
  (princ "\n  Strategy: Short-radius heads hugging edges...")
  (setq perimeter-heads (qtech:place-perimeter vertices offset-dist settings))
  (princ (strcat "\n  Result: " (itoa (length perimeter-heads)) " perimeter heads placed.\n"))
  
  ;; Phase 2: Interior puzzle-like infill
  (princ "\n▶ PHASE 2: Inward Puzzle-Like Infill")
  (princ "\n  Goal: Fill coverage gaps from perimeter inward")
  (princ "\n  Strategy: Concentric layers, largest gaps first...")
  (setq interior-heads (qtech:place-interior vertices perimeter-heads bounds settings))
  (princ (strcat "\n  Result: " (itoa (length interior-heads)) " interior heads placed.\n"))
  
  ;; Combine all heads
  (setq all-heads (append perimeter-heads interior-heads))
  (setq *qtech-placements* all-heads)
  
  ;; Calculate metrics
  (setq total-flow (qtech:list-sum (mapcar '(lambda (h) (cdr (assoc "flow" h))) all-heads)))
  
  ;; Run simulation for uniformity check
  (princ "\n▶ PHASE 3: Coverage Validation")
  (princ "\n  Running precipitation simulation...")
  (qtech:simulate-coverage all-heads)
  (setq metrics (qtech:calculate-uniformity *qtech-grid*))
  (setq cu (cdr (assoc "cu" metrics)))
  (setq du (cdr (assoc "du" metrics)))
  (setq coverage (cdr (assoc "coverage" metrics)))
  
  ;; Summary
  (princ "\n\n")
  (princ "╔══════════════════════════════════════════════════════════════╗\n")
  (princ "║                   PLACEMENT SUMMARY                          ║\n")
  (princ "╠══════════════════════════════════════════════════════════════╣\n")
  (princ (strcat "║  Area:              " (qtech:string-pad (strcat (rtos area 2 1) " m²") 20 " ") "              ║\n"))
  (princ (strcat "║  Total Heads:       " (qtech:string-pad (itoa (length all-heads)) 20 " ") "              ║\n"))
  (princ (strcat "║    - Perimeter:     " (qtech:string-pad (itoa (length perimeter-heads)) 20 " ") "              ║\n"))
  (princ (strcat "║    - Interior:      " (qtech:string-pad (itoa (length interior-heads)) 20 " ") "              ║\n"))
  (princ (strcat "║  Total Flow:        " (qtech:string-pad (strcat (rtos total-flow 2 3) " m³/h") 20 " ") "              ║\n"))
  (princ "╠══════════════════════════════════════════════════════════════╣\n")
  (princ (strcat "║  Uniformity CU:     " (qtech:string-pad (strcat (rtos cu 2 1) "% " (qtech:cu-rating cu)) 20 " ") "              ║\n"))
  (princ (strcat "║  Distribution DU:   " (qtech:string-pad (strcat (rtos du 2 1) "% " (qtech:du-rating du)) 20 " ") "              ║\n"))
  (princ (strcat "║  Coverage:          " (qtech:string-pad (strcat (rtos coverage 2 1) "%") 20 " ") "              ║\n"))
  (princ "╚══════════════════════════════════════════════════════════════╝\n")
  
  ;; Recommendations
  (if (< cu (cdr (assoc "target-cu" settings)))
    (princ "\n⚠ CU below target - recommend running QIRROPTIMIZE for GA refinement.\n")
  )
  
  ;; Draw heads
  (princ "\nDrawing sprinkler heads...")
  (qtech:draw-all-heads all-heads)
  (princ " Done.\n")
  
  (princ "\nNext steps:")
  (princ "\n  • QIRRPATTERN  - Visualize spray coverage")
  (princ "\n  • QIRROPTIMIZE - GA optimization for better uniformity")
  (princ "\n  • QIRRVALIDATE - Detailed uniformity analysis\n")
  
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; Perimeter Placement
;;; ----------------------------------------------------------------------------

(defun qtech:place-perimeter (vertices offset-dist settings / heads n i v0 v1 v2 
                                corner-arc edge-heads spacing nozzle pressure brand eff)
  "Place sprinklers around the perimeter"
  (setq heads '())
  (setq n (length vertices))
  (setq pressure (cdr (assoc "pressure" settings)))
  (setq brand (cdr (assoc "preferred-brand" settings)))
  (setq eff (cdr (assoc "efficiency-priority" settings)))
  (setq spacing-factor (cdr (assoc "spacing-factor" settings)))
  
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
        ;; Find best nozzle for corner
        (setq nozzle (qtech:select-best-nozzle 
                       3.0  ; Start with small radius for perimeter
                       (qtech:snap-arc corner-arc)
                       pressure
                       (cdr (assoc "target-precip" settings))
                       brand
                       eff))
        (if nozzle
          (setq heads (append heads 
                        (list (qtech:create-head-data 
                                (qtech:offset-corner v0 v1 v2 offset-dist)
                                nozzle
                                (qtech:snap-arc corner-arc)
                                "perimeter"))))
        )
      )
    )
    
    ;; Place edge heads between corners
    (setq edge-heads (qtech:place-edge-heads v1 v2 offset-dist settings))
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
  "Calculate offset position for corner head"
  (setq ang1 (qtech:angle-2d v1 v0))
  (setq ang2 (qtech:angle-2d v1 v2))
  (setq ang-bisect (/ (+ ang1 ang2) 2.0))
  ;; Offset inward along bisector
  (qtech:polar-point v1 ang-bisect dist)
)

(defun qtech:place-edge-heads (v1 v2 offset-dist settings / edge-len spacing n-heads
                                pts ang nozzle pressure brand eff heads)
  "Place sprinkler heads along an edge"
  (setq heads '())
  (setq edge-len (qtech:distance-2d v1 v2))
  (setq pressure (cdr (assoc "pressure" settings)))
  (setq brand (cdr (assoc "preferred-brand" settings)))
  (setq eff (cdr (assoc "efficiency-priority" settings)))
  (setq spacing-factor (cdr (assoc "spacing-factor" settings)))
  
  ;; Find appropriate nozzle for edge
  (setq nozzle (qtech:select-best-nozzle 
                 4.0  ; Typical edge head radius
                 180  ; Half-circle for edges
                 pressure
                 (cdr (assoc "target-precip" settings))
                 brand
                 eff))
  
  (if nozzle
    (progn
      ;; Calculate spacing based on nozzle radius
      (setq spacing (* (qtech:cat-radius nozzle) spacing-factor 2))
      
      ;; Number of heads needed (excluding corners)
      (setq n-heads (fix (/ edge-len spacing)))
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
            (setq offset-pt (qtech:polar-point edge-pt perp-ang offset-dist))
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
                              grid coverage heads uncovered nozzle)
  "Place interior sprinkler heads to fill coverage gaps"
  (setq heads '())
  
  ;; Generate coverage grid
  (setq grid-res (cdr (assoc "grid-resolution" settings)))
  (setq grid (qtech:generate-grid bounds grid-res))
  (setq grid (qtech:filter-points-in-polygon grid vertices))
  
  ;; Calculate initial coverage from perimeter heads
  (setq coverage (qtech:calculate-coverage grid perimeter-heads))
  
  ;; Find best nozzle for interior (full circle)
  (setq nozzle (qtech:select-best-nozzle 
                 5.0  ; Medium radius for interior
                 360  ; Full circle
                 (cdr (assoc "pressure" settings))
                 (cdr (assoc "target-precip" settings))
                 (cdr (assoc "preferred-brand" settings))
                 (cdr (assoc "efficiency-priority" settings))))
  
  (if nozzle
    (progn
      (setq spacing (* (qtech:cat-radius nozzle) 
                       (cdr (assoc "spacing-factor" settings)) 
                       2))
      
      ;; Iteratively place heads in uncovered areas
      (setq uncovered (qtech:find-uncovered-areas grid coverage 0.5))
      (setq iteration 0)
      (setq max-iterations 50)
      
      (while (and uncovered (< iteration max-iterations))
        ;; Find best position for new head
        (setq best-pos (qtech:find-best-interior-position 
                         uncovered vertices heads spacing))
        
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

(defun qtech:point-coverage (pt heads / total)
  "Calculate total precipitation at a point from all heads"
  (setq total 0.0)
  (foreach h heads
    (setq head-pos (cdr (assoc "position" h)))
    (setq radius (cdr (assoc "radius" h)))
    (setq precip (cdr (assoc "precip" h)))
    (setq arc (cdr (assoc "arc" h)))
    (setq dist (qtech:distance-2d pt head-pos))
    
    ;; Check if point is within range and arc
    (if (and (<= dist radius)
             (qtech:point-in-arc pt head-pos arc (cdr (assoc "rotation" h))))
      ;; Simple linear falloff model
      (setq total (+ total (* precip (- 1.0 (/ dist radius)))))
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

(defun qtech:find-best-interior-position (uncovered vertices existing-heads spacing / 
                                           best-pos best-score pt score)
  "Find the best position for a new interior head"
  (setq best-pos nil)
  (setq best-score -1)
  
  (foreach uc uncovered
    (setq pt (car uc))
    ;; Check minimum distance from existing heads
    (if (qtech:min-distance-ok pt existing-heads spacing)
      (progn
        ;; Score based on how many uncovered points it would cover
        (setq score (qtech:score-interior-position pt uncovered spacing))
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

(defun qtech:min-distance-ok (pt heads min-dist / ok)
  "Check if point maintains minimum distance from all existing heads"
  (setq ok T)
  (foreach h heads
    (if (< (qtech:distance-2d pt (cdr (assoc "position" h))) min-dist)
      (setq ok nil)
    )
  )
  ok
)

(defun qtech:score-interior-position (pt uncovered radius / count)
  "Score a potential interior position by coverage potential"
  (setq count 0)
  (foreach uc uncovered
    (if (<= (qtech:distance-2d pt (car uc)) radius)
      (setq count (1+ count))
    )
  )
  count
)

;;; ----------------------------------------------------------------------------
;;; Head Data Management
;;; ----------------------------------------------------------------------------

(defun qtech:create-head-data (position nozzle arc placement-type / data)
  "Create a head data association list"
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
    (cons "rotation" 0)  ; Rotation angle for arc orientation
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

(defun qtech:draw-head (head-data / pos radius arc symbol-size ang)
  "Draw a single sprinkler head with directional indicator"
  (setq pos (cdr (assoc "position" head-data)))
  (setq arc (cdr (assoc "arc" head-data)))
  (setq rotation (cdr (assoc "rotation" head-data)))
  (if (not rotation) (setq rotation 0))
  (setq symbol-size 0.15)  ; Symbol radius for display
  
  ;; Draw base circle for head location
  (command "._CIRCLE" pos symbol-size)
  
  ;; Add cross marker
  (command "._LINE" 
           (list (- (car pos) symbol-size) (cadr pos))
           (list (+ (car pos) symbol-size) (cadr pos))
           "")
  (command "._LINE"
           (list (car pos) (- (cadr pos) symbol-size))
           (list (car pos) (+ (cadr pos) symbol-size))
           "")
  
  ;; Add arc indicator for non-360 heads
  (if (/= arc 360)
    (progn
      (setq half-arc (/ arc 2.0))
      (setq start-ang (- rotation half-arc))
      (setq end-ang (+ rotation half-arc))
      
      ;; Draw small arc indicator
      (command "._ARC" "_C" pos
               (qtech:polar-point pos (qtech:degrees-to-radians start-ang) (* symbol-size 1.5))
               (qtech:polar-point pos (qtech:degrees-to-radians end-ang) (* symbol-size 1.5)))
    )
  )
  
  ;; Add label with arc degree
  (qtech:set-layer "IRR-TEXT")
  (command "._TEXT" 
           (list (+ (car pos) (* symbol-size 1.8)) (- (cadr pos) 0.1))
           0.15 0 
           (strcat (itoa arc) "°"))
  (qtech:set-layer "IRR-SPRINKLER")
)

(defun qtech:draw-head-with-attributes (head-data / pos)
  "Draw sprinkler head with full block attributes"
  (setq pos (cdr (assoc "position" head-data)))
  
  ;; Draw basic symbol
  (qtech:draw-head head-data)
  
  ;; Add attribute text (simplified - full would use INSERT with block)
  (qtech:set-layer "IRR-TEXT")
  (command "._TEXT"
           (list (car pos) (- (cadr pos) 0.4))
           0.12 0
           (strcat (cdr (assoc "model" head-data)) " " (cdr (assoc "nozzle" head-data))))
  (qtech:set-layer "IRR-SPRINKLER")
)

;;; ----------------------------------------------------------------------------
;;; Manual Placement Command
;;; ----------------------------------------------------------------------------

(defun c:QIRRMANUAL (/ pt nozzle arc radius model continue)
  "Manual sprinkler placement mode"
  (princ "\n\n=== MANUAL PLACEMENT MODE ===\n")
  (princ "Click to place sprinklers. Press ENTER to exit.\n")
  
  ;; Get nozzle selection
  (princ "\nEnter radius in meters [3.0]: ")
  (setq radius (getreal))
  (if (not radius) (setq radius 3.0))
  
  (princ "Enter arc angle (90/180/270/360) [360]: ")
  (setq arc (getint))
  (if (not arc) (setq arc 360))
  
  ;; Find matching nozzle
  (setq nozzle (qtech:select-best-nozzle 
                 radius arc
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
            
            (princ (strcat "Placed at (" 
                          (rtos (car pt) 2 2) ", " 
                          (rtos (cadr pt) 2 2) ")"))
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

