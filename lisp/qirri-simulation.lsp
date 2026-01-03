;;; ============================================================================
;;; QTECH DESIGN 2026 - Simulation Grid & Uniformity Analysis
;;; Grid-based coverage simulation with CU/DU calculations
;;; Copyright (c) 2026 QTech Design - All Rights Reserved
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; Global Simulation Variables
;;; ----------------------------------------------------------------------------

(setq *qtech-grid* nil)              ; List of grid points ((x y precip) ...)
(setq *qtech-grid-resolution* 0.5)   ; Grid spacing in meters
(setq *qtech-sim-results* nil)       ; Last simulation results

;;; ----------------------------------------------------------------------------
;;; Grid Generation
;;; ----------------------------------------------------------------------------

(defun qtech:generate-simulation-grid (vertices resolution / bounds grid pts)
  "Generate dense simulation grid inside polygon"
  (princ (strcat "\nGenerating simulation grid (res=" (rtos resolution 2 2) "m)..."))
  
  (setq *qtech-grid-resolution* resolution)
  (setq bounds (qtech:pline-bounds vertices))
  
  ;; Generate candidate points
  (setq pts (qtech:generate-grid bounds resolution))
  
  ;; Filter to points inside polygon
  (setq grid (mapcar 
    '(lambda (pt) 
       (list (car pt) (cadr pt) 0.0))  ; (x y precip)
    (qtech:filter-points-in-polygon pts vertices)
  ))
  
  (setq *qtech-grid* grid)
  (princ (strcat " " (itoa (length grid)) " points created.\n"))
  grid
)

(defun c:QIRRGRID (/ vertices)
  "Display simulation grid"
  (if (not *qtech-area-data*)
    (progn (princ "\nNo area selected. Run QIRRAREA first.") (princ) (exit))
  )
  
  (setq vertices (cdr (assoc "vertices" *qtech-area-data*)))
  
  ;; Generate grid if not exists
  (if (not *qtech-grid*)
    (qtech:generate-simulation-grid vertices *qtech-grid-resolution*)
  )
  
  ;; Draw grid points
  (qtech:set-layer "IRR-GRID")
  (princ "\nDrawing grid...")
  (setvar "PDMODE" 3)
  (setvar "PDSIZE" 0.1)
  
  (foreach gp *qtech-grid*
    (command "._POINT" (list (car gp) (cadr gp)))
  )
  
  (princ (strcat " " (itoa (length *qtech-grid*)) " points drawn.\n"))
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; Precipitation Simulation
;;; ----------------------------------------------------------------------------

(defun qtech:simulate-coverage (heads / grid-copy)
  "Simulate precipitation on grid from all heads"
  (if (not *qtech-grid*)
    (progn
      (if *qtech-area-data*
        (qtech:generate-simulation-grid 
          (cdr (assoc "vertices" *qtech-area-data*)) 
          *qtech-grid-resolution*)
        (progn (princ "\nNo grid available.") (exit))
      )
    )
  )
  
  ;; Reset grid precipitation
  (setq grid-copy (mapcar 
    '(lambda (gp) (list (car gp) (cadr gp) 0.0))
    *qtech-grid*
  ))
  
  ;; Accumulate precipitation from each head
  (foreach head heads
    (setq grid-copy (qtech:add-head-precipitation grid-copy head))
  )
  
  (setq *qtech-grid* grid-copy)
  grid-copy
)

(defun qtech:add-head-precipitation (grid head / pos radius precip arc rotation)
  "Add precipitation contribution from a single head to grid"
  (setq pos (cdr (assoc "position" head)))
  (setq radius (cdr (assoc "radius" head)))
  (setq precip (cdr (assoc "precip" head)))
  (setq arc (cdr (assoc "arc" head)))
  (setq rotation (cdr (assoc "rotation" head)))
  (if (not rotation) (setq rotation 0))
  
  (mapcar 
    '(lambda (gp / pt dist contrib)
       (setq pt (list (car gp) (cadr gp)))
       (setq dist (qtech:distance-2d pt pos))
       
       (if (and (<= dist radius)
                (qtech:point-in-spray-arc pt pos arc rotation))
         (progn
           ;; Radial decay model: precip decreases quadratically
           (setq contrib (* precip (- 1.0 (expt (/ dist radius) 2))))
           (list (car gp) (cadr gp) (+ (caddr gp) contrib))
         )
         gp  ; No change if outside spray area
       )
    )
    grid
  )
)

(defun qtech:point-in-spray-arc (pt center arc rotation / ang pt-ang half-arc 
                                  start-ang end-ang)
  "Check if point is within sprinkler spray arc"
  (if (= arc 360)
    T
    (progn
      (setq pt-ang (qtech:radians-to-degrees 
                    (qtech:normalize-angle (qtech:angle-2d center pt))))
      (setq half-arc (/ arc 2.0))
      (setq start-ang (qtech:normalize-angle-deg (- rotation half-arc)))
      (setq end-ang (qtech:normalize-angle-deg (+ rotation half-arc)))
      
      (qtech:angle-in-range pt-ang start-ang end-ang)
    )
  )
)

(defun qtech:normalize-angle-deg (ang)
  "Normalize angle to 0-360 range"
  (while (< ang 0) (setq ang (+ ang 360)))
  (while (>= ang 360) (setq ang (- ang 360)))
  ang
)

(defun qtech:angle-in-range (ang start end)
  "Check if angle is within arc range (handles wrap-around)"
  (if (<= start end)
    (and (>= ang start) (<= ang end))
    (or (>= ang start) (<= ang end))
  )
)

;;; ----------------------------------------------------------------------------
;;; Uniformity Calculations
;;; ----------------------------------------------------------------------------

(defun qtech:calculate-uniformity (grid / precip-values n total avg deviations cu 
                                    sorted low-quarter lq-count lq-sum du)
  "Calculate CU and DU from grid precipitation data"
  
  ;; Extract non-zero precipitation values
  (setq precip-values (vl-remove-if 
    '(lambda (p) (<= p 0))
    (mapcar 'caddr grid)
  ))
  
  (if (not precip-values)
    (progn
      (princ "\nNo precipitation data in grid.")
      (list (cons "cu" 0) (cons "du" 0) (cons "coverage" 0))
    )
    (progn
      (setq n (length precip-values))
      (setq total (apply '+ precip-values))
      (setq avg (/ total (float n)))
      
      ;; Christiansen's Uniformity Coefficient (CU)
      ;; CU = 100 * (1 - Σ|depth_i - avg| / (n * avg))
      (setq deviations (apply '+ (mapcar '(lambda (p) (abs (- p avg))) precip-values)))
      (setq cu (* 100.0 (- 1.0 (/ deviations (* n avg)))))
      
      ;; Distribution Uniformity (DU) - Low Quarter Method
      ;; DU = 100 * (low_quarter_avg / overall_avg)
      (setq sorted (vl-sort precip-values '<))
      (setq lq-count (max 1 (fix (/ n 4.0))))
      (setq lq-sum (apply '+ (qtech:list-take sorted lq-count)))
      (setq du (* 100.0 (/ (/ lq-sum lq-count) avg)))
      
      ;; Coverage percentage
      (setq covered-count (length (vl-remove-if '(lambda (gp) (<= (caddr gp) 0)) grid)))
      (setq coverage (* 100.0 (/ (float covered-count) (length grid))))
      
      (list 
        (cons "cu" (qtech:clamp cu 0 100))
        (cons "du" (qtech:clamp du 0 100))
        (cons "avg-precip" avg)
        (cons "min-precip" (car sorted))
        (cons "max-precip" (nth (1- n) sorted))
        (cons "coverage" coverage)
        (cons "grid-points" (length grid))
        (cons "covered-points" covered-count)
      )
    )
  )
)

(defun c:QIRRVALIDATE (/ results cu du coverage)
  "Validate current design - calculate and display uniformity metrics"
  
  (if (not *qtech-placements*)
    (progn (princ "\nNo sprinklers placed.") (princ) (exit))
  )
  
  (princ "\n\n=== UNIFORMITY VALIDATION ===\n")
  (princ "Simulating coverage...")
  
  ;; Run simulation
  (qtech:simulate-coverage *qtech-placements*)
  
  ;; Calculate uniformity
  (setq results (qtech:calculate-uniformity *qtech-grid*))
  (setq *qtech-sim-results* results)
  
  (setq cu (cdr (assoc "cu" results)))
  (setq du (cdr (assoc "du" results)))
  (setq coverage (cdr (assoc "coverage" results)))
  
  ;; Display results
  (princ "\n")
  (princ "╔═══════════════════════════════════════════════════════╗\n")
  (princ "║              UNIFORMITY ANALYSIS RESULTS              ║\n")
  (princ "╠═══════════════════════════════════════════════════════╣\n")
  (princ (strcat "║  Christiansen's Uniformity (CU): " 
                 (qtech:string-pad (strcat (rtos cu 2 1) "%") 8 " ")
                 (qtech:cu-rating cu) "    ║\n"))
  (princ (strcat "║  Distribution Uniformity (DU):   " 
                 (qtech:string-pad (strcat (rtos du 2 1) "%") 8 " ")
                 (qtech:du-rating du) "    ║\n"))
  (princ (strcat "║  Area Coverage:                  " 
                 (qtech:string-pad (strcat (rtos coverage 2 1) "%") 8 " ")
                 "             ║\n"))
  (princ "╠═══════════════════════════════════════════════════════╣\n")
  (princ (strcat "║  Average Precipitation: " 
                 (qtech:string-pad (strcat (rtos (cdr (assoc "avg-precip" results)) 2 1) " mm/hr") 14 " ")
                 "            ║\n"))
  (princ (strcat "║  Min Precipitation:     " 
                 (qtech:string-pad (strcat (rtos (cdr (assoc "min-precip" results)) 2 1) " mm/hr") 14 " ")
                 "            ║\n"))
  (princ (strcat "║  Max Precipitation:     " 
                 (qtech:string-pad (strcat (rtos (cdr (assoc "max-precip" results)) 2 1) " mm/hr") 14 " ")
                 "            ║\n"))
  (princ "╠═══════════════════════════════════════════════════════╣\n")
  (princ (strcat "║  Grid Points: " (itoa (cdr (assoc "grid-points" results)))
                 "  |  Covered: " (itoa (cdr (assoc "covered-points" results)))
                 "                    ║\n"))
  (princ "╚═══════════════════════════════════════════════════════╝\n")
  
  ;; Recommendations
  (if (< cu 90)
    (princ "\n⚠ WARNING: CU below 90% target. Consider running QIRROPTIMIZE.\n")
  )
  (if (< du 85)
    (princ "\n⚠ WARNING: DU below 85% target. Check for dry spots.\n")
  )
  (if (< coverage 95)
    (princ "\n⚠ WARNING: Coverage below 95%. Add more heads or adjust spacing.\n")
  )
  
  (if (and (>= cu 90) (>= du 85) (>= coverage 95))
    (princ "\n✓ Design meets all uniformity targets!\n")
  )
  
  (princ)
)

(defun qtech:cu-rating (cu)
  "Get rating string for CU value"
  (cond
    ((>= cu 95) "EXCELLENT")
    ((>= cu 90) "GOOD     ")
    ((>= cu 85) "FAIR     ")
    ((>= cu 80) "POOR     ")
    (T          "CRITICAL ")
  )
)

(defun qtech:du-rating (du)
  "Get rating string for DU value"
  (cond
    ((>= du 90) "EXCELLENT")
    ((>= du 85) "GOOD     ")
    ((>= du 75) "FAIR     ")
    ((>= du 65) "POOR     ")
    (T          "CRITICAL ")
  )
)

;;; ----------------------------------------------------------------------------
;;; Coverage Heatmap Visualization
;;; ----------------------------------------------------------------------------

(defun c:QIRRCOVERAGE (/ grid max-precip)
  "Draw coverage heatmap"
  
  (if (not *qtech-grid*)
    (progn (princ "\nNo simulation data. Run QIRRVALIDATE first.") (princ) (exit))
  )
  
  (princ "\nDrawing coverage heatmap...")
  
  ;; Find max precipitation for normalization
  (setq max-precip (apply 'max (mapcar 'caddr *qtech-grid*)))
  (if (<= max-precip 0) (setq max-precip 1.0))
  
  (setvar "PDMODE" 0)
  (setvar "PDSIZE" (* *qtech-grid-resolution* 0.8))
  
  (foreach gp *qtech-grid*
    (qtech:draw-coverage-point gp max-precip)
  )
  
  ;; Draw legend
  (qtech:draw-coverage-legend max-precip)
  
  (princ " Done.\n")
  (princ)
)

(defun qtech:draw-coverage-point (gp max-precip / pt precip color)
  "Draw a single coverage point with color coding"
  (setq pt (list (car gp) (cadr gp)))
  (setq precip (caddr gp))
  (setq color (qtech:precip-to-color precip max-precip))
  
  (command "._COLOR" color)
  (command "._POINT" pt)
  (command "._COLOR" "BYLAYER")
)

(defun qtech:precip-to-color (precip max-precip / ratio)
  "Convert precipitation value to AutoCAD color index"
  (setq ratio (/ precip max-precip))
  (cond
    ((<= ratio 0.0)  252)   ; Dark gray - no coverage
    ((<= ratio 0.2)  1)     ; Red - very low
    ((<= ratio 0.4)  30)    ; Orange - low
    ((<= ratio 0.6)  2)     ; Yellow - medium
    ((<= ratio 0.8)  3)     ; Green - good
    ((<= ratio 1.0)  4)     ; Cyan - optimal
    (T               5)     ; Blue - over-watered
  )
)

(defun qtech:draw-coverage-legend (max-precip / insert-pt y-off)
  "Draw coverage heatmap legend"
  (setq insert-pt (getvar "VIEWCTR"))
  (setq insert-pt (list (+ (car insert-pt) 20) (+ (cadr insert-pt) 10)))
  
  (qtech:set-layer "IRR-TEXT")
  (command "._TEXT" insert-pt 0.5 0 "COVERAGE LEGEND")
  
  (setq y-off -1.0)
  (foreach item '((252 "No coverage") (1 "Critical (<20%)") (30 "Low (20-40%)")
                  (2 "Medium (40-60%)") (3 "Good (60-80%)") (4 "Optimal (80-100%)")
                  (5 "Over-watered (>100%)"))
    (command "._COLOR" (car item))
    (command "._CIRCLE" (list (car insert-pt) (+ (cadr insert-pt) y-off)) 0.3)
    (command "._COLOR" "BYLAYER")
    (command "._TEXT" (list (+ (car insert-pt) 0.8) (+ (cadr insert-pt) y-off -0.15))
             0.35 0 (cadr item))
    (setq y-off (- y-off 1.0))
  )
)

;;; ----------------------------------------------------------------------------
;;; Gap Analysis
;;; ----------------------------------------------------------------------------

(defun qtech:find-coverage-gaps (grid threshold / gaps)
  "Find grid points with coverage below threshold"
  (setq gaps (vl-remove-if-not
    '(lambda (gp) (< (caddr gp) threshold))
    grid
  ))
  gaps
)

(defun qtech:find-largest-gap (gaps / clusters largest)
  "Find the largest continuous gap region"
  (if (not gaps)
    nil
    (progn
      ;; Simple approach: find centroid of gap points
      (setq cx (qtech:list-avg (mapcar 'car gaps)))
      (setq cy (qtech:list-avg (mapcar 'cadr gaps)))
      (list cx cy (length gaps))
    )
  )
)

(defun qtech:calculate-gap-score (position gaps radius)
  "Calculate how many gap points would be covered by placing head at position"
  (length (vl-remove-if-not
    '(lambda (gp)
       (<= (qtech:distance-2d position (list (car gp) (cadr gp))) radius))
    gaps
  ))
)

;;; ----------------------------------------------------------------------------
;;; Target Precipitation Analysis
;;; ----------------------------------------------------------------------------

(defun qtech:analyze-precip-match (grid target-precip tolerance / in-range low high)
  "Analyze how well precipitation matches target"
  (setq in-range 0 low 0 high 0)
  (setq min-ok (* target-precip (- 1.0 tolerance)))
  (setq max-ok (* target-precip (+ 1.0 tolerance)))
  
  (foreach gp grid
    (setq p (caddr gp))
    (cond
      ((< p min-ok) (setq low (1+ low)))
      ((> p max-ok) (setq high (1+ high)))
      (T (setq in-range (1+ in-range)))
    )
  )
  
  (list 
    (cons "in-range" in-range)
    (cons "under" low)
    (cons "over" high)
    (cons "match-pct" (* 100.0 (/ (float in-range) (length grid))))
  )
)

;;; ============================================================================
;;; End of qtech-simulation.lsp
;;; ============================================================================

(princ)

