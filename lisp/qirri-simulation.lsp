;;; ============================================================================
;;; QIRRI - Simulation Grid & Uniformity Analysis
;;; Grid-based coverage simulation with CU/DU calculations
;;; OPTIMIZED for speed with progress indicators
;;; Copyright (c) 2026 QTech Design - www.qtech.hr
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; Global Simulation Variables
;;; ----------------------------------------------------------------------------

(setq *qtech-grid* nil)              ; List of grid points ((x y precip) ...)
(setq *qtech-grid-resolution* 1.0)   ; Grid spacing in METERS (1m default for speed)
(setq *qtech-sim-results* nil)       ; Last simulation results

;;; ----------------------------------------------------------------------------
;;; Grid Generation - OPTIMIZED
;;; ----------------------------------------------------------------------------

(defun qtech:generate-simulation-grid (vertices resolution-meters / bounds grid pts 
                                        resolution-dwg width height num-x num-y max-pts
                                        total-candidate)
  "Generate simulation grid inside polygon (resolution in METERS)"
  
  ;; Ensure units are detected
  (if (not *qirri-scale-factor*) (qtech:detect-units))
  
  ;; Convert resolution from meters to drawing units
  (setq resolution-dwg (qtech:from-meters resolution-meters))
  
  (setq *qtech-grid-resolution* resolution-meters)
  (setq bounds (qtech:pline-bounds vertices))
  
  ;; Calculate grid dimensions
  (setq width (- (caadr bounds) (caar bounds)))
  (setq height (- (cadadr bounds) (cadar bounds)))
  (setq num-x (fix (/ width resolution-dwg)))
  (setq num-y (fix (/ height resolution-dwg)))
  (setq total-candidate (* num-x num-y))
  
  (princ "\n\n=== SIMULATION GRID ===")
  (princ (strcat "\nResolution: " (rtos resolution-meters 2 2) " m"
                 " = " (rtos resolution-dwg 2 1) " " *qirri-units*))
  (princ (strcat "\nArea size: " (rtos (qtech:to-meters width) 2 2) " x " 
                 (rtos (qtech:to-meters height) 2 2) " m"))
  (princ (strcat "\nGrid estimate: " (itoa num-x) " x " (itoa num-y) 
                 " = " (itoa total-candidate) " points"))
  
  ;; STRICT limit - 10k points max for responsiveness
  (setq max-pts 10000)
  (if (> total-candidate max-pts)
    (progn
      (princ (strcat "\n\n** Grid too large! Adjusting resolution..."))
      ;; Calculate required resolution
      (while (> (* num-x num-y) max-pts)
        (setq resolution-meters (* resolution-meters 1.5))
        (setq resolution-dwg (qtech:from-meters resolution-meters))
        (setq num-x (fix (/ width resolution-dwg)))
        (setq num-y (fix (/ height resolution-dwg)))
      )
      (princ (strcat "\n   New resolution: " (rtos resolution-meters 2 2) " m"))
      (princ (strcat "\n   New grid: " (itoa (* num-x num-y)) " points"))
      (setq *qtech-grid-resolution* resolution-meters)
    )
  )
  
  (princ "\n\nGenerating grid points...")
  
  ;; Generate candidate points (fast)
  (setq pts (qtech:generate-grid-fast bounds resolution-dwg))
  (princ (strcat " " (itoa (length pts)) " candidates"))
  
  (princ "\nFiltering to polygon (please wait)...")
  
  ;; Filter to points inside polygon (slower, show progress)
  (setq grid (qtech:filter-points-in-polygon-progress pts vertices))
  
  ;; Add precipitation field
  (setq *qtech-grid* (mapcar '(lambda (pt) (list (car pt) (cadr pt) 0.0)) grid))
  
  (princ (strcat "\nGrid ready: " (itoa (length *qtech-grid*)) " points inside polygon."))
  *qtech-grid*
)

(defun qtech:generate-grid-fast (bounds resolution / minpt maxpt x y pts row-pts)
  "Fast grid generation without append overhead"
  (setq minpt (car bounds) maxpt (cadr bounds))
  (setq pts '())
  (setq y (cadr minpt))
  (while (<= y (cadr maxpt))
    (setq x (car minpt))
    (setq row-pts '())
    (while (<= x (car maxpt))
      (setq row-pts (cons (list x y) row-pts))
      (setq x (+ x resolution))
    )
    (setq pts (append (reverse row-pts) pts))
    (setq y (+ y resolution))
  )
  (reverse pts)
)

(defun qtech:filter-points-in-polygon-progress (pts vertices / result count total pct last-pct)
  "Filter points with progress indicator"
  (setq result '())
  (setq count 0)
  (setq total (length pts))
  (setq last-pct -1)
  
  (foreach pt pts
    ;; Progress every 10%
    (setq pct (fix (* 100 (/ (float count) total))))
    (if (and (= (rem pct 10) 0) (/= pct last-pct))
      (progn
        (princ (strcat " " (itoa pct) "%"))
        (setq last-pct pct)
      )
    )
    
    ;; Test point
    (if (qtech:point-in-polygon pt vertices)
      (setq result (cons pt result))
    )
    (setq count (1+ count))
  )
  (princ " 100%")
  (reverse result)
)

(defun c:QIRRGRID (/ vertices res)
  "Display simulation grid"
  (if (not *qtech-area-data*)
    (progn (princ "\nNo area selected. Run QIRRAREA first.") (princ) (exit))
  )
  
  (setq vertices (cdr (assoc "vertices" *qtech-area-data*)))
  
  ;; Ask for resolution
  (princ (strcat "\nCurrent resolution: " (rtos *qtech-grid-resolution* 2 2) " m"))
  (setq res (getreal "\nEnter grid resolution in meters [1.0]: "))
  (if (not res) (setq res 1.0))
  (if (< res 0.25) (setq res 0.25))  ; Min 25cm
  
  ;; Generate grid
  (qtech:generate-simulation-grid vertices res)
  
  (if (and *qtech-grid* (> (length *qtech-grid*) 0))
    (progn
      ;; Draw grid points
      (qtech:set-layer "IRR-GRID")
      (princ "\nDrawing grid points...")
      (setvar "PDMODE" 3)
      (setvar "PDSIZE" (qtech:from-meters 0.1))
      
      (setq count 0)
      (foreach gp *qtech-grid*
        (command "._POINT" (list (car gp) (cadr gp)))
        (setq count (1+ count))
        (if (= (rem count 500) 0)
          (princ ".")
        )
      )
      
      (princ (strcat " Done! " (itoa (length *qtech-grid*)) " points.\n"))
    )
    (princ "\nNo grid points generated.")
  )
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; Precipitation Simulation - SIMPLIFIED
;;; ----------------------------------------------------------------------------

(defun qtech:simulate-coverage (heads / grid-copy count)
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
  
  (princ (strcat "\nSimulating " (itoa (length heads)) " heads on " 
                 (itoa (length *qtech-grid*)) " grid points..."))
  
  ;; Reset grid precipitation
  (setq grid-copy (mapcar 
    '(lambda (gp) (list (car gp) (cadr gp) 0.0))
    *qtech-grid*
  ))
  
  ;; Accumulate precipitation from each head
  (setq count 0)
  (foreach head heads
    (setq grid-copy (qtech:add-head-precipitation-fast grid-copy head))
    (setq count (1+ count))
    (princ ".")
  )
  
  (setq *qtech-grid* grid-copy)
  (princ " Done.")
  grid-copy
)

(defun qtech:add-head-precipitation-fast (grid head / pos radius-m radius-dwg precip)
  "Add precipitation - optimized version"
  (setq pos (cdr (assoc "position" head)))
  (setq radius-m (cdr (assoc "radius" head)))    ; in meters
  (setq radius-dwg (qtech:from-meters radius-m))
  (setq precip (cdr (assoc "precip" head)))
  (setq arc (cdr (assoc "arc" head)))
  (setq rotation (cdr (assoc "rotation" head)))
  (if (not rotation) (setq rotation 0))
  
  ;; Pre-calculate squared radius for faster distance check
  (setq radius-sq (* radius-dwg radius-dwg))
  
  (mapcar 
    '(lambda (gp / dx dy dist-sq contrib)
       (setq dx (- (car gp) (car pos)))
       (setq dy (- (cadr gp) (cadr pos)))
       (setq dist-sq (+ (* dx dx) (* dy dy)))
       
       ;; Quick distance check (squared, no sqrt needed)
       (if (and (<= dist-sq radius-sq)
                (or (= arc 360)
                    (qtech:point-in-spray-arc-fast (car gp) (cadr gp) pos arc rotation)))
         (progn
           ;; Radial decay
           (setq contrib (* precip (- 1.0 (/ dist-sq radius-sq))))
           (list (car gp) (cadr gp) (+ (caddr gp) contrib))
         )
         gp
       )
    )
    grid
  )
)

(defun qtech:point-in-spray-arc-fast (px py center arc rotation / pt-ang half-arc 
                                       start-ang end-ang)
  "Fast arc check"
  (if (= arc 360)
    T
    (progn
      (setq pt-ang (qtech:radians-to-degrees 
                    (qtech:normalize-angle (atan (- py (cadr center)) (- px (car center))))))
      (setq half-arc (/ arc 2.0))
      (setq start-ang (qtech:normalize-angle-deg (- rotation half-arc)))
      (setq end-ang (qtech:normalize-angle-deg (+ rotation half-arc)))
      (qtech:angle-in-range pt-ang start-ang end-ang)
    )
  )
)

(defun qtech:point-in-spray-arc (pt center arc rotation)
  "Check if point is within sprinkler spray arc"
  (qtech:point-in-spray-arc-fast (car pt) (cadr pt) center arc rotation)
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
                                    sorted low-quarter lq-count lq-sum du covered-count coverage)
  "Calculate CU and DU from grid precipitation data"
  
  (princ "\nCalculating uniformity...")
  
  ;; Extract non-zero precipitation values
  (setq precip-values (vl-remove-if 
    '(lambda (p) (<= p 0))
    (mapcar 'caddr grid)
  ))
  
  (if (not precip-values)
    (progn
      (princ " No precipitation data.")
      (list (cons "cu" 0) (cons "du" 0) (cons "coverage" 0))
    )
    (progn
      (setq n (length precip-values))
      (setq total (apply '+ precip-values))
      (setq avg (/ total (float n)))
      
      ;; CU
      (setq deviations (apply '+ (mapcar '(lambda (p) (abs (- p avg))) precip-values)))
      (setq cu (* 100.0 (- 1.0 (/ deviations (* n avg)))))
      
      ;; DU
      (setq sorted (vl-sort precip-values '<))
      (setq lq-count (max 1 (fix (/ n 4.0))))
      (setq lq-sum (apply '+ (qtech:list-take sorted lq-count)))
      (setq du (* 100.0 (/ (/ lq-sum lq-count) avg)))
      
      ;; Coverage
      (setq covered-count (length precip-values))
      (setq coverage (* 100.0 (/ (float covered-count) (length grid))))
      
      (princ " Done.")
      
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
  "Validate current design"
  
  (if (not *qtech-placements*)
    (progn (princ "\nNo sprinklers placed. Run QIRRPLACE first.") (princ) (exit))
  )
  
  (princ "\n\n=== UNIFORMITY VALIDATION ===")
  
  ;; Run simulation
  (qtech:simulate-coverage *qtech-placements*)
  
  ;; Calculate uniformity
  (setq results (qtech:calculate-uniformity *qtech-grid*))
  (setq *qtech-sim-results* results)
  
  (setq cu (cdr (assoc "cu" results)))
  (setq du (cdr (assoc "du" results)))
  (setq coverage (cdr (assoc "coverage" results)))
  
  ;; Display
  (princ "\n")
  (princ "\n+------------------------------------------+")
  (princ "\n|        UNIFORMITY RESULTS                |")
  (princ "\n+------------------------------------------+")
  (princ (strcat "\n|  CU: " (rtos cu 2 1) "% " (qtech:cu-rating cu)))
  (princ (strcat "\n|  DU: " (rtos du 2 1) "% " (qtech:du-rating du)))
  (princ (strcat "\n|  Coverage: " (rtos coverage 2 1) "%"))
  (princ (strcat "\n|  Avg precip: " (rtos (cdr (assoc "avg-precip" results)) 2 1) " mm/hr"))
  (princ "\n+------------------------------------------+")
  
  (if (and (>= cu 90) (>= du 85))
    (princ "\n\n>> PASS: Design meets targets!")
    (princ "\n\n>> Consider running QIRROPTIMIZE")
  )
  
  (princ "\n")
  (princ)
)

(defun qtech:cu-rating (cu)
  (cond ((>= cu 95) "EXCELLENT") ((>= cu 90) "GOOD") ((>= cu 85) "FAIR") (T "POOR"))
)

(defun qtech:du-rating (du)
  (cond ((>= du 90) "EXCELLENT") ((>= du 85) "GOOD") ((>= du 75) "FAIR") (T "POOR"))
)

;;; ----------------------------------------------------------------------------
;;; Coverage Heatmap - SIMPLIFIED
;;; ----------------------------------------------------------------------------

(defun c:QIRRCOVERAGE (/ max-precip count)
  "Draw coverage heatmap"
  
  (if (not *qtech-grid*)
    (progn (princ "\nNo simulation data. Run QIRRVALIDATE first.") (princ) (exit))
  )
  
  (princ "\nDrawing heatmap...")
  
  (setq max-precip (apply 'max (mapcar 'caddr *qtech-grid*)))
  (if (<= max-precip 0) (setq max-precip 1.0))
  
  (setvar "PDMODE" 0)
  (setvar "PDSIZE" (qtech:from-meters (* *qtech-grid-resolution* 0.8)))
  
  (setq count 0)
  (foreach gp *qtech-grid*
    (qtech:draw-coverage-point gp max-precip)
    (setq count (1+ count))
    (if (= (rem count 200) 0) (princ "."))
  )
  
  (princ " Done.\n")
  (princ)
)

(defun qtech:draw-coverage-point (gp max-precip / pt precip color)
  (setq pt (list (car gp) (cadr gp)))
  (setq precip (caddr gp))
  (setq color (qtech:precip-to-color precip max-precip))
  (command "._COLOR" color)
  (command "._POINT" pt)
  (command "._COLOR" "BYLAYER")
)

(defun qtech:precip-to-color (precip max-precip / ratio)
  (setq ratio (/ precip max-precip))
  (cond
    ((<= ratio 0.0)  252)
    ((<= ratio 0.2)  1)
    ((<= ratio 0.4)  30)
    ((<= ratio 0.6)  2)
    ((<= ratio 0.8)  3)
    ((<= ratio 1.0)  4)
    (T               5)
  )
)

;;; ----------------------------------------------------------------------------
;;; Gap Analysis
;;; ----------------------------------------------------------------------------

(defun qtech:find-coverage-gaps (grid threshold)
  (vl-remove-if-not '(lambda (gp) (< (caddr gp) threshold)) grid)
)

(defun qtech:find-largest-gap (gaps / cx cy)
  (if gaps
    (progn
      (setq cx (qtech:list-avg (mapcar 'car gaps)))
      (setq cy (qtech:list-avg (mapcar 'cadr gaps)))
      (list cx cy (length gaps))
    )
  )
)

(defun qtech:calculate-gap-score (position gaps radius)
  (length (vl-remove-if-not
    '(lambda (gp) (<= (qtech:distance-2d position (list (car gp) (cadr gp))) radius))
    gaps
  ))
)

;;; ============================================================================

(princ "\n  qirri-simulation.lsp loaded")
(princ)
