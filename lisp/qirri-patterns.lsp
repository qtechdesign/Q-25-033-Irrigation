;;; ============================================================================
;;; QTECH IRRIGATION - Spray Pattern Visualization
;;; Draw spray patterns (circles, arcs) for sprinkler heads
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; Pattern Drawing Command
;;; ----------------------------------------------------------------------------

(defun c:QIRRPATTERN (/ mode)
  "Draw spray patterns for placed sprinklers"
  
  (if (not *qtech-placements*)
    (progn
      (princ "\nNo sprinklers placed. Run QIRRPLACE first.")
      (princ)
      (exit)
    )
  )
  
  (princ "\n\n=== SPRAY PATTERN VISUALIZATION ===\n")
  (princ "Options:\n")
  (princ "  1. Draw all patterns\n")
  (princ "  2. Draw perimeter only\n")
  (princ "  3. Draw interior only\n")
  (princ "  4. Clear existing patterns\n")
  (princ "  5. Cancel\n")
  
  (setq mode (getint "\nSelect option [1-5]: "))
  
  (cond
    ((= mode 1) (qtech:draw-all-patterns *qtech-placements*))
    ((= mode 2) (qtech:draw-patterns-by-type *qtech-placements* "perimeter"))
    ((= mode 3) (qtech:draw-patterns-by-type *qtech-placements* "interior"))
    ((= mode 4) (qtech:clear-patterns))
    (T (princ "\nCancelled."))
  )
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; Pattern Drawing Functions
;;; ----------------------------------------------------------------------------

(defun qtech:draw-all-patterns (heads / count)
  "Draw spray patterns for all heads"
  (princ "\nDrawing spray patterns...")
  (qtech:set-layer "IRR-PATTERN")
  (setq count 0)
  
  (foreach h heads
    (qtech:draw-single-pattern h)
    (setq count (1+ count))
  )
  
  (princ (strcat " " (itoa count) " patterns drawn.\n"))
)

(defun qtech:draw-patterns-by-type (heads ptype / filtered count)
  "Draw patterns for specific placement type"
  (setq filtered (vl-remove-if-not
                   '(lambda (h) (= (cdr (assoc "type" h)) ptype))
                   heads))
  
  (if filtered
    (progn
      (princ (strcat "\nDrawing " ptype " patterns..."))
      (qtech:set-layer "IRR-PATTERN")
      (setq count 0)
      
      (foreach h filtered
        (qtech:draw-single-pattern h)
        (setq count (1+ count))
      )
      
      (princ (strcat " " (itoa count) " patterns drawn.\n"))
    )
    (princ (strcat "\nNo " ptype " heads found.\n"))
  )
)

(defun qtech:draw-single-pattern (head-data / pos radius arc rotation)
  "Draw spray pattern for a single head"
  (setq pos (cdr (assoc "position" head-data)))
  (setq radius (cdr (assoc "radius" head-data)))
  (setq arc (cdr (assoc "arc" head-data)))
  (setq rotation (cdr (assoc "rotation" head-data)))
  (if (not rotation) (setq rotation 0))
  
  (if (= arc 360)
    ;; Full circle
    (qtech:draw-circle-pattern pos radius)
    ;; Arc
    (qtech:draw-arc-pattern pos radius arc rotation)
  )
)

(defun qtech:draw-circle-pattern (center radius)
  "Draw a full circle spray pattern"
  (command "._CIRCLE" center radius)
)

(defun qtech:draw-arc-pattern (center radius arc rotation / start-ang end-ang)
  "Draw an arc spray pattern"
  ;; Calculate start and end angles
  (setq half-arc (/ arc 2.0))
  (setq start-ang (- rotation half-arc))
  (setq end-ang (+ rotation half-arc))
  
  ;; Draw arc
  (command "._ARC" "_C" center
           (qtech:polar-point center (qtech:degrees-to-radians start-ang) radius)
           (qtech:polar-point center (qtech:degrees-to-radians end-ang) radius))
  
  ;; Draw radius lines to show coverage area
  (command "._LINE" center
           (qtech:polar-point center (qtech:degrees-to-radians start-ang) radius)
           "")
  (command "._LINE" center
           (qtech:polar-point center (qtech:degrees-to-radians end-ang) radius)
           "")
)

;;; ----------------------------------------------------------------------------
;;; Pattern Style Options
;;; ----------------------------------------------------------------------------

(defun qtech:draw-pattern-filled (head-data / pos radius arc rotation)
  "Draw filled/hatched spray pattern"
  (setq pos (cdr (assoc "position" head-data)))
  (setq radius (cdr (assoc "radius" head-data)))
  (setq arc (cdr (assoc "arc" head-data)))
  (setq rotation (cdr (assoc "rotation" head-data)))
  (if (not rotation) (setq rotation 0))
  
  ;; Create boundary and hatch
  (if (= arc 360)
    (progn
      (command "._CIRCLE" pos radius)
      ;; Could add hatch here if desired
    )
    (progn
      ;; Create closed polyline for arc sector
      (qtech:draw-arc-sector pos radius arc rotation)
    )
  )
)

(defun qtech:draw-arc-sector (center radius arc rotation / pts start-ang end-ang 
                                step n-pts i ang)
  "Draw a closed polyline representing an arc sector"
  (setq half-arc (/ arc 2.0))
  (setq start-ang (- rotation half-arc))
  (setq end-ang (+ rotation half-arc))
  
  ;; Build points list
  (setq pts (list center))
  (setq step 5.0)  ; 5 degree increments
  (setq n-pts (fix (/ arc step)))
  (if (< n-pts 2) (setq n-pts 2))
  (setq step (/ (float arc) n-pts))
  
  (setq i 0)
  (while (<= i n-pts)
    (setq ang (+ start-ang (* i step)))
    (setq pts (append pts 
                (list (qtech:polar-point center 
                        (qtech:degrees-to-radians ang) radius))))
    (setq i (1+ i))
  )
  
  ;; Draw polyline
  (command "._PLINE")
  (foreach pt pts (command pt))
  (command "_C")  ; Close polyline
)

;;; ----------------------------------------------------------------------------
;;; Coverage Analysis Visualization
;;; ----------------------------------------------------------------------------

(defun qtech:draw-coverage-grid (grid coverage / pt val color)
  "Draw color-coded coverage grid"
  (princ "\nDrawing coverage analysis...")
  (qtech:set-layer "IRR-PATTERN")
  
  (foreach gc coverage
    (setq pt (car gc))
    (setq val (cdr gc))
    
    ;; Color based on coverage value
    (setq color (qtech:coverage-to-color val))
    
    ;; Draw small marker
    (command "._COLOR" color)
    (command "._POINT" pt)
  )
  
  (command "._COLOR" "BYLAYER")
  (princ " Done.\n")
)

(defun qtech:coverage-to-color (coverage / normalized)
  "Convert coverage value to AutoCAD color"
  (setq normalized (qtech:clamp (/ coverage 20.0) 0.0 1.0))
  (cond
    ((< normalized 0.2) 1)   ; Red - under-watered
    ((< normalized 0.4) 30)  ; Orange
    ((< normalized 0.6) 2)   ; Yellow
    ((< normalized 0.8) 3)   ; Green - optimal
    (T 5)                     ; Blue - over-watered
  )
)

;;; ----------------------------------------------------------------------------
;;; Pattern Management
;;; ----------------------------------------------------------------------------

(defun qtech:clear-patterns (/ ss)
  "Clear all spray patterns from IRR-PATTERN layer"
  (princ "\nClearing existing patterns...")
  
  ;; Select all entities on pattern layer
  (setq ss (ssget "_X" '((8 . "IRR-PATTERN"))))
  
  (if ss
    (progn
      (command "._ERASE" ss "")
      (princ (strcat " " (itoa (sslength ss)) " entities removed.\n"))
    )
    (princ " No patterns found.\n")
  )
)

(defun qtech:update-pattern (head-data)
  "Update/redraw pattern for a single head"
  ;; This would require tracking entity handles
  ;; For now, recommend clearing and redrawing all
  (qtech:draw-single-pattern head-data)
)

;;; ----------------------------------------------------------------------------
;;; Pattern Clipping to Boundary
;;; ----------------------------------------------------------------------------

(defun qtech:clip-patterns-to-boundary (boundary-ent / vertices)
  "Clip spray patterns to stay within irrigation boundary"
  ;; Note: This is complex in pure LISP - would typically use TRIM command
  ;; or boundary hatch. For now, we'll implement a visual-only solution.
  
  (setq vertices (qtech:get-pline-vertices boundary-ent))
  
  (princ "\nNote: Pattern clipping to boundary is visual reference only.")
  (princ "\nPatterns extend beyond boundary to show actual coverage.\n")
)

;;; ----------------------------------------------------------------------------
;;; Legend/Key Drawing
;;; ----------------------------------------------------------------------------

(defun qtech:draw-pattern-legend (insert-pt / y-offset)
  "Draw a legend explaining pattern colors/styles"
  (qtech:set-layer "IRR-TEXT")
  (setq y-offset 0)
  
  (command "._TEXT" insert-pt 0.5 0 "SPRAY PATTERN LEGEND")
  (setq y-offset (- y-offset 1.0))
  
  (command "._COLOR" 3)
  (command "._CIRCLE" (list (car insert-pt) (+ (cadr insert-pt) y-offset)) 0.3)
  (command "._COLOR" "BYLAYER")
  (command "._TEXT" (list (+ (car insert-pt) 0.8) (+ (cadr insert-pt) y-offset -0.15)) 
           0.35 0 "Full Circle (360°)")
  
  (setq y-offset (- y-offset 1.0))
  (command "._TEXT" (list (+ (car insert-pt) 0.8) (+ (cadr insert-pt) y-offset -0.15))
           0.35 0 "Half Circle (180°) - Edge heads")
  
  (setq y-offset (- y-offset 1.0))
  (command "._TEXT" (list (+ (car insert-pt) 0.8) (+ (cadr insert-pt) y-offset -0.15))
           0.35 0 "Quarter Circle (90°) - Corner heads")
)

;;; ============================================================================
;;; End of qtech-patterns.lsp
;;; ============================================================================

(princ)

