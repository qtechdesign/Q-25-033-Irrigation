;;; ============================================================================
;;; QTECH IRRIGATION - Zone Management
;;; Automatic and manual zone assignment for sprinkler heads
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; Zone Management Command
;;; ----------------------------------------------------------------------------

(defun c:QIRRZONE (/ mode)
  "Zone management interface"
  
  (if (not *qtech-placements*)
    (progn
      (princ "\nNo sprinklers placed. Run QIRRPLACE first.")
      (princ)
      (exit)
    )
  )
  
  (princ "\n\n=== ZONE MANAGEMENT ===\n")
  (princ "Options:\n")
  (princ "  1. Auto-assign zones (by flow limit)\n")
  (princ "  2. Auto-assign zones (by proximity)\n")
  (princ "  3. Manual zone assignment\n")
  (princ "  4. View zone summary\n")
  (princ "  5. Clear all zones\n")
  (princ "  6. Color-code by zone\n")
  (princ "  7. Cancel\n")
  
  (setq mode (getint "\nSelect option [1-7]: "))
  
  (cond
    ((= mode 1) (qtech:auto-zone-by-flow))
    ((= mode 2) (qtech:auto-zone-by-proximity))
    ((= mode 3) (qtech:manual-zone-assignment))
    ((= mode 4) (qtech:zone-summary))
    ((= mode 5) (qtech:clear-zones))
    ((= mode 6) (qtech:color-code-zones))
    (T (princ "\nCancelled."))
  )
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; Auto-Zone by Flow
;;; ----------------------------------------------------------------------------

(defun qtech:auto-zone-by-flow (/ max-flow heads sorted zone-num current-flow 
                                   current-zone head flow)
  "Automatically assign zones based on maximum flow per zone"
  (setq max-flow (cdr (assoc "max-flow" *qtech-settings*)))
  
  (princ (strcat "\nMax flow per zone: " (rtos max-flow 2 2) " m³/h"))
  (princ "\nAssigning zones by flow constraint...\n")
  
  ;; Sort heads by position (group nearby heads)
  (setq heads (qtech:sort-heads-by-position *qtech-placements*))
  
  (setq zone-num 1)
  (setq current-flow 0.0)
  (setq current-zone '())
  
  (foreach head heads
    (setq flow (cdr (assoc "flow" head)))
    
    ;; Check if adding this head exceeds max flow
    (if (> (+ current-flow flow) max-flow)
      (progn
        ;; Start new zone
        (setq zone-num (1+ zone-num))
        (setq current-flow 0.0)
      )
    )
    
    ;; Assign zone to head
    (qtech:set-head-zone head zone-num)
    (setq current-flow (+ current-flow flow))
  )
  
  (princ (strcat "Created " (itoa zone-num) " zones.\n"))
  (qtech:zone-summary)
)

(defun qtech:sort-heads-by-position (heads)
  "Sort heads by position for zone grouping"
  ;; Sort by Y then X (row-by-row)
  (vl-sort heads
    '(lambda (a b)
       (if (= (cadr (cdr (assoc "position" a)))
              (cadr (cdr (assoc "position" b))))
         (< (car (cdr (assoc "position" a)))
            (car (cdr (assoc "position" b))))
         (< (cadr (cdr (assoc "position" a)))
            (cadr (cdr (assoc "position" b))))
       )
    )
  )
)

;;; ----------------------------------------------------------------------------
;;; Auto-Zone by Proximity
;;; ----------------------------------------------------------------------------

(defun qtech:auto-zone-by-proximity (/ num-zones heads clusters)
  "Assign zones based on spatial proximity (k-means clustering)"
  (princ "\nEnter number of zones [4]: ")
  (setq num-zones (getint))
  (if (not num-zones) (setq num-zones 4))
  
  (princ (strcat "\nGrouping heads into " (itoa num-zones) " zones by proximity...\n"))
  
  ;; Simple clustering by grid sectors
  (setq clusters (qtech:cluster-heads *qtech-placements* num-zones))
  
  ;; Assign zones
  (setq zone-num 1)
  (foreach cluster clusters
    (foreach head cluster
      (qtech:set-head-zone head zone-num)
    )
    (setq zone-num (1+ zone-num))
  )
  
  (princ "Zone assignment complete.\n")
  (qtech:zone-summary)
)

(defun qtech:cluster-heads (heads num-clusters / bounds grid-w grid-h 
                             sectors i j sector-heads)
  "Simple grid-based clustering"
  ;; Get bounding box of all heads
  (setq positions (mapcar '(lambda (h) (cdr (assoc "position" h))) heads))
  (setq bounds (qtech:pline-bounds positions))
  
  ;; Calculate grid dimensions
  (setq grid-cols (ceiling (sqrt num-clusters)))
  (setq grid-rows (ceiling (/ (float num-clusters) grid-cols)))
  
  (setq width (- (car (cadr bounds)) (caar bounds)))
  (setq height (- (cadr (cadr bounds)) (cadar bounds)))
  (setq cell-w (/ width grid-cols))
  (setq cell-h (/ height grid-rows))
  
  ;; Assign heads to grid cells
  (setq sectors '())
  (setq i 0)
  (while (< i num-clusters)
    (setq sectors (append sectors (list '())))
    (setq i (1+ i))
  )
  
  (foreach head heads
    (setq pos (cdr (assoc "position" head)))
    (setq col (min (1- grid-cols) (fix (/ (- (car pos) (caar bounds)) cell-w))))
    (setq row (min (1- grid-rows) (fix (/ (- (cadr pos) (cadar bounds)) cell-h))))
    (setq sector-idx (min (1- num-clusters) (+ (* row grid-cols) col)))
    
    ;; Add to sector
    (setq current-sector (nth sector-idx sectors))
    (setq sectors (qtech:list-set sectors sector-idx 
                    (append current-sector (list head))))
  )
  
  ;; Remove empty sectors
  (vl-remove-if 'null sectors)
)

(defun qtech:list-set (lst idx val / i result)
  "Set element at index in list"
  (setq i 0 result '())
  (foreach item lst
    (if (= i idx)
      (setq result (append result (list val)))
      (setq result (append result (list item)))
    )
    (setq i (1+ i))
  )
  result
)

;;; ----------------------------------------------------------------------------
;;; Manual Zone Assignment
;;; ----------------------------------------------------------------------------

(defun qtech:manual-zone-assignment (/ zone-num ss i ent head)
  "Manually assign heads to zones"
  (princ "\nEnter zone number to assign: ")
  (setq zone-num (getint))
  
  (if zone-num
    (progn
      (princ "\nSelect sprinkler heads to assign to zone ")
      (princ (itoa zone-num))
      (princ ": ")
      
      (setq ss (ssget '((0 . "CIRCLE")(8 . "IRR-SPRINKLER"))))
      
      (if ss
        (progn
          (setq i 0)
          (while (< i (sslength ss))
            (setq ent (ssname ss i))
            ;; Find corresponding head in placements
            (setq head (qtech:find-head-by-position 
                        (cdr (assoc 10 (entget ent)))))
            (if head
              (qtech:set-head-zone head zone-num)
            )
            (setq i (1+ i))
          )
          (princ (strcat "\nAssigned " (itoa (sslength ss)) " heads to zone " 
                        (itoa zone-num) ".\n"))
        )
        (princ "\nNo heads selected.\n")
      )
    )
  )
)

(defun qtech:find-head-by-position (pos / found)
  "Find head data by position"
  (setq found nil)
  (foreach head *qtech-placements*
    (if (and (not found)
             (< (qtech:distance-2d pos (cdr (assoc "position" head))) 0.01))
      (setq found head)
    )
  )
  found
)

(defun qtech:set-head-zone (head zone-num)
  "Set zone for a head (modifies in place)"
  (setq *qtech-placements*
        (subst (subst (cons "zone" zone-num)
                      (assoc "zone" head)
                      head)
               head
               *qtech-placements*))
)

;;; ----------------------------------------------------------------------------
;;; Zone Summary
;;; ----------------------------------------------------------------------------

(defun qtech:zone-summary (/ zones zone-data zone heads total-flow)
  "Display summary of zone assignments"
  (princ "\n═══════════════════════════════════════════════════\n")
  (princ "                    ZONE SUMMARY                    \n")
  (princ "═══════════════════════════════════════════════════\n")
  
  ;; Collect zone data
  (setq zones (qtech:get-unique-zones *qtech-placements*))
  (setq zones (vl-sort zones '<))
  
  (princ "Zone    Heads    Total Flow    Avg Precip    Status\n")
  (princ "                   (m³/h)       (mm/hr)\n")
  (princ "───────────────────────────────────────────────────\n")
  
  (setq grand-total-flow 0.0)
  (setq grand-total-heads 0)
  
  (foreach zone zones
    (if zone
      (progn
        (setq heads (qtech:get-heads-in-zone *qtech-placements* zone))
        (setq total-flow (qtech:list-sum 
                          (mapcar '(lambda (h) (cdr (assoc "flow" h))) heads)))
        (setq avg-precip (qtech:list-avg
                          (mapcar '(lambda (h) (cdr (assoc "precip" h))) heads)))
        
        (setq grand-total-flow (+ grand-total-flow total-flow))
        (setq grand-total-heads (+ grand-total-heads (length heads)))
        
        ;; Check against max flow
        (setq max-flow (cdr (assoc "max-flow" *qtech-settings*)))
        (setq status (if (<= total-flow max-flow) "OK" "OVER"))
        
        (princ (strcat 
          (qtech:string-pad (itoa zone) 8 " ")
          (qtech:string-pad (itoa (length heads)) 9 " ")
          (qtech:string-pad (rtos total-flow 2 3) 14 " ")
          (qtech:string-pad (rtos avg-precip 2 1) 14 " ")
          status
          "\n"))
      )
    )
  )
  
  ;; Show unassigned if any
  (setq unassigned (qtech:get-heads-in-zone *qtech-placements* nil))
  (if unassigned
    (progn
      (setq total-flow (qtech:list-sum 
                        (mapcar '(lambda (h) (cdr (assoc "flow" h))) unassigned)))
      (princ (strcat 
        (qtech:string-pad "N/A" 8 " ")
        (qtech:string-pad (itoa (length unassigned)) 9 " ")
        (qtech:string-pad (rtos total-flow 2 3) 14 " ")
        "-"
        "\n"))
      (setq grand-total-flow (+ grand-total-flow total-flow))
      (setq grand-total-heads (+ grand-total-heads (length unassigned)))
    )
  )
  
  (princ "───────────────────────────────────────────────────\n")
  (princ (strcat 
    (qtech:string-pad "TOTAL" 8 " ")
    (qtech:string-pad (itoa grand-total-heads) 9 " ")
    (qtech:string-pad (rtos grand-total-flow 2 3) 14 " ")
    "\n"))
  (princ "═══════════════════════════════════════════════════\n")
)

(defun qtech:get-unique-zones (heads / zones)
  "Get list of unique zone numbers"
  (setq zones '())
  (foreach head heads
    (setq z (cdr (assoc "zone" head)))
    (if (and z (not (member z zones)))
      (setq zones (append zones (list z)))
    )
  )
  zones
)

(defun qtech:get-heads-in-zone (heads zone-num)
  "Get all heads in a specific zone"
  (vl-remove-if-not
    '(lambda (h) (= (cdr (assoc "zone" h)) zone-num))
    heads
  )
)

;;; ----------------------------------------------------------------------------
;;; Zone Visualization
;;; ----------------------------------------------------------------------------

(defun qtech:color-code-zones (/ zones zone heads color layer-name)
  "Color-code sprinklers by zone assignment"
  (princ "\nColor-coding heads by zone...\n")
  
  (setq zones (qtech:get-unique-zones *qtech-placements*))
  (setq zone-colors '((1 . 30) (2 . 40) (3 . 50) (4 . 60) (5 . 70)
                      (6 . 80) (7 . 90) (8 . 100) (9 . 110) (10 . 120)))
  
  (foreach zone zones
    (if zone
      (progn
        (setq color (cdr (assoc zone zone-colors)))
        (if (not color) (setq color (* zone 10)))
        
        (setq layer-name (strcat "IRR-ZONE-" (itoa zone)))
        (qtech:make-layer layer-name color "Continuous")
        
        (setq heads (qtech:get-heads-in-zone *qtech-placements* zone))
        (foreach head heads
          ;; Would need entity handles to move - for now just report
          (princ (strcat "Zone " (itoa zone) ": " (itoa (length heads)) " heads\n"))
        )
      )
    )
  )
  
  (princ "\nNote: Redraw heads on zone layers for visual distinction.\n")
)

;;; ----------------------------------------------------------------------------
;;; Clear Zones
;;; ----------------------------------------------------------------------------

(defun qtech:clear-zones ()
  "Clear all zone assignments"
  (foreach head *qtech-placements*
    (qtech:set-head-zone head nil)
  )
  (princ "\nAll zone assignments cleared.\n")
)

;;; ----------------------------------------------------------------------------
;;; Zone Validation
;;; ----------------------------------------------------------------------------

(defun qtech:validate-zones (/ zones errors max-flow)
  "Validate zone assignments against constraints"
  (setq errors '())
  (setq max-flow (cdr (assoc "max-flow" *qtech-settings*)))
  (setq zones (qtech:get-unique-zones *qtech-placements*))
  
  (foreach zone zones
    (if zone
      (progn
        (setq heads (qtech:get-heads-in-zone *qtech-placements* zone))
        (setq total-flow (qtech:list-sum 
                          (mapcar '(lambda (h) (cdr (assoc "flow" h))) heads)))
        
        (if (> total-flow max-flow)
          (setq errors (append errors 
                        (list (strcat "Zone " (itoa zone) 
                                     " exceeds max flow: " 
                                     (rtos total-flow 2 2) " > "
                                     (rtos max-flow 2 2) " m³/h"))))
        )
      )
    )
  )
  
  ;; Check for unassigned heads
  (setq unassigned (qtech:get-heads-in-zone *qtech-placements* nil))
  (if unassigned
    (setq errors (append errors 
                  (list (strcat (itoa (length unassigned)) 
                               " heads not assigned to any zone"))))
  )
  
  errors
)

;;; ============================================================================
;;; End of qtech-zones.lsp
;;; ============================================================================

(princ)

